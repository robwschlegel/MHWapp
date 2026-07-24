# MHW_daily_functions.R
# This script houses all of the functions used in "MHW_daily.R"
# This is done to keep everything tidier and easier to read
# 1: Setup the environment
# 2: Extract MHW results functions
# 3: Update OISST data functions
# 4: Update MHW event and category data functions
# 5: Create daily global file functions


# 1: Setup ----------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
# remotes::install_github("robwschlegel/heatwave3@dev", force = TRUE)
suppressPackageStartupMessages({
library(tidyverse)
library(raster)
library(ncdf4)
library(tidync)
library(abind)
library(RCurl)
library(XML)
# library(heatwaveR)
library(heatwave3)
library(future)
library(furrr)
})

# print(paste0("heatwaveR version = ",packageDescription("heatwaveR")$Version))
print(paste0("heatwave3 version = ",packageDescription("heatwave3")$Version))

# NB: no parallel plan is set up here - workers are configured to run this
# same file at startup (see MHW_daily.R's cluster setup), so this file must
# stay free of any plan()/cluster-creation side effects, or every worker
# would try to spin up its own cluster recursively when sourcing it.

# Load metadata
source("metadata/metadata.R")


# 2: Extract MHW results functions ----------------------------------------

# NB: MHW_clim(), MHW_event(), MHW_cat_clim(), and MHW_cat_event() were removed -
# they had zero callers anywhere in the repo. Their nested list-column unnesting
# (splitting rows on odd/even row_number()) matched heatwaveR's old output shape,
# not heatwave3's flat data.frame outputs, so they were pre-migration leftovers.


# 3: Update OISST data functions ------------------------------------------

# Wrapper function to coerce ERDDAP date format to R
NOAA_date <- function(date_string, piece){
  res <- as.Date(as.POSIXct(as.numeric(sapply(strsplit(as.character(date_string), ", "), "[[", piece)),
                            origin = "1970-01-01 00:00:00"))
}

# Find the URLs for all files that need to be downloaded
OISST_url_daily <- function(target_month){
  OISST_url <- paste0(OISST_url_month, target_month,"/")
  OISST_url_get <- getURL(OISST_url)
  OISST_table <- readHTMLTable(OISST_url_get)[[1]] |> 
    rename(files = V1) |> 
    drop_na()
  OISST_new <- slice(OISST_table, which(OISST_table$files == "Parent Directory")+1:nrow(OISST_table)) |> 
    mutate(files = as.character(files)) |> 
    filter(grepl("avhrr", files)) |> 
    mutate(t = lubridate::as_date(sapply(strsplit(files, "[.]"), "[[", 2)),
           full_name = paste0(OISST_url, files)) |> 
    filter(!(files %in% basename(OISST_daily_nc_files)))
  return(OISST_new)
}

# Download any number of desired OISST files and save them locally
OISST_url_daily_save <- function(target_URL){
  
  # Set file destination
  file_year <- substr(sapply(strsplit(target_URL, split = "/"), "[[", 9), 1, 4)
  file_name <- sapply(strsplit(target_URL, split = "/"), "[[", 10)
  file_dest <- paste0("../data/OISST/daily/",file_year,"/",file_name)
  dir.create(paste0("../data/OISST/daily/",file_year), showWarnings = FALSE)
  
  # Check if file already exists and download if needed
  if(file.exists(file_dest)){
    # Intentionally blank
  } else {
    download.file(url = target_URL, method = "libcurl", destfile = file_dest)
  }
  
  # Check if there is a duplicate preliminary data file and remove it if needed
  file_date <- str_remove(sapply(strsplit(file_name, "\\."), "[[", 2), "_preliminary")
  file_folder <- dir(paste0("../data/OISST/daily/",file_year), full.names = T)
  dup_file <- file_folder[grepl(file_date, file_folder)]
  if(length(dup_file) > 1){
    file.remove(dup_file[grepl("preliminary", dup_file)])
  }
  return()
  # rm(target_URL, file_year, file_name, file_dest, file_date, file_folder, dup_file)
}

# Derives the final/prelim date indexes directly from the local daily
# archive's file names, replacing the old final_dates.Rdata/prelim_dates.Rdata
# on-disk indexes. A file tagged "_preliminary" means NOAA has only published
# the preliminary version of that date so far - anything else is final. Uses
# the same "prelim" tag match as final_index/prelim_index in MHW_daily.R.
# NB: defaults to a fresh re-scan of the daily archive folder rather than the
# OISST_daily_nc_files snapshot from metadata.R, so a call after this run's
# downloads reflects the files just added - pass OISST_daily_nc_files
# explicitly when the snapshot from script start is what's wanted instead.
OISST_dates_index <- function(daily_files = dir("../data/OISST/daily", pattern = "oisst-avhrr",
                                                full.names = TRUE, recursive = TRUE)){
  file_stub <- sapply(strsplit(basename(daily_files), "[.]"), "[[", 2)
  is_prelim <- grepl("prelim", basename(daily_files))
  file_date <- as.Date(str_remove(file_stub, "_preliminary"), format = "%Y%m%d")
  list(final_dates = sort(unique(file_date[!is_prelim])),
       prelim_dates = sort(unique(file_date[is_prelim])))
}

# Prepares downloaded OISST data for merging with larger files
# NB: rewritten to use raw ncdf4 instead of tidync - same ~50x per-call
# overhead win as OISST_lon_filter()/OISST_day_read() (benchmarked ~5.8s vs
# ~0.09s per file), verified byte-identical output against the tidync version
OISST_prep <- function(file_name){
  nc <- nc_open(file_name)
  sst <- ncvar_get(nc, "sst", start = c(1,1,1,1), count = c(-1,-1,1,1))  # [lon, lat] matrix
  lon_vals <- as.numeric(nc$dim$lon$vals)
  lat_vals <- as.numeric(nc$dim$lat$vals)
  time_val <- as.numeric(ncvar_get(nc, "time"))
  nc_close(nc)
  data.frame(lon = rep(lon_vals, times = length(lat_vals)),
            lat = rep(lat_vals, each = length(lon_vals)),
            t = as.Date(time_val, origin = "1978-01-01"),
            temp = as.vector(sst))
}

# Function for creating arrays from data.frames
# df <- filter(OISST_step, t == 18413)
# df <- filter(df_dl, t == "2021-03-23")
OISST_acast <- function(df){

  # Ensure correct grid size
  lon_lat_OISST_sub <- lon_lat_OISST |>
    filter(lon == df$lon[1])

  # Round data for massive file size reduction
  df$temp <- round(df$temp, 2)

  # Force grid
  res <- df |>
    right_join(lon_lat_OISST_sub, by = c("lon", "lat")) |>
    arrange(lon, lat)

  # Create array
  res_array <- base::array(res$temp, dim = c(720,1,1))
  dimnames(res_array) <- list(lat = lon_lat_OISST_sub$lat,
                              lon = unique(lon_lat_OISST_sub$lon),
                              t = unique(na.omit(res$t)))
  return(res_array)
}

# Wrapper function for last step before data are entered into NetCDF files
# df <- OISST_final_sub
OISST_temp <- function(df){

  # Filter NA and convert dates to integer
  OISST_step <- df |>
    mutate(temp = ifelse(is.na(temp), NA, temp),
           t = as.integer(t))# |>
    # na.omit() # Breaks data with missing days

  # Acast
  dfa <- OISST_step |>
    mutate(t2 = t) |>
    group_by(t2) |>
    nest() |>
    mutate(data2 = purrr::map(data, OISST_acast)) |>
    dplyr::select(-data)

  # Final form
  dfa_temp <- abind(dfa$data2, along = 3, hier.names = T)
  # dimnames(dfa_temp)
  return(dfa_temp)
}

# Wrapper to help rectangle data from wide to long in `OISST_lon_NetCDF()`
# NB: rewritten to use raw ncdf4 indexed reads instead of tidync's
# hyper_filter()/hyper_tibble() - benchmarked at ~0.02s vs ~1.2s per file for
# pulling one longitude out of a daily file, i.e. tidync's per-call overhead
# (not file I/O) was the dominant cost of the old per-longitude build.
OISST_lon_filter <- function(file_name, lon_slice){
  nc <- nc_open(file_name)
  lon_idx <- which(nc$dim$lon$vals == lon_slice)
  lat_vals <- nc$dim$lat$vals
  time_val <- as.numeric(ncvar_get(nc, "time"))
  sst_vals <- as.numeric(ncvar_get(nc, "sst", start = c(lon_idx, 1, 1, 1), count = c(1, -1, 1, 1)))
  nc_close(nc)
  data.frame(lon = lon_slice, lat = lat_vals,
             t = as.Date(time_val, origin = "1978-01-01"), temp = sst_vals)
}

# Reads one daily file's full global SST grid in a single call. Used by
# OISST_database_build() so each file is only opened/decompressed once for the
# whole 1440-longitude build, instead of once per longitude (1440x redundant
# reads) - benchmarked at ~0.04s per file, barely more than reading just one
# longitude, since the file's [lon,lat] grid is stored as a single HDF5 chunk.
OISST_day_read <- function(file_name){
  nc <- nc_open(file_name)
  sst <- round(ncvar_get(nc, "sst", start = c(1,1,1,1), count = c(-1,-1,1,1)), 2)
  date_val <- as.Date(as.numeric(ncvar_get(nc, "time")), origin = "1978-01-01")
  nc_close(nc)
  list(t = date_val, sst = sst)
}

# Create new OISST lon slice NetCDF
# NB: reads every file in OISST_daily_nc_files (metadata.R), so it must already
# be populated with the full daily archive before this is called
OISST_lon_NetCDF <- function(lon_row, date_max){

  # Determine lon slice
  lon_val <- lon_OISST[lon_row]; lon_val_360 <- lon_val
  if(lon_val_360 < 0) lon_val_360 <- lon_val_360 + 360
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")

  # Set file name
  ncdf_file_name <- paste0("../data/OISST/oisst-avhrr-v02r01.ts.",lon_row_pad,".nc")


  # Compile data ------------------------------------------------------------

  # List of all downloaded daily files
  # OISST_nc_files <- dir("../data/OISST/daily", pattern = "v02r01", recursive = TRUE, full.names = TRUE)

  # Takes a few minutes for ~40 years of data
  # NB: parallelized via furrr/multisession (separate worker processes),
  # never fork-based (doParallel/mclapply) - forking while HDF5 has any state
  # loaded is not safe (a child can inherit an already broken/locked copy of
  # HDF5's internal state, hanging forever on its first netCDF call).
  print(paste0("Began compiling data from 1982-01-01 to ",date_max," at ", Sys.time()," for lon ",lon_val))
  df_dl <- furrr::future_map_dfr(OISST_daily_nc_files, OISST_lon_filter, lon_slice = lon_val_360)


  # Define dimensions -------------------------------------------------------

  # Set the dataframe in question
  dat_base <- df_dl |>
    mutate(t = as.integer(t),
           lon = ifelse(lon > 180, lon-360, lon))

  # lon
  xvals <- unique(dat_base$lon)
  if(length(xvals) > 1) stop("Too many lon values. Should only be one.")
  # xvals_df <- data.frame(lon = lon_lat_OISST$lon)
  nx <- length(xvals)
  lon_def <- ncdim_def("lon", "degrees_east", xvals)

  # lat
  yvals <- unique(lon_lat_OISST$lat)
  # yvals_df <- data.frame(lat = lon_lat_OISST$lat)
  ny <- length(yvals)
  lat_def <- ncdim_def("lat", "degrees_north", yvals)

  # time
  tunits <- "days since 1970-01-01 00:00:00"
  tvals <- seq(min(dat_base$t), max(dat_base$t))
  nt <- length(tvals)
  time_def <- ncdim_def("time", tunits, tvals, unlim = TRUE)


  # Create data arrays ------------------------------------------------------

  print(paste0("Began creating arrays at ", Sys.time()))
  # system.time(
  dfa_temp <- OISST_temp(dat_base)
  # ) # ~4.5 minutes for ~ 40 years of data


  # Define variables --------------------------------------------------------

  temp_def <- ncvar_def(name = "sst", units = "deg_C",
                        dim = list(lat_def, lon_def, time_def),
                        longname = "Sea Surface Temperature",
                        missval = -999, prec = "float")


  # Create NetCDF files -----------------------------------------------------

  # Check if file exists and delete beforehand
  if(file.exists(ncdf_file_name)) file.remove(ncdf_file_name)

  # Create the new file
  ncout <- nc_create(filename = ncdf_file_name, vars = list(temp_def), force_v4 = T)


  # Put variables -----------------------------------------------------------

  ncvar_put(nc = ncout, varid = temp_def, vals = dfa_temp)


  # Additional attributes ---------------------------------------------------

  ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"time","axis","T")


  # close the file, writing data to disk
  nc_close(ncout)
}

# Creates or appends to one longitude's NetCDF file with a batch of
# already-read daily grids (see OISST_database_build()). Array index i in
# 1:1440 maps directly to lon_OISST[i] - verified against the daily files'
# native lon dimension, which runs 0.125 to 359.875 ascending, i.e. the same
# sequence as lon_OISST just with the back half relabelled negative - so no
# lon-value lookup/conversion is needed here, unlike the old per-longitude
# approach which filtered files by value.
# NB: takes lon_slice pre-extracted from the full batch_array by the caller,
# rather than the whole array - with furrr/multisession workers (separate
# processes, no shared memory), passing the full multi-GB batch_array to all
# 1440 tasks would mean re-serializing it repeatedly over sockets; each
# worker only ever needs its own ~KB-MB slice.
# NB: on append, only dates that are both (a) not already present and (b)
# strictly after the file's current last date get written - this makes
# re-running OISST_database_build() over already-completed years a safe
# no-op instead of duplicating data. A file with a gap *before* its current
# end (e.g. a stalled worker left an early year out - see OISST_database_verify())
# can't be fixed by appending, since NetCDF's unlimited time dimension can
# only grow at the end without rewriting the whole file - those cases are
# left alone here and repaired by OISST_database_verify() instead.
OISST_lon_write_batch <- function(lon_row, lon_slice, batch_dates){
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  ncdf_file_name <- paste0("../data/OISST/oisst-avhrr-v02r01.ts.",lon_row_pad,".nc")

  if(!file.exists(ncdf_file_name)){
    tvals <- as.integer(batch_dates)
    lon_def <- ncdim_def("lon", "degrees_east", lon_OISST[lon_row])
    lat_def <- ncdim_def("lat", "degrees_north", lat_OISST)
    time_def <- ncdim_def("time", "days since 1970-01-01 00:00:00", tvals, unlim = TRUE)
    temp_def <- ncvar_def(name = "sst", units = "deg_C",
                          dim = list(lat_def, lon_def, time_def),
                          longname = "Sea Surface Temperature",
                          missval = -999, prec = "float")
    ncout <- nc_create(filename = ncdf_file_name, vars = list(temp_def), force_v4 = TRUE)
    ncvar_put(nc = ncout, varid = temp_def, vals = lon_slice)
    ncatt_put(ncout, "lon", "axis", "X")
    ncatt_put(ncout, "lat", "axis", "Y")
    ncatt_put(ncout, "time", "axis", "T")
    nc_close(ncout)
  } else {
    nc <- nc_open(ncdf_file_name, write = TRUE)
    existing_dates <- as.Date(nc$dim$time$vals, origin = "1970-01-01")
    new_dates <- batch_dates[!(batch_dates %in% existing_dates) & batch_dates > max(existing_dates)]
    if(length(new_dates) > 0){
      new_idx <- match(new_dates, batch_dates)
      tvals <- as.integer(new_dates)
      existing_n <- length(existing_dates)
      ncvar_put(nc = nc, varid = "time", vals = tvals,
                start = existing_n + 1, count = length(tvals))
      ncvar_put(nc = nc, varid = "sst", vals = lon_slice[, new_idx, drop = FALSE],
                start = c(1, 1, existing_n + 1), count = c(720, 1, length(tvals)))
    }
    nc_close(nc)
  }
}

# Checks one longitude's file against the full expected date range and, if
# any dates are missing (whether the file is entirely absent, or short by a
# gap anywhere in its record), deletes and fully rebuilds it from scratch via
# OISST_lon_NetCDF() - simpler and more robust than trying to patch a gap in
# place, since NetCDF's unlimited time dimension can't have dates inserted
# before its current end without rewriting the file anyway.
# lon_row <- 1425; expected_dates <- ...; date_max <- max(final_dates)
OISST_lon_verify <- function(lon_row, expected_dates, date_max){
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  ncdf_file_name <- paste0("../data/OISST/oisst-avhrr-v02r01.ts.",lon_row_pad,".nc")

  complete <- FALSE
  if(file.exists(ncdf_file_name)){
    nc <- nc_open(ncdf_file_name)
    existing_dates <- as.Date(nc$dim$time$vals, origin = "1970-01-01")
    nc_close(nc)
    complete <- all(expected_dates %in% existing_dates)
  }

  if(!complete){
    if(file.exists(ncdf_file_name)) file.remove(ncdf_file_name)
    OISST_lon_NetCDF(lon_row, date_max)
  }
  return(!complete)
}

# Checks all 1440 OISST lon-slice files against the full daily archive and
# rebuilds (from scratch) any that are missing dates. Makes it safe to just
# re-run OISST_database_build() any time - this is called automatically at
# the end of that function, but can also be run on its own.
# NB: deliberately sequential, not parallel (plain purrr, not furrr). The
# check itself (just opening each file and reading its time dimension) is
# cheap enough not to need it (~1440 quick opens), and any repairs run
# through OISST_lon_NetCDF(), which already parallelizes internally across
# the daily archive for a single longitude - there's no need to also
# parallelize this outer loop on top of that, since breakages are expected to
# be rare and few.
# date_max <- max(final_dates)
OISST_database_verify <- function(date_max){
  file_dates <- as.Date(str_remove(sapply(strsplit(basename(OISST_daily_nc_files), "[.]"), "[[", 2),
                                   "_preliminary"), format = "%Y%m%d")
  expected_dates <- sort(unique(file_dates[file_dates <= as.Date(date_max)]))

  print(paste0("Checking all 1440 files for missing dates at ", Sys.time()))
  needs_rebuild <- purrr::map_lgl(1:1440, OISST_lon_verify,
                                  expected_dates = expected_dates, date_max = date_max)

  bad_lons <- which(needs_rebuild)
  if(length(bad_lons) == 0){
    print("All 1440 files are complete - nothing to repair.")
  } else {
    print(paste0(length(bad_lons)," of 1440 files were incomplete and have been rebuilt: ",
                paste(bad_lons, collapse = ", ")))
  }
  return(invisible(bad_lons))
}

# Builds the entire 1440-file OISST database from scratch in ONE pass over the
# daily archive, instead of the old approach of looping over each of the 1440
# longitudes and re-reading (and re-decompressing) the whole archive each time
# - that redundancy, not lack of parallelism, was what made the first attempt
# at this look like it would take weeks. Each daily file is now read exactly
# once (via OISST_day_read()) and scattered out to all 1440 per-longitude
# files (via OISST_lon_write_batch()) in yearly batches, so at most 1440 file
# handles are ever open at a time (and only briefly, once per batch) rather
# than reopening every file 1440 times or holding 1440 handles open at once.
# NB: safe to re-run any time. Batch years already fully present (checked
# cheaply against a single representative file, lon_row 1) are skipped
# instead of being re-read and re-written, and OISST_database_verify() runs
# at the end to catch and rebuild any individual longitude left short by a
# stalled/crashed worker in an earlier run (see the fork/HDF5-hang note on
# OISST_lon_write_batch()) - a representative-file check only catches gaps
# shared by (almost) every longitude, not a lone straggler.
# date_max <- max(final_dates); batch_years <- 1
OISST_database_build <- function(date_max, batch_years = 1){

  file_dates <- as.Date(str_remove(sapply(strsplit(basename(OISST_daily_nc_files), "[.]"), "[[", 2),
                                   "_preliminary"), format = "%Y%m%d")
  yr_end <- lubridate::year(as.Date(date_max))

  for(yr in seq(1982, yr_end, by = batch_years)){
    yr_range <- yr:min(yr + batch_years - 1, yr_end)
    yr_dates <- file_dates[lubridate::year(file_dates) %in% yr_range]
    batch_files <- OISST_daily_nc_files[lubridate::year(file_dates) %in% yr_range]
    if(length(batch_files) == 0) next

    # Skip this batch entirely if a representative file already has every
    # date it would contribute - avoids re-reading/re-writing years that are
    # already done on a re-run
    rep_file <- "../data/OISST/oisst-avhrr-v02r01.ts.0001.nc"
    if(file.exists(rep_file)){
      rep_nc <- nc_open(rep_file)
      rep_dates <- as.Date(rep_nc$dim$time$vals, origin = "1970-01-01")
      nc_close(rep_nc)
      if(all(sort(unique(yr_dates)) %in% rep_dates)){
        print(paste0("Batch ",min(yr_range),"-",max(yr_range)," already complete - skipping"))
        next
      }
    }

    batch_order <- order(yr_dates)
    batch_files <- batch_files[batch_order]

    print(paste0("Reading batch ",min(yr_range),"-",max(yr_range),
                " (",length(batch_files)," files) at ", Sys.time()))
    # NB: parallelized via furrr/multisession - each file is independent, no
    # shared state, so this is a clean parallelization win
    batch_data <- furrr::future_map(batch_files, OISST_day_read)

    batch_array <- array(NA_real_, dim = c(1440, 720, length(batch_data)))
    for(i in seq_along(batch_data)) batch_array[,,i] <- batch_data[[i]]$sst
    batch_dates <- do.call(c, lapply(batch_data, `[[`, "t"))
    rm(batch_data); gc()

    print(paste0("Writing batch ",min(yr_range),"-",max(yr_range)," at ", Sys.time()))
    # NB: pre-slice batch_array into one small piece per longitude before
    # dispatching, rather than handing the whole (multi-GB) array to every
    # task - see OISST_lon_write_batch() for why
    lon_slices <- purrr::map(1:1440, ~ batch_array[.x,,])
    furrr::future_walk2(1:1440, lon_slices, OISST_lon_write_batch,
                        batch_dates = batch_dates)
    rm(batch_array, lon_slices); gc()
    print(paste0("Finished batch ",min(yr_range),"-",max(yr_range)," at ", Sys.time()))
  }

  OISST_database_verify(date_max)
}

# Function for merging OISST data into existing NetCDF files
# tester...
# lon_step <- lon_OISST[1]
# lon_step <- -60.875
# df <- OISST_dat
OISST_merge <- function(lon_step, df){

  ### Determine the correct lon slice/file
  # Determine lon slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  # Determine file name
  ncdf_file_name <- paste0("../data/OISST/oisst-avhrr-v02r01.ts.",lon_row_pad,".nc")

  ### Open NetCDF and determine dates present
  nc <- nc_open(ncdf_file_name, write = T)
  time_vals <- as.Date(nc$dim$time$vals, origin = "1970-01-01")
  # tail(time_vals)

  ### Grab the lon slice intended for the chosen NetCDF file
  OISST_prelim_sub <- df |>
    filter(lon  == lon_step,
           t > max(time_vals))
  OISST_final_sub <- df |>
    filter(lon  == lon_step,
           t <= max(time_vals))

  ### Check again for errors in the data
  if(nrow(OISST_final_sub) > 1){
    if(max(OISST_final_sub$temp, na.rm = T) > 100){
      stop(paste0("There are errors in the final OISST data for ",ncdf_file_name))
    }
  }
  if(nrow(OISST_prelim_sub) > 1){
    if(max(OISST_prelim_sub$temp, na.rm = T) > 100){
      stop(paste0("There are errors in the prelim OISST data for ",ncdf_file_name))
    }
  }

  ### Create data arrays and insert into NetCDF file
  if(nrow(OISST_prelim_sub) > 0){

    prelim_temp <- OISST_temp(OISST_prelim_sub)

    for(i in 1:length(prelim_temp[1,1,])){
      ncvar_put(nc = nc, varid = "sst", vals = prelim_temp[,,i], verbose = FALSE,
                start = c(1,1,(length(nc$dim$time$vals)+i)), count = c(720,1,1))
      ncvar_put(nc = nc, varid = "time", vals = as.integer(dimnames(prelim_temp)[[3]])[i],
                start = (length(nc$dim$time$vals)+i), verbose = FALSE)
      nc_sync(nc)
    }
  }

  if(nrow(OISST_final_sub) > 0){

    final_temp <- OISST_temp(OISST_final_sub)

    for(i in 1:length(final_temp[1,1,])){
      date_put <- which(nc$dim$time$vals == as.integer(dimnames(final_temp)[[3]])[i])
      ncvar_put(nc = nc, varid = "sst", vals = final_temp[,,i], verbose = FALSE,
                start = c(1,1,date_put), count = c(720,1,1))
      nc_sync(nc)
    }
  }
  # sst <- ncvar_get(nc, "sst")
  # tail(as.Date(nc$dim$time$vals, origin = "1970-01-01"))

  ### Close file and exit
  nc_close(nc)
}


# 4: Update MHW event and category data functions -------------------------

# Function that is used to create the seas + thresh files
# lon_row <- 1; base_years <- c(1982, 2011)
create_thresh <- function(lon_row, base_years){

  # Set baseline and pad lon_row for file name
  base_line <- c(paste0(base_years[1],"-01-01"), paste0(base_years[2],"-12-31"))
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")

  # File names
  file_name_MHW <- paste0("../data/thresh/MHW_",lon_row_pad,"_",base_years[1],"-",base_years[2])
  file_name_MCS <- paste0("../data/thresh/MCS/MCS_",lon_row_pad,"_",base_years[1],"-",base_years[2])

  # Calculate MHW clims
  if(!file.exists(paste0(file_name_MHW, "_clim.nc"))){
    heatwave3::ts2clm3(
      file_in = OISST_files[lon_row],
      name = file_name_MHW,
      climatologyPeriod = base_line,
      n_threads = 1,
      quiet = TRUE
    ) # ~2 seconds
  }

  # Calculate MCS clims
  if(!file.exists(paste0(file_name_MCS, "_clim.nc"))){
    heatwave3::ts2clm3(
      file_in = OISST_files[lon_row],
      name = file_name_MCS,
      climatologyPeriod = base_line,
      n_threads = 1,
      pctile = 10,
      quiet = TRUE
    ) # ~2 seconds
  }

  return()
  }

# Function that loads and merges sst/seas/thresh for a given lon_step
# lon_step <- lon_OISST[2]
# lat_range <- c(82.125, 82.875)
# date_range <- as.Date("2020-01-01")
# date_range <- as.Date("1982-01-01")
# date_range <- c(as.Date("2016-02-01"), as.Date("2017-04-01"))
sst_seas_thresh_merge <- function(lon_step, date_range,  base_years = c(1982, 2011), lat_range = NULL){
  
  # NB: If this needs to be faster, it could be replaced with category_daily3()
  
  # Establish lon row number
  lon_row <- which(lon_OISST == lon_step)
  
  # Establish date range
  if(length(date_range) == 1) date_range <- c(date_range, Sys.Date())
  
  # OISST data
  ## Base
  if(is.null(lat_range[1])){
    tidync_OISST_base <- tidync(OISST_files[lon_row]) |> 
      hyper_filter(time = between(time, as.integer(date_range[1]), as.integer(date_range[2]))) |> 
      hyper_tibble(na.rm = FALSE, force = TRUE, drop = FALSE) |> 
      mutate(lon = as.numeric(lon),
             lat = as.numeric(lat)) 
  } else if(length(lat_range) == 2){
    lat_row_1 <- which(lat_OISST == lat_range[1])
    lat_row_2 <- which(lat_OISST == lat_range[2])
    tidync_OISST_base <- tidync(OISST_files[lon_row]) |>  
      hyper_filter(time = between(time, as.integer(date_range[1]), as.integer(date_range[2])),
                   # lat = between(lat, as.integer(lat_row_1), as.integer(lat_row_2))) |> 
                   lat = between(lat, lat_range[1], lat_range[2])) |> 
      hyper_tibble(na.rm = FALSE, force = TRUE, drop = FALSE) |> 
      mutate(lon = as.numeric(lon),
             lat = as.numeric(lat)) 
  } else {
    stop()
  }
  
  ## Process
  tidync_OISST <- tidync_OISST_base |> 
    mutate(time = as.Date(time),
           year = year(time)) |> 
    dplyr::rename(t = time, temp = sst) |>
    mutate(doy = yday(t)) |> 
    group_by(year) |> 
    mutate(doy = ifelse(!leap_year(year),
                        ifelse(doy > 59, doy+1, doy), doy)) |> 
    ungroup() |>
    dplyr::select(lon, lat, t, doy, temp)
  
  # Load MHW and MCS
  base_years_text <- paste0(base_years[1],"-", base_years[2])
  MHW_seas_thresh_files_base <- str_subset(MHW_seas_thresh_files, base_years_text)
  MCS_seas_thresh_files_base <- str_subset(MCS_seas_thresh_files, base_years_text)  
  seas_base_MHW <- tidync(MHW_seas_thresh_files_base[lon_row]) |> 
    hyper_tibble(drop = FALSE) |> 
    mutate(doy = as.numeric(doy),
           lon = as.numeric(lon),
           lat = as.numeric(lat)) 
  seas_base_MCS <- tidync(MCS_seas_thresh_files_base[lon_row]) |> 
    hyper_tibble(drop = FALSE) |> 
    mutate(doy = as.numeric(doy),
           lon = as.numeric(lon),
           lat = as.numeric(lat)) 
  
  # Merge to seas/thresh and exit
  sst_seas_thresh <- tidync_OISST |>
    left_join(seas_base_MHW, by = c("lon", "lat", "doy")) |>
    left_join(seas_base_MCS, by = c("lon", "lat", "doy")) |>
    dplyr::select(-seas.y) |> 
    dplyr::rename(seas = seas.x, thresh_MHW = thresh.x, thresh_MCS = thresh.y) |> 
    mutate(anom = round(temp - seas, 2))
  rm(tidync_OISST_base, tidync_OISST); gc()
  return(sst_seas_thresh)
}

# A single function to run the daily calculations. Much less complicated.
event_cat_calc <- function(lon_row, base_years = c(1982, 2011)){

  # Set baseline and pad lon_row for file name
  base_line <- c(paste0(base_years[1],"-01-01"), paste0(base_years[2],"-12-31"))
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")

  # Clim file names
  file_clim_MHW <- paste0("../data/thresh/MHW_",lon_row_pad,"_",base_years[1],"-",base_years[2],"_clim.nc")
  file_clim_MCS <- paste0("../data/thresh/MCS/MCS_",lon_row_pad,"_",base_years[1],"-",base_years[2],"_clim.nc")

  # Event file names
  # NB: When moving from heatwaveR to heatwave3
  # All event and categories are calculated into the same .nc files
  # So there is no longer a "../data/cat_lon/" output
  file_event_MHW <- paste0("../data/event/MHW_",lon_row_pad,"_",base_years[1],"-",base_years[2])
  file_event_MCS <- paste0("../data/event/MCS/MCS_",lon_row_pad,"_",base_years[1],"-",base_years[2])

  # Calculate MHW events + cats
  heatwave3::detect_event3(
    file_in = OISST_files[lon_row],
    name = file_event_MHW,
    clim_file = file_clim_MHW,
    category = TRUE,
    n_threads = 1,
    quiet = TRUE
  ) # ~1 seconds

  # Calculate MCS clims
  heatwave3::detect_event3(
    file_in = OISST_files[lon_row],
    name = file_event_MCS,
    clim_file = file_clim_MCS,
    coldSpells = TRUE,
    category = TRUE,
    n_threads = 1,
    quiet = TRUE
  )

  return()
}


# 5: Create daily global file functions -----------------------------------

# Function for loading a cat_lon slice and extracting a single day of values
# testers...
# lon_step <- 1
# date_range <- c(as.Date("2026-05-15"), as.Date("2026-05-31"))
# base_years <- c(1982, 2011); MHW = TRUE
load_sub_cat_clim <- function(lon_step, date_range, base_years, MHW = TRUE){

  # Create text label
  base_years_text <- paste0(base_years[1],"-", base_years[2])

  # Get correct baseline files
  MHW_seas_thresh_files_base <- str_subset(MHW_seas_thresh_files, base_years_text)
  MCS_seas_thresh_files_base <- str_subset(MCS_seas_thresh_files, base_years_text)
  MHW_event_files_base <- str_subset(MHW_event_files, base_years_text)
  MCS_event_files_base <- str_subset(MCS_event_files, base_years_text)

  # Calculate daily values
  if(MHW){
    cat_daily <- category_daily3(
      sst_file = OISST_files[lon_step],
      clim_file = MHW_seas_thresh_files_base[lon_step],
      event_file =  MHW_event_files_base[lon_step],
      time_range = date_range
      ) |> filter(category >= 1)
  } else {
    cat_daily <- category_daily3(
      sst_file = OISST_files[lon_step],
      clim_file = MCS_seas_thresh_files_base[lon_step],
      event_file =  MCS_event_files_base[lon_step],
      time_range = date_range,
      coldSpells = TRUE
    ) |> filter(category >= 1)
  }
  return(cat_daily)
}

# Function for saving daily global cat files
# date_choice <- max(current_dates)+1
# date_choice <- as.Date("2025-08-25")
save_sub_cat_clim <- function(date_choice, df, event_type, base_years){
  
  # Create text label
  base_years_text <- paste0(base_years[1],"-", base_years[2])
  
  # Establish file name and save location
  cat_clim_year <- lubridate::year(date_choice)
  if(event_type == "MCS"){
    cat_clim_dir <- paste0("../data/cat_clim/MCS/",cat_clim_year)
    cat_clim_name <- paste0("cat.clim.MCS.",date_choice,".",base_years_text,".Rds")
    cat_rast_name <- paste0("cat.clim.MCS.",date_choice,".",base_years_text,".tif")
  } else {
    cat_clim_dir <- paste0("../data/cat_clim/",cat_clim_year)
    cat_clim_name <- paste0("cat.clim.",date_choice,".",base_years_text,".Rda")
    cat_rast_name <- paste0("cat.clim.",date_choice,".",base_years_text,".tif")
  }
  dir.create(as.character(cat_clim_dir), showWarnings = F)
  
  # Extract data and save
  df_sub <- filter(df, t == date_choice)
  saveRDS(df_sub, file = paste0(cat_clim_dir,"/",cat_clim_name))
  
  # Project raster to EPSG:3857 (shiny/leaflet) and save
  df_rast <- dplyr::select(df_sub, lon, lat, category)
  colnames(df_rast) <- c("X", "Y", "Z")
  df_rast$Z <- as.integer(df_rast$Z)
  rasterNonProj <- raster::rasterFromXYZ(df_rast, res = c(0.25, 0.25),
                                         digits = 3, crs = "EPSG:4326")
  
  # The next step is for the future. Requires new leaflet workflow...
  # rasterNonProj <- terra::rast(MHW_raster, digits = 3, crs = inputProj)
  # NB: EPSG:3857 projection behaving strangely after update on 2025-11-17; leaflet v2.2.3
  rasterProj <- raster::projectRaster(rasterNonProj, crs = "EPSG:3857", method = "ngb")
  rasterProj <- raster::crop(rasterProj, raster::extent(-20037508, 20037508, -14642047, 16800000))
  raster::writeRaster(rasterProj, format = "GTiff", overwrite = TRUE,
                      filename = paste0(cat_clim_dir,"/",cat_rast_name))
}

# Function for loading, prepping, and saving the daily global category slices
# date_range <- c(as.Date("2026-05-20"), as.Date("2026-05-26")); base_years = c(1982, 2011)
cat_clim_global_daily <- function(date_range, base_years = c(1982, 2011)){

  # Extract data
  # system.time(
  MHW_cat_clim_daily <- furrr::future_map_dfr(1:1440, load_sub_cat_clim,
                                              date_range = date_range, base_years = base_years, MHW = TRUE,
                                              .options = furrr::furrr_options(seed = TRUE, conditions = character())) |>
    mutate(category = factor(category, levels = 1:4,
                             labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")))
  # ) # 1 second per cycle. ~7 seconds for all.
  MCS_cat_clim_daily <- furrr::future_map_dfr(1:1440, load_sub_cat_clim,
                                              date_range = date_range, base_years = base_years, MHW = FALSE,
                                              .options = furrr::furrr_options(seed = TRUE, conditions = character())) |>
    mutate(category = factor(category, levels = 1:5,
                             labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme", "V Ice")))

  # Save data as .Rda and as rasters projected to the shiny EPSG:3857
  # NB: Running this on too many cores may cause RAM issues
  furrr::future_walk(seq(min(MHW_cat_clim_daily$t), max(MHW_cat_clim_daily$t), by = "day"),
                     save_sub_cat_clim, df = MHW_cat_clim_daily,
                     event_type = "MHW", base_years = base_years,
                     .options = furrr::furrr_options(seed = TRUE, conditions = character()))
  rm(MHW_cat_clim_daily); gc()
  furrr::future_walk(seq(min(MCS_cat_clim_daily$t), max(MCS_cat_clim_daily$t), by = "day"),
                     save_sub_cat_clim, df = MCS_cat_clim_daily,
                     event_type = "MCS", base_years = base_years,
                     .options = furrr::furrr_options(seed = TRUE, conditions = character()))
  rm(MCS_cat_clim_daily); gc()
  return()
}

