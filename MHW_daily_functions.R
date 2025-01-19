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
suppressPackageStartupMessages({
library(tidyverse)
library(terra)
library(raster)
library(leaflet)
library(ncdf4)
library(tidync)
library(abind)
library(padr)
library(RCurl)
library(XML)
library(threadr)
library(heatwaveR)
library(doParallel)
})

print(paste0("heatwaveR version = ",packageDescription("heatwaveR")$Version))
registerDoParallel(cores = 25)

# Load metadata
source("metadata/metadata.R")


# 2: Extract MHW results functions ----------------------------------------

# Pull out climatologies
MHW_clim <- function(df){
  clim <- df |> 
    unnest(event) |> 
    filter(row_number() %% 2 == 1) |> 
    unnest(event)
}

# Pull out events
MHW_event <- function(df){
  event <- df |> 
    unnest(event) |> 
    filter(row_number() %% 2 == 0) |> 
    unnest(event)
}

# Pull out category climatologies
MHW_cat_clim <- function(df, long = FALSE){
  cat_clim <- df |> 
    unnest(cat) |> 
    filter(row_number() %% 2 == 1) |> 
    unnest(cat)
  if(long){
    cat_clim_long <- cat_clim |> 
      group_by(lon, lat) |>
      nest() |>
      mutate(long = map(data, pad, interval = "day", 
                        start_val = as.Date("1982-01-01"))) |> 
      dplyr::select(-data) |>
      unnest()
  } else {
    return(cat_clim)
  }
}

# Pull out event category summaries
MHW_cat_event <- function(df){
  suppressWarnings(
    cat_event <- df |> 
      unnest(cat) |> 
      filter(row_number() %% 2 == 0) |> 
      unnest(cat)
  )
}


# 3: Update OISST data functions ------------------------------------------

# Wrapper function to coerce ERDDAP date format to R
NOAA_date <- function(date_string, piece){
  res <- as.Date(as.POSIXct(as.numeric(sapply(strsplit(as.character(date_string), ", "), "[[", piece)),
                            origin = "1970-01-01 00:00:00"))
}

# Find the URLs for all files that need to be downloaded
OISST_url_daily <- function(target_month, final_dates){
  OISST_url <- paste0(OISST_url_month, target_month,"/")
  OISST_url_get <- getURL(OISST_url)
  OISST_table <- data.frame(files = readHTMLTable(OISST_url_get, skip.rows = 1:2)[[1]]$Name) |> 
    mutate(files = as.character(files)) |> 
    filter(grepl("avhrr", files)) |> 
    mutate(t = lubridate::as_date(sapply(strsplit(files, "[.]"), "[[", 2)),
           full_name = paste0(OISST_url, files)) |> 
    filter(t > max(final_dates))
  return(OISST_table)
}


# Download any number of desired OISST files and save them locally
OISST_url_daily_save <- function(target_URL){
  
  # Set file destination
  file_year <- substr(sapply(strsplit(target_URL, split = "/"), "[[", 9), 1, 4)
  file_name <- sapply(strsplit(target_URL, split = "/"), "[[", 10)
  file_dest <- paste0("../data/OISST/daily/",file_year,"/",file_name)
  dir.create(paste0("../data/OISST/daily/",file_year))
  
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

# Prepares downloaded OISST data for merging with larger files
OISST_prep <- function(file_name){
  tidync(file_name) |>  
    hyper_tibble(na.rm = FALSE, force = TRUE, drop = FALSE) |> 
    dplyr::select(lon, lat, time, sst) |> 
    dplyr::rename(t = time, temp = sst) |>  
    mutate(t = as.Date(t, origin = "1978-01-01"),
           lon = as.numeric(lon),
           lat = as.numeric(lat))
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
OISST_lon_filter <- function(file_name, lon_slice){
  tidync(file_name) |>  
    hyper_filter(lon = lon == lon_slice) |> 
    hyper_tibble(na.rm = FALSE, force = TRUE, drop = FALSE) |> 
    dplyr::select(lon, lat, time, sst) |> 
    dplyr::rename(t = time, temp = sst) |>  
    mutate(t = as.Date(t, origin = "1978-01-01"),
           lon = as.numeric(lon),
           lat = as.numeric(lat))
}

# Create new OISST lon slice NetCDF
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
  print(paste0("Began compiling data from 1982-01-01 to ",date_max," at ", Sys.time()," for lon ",lon_val))
  # doParallel::registerDoParallel(cores = 50)
  # system.time(
  df_dl <- plyr::ldply(OISST_daily_nc_files, OISST_lon_filter, .parallel = T, lon_slice = lon_val_360)
  # ) # ~4 minutes for ~40 years of data
  
  
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
  
  
  # Global attributes -------------------------------------------------------
  
  # ncatt_put(ncout,0,"title",title$value)
  # ncatt_put(ncout,0,"institution",institution$value)
  # ncatt_put(ncout,0,"source",datasource$value)
  # ncatt_put(ncout,0,"references",references$value)
  # history <- paste("P.J. Bartlein", date(), sep=", ")
  # ncatt_put(ncout,0,"history",history)
  # ncatt_put(ncout,0,"Conventions",Conventions$value)
  
  # close the file, writing data to disk
  nc_close(ncout)
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
  # tester...
  # ncdf_file_name <- paste0("../data/test/oisst-avhrr-v02r01.ts.",lon_row_pad,".nc")
  
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
# lon_row <- 994; base_years <- c(1991, 2020)
create_thresh <- function(lon_row, base_years){
  
  # TODO: Investigate why lon_row 994 is missing dates
  ## Tried recreating NetCDF file but throwing unexpected errors
  
  # Load SST
  sst_lon <- tidync(OISST_files[lon_row]) |> 
    hyper_tibble(na.rm = FALSE, force = TRUE, drop = FALSE) |> 
    mutate(time = as.Date(time, origin = "1970-01-01"),
           lon = as.numeric(lon),
           lat = as.numeric(lat)) |>  
    dplyr::rename(t = time, temp = sst) |> 
    filter(!is.na(t)) # NB: Remove this once the 994 issue is resolved
  
  # Set baseline and pad lon_row for file name
  base_line <- c(paste0(base_years[1],"-01-01"), paste0(base_years[2],"-12-31"))
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  # MHW
  # system.time(
  MHW_thresh <- sst_lon |>
    group_by(lon, lat) |> nest() |>
    mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = base_line)) |> 
    dplyr::select(-data) |> unnest(clim) |>
    dplyr::select(lon, lat, doy, seas, thresh) |> 
    distinct() |> arrange(lon, lat, doy) |> 
    mutate(seas = round(seas, 2), thresh = round(thresh, 2))
  # ) # ~50 seconds for one lon column
  
  # system.time(
  MCS_thresh <- sst_lon |>
    group_by(lon, lat) |> nest() |>
    mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = base_line, pctile = 10)) |> 
    dplyr::select(-data) |> unnest(clim) |> 
    dplyr::select(lon, lat, doy, seas, thresh) |> 
    distinct() |> arrange(lon, lat, doy) |> 
    mutate(seas = round(seas, 2), thresh = round(thresh, 2))
  # ) # ~50 seconds for one lon column
  
  # Save and exit
  saveRDS(MHW_thresh, paste0("../data/thresh/MHW.seas.thresh.", lon_row_pad,"_",base_years[1],"-",base_years[2],".Rds"))
  saveRDS(MCS_thresh, paste0("../data/thresh/MCS/MCS.seas.thresh.", lon_row_pad,"_",base_years[1],"-",base_years[2],".Rds"))
  return()
}

# Function that loads and merges sst/seas/thresh for a given lon_step
# lon_step <- lon_OISST[2]
# lat_range <- c(82.125, 82.875)
# date_range <- as.Date("2020-01-01")
# date_range <- as.Date("1982-01-01")
# date_range <- c(as.Date("2016-02-01"), as.Date("2017-04-01"))
sst_seas_thresh_merge <- function(lon_step, date_range, lat_range = NULL){
  
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
      hyper_tibble(na.rm = FALSE, orce = TRUE, drop = FALSE) |> 
      mutate(lon = as.numeric(lon),
             lat = as.numeric(lat)) 
  } else {
    stop()
  }
  ## Process
  tidync_OISST <- tidync_OISST_base |> 
    mutate(time = as.Date(time, origin = "1970-01-01"),
           year = year(time)) |> 
    dplyr::rename(t = time, temp = sst) |>
    mutate(doy = yday(t)) |> 
    group_by(year) |> 
    mutate(doy = ifelse(!leap_year(year),
                        ifelse(doy > 59, doy+1, doy), doy)) |> 
    ungroup() |>
    dplyr::select(lon, lat, t, doy, temp)
  
  # Merge to seas/thresh and exit
  sst_seas_thresh <- tidync_OISST |>
    left_join(hyper_tibble(tidync(MHW_seas_thresh_files[lon_row])),
              by = c("lon", "lat", "doy" = "time")) |>
    left_join(readRDS(MCS_seas_thresh_files[lon_row]),
              by = c("lon", "lat", "doy" = "time")) |>
    dplyr::select(-seas.y) |> 
    dplyr::rename(seas = seas.x, thresh_MHW = thresh.x, thresh_MCS = thresh.y) |> 
    mutate(anom = round(temp - seas, 2))
  rm(tidync_OISST_base, tidync_OISST); gc()
  return(sst_seas_thresh)
}

# Function for updating the MHW event metric lon slice files
# tester...
# lon_step <- lon_OISST[1132]; full = TRUE
event_cat_update <- function(lon_step, full = FALSE){
  
  # load the final download date
  if(!exists("final_dates")) load("metadata/final_dates.Rdata")
  
  # Determine correct lon/row/slice
  lon_row <- which(lon_OISST == lon_step)
  # lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  # Begin the calculations
  # print(paste0("Began run on ",MHW_event_files[lon_row]," at ",Sys.time()))
  
  # Load current lon slice for event/category
  if(full){
    MHW_event_data <- data.frame()
    MCS_event_data <- data.frame()
    MHW_cat_lon <- data.frame()
    MCS_cat_lon <- data.frame()
    MHW_previous_event_index <- data.frame(lon = lon_step, lat = lat_OISST,
                                           event_no = 0, date_end = as.Date("1982-01-01"))
    MCS_previous_event_index <- data.frame(lon = lon_step, lat = lat_OISST,
                                           event_no = 0, date_end = as.Date("1982-01-01"))
  } else {
    MHW_event_data <- na.omit(readRDS(MHW_event_files[lon_row]))
    MCS_event_data <- na.omit(readRDS(MCS_event_files[lon_row]))
    if(MHW_event_data$lon[1] != lon_step) stop(paste0("The lon_row indexing has broken down somewhere for ",lon_row_pad))
    MHW_cat_lon <- na.omit(readRDS(MHW_cat_lon_files[lon_row]))
    MCS_cat_lon <- na.omit(readRDS(MCS_cat_lon_files[lon_row]))
    if(MHW_cat_lon$lon[1] != lon_step) stop(paste0("The lon_row indexing has broken down somewhere for ",lon_row_pad))
    
    # Determine how far back in time to get old data based on the occurrence of previous MHWs
    # The problem here is that sometimes the prelim data will dip below the threshold long enough
    # that the MHW will be considered finished and the code would incorrectly begin calculating another MHW
    # To correct for this we must use the final_dates R object, which is tied to the OISST downloading
    # We then go back one event before the final data end date to ensure we are not missing anything
    MHW_previous_event_index <- MHW_event_data |> 
      group_by(lon, lat) |> 
      filter(date_start < max(final_dates)) |>
      filter(event_no == max(event_no)-2)
    MCS_previous_event_index <- MCS_event_data |> 
      group_by(lon, lat) |> 
      filter(date_start < max(final_dates)) |>
      filter(event_no == max(event_no)-2)
  }
  
  # Extract each pixel time series based on how far back the oldest event occurred for the entire longitude slice
  # Or calculate events for the full time series
  if(full){
    sst_seas_thresh <- sst_seas_thresh_merge(lon_step,
                                             date_range = as.Date("1982-01-01"))
  } else {
    sst_seas_thresh <- sst_seas_thresh_merge(lon_step, 
                                             date_range = min(c(min(MHW_previous_event_index$date_start),
                                                                min(MCS_previous_event_index$date_start))))
  }
  
  # Detect and save new event metrics with new data for MHWs as necessary
  # system.time(
  event_proc(MHW_previous_event_index, sst_seas_thresh, MHW_event_data, MHW_cat_lon, 
             MHW_event_files[lon_row], MHW_cat_lon_files[lon_row], full)
  # ) # 66 seconds for 478 pixels
  # Detect and save new event metrics with new data for MCSs as necessary
  # system.time(
  event_proc(MCS_previous_event_index, sst_seas_thresh, MCS_event_data, MCS_cat_lon, 
             MCS_event_files[lon_row], MCS_cat_lon_files[lon_row], full, cold_choice = T)
  # ) # 102 seconds for 478 pixels
  rm(sst_seas_thresh, MHW_previous_event_index, MHW_event_data, MHW_cat_lon,
     MCS_previous_event_index, MCS_event_data, MCS_cat_lon); gc()
  # return()
}

# Wrapper function to detect events and save the results
# df <- MHW_previous_event_index; event_data <- MHW_event_data; cat_lon <- MHW_cat_lon
# event_file <- MHW_event_files[lon_row]; cat_lon_file <- MHW_cat_lon_files[lon_row]; cold_choice = F
# df <- MCS_previous_event_index; event_data <- MCS_event_data; cat_lon <- MCS_cat_lon
# event_file <- MCS_event_files[lon_row]; cat_lon_file <- MCS_cat_lon_files[lon_row]; cold_choice = T
event_proc <- function(df, sst_seas_thresh, event_data, cat_lon, event_file, cat_lon_file, full, cold_choice = F){
  
  # Calculate new event metrics with new data for MHW/MCSs as necessary
  # system.time(
  event_cat <- df |> 
    mutate(lat2 = lat) |> 
    group_by(lat2) |> 
    nest() |> 
    mutate(event_cat_res = purrr::map(data, event_calc,
                                      sst_seas_thresh = sst_seas_thresh,
                                      event_data = event_data,
                                      cat_lon = cat_lon,
                                      full = full,
                                      cold_choice = cold_choice)) |> 
    ungroup() |> 
    dplyr::select(-data, -lat2) |>
    unnest(cols = event_cat_res)
  # ) # 144 seconds for 720 pixels
  
  # Save results and exit
  event_new <- event_cat |>
    filter(row_number() %% 2 == 1) |>
    unnest(cols = event_cat_res)
  saveRDS(event_new, file = event_file)
  cat_new <- event_cat |>
    filter(row_number() %% 2 == 0) |>
    unnest(cols = event_cat_res)
  saveRDS(cat_new, file = cat_lon_file)
  rm(event_cat, event_new, cat_new); gc()
  # return()
}

# Function for extracting correct SST data based on pre-determined subsets
# It also calculates and returns corrected MHW metric results
# df <- MHW_previous_event_index[1,]
# event_data <- MHW_event_data[MHW_event_data$lat == df$lat,]
# cat_lon <- MHW_cat_lon[MHW_cat_lon$lat == df$lat,]
# full <- F; cold_choice <- F
# df <- MCS_previous_event_index[49,]
# event_data <- MCS_event_data[MCS_event_data$lat == df$lat,]
# cat_lon <- MCS_cat_lon[MCS_cat_lon$lat == df$lat,]
# full <- F; cold_choice <- T
event_calc <- function(df, sst_seas_thresh, event_data, cat_lon, full, cold_choice){
  
  # Extract necessary SST
  sst_step_1 <- sst_seas_thresh |> 
    filter(lat == df$lat,
           t >= df$date_end)
  if(nrow(sst_step_1) == 0) return()
  
  # Get correct thresh column
  if(cold_choice){
    sst_step_1 <- sst_step_1 |> 
      dplyr::rename(thresh = thresh_MCS) |> 
      dplyr::select(-thresh_MHW)
  } else {
    sst_step_1 <- sst_step_1 |> 
      dplyr::rename(thresh = thresh_MHW) |> 
      dplyr::select(-thresh_MCS)
  }
  
  # Calculate events+cats
  event_base <- detect_event(sst_step_1, coldSpells = cold_choice, categories = TRUE,
                             climatology = TRUE, season = "peak", MCScorrect = cold_choice, MCSice = cold_choice)
  
  # Get event steps
  event_step_1 <- event_base$event |> 
    mutate(lon = df$lon, lat = df$lat) |> 
    dplyr::select(lon, lat, event_no, duration:intensity_max, intensity_cumulative, category:season) |>
    mutate_if(is.numeric, round, 3)
  if(nrow(filter(event_step_1, !is.na(event_no))) == 0) return()
  if(full){
    event_step_2 <- event_step_1
  } else{
    event_step_2 <- event_data |>
      filter(lat == df$lat,
             date_end <= df$date_end) |>
      rbind(event_step_1) |>
      mutate(event_no = seq_len(n()))
  }
  
  # Get category steps
  cat_step_1 <- event_base$climatology |> 
    mutate(event_no = event_no + df$event_no,
           lon = df$lon,
           lat = df$lat,
           t = as.Date(t),
           category = as.character(category)) |> 
    dplyr::select(t, lon, lat, event_no, intensity, category) |> 
    filter(!is.na(category))
  if(full){
    cat_step_2 <- cat_step_1
  } else{
    cat_step_2 <- cat_lon |>
      filter(lat == df$lat,
             t <= as.Date(df$date_end)) |>
      bind_rows(cat_step_1)
  }
  
  # Exit
  event_cat <- list(event = event_step_2,
                    cat = cat_step_2)
  return(event_cat)
}

# Convenience wrapper for function below
event_cat_unpack <- function(nest_df){
  
  # Unpack 'cat_lon' data
  cat_lon_df <- nest_df |> 
    unnest(event) |> 
    filter(row_number() %% 2 == 1) |> 
    unnest(event) |> 
    dplyr::select(t, lon, lat, event_no, intensity, category) |>
    filter(!is.na(event_no)) |> 
    mutate(intensity = round(intensity, 2))
  
  # Unnest 'event' data
  event_df <- nest_df |> 
    unnest(event) |> 
    filter(row_number() %% 2 == 0) |> 
    unnest(event) |> 
    dplyr::select(lon, lat, event_no, duration, date_start, date_peak, date_end, 
                  intensity_mean, intensity_max, intensity_cumulative,
                  category, p_moderate, p_strong, p_severe, p_extreme, season) |> 
    mutate(intensity_mean = round(intensity_mean, 3),
           intensity_max = round(intensity_max, 2),
           intensity_cumulative = round(intensity_cumulative, 2))
  
  # Exit
  event_cat <- list(event = event_df,
                    cat = cat_lon_df)
  return(event_cat)
}

# A new single function to run the daily calculations. Much less complicated.
event_cat_calc <- function(lon_row, base_years = "1982-2011"){
  
  # Load SST
  sst_lon <- tidync(OISST_files[lon_row]) |> 
    hyper_tibble(na.rm = FALSE, force = TRUE, drop = FALSE) |> 
    mutate(time = as.Date(time, origin = "1970-01-01"),
           doy = yday(time),
           lon = as.numeric(lon),
           lat = as.numeric(lat)) |> 
    dplyr::rename(t = time, temp = sst)
  
  # Load thresh files
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  MHW_thresh <- readRDS(paste0("../data/thresh/MHW.seas.thresh.", lon_row_pad,"_",base_years,".Rds"))
  MCS_thresh <- readRDS(paste0("../data/thresh/MCS/MCS.seas.thresh.", lon_row_pad,"_",base_years,".Rds"))
  
  # testing...
  # sst_test <- sst_lon |> filter(lat == -77.625) |> left_join(MHW_thresh, by = c("lon", "lat", "doy"))
  # event_test <- detect_event(sst_test, categories = TRUE, climatology = TRUE, season = "peak")
  # write_csv(sst_test, "sst_test.csv")
  
  # MHWs
  ## Detect events+cats
  # system.time(
  MHW_nest <- sst_lon |>
    left_join(MHW_thresh, by = c("lon", "lat", "doy")) |>
    group_by(lon, lat) |> nest() |>
    mutate(event = purrr::map(data, detect_event, categories = TRUE, climatology = TRUE, season = "peak")) |>
    dplyr::select(-data) |> ungroup()
  # ) # 74 seconds for one full lon slice
  ## Unpack
  MHW_list <- event_cat_unpack(MHW_nest)
  ## Save 
  saveRDS(MHW_list$event, file = paste0("../data/event/MHW.event.",lon_row_pad,".",base_years,".Rda"))
  saveRDS(MHW_list$cat, file = paste0("../data/cat_lon/MHW.cat.",lon_row_pad,".",base_years,".Rda"))
  
  # MCSs
  ## Detect events+cats
  # system.time(
  MCS_nest <- sst_lon |>
    left_join(MCS_thresh, by = c("lon", "lat", "doy")) |> 
    group_by(lon, lat) |> nest() |>
    mutate(event = purrr::map(data, detect_event, coldSpells = TRUE, categories = TRUE,
                              climatology = TRUE, season = "peak", MCScorrect = TRUE, MCSice = TRUE)) |> 
    dplyr::select(-data) |> ungroup()
  # ) # 80 seconds for one full lon slice
  ## Unpack
  MCS_list <- event_cat_unpack(MCS_nest)
  ## Save 
  saveRDS(MCS_list$event, file = paste0("../data/event/MCS/MCS.event.",lon_row_pad,".",base_years,".Rda"))
  saveRDS(MCS_list$cat, file = paste0("../data/cat_lon/MCS/MCS.cat.",lon_row_pad,".",base_years,".Rda"))
  
  # Exit
  rm(sst_lon, 
     MHW_nest, MHW_list,
     MCS_nest, MCS_list); gc()
  return()
}
  

# 5: Create daily global file functions -----------------------------------

# Function for loading a cat_lon slice and extracting a single day of values
# testers...
# cat_lon_file <- cat_lon_files[1118]
# date_range <- c(as.Date("2019-11-01"), as.Date("2020-01-07"))
load_sub_cat_clim <- function(cat_lon_file, date_range){
  cat_clim <- readRDS(cat_lon_file)
  cat_clim_sub <- cat_clim |>
    filter(t >= date_range[1], t <= date_range[2])
  rm(cat_clim)
  return(cat_clim_sub)
}

# Function for saving daily global cat files
# date_choice <- max(current_dates)+1
# date_choice <- as.Date("2020-01-01")
save_sub_cat_clim <- function(date_choice, df, event_type, base_years){
  
  # Establish file name and save location
  cat_clim_year <- lubridate::year(date_choice)
  if(event_type == "MCS"){
    cat_clim_dir <- paste0("../data/cat_clim/MCS/",cat_clim_year)
    cat_clim_name <- paste0("cat.clim.MCS.",date_choice,".",base_years,".Rds")
    cat_rast_name <- paste0("cat.clim.MCS.",date_choice,".",base_years,".tif")
  } else {
    cat_clim_dir <- paste0("../data/cat_clim/",cat_clim_year)
    cat_clim_name <- paste0("cat.clim.",date_choice,".",base_years,".Rda")
    cat_rast_name <- paste0("cat.clim.",date_choice,".",base_years,".tif")
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
  # The next step in for the future. Requires new leaflet workflow...
  # rasterNonProj <- terra::rast(MHW_raster, digits = 3, crs = inputProj)
  suppressWarnings(
    rasterProj <- leaflet::projectRasterForLeaflet(rasterNonProj, method = "ngb")
  )
  raster::writeRaster(rasterProj, format = "GTiff", overwrite = TRUE,
              filename = paste0(cat_clim_dir,"/",cat_rast_name))
}

# Function for loading, prepping, and saving the daily global category slices
# date_range <- c(as.Date("1982-01-01"), as.Date("1982-01-01"))
cat_clim_global_daily <- function(date_range, base_years = "1982-2011"){
  
  # Get correct baseline files
  MHW_cat_lon_files_base <- str_filter(MHW_cat_lon_files, base_years)
  MCS_cat_lon_files_base <- str_filter(MCS_cat_lon_files, base_years)
  
  # Extract data
  MHW_cat_clim_daily <- plyr::ldply(MHW_cat_lon_files_base, load_sub_cat_clim, 
                                    .parallel = T, date_range = date_range) |>
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme"))) |>
    na.omit()
  MCS_cat_clim_daily <- plyr::ldply(MCS_cat_lon_files_base, load_sub_cat_clim, 
                                    .parallel = T, date_range = date_range) |> 
    mutate(category = factor(category, 
                             levels = c("I Moderate", "II Strong",
                                        "III Severe", "IV Extreme", "V Ice"))) |> 
    na.omit()
  
  # Save data as .Rda and as rasters projected to the shiny EPSG:3857
  # NB: Running this on too many cores may cause RAM issues
  # doParallel::registerDoParallel(cores = 20)
  plyr::l_ply(seq(min(MHW_cat_clim_daily$t), max(MHW_cat_clim_daily$t), by = "day"),
              save_sub_cat_clim, .parallel = T, df = MHW_cat_clim_daily, 
              event_type = "MHW", base_years = base_years)
  rm(MHW_cat_clim_daily); gc()
  plyr::l_ply(seq(min(MCS_cat_clim_daily$t), max(MCS_cat_clim_daily$t), by = "day"), 
              save_sub_cat_clim, .parallel = T, df = MCS_cat_clim_daily, 
              event_type = "MCS", base_years = base_years)
  rm(MCS_cat_clim_daily); gc()
}

# Function for saving daily global anom files
# date_choice <- max(current_dates)+1
# date_choice <- as.Date("1982-01-01")
save_sub_anom <- function(date_choice, df){
  
  # Establish file name and save location
  anom_year <- lubridate::year(date_choice)
  anom_dir <- paste0("../data/OISST/daily/",anom_year)
  dir.create(as.character(anom_dir), showWarnings = F)
  anom_name <- paste0("daily.",date_choice,".Rda")
  
  # Extract data and save
  df_sub <- df |> 
    filter(t == date_choice)
  rm(df); gc()
  saveRDS(df_sub, file = paste0(anom_dir,"/",anom_name))
}

# Function for loading global clims and saving each daily file
# date_range <- c(as.Date("2020-05-31"), as.Date("2020-06-22"))
anom_global_daily <- function(date_range){
  # print(paste0("Began loading anom data at ",Sys.time()))
  # system.time(
  global_anom <- plyr::ldply(lon_OISST, sst_seas_thresh_merge, .parallel = T,
                             date_range = date_range)
  # ) # ~50 seconds for 1 day, ~60 seconds for 1 year
  
  # NB: Running this on too many cores may cause RAM issues
  # print(paste0("Began saving anom data at ",Sys.time()))
  doParallel::registerDoParallel(cores = 20)
  plyr::l_ply(seq(min(global_anom$t), max(global_anom$t), by = "day"), 
              save_sub_anom, .parallel = T, df = global_anom)
}

