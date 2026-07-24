# MHW_daily.R
# This script runs the full daily analysis that produces the results fed to the MHW Tracker
  # The functions that this script draws on are found in "MHW_daily_functions.R"
# This script is designed to be run autonomously once per day via a cron job
# NB: Better to run this script via source("MHW_daily.R") in an R terminal
# It performs the following tasks:
## 1: Downloads the most recent final and prelim NOAA OISST
##    data available and updates the local NetCDF files
## 2: Updates MHW event and category data
## 3: Creates daily global MHW category file(s)
## 4: Check the `current_dates` index to make sure no days are missing
## 5: Run the annual summary update
## 6: Push to GitHub
## 7: Maintenance

source("MHW_daily_functions.R")

# Parallel worker plan - separate processes (multisession), never fork-based
# (multicore/doParallel) - forking around ncdf4/HDF5 hangs unpredictably (see
# the fork/HDF5-hang notes in MHW_daily_functions.R). Workers independently
# source this same functions file at startup so they end up with identical
# state to the main session (all libraries, metadata, custom functions) -
# this avoids relying on future's automatic global/package detection, which
# is easy to get subtly wrong for code that isn't a package.
n_workers <- 25
oisst_cwd <- getwd()
oisst_cl <- parallelly::makeClusterPSOCK(
  n_workers, rscript_libs = .libPaths(),
  rscript_startup = bquote({setwd(.(oisst_cwd)); source("MHW_daily_functions.R")})
)
future::plan(future::cluster, workers = oisst_cl)

# Derive the final/prelim date indexes directly from the local daily archive
# file names (replaces the old final_dates.Rdata/prelim_dates.Rdata indexes)
oisst_dates <- OISST_dates_index(OISST_daily_nc_files)
final_dates <- oisst_dates$final_dates
prelim_dates <- oisst_dates$prelim_dates
if(length(final_dates) == 0) stop("Final date indexing has broken.")
if(length(prelim_dates) == 0) stop("Prelim date indexing has broken.")


# 1: Update OISST data ----------------------------------------------------

# If setting up this pipeline for the first time (or rebuilding the whole
# archive), build all 1440 per-longitude files from the local daily archive:
# OISST_database_build(date_max = max(final_dates))
# NB: It should take ~30 seconds per year of data when building for first time
# NB: If anything goes wrong while running this, re-run it and it can self heal,
# though it may be faster to delete everything and start fresh

# Get the source index monthly file folders with new data
print(paste0("Fetching OISST folder names at ",Sys.time()))
OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
OISST_url_month_get <- getURL(OISST_url_month)
OISST_table <- readHTMLTable(OISST_url_month_get, as.data.frame = TRUE)[[1]] |> 
  rename(months = V1) |> drop_na()
OISST_months <- slice(OISST_table, which(OISST_table$months == "198109/"):nrow(OISST_table))  |>
  mutate(months = lubridate::as_date(str_replace(as.character(months), "/", "01"))) |>
  filter(months >= max(lubridate::floor_date(final_dates, unit = "month"))) |>
  mutate(months = gsub("-", "", substr(months, 1, 7)))

# Uncomment to manually control downloads
# final_dates <- as.Date("1981-12-31") # To re-download everything
# final_dates <- seq(as.Date("1981-12-31"), as.Date("2024-12-25"), by = "day") # Or just from the latest date
# OISST_months <- data.frame(months = apply(expand.grid(2023, stringr::str_pad(seq(1:5), 2, pad = "0")), 
#                                           1, paste, collapse = "")) |> arrange(months)

# Check if new data need downloading
OISST_filenames <- furrr::future_map_dfr(OISST_months$months, OISST_url_daily) |>
  dplyr::select(files, t, full_name)

# Download and save files locally
if(length(OISST_filenames$files) > 0){
  cat(paste0("Saving new data locally at ", Sys.time(),": \n ",
            paste0(OISST_filenames$files, collapse = ",")), "\n")
  furrr::future_walk(OISST_filenames$full_name, OISST_url_daily_save)
}

# Get list of final files
final_index <- filter(OISST_filenames, !grepl("prelim", files))
if(nrow(final_index) == 0){
  print("No new final data to download")
  final_index <- data.frame(files = NA, t = NA, full_name = NA)
}

# Get list of preliminary files
prelim_index <- filter(OISST_filenames, grepl("prelim", files), t > max(prelim_dates))
if(nrow(prelim_index) == 0){
  print("No new prelim data to download")
  prelim_index <- data.frame(files = NA, t = NA, full_name = NA)
}

# Bind lists
OISST_new <- na.omit(rbind(final_index, prelim_index))
if(nrow(OISST_new) > 0){
  OISST_new <- OISST_new |> 
    mutate(file_year = substr(sapply(strsplit(full_name, split = "/"), "[[", 9), 1, 4),
           file_stub = sapply(strsplit(full_name, split = "/"), "[[", 10),
           file_name = paste0("../data/OISST/daily/",file_year,"/",file_stub))
}

# Or manually control which data are added to the NetCDF database
# OISST_new <- data.frame(file_name = dir("../data/OISST/daily/2014", 
#                                         pattern = ".nc",full.names = TRUE, recursive = TRUE))

# Prep data for merging with existing files
# if(nrow(OISST_new) > 50) stop("A suspicious amount of new files are attempting to be downloaded.")
if(nrow(OISST_new) > 0){
  print(paste0("Prepping new data at ", Sys.time()))
  OISST_dat <- purrr::map_dfr(OISST_new$file_name, OISST_prep)
  OISST_dat$lon <- ifelse(OISST_dat$lon > 180, OISST_dat$lon-360, OISST_dat$lon)
} else {
  print("No new data to prep")
  OISST_dat <- data.frame(lon = NA, lat = NA, t = NA, temp = NA)
}

# Catch the dates when the data may be incorrect and remove anything from there forward
if(nrow(OISST_dat) > 2){
  if(max(OISST_dat$temp, na.rm = T) > 100){
    final_date_error <- min(filter(OISST_dat, temp > 100)$t)
    OISST_dat <- filter(OISST_dat, t < final_date_error)
    print(paste0("There was an error in the final OISST data on", final_date_error))
  }
}

# Add new prelim and final data to the files
if(nrow(OISST_dat) > 2){
  print(paste0("Adding new data to NetCDF files at ", Sys.time()))
  furrr::future_walk(lon_OISST, OISST_merge, df = OISST_dat)
  print(paste0("Finished at ", Sys.time()))

  # Refresh the date indexes from the now-updated daily archive (this run's
  # new downloads aren't reflected in the OISST_daily_nc_files snapshot taken
  # at script start, so re-scan rather than reuse it)
  oisst_dates <- OISST_dates_index()
  final_dates <- oisst_dates$final_dates
  prelim_dates <- oisst_dates$prelim_dates
}
# return()

# Fix files that didn't run correctly:
# This happens every few months, usually due to a core slipping
# Move or delete the affected date(s) out of "../data/OISST/daily/<year>/" 
# so they no longer appear in the archive, which naturally lowers 
# max(final_dates) (or max(prelim_dates)) and makes the download step 
# re-fetch everything from that point forward on the next run.
# One then runs source() on this script IN A TERMINAL AND NOT RSTUDIO
# Check the MHW Tracker in a few minutes after this finishes running again
# to see if the correction propagated through successfully.

# If a NetCDF file breaks, re-create it with:
# OISST_lon_NetCDF(994, tail(prelim_dates, 1))
# As long as the final date matches the other NetCDF files, there shouldn't be an issue
# Otherwise it will be necessary to remove the affected daily files (see above) and
# re-run the OISST daily file downloading so that all files have the exact same date range


# 2: Update MHW event and category data -----------------------------------

# Create baseline seas+thresh files per longitude step
## ~2 seconds per cycle
# furrr::future_walk(1:1440, create_thresh, base_years = c(1982, 2011),
#                    .options = furrr::furrr_options(seed = TRUE))
# furrr::future_walk(1:1440, create_thresh, base_years = c(1991, 2020),
#                    .options = furrr::furrr_options(seed = TRUE))

# Prep guide info for this section
# NB: on a brand new setup the event file below won't exist yet - the check just
# falls back to a very old date in that case, so the first run always proceeds
ncdf_1440 <- nc_open("../data/OISST/oisst-avhrr-v02r01.ts.1440.nc")
ncdf_date <- max(as.Date(ncdf_1440$dim$time$vals, origin = "1970-01-01"))
nc_close(ncdf_1440)
event_file_check <- "../data/event/MHW_1440_1991-2020_events.nc"
if(file.exists(event_file_check)){
  ncdf_event_check <- nc_open(event_file_check)
  event_date <- as.Date(max(ncvar_get(ncdf_event_check, "date_end")), origin = "1982-01-01")
  nc_close(ncdf_event_check)
} else {
  event_date <- as.Date("1980-01-01")
}

# This takes roughly 14 minutes
if(ncdf_date > event_date){
  print(paste0("Updating MHW/MCS lon files:"))

  # 1982-2011 calcs
  print(paste0("Began 1982-2011 baseline calcs at ", Sys.time()))
  # system.time(
  furrr::future_walk(1:1440, event_cat_calc, 
                     .options = furrr::furrr_options(seed = TRUE, conditions = character())); gc()
  # ) # ~2 seconds per cycle

  # 1991-2020 calcs
  print(paste0("Began 1991-2020 baseline calcs at ", Sys.time()))
  furrr::future_walk(1:1440, event_cat_calc, base_years = c(1991, 2020),
                     .options = furrr::furrr_options(seed = TRUE, conditions = character())); gc()

  print(paste0("Finished MHW/MCS lon files at ", Sys.time()))
}


# Occasionally the event files don't come right
# One can usually tell if the size is under 400 kb
# This function can fix a specific file

# Run one
# event_cat_calc(994)

# Run many
# furrr::future_walk(1300:1365, event_cat_calc)

# Run ALL
# furrr::future_walk(1:1440, event_cat_calc) # 10 minutes on 50 cores

# Find files that haven't been run since a certain date
# file_dates <- file.info(dir("../data/cat_lon/MCS", full.names = T)) |>
#   rownames_to_column(var = "file_name") |>
#   mutate(file_name = sapply(strsplit(file_name, "/"), "[[", 5)) |>
#   mutate(file_num = as.integer(sapply(strsplit(file_name, "[.]"), "[[", 3))) |>
#   filter(ctime < Sys.Date()-1)
# filter(ctime < Sys.time()-72000)
# filter(size < 600000)
# furrr::future_walk(file_dates$file_num, event_cat_calc)


# 3: Create daily global files --------------------------------------------

# Get most current processed OISST dates
ncdf_1440 <- nc_open("../data/OISST/oisst-avhrr-v02r01.ts.1440.nc")
time_index <- as.Date(ncdf_1440$dim$time$vals, origin = "1970-01-01")
nc_close(ncdf_1440)

# Get the range of dates that need to be run
update_dates <- time_index[which(time_index >= max(prelim_dates)-14)] # ~4 minutes
# Or manually control dates as desired
# update_dates <- seq(as.Date("2026-05-10"), as.Date("2026-05-20"), by = "day")
# update_dates <- time_index[which(time_index >= max(final_dates)-7)]
if(length(update_dates) > 0) {
  print(paste0("Updating global daily MHW/MCS cat files from ",min(update_dates)," to ",max(update_dates)))
  
  # 1982-2011 calcs
  print(paste0("Began 1982-2011 baseline calcs at ", Sys.time()))
  cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)))
  
  # Quick break
  gc(); Sys.sleep(10)
  
  # 1991-2020 calcs
  print(paste0("Began 1991-2020 baseline calcs at ", Sys.time()))
  cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)), base_years = c(1991, 2020))

  print(paste0("Finished global daily MHW/MCS files at ", Sys.time()))
}


# If any of the files created in this section break for any reason,
# simply change the 'update_dates' object manually and re-run the section.


# For loop to reprocess large sets of data one year at a time in 6 month batches
# for(i in 1982:2025){
#   update_dates <- seq(as.Date(paste0(i,"-01-01")), as.Date(paste0(i,"-06-30")), by = "day")
#   print(paste0("Updating from ",min(update_dates)," to ",max(update_dates)," at ", Sys.time()))
#   cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)), base_years = c(1991, 2020))
#   print(paste0("Finished at ", Sys.time()))
#   gc(); Sys.sleep(10)
#   update_dates <- seq(as.Date(paste0(i,"-07-01")), as.Date(paste0(i,"-12-31")), by = "day")
#   print(paste0("Updating from ",min(update_dates)," to ",max(update_dates)," at ", Sys.time()))
#   cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)), base_years = c(1991, 2020))
#   print(paste0("Finished at ", Sys.time()))
#   gc(); Sys.sleep(10)
# }


# 4: Check current dates --------------------------------------------------

# Indexes all files throughout the OISST/daily sub-folders to create the current_dates index
current_dates <- as.Date(sapply(stringr::str_split(list.files("../data/OISST/daily/", 
                                                              recursive = T, pattern = "nc"), "[.]"), "[[", 2),
                         format = "%Y%m%d")

# Check that no days are missing
possible_dates <- seq(as.Date("1982-01-01"), max(current_dates), by = "day")
if(length(possible_dates) > length(current_dates)){
  error_dates <- possible_dates[!possible_dates %in% current_dates]
  stop(paste0("The following dates are missing: ",error_dates))
}


# 5: Run annual summary ---------------------------------------------------

# NB: Need a week of data into the new year before processing stats
if(lubridate::yday(Sys.Date()) > 6) source("MHW_annual_summary.R")

# Shut down the parallel worker pool cleanly now that nothing else in this
# script needs it (MHW_annual_summary.R, if sourced above, was the last
# consumer of the plan set up at the top of this script)
future::plan(future::sequential)
parallel::stopCluster(oisst_cl)


# 6: Push to GitHub -------------------------------------------------------

system("git commit -a -m 'Daily run'")
system("git pull")
system("git push")


# 7: Maintenance ----------------------------------------------------------

# Active folders
## Daily/lon files
### "../data/thresh" # The seas+clim static threshold files per lon slice - medium .nc files
### "../data/event" # The lon slice files containing the MHW/MCS event results - large .nc files
### "../data/cat_lon # The lon slice files containing the daily MHW/MCS category results - DEPRECATED after heatwave3
### "../data/cat_clim # Global daily files containing MHW/MCS spatial categories - small-medium, .Rda and .tif files
## Annual summaries
### "data/annual_summary" # Contains pixel and day based annual summaries for OISST, CCI, and CMC datasets


# Find and remove old files from before the second baseline was introduced
# NB: This was done manually for all of the daily/lon files except the cat_clim files as they are too numerous
# file_dates_cat_clim <- file.info(dir("../data/cat_clim", full.names = TRUE, recursive = TRUE)) |>
#   rownames_to_column(var = "file_name") |>
#   filter(ctime < as.POSIXct("2024-07-10"))
# file.remove(file_dates_cat_clim$file_name)


# Move files as necessary during heatwave3 integration process
# files_to_move <- fs::dir_ls("../data/cat_lon/MCS", glob = glue::glue("*.nc$"))
# tail(files_to_move)
# dest_dir <- "../data/event/MCS"
# tail(fs::dir_ls(dest_dir))
# fs::file_move(files_to_move, dest_dir)

# Delete files as necessary during heatwave3 integration process
# files_to_delete <- fs::dir_ls("../data/OISST/daily", recurse = TRUE, glob = "*.Rda$") #|> stringr::str_subset("MHW.cat")
# head(files_to_delete); tail(files_to_delete)
# fs::file_delete(files_to_delete)

