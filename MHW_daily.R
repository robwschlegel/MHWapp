# MHW_daily.R
# This script runs the full daily analysis that produces the results fed to the MHW Tracker
  # The functions that this script draws on are found in "MHW_daily_functions.R"
# This script is designed to be run autonomously once per day via a cron job
# It performs the following tasks:
## 1: Downloads the most recent final and prelim NOAA OISST
##    data available and updates the local NetCDF files
## 2: Updates MHW event and category data
## 3: Creates daily global MHW category file(s)
## 4: Check the `current_dates` index to make sure no days are missing
## 5: Run the annual summary update
## 6: Push to GitHub

source("MHW_daily_functions.R")

## NB: Run this script via source("MHW_daily.R") in an R terminal
## NB: The NetCDF files seem to stop cooperating if they are opened manually via RStudio Server
## NB: My thinking is this somehow confuses the write privileges for the files
## NB: This is likely an Rstudio Server issue and it may not occur locally


# 1: Update OISST data ----------------------------------------------------

# The most up-to-date data downloaded
# For manually testing
# final_dates <- seq(as.Date("1982-01-01"), as.Date("2020-05-30"), by = "day")
# save(final_dates, file = "metadata/final_dates.Rdata")
# prelim_dates <- seq(as.Date("2016-01-01"), as.Date("2020-04-19"), by = "day")
# save(prelim_dates, file = "metadata/prelim_dates.Rdata")
load("metadata/final_dates.Rdata")
load("metadata/prelim_dates.Rdata")
if(length(final_dates) == 0) stop("Final date indexing has broken.")
if(length(prelim_dates) == 0) stop("Prelim date indexing has broken.")


# Get the source index monthly file folders with new data
print(paste0("Fetching OISST folder names at ",Sys.time()))
# This is he address for the v2.0 data, which are used from 1982-01-01 to 2015-12-31
# OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/"
OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
OISST_url_month_get <- getURL(OISST_url_month)
OISST_months <- data.frame(months = readHTMLTable(OISST_url_month_get, skip.rows = 1:2)[[1]]$Name) %>% 
  mutate(months = lubridate::as_date(str_replace(as.character(months), "/", "01"))) %>% 
  filter(months >= max(lubridate::floor_date(final_dates, unit = "month"))) %>%
  mutate(months = gsub("-", "", substr(months, 1, 7)))


# Check if new data need downloading
OISST_filenames <- plyr::ldply(OISST_months$months, .fun = OISST_url_daily)
final_index <- OISST_filenames %>% 
  filter(!grepl("prelim", files))
if(nrow(final_index) == 0){
  print("No new final data to download")
  final_index <- data.frame(files = NA, t = NA, full_name = NA)
}
prelim_index <- OISST_filenames %>% 
  filter(grepl("prelim", files),
         t > max(prelim_dates))
if(nrow(prelim_index) == 0){
  print("No new prelim data to download")
  prelim_index <- data.frame(files = NA, t = NA, full_name = NA)
}
OISST_new <- rbind(final_index, prelim_index) %>% 
  na.omit()


# Download the new data
if(nrow(OISST_new) > 10) stop("A suspicious amount of new files are attempting to be downloaded.")
if(nrow(OISST_new) > 0){
  print(paste0("Downloading new data at ", Sys.time()))
  OISST_dat <- plyr::ldply(OISST_new$full_name, .fun = OISST_url_daily_dl, .parallel = F)
  OISST_dat$lon <- ifelse(OISST_dat$lon > 180, OISST_dat$lon-360, OISST_dat$lon)
} else {
  print("No new data to download")
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
## NB: -NEVER- run the following if statement in RStudio Server!
  ## It breaks the NetCDF write privileges
if(nrow(OISST_dat) > 2){
  print(paste0("Adding new data to NetCDF files at ", Sys.time()))
  ## NB: 50 cores uses too much RAM if more than a few days are being added
  doParallel::registerDoParallel(cores = 25)
  plyr::l_ply(lon_OISST, .fun = OISST_merge, .parallel = TRUE, df = OISST_dat)
  print(paste0("Finished at ", Sys.time()))
  
  # Create date indexes
  ncdf_dates <- as.Date(tidync("../data/OISST/avhrr-only-v2.ts.1440.nc")$transforms$time$time, origin = "1970-01-01")

  # final_dates index
  if(!is.na(final_index$files[1])){
    final_dates <- ncdf_dates[ncdf_dates <= max(final_index$t)]
    save(final_dates, file = "metadata/final_dates.Rdata")
  }

  # prelim_dates index
  if(!is.na(prelim_index$files[1])){
    prelim_dates <- ncdf_dates[ncdf_dates >= min(OISST_filenames$t[grepl("prelim", OISST_filenames$files)])]
    save(prelim_dates, file = "metadata/prelim_dates.Rdata")
  }
}


# Fix files that didn't run correctly:
# This happens every few months, usually due to a core slipping
# The easiest way to fix this is to load the `final_dates` and `prelim_dates` objects,
# alter them to require re-downloading the affected data, and then save the files.
# One then runs source() on this script IN A TERMINAL AND NOT RSTUDIO
# Check the MHW Tracker in a few minutes after this finishes running again
# to see if the correction propagated through successfully.


# 2: Update MHW event and category data -----------------------------------

# Prep guide info for this section
doParallel::registerDoParallel(cores = 25)
ncdf_date <- max(as.Date(tidync("../data/OISST/avhrr-only-v2.ts.1440.nc")$transforms$time$time, origin = "1970-01-01"))
cat_lon_date <- max(readRDS("../data/cat_lon/MHW.cat.1440.Rda")$t)

# This takes roughly 45 minutes and is by far the largest time requirement
if(ncdf_date > cat_lon_date){
  print(paste0("Updating MHW results at ", Sys.time()))
  # system.time(
  plyr::l_ply(lon_OISST, .fun = MHW_event_cat_update, .parallel = TRUE)
  # ) # ~ 40 seconds per cycle
  print(paste0("Finished MHW results at ", Sys.time()))
}

# Occasionaly the cat_lon files don't come right
# One can usually tell if the size is under 400 kb
# This function can fix a specific file

# Run one
# MHW_event_cat_update(lon_OISST[88], full = TRUE)

# Run many
# plyr::l_ply(lon_OISST[1300:1365], .fun = MHW_event_cat_update, .parallel = TRUE, full = TRUE)

# Run ALL
# plyr::l_ply(lon_OISST, .fun = MHW_event_cat_update, .parallel = TRUE, full = TRUE) # ~1.5 hours on 50 cores

# Find files that haven't been run since a certain date
# file_dates <- file.info(dir("../data/cat_lon", full.names = T)) %>%
#   mutate(file_name = sapply(strsplit(row.names(.), "/"), "[[", 4)) %>%
#   mutate(file_num = as.integer(sapply(strsplit(file_name, "[.]"), "[[", 3))) %>%
#   filter(ctime < Sys.Date()-2)
# plyr::l_ply(lon_OISST[file_dates$file_num], .fun = MHW_event_cat_update, .parallel = TRUE)


# 3: Create daily global files --------------------------------------------

# Get most current processed OISST dates
time_index <- as.Date(tidync("../data/OISST/avhrr-only-v2.ts.1440.nc")$transforms$time$time, origin = "1970-01-01")
load("metadata/final_dates.Rdata")

# Get the range of dates that need to be run
  # Manually control dates as desired
# update_dates <- seq(as.Date("2020-05-31"), as.Date("2020-06-22"), by = "day")
update_dates <- time_index[which(time_index >= max(final_dates)-2)]
if(length(update_dates) > 0) {
  print(paste0("Updating global MHW files from ",min(update_dates)," to ",max(update_dates)))
  print(paste0("Updating daily cat files at ", Sys.time()))
  doParallel::registerDoParallel(cores = 50)
  # system.time(
  cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)))
  # ) # ~28 seconds
  print(paste0("Updating daily anom files at ", Sys.time()))
  doParallel::registerDoParallel(cores = 50)
  # system.time(
  anom_global_daily(date_range = c(min(update_dates), max(update_dates)))
  # ) # 455 seconds
  print(paste0("Finished global daily MHW files at ", Sys.time()))
}

# If any of the files created in this section break for any reason,
# simply change the 'update_dates' object manually and re-run the section.


# 4: Check current dates --------------------------------------------------

# Indexes all files throughout the OISST/daily sub-folders to create the current_dates index
current_dates <- as.Date(sapply(str_split(list.files("../data/OISST/daily/", recursive = T), "[.]"), "[[", 2))

# Check that no days are missing
possible_dates <- seq(as.Date("1982-01-01"), max(current_dates), by = "day")
if(length(possible_dates) > length(current_dates)){
  error_dates <- possible_dates[!possible_dates %in% current_dates]
  stop(paste0("The following dates are missing: ",error_dates))
}


# 5: Run annual summary ---------------------------------------------------

# NB: Currently disabled until the foundational changes to the pipeline are updated project-wide
# source("MHW_annual_summary.R")


# 6: Push to GitHub -------------------------------------------------------

system("git commit -a -m 'Daily run'")
system("git pull")
system("git push")

