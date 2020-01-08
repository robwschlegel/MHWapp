# This script combines all previous work towards the MHWapp into one location
  # The functions that this script draws on are found in "MHW_daily_functions.R"
# This script is designed to be run autonomously once per day by a chron job
# It performs the following tasks:
## 1: Downloads the most recent final and prelim NOAA OISST 
##    data available and updates the local NetCDF files
## 2: Updates MHW event and category data
## 3: Creates daily global MHW category file(s)
## 4: Updates the `final_dates`, `prelim_dates`, and `current_dates` indexes

source("MHW_daily_functions.R")
# source("../MHWapp/MHW_daily_fixes.R")

## NB: Don't run any of this code manually
## NB: only run it via source("MHW_daily.R") in an R terminal
## NB: The NetCDF files seem to stop cooperating if they are opened manually via RStudio Server
## NB: My thinking is this somehow confuses the write privileges for the files


# 1: Update OISST data ----------------------------------------------------

# The most up-to-date data downloaded
load("metadata/final_dates.Rdata")
load("metadata/prelim_dates.Rdata")


# Get the source index monthly file folders with new data
print(paste0("Fetching OISST folder names at ",Sys.time()))
OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/"
OISST_url_month_get <- getURL(OISST_url_month)
OISST_months <- data.frame(months = readHTMLTable(OISST_url_month_get, skip.rows = 1:2)[[1]]$Name) %>% 
  mutate(months = lubridate::as_date(str_replace(as.character(months), "/", "01"))) %>% 
  filter(months >= max(lubridate::floor_date(final_dates, unit = "month"))) %>% 
  mutate(months = gsub("-", "", substr(months, 1, 7)))


# Check if new data need downloading
OISST_filenames <- plyr::ldply(OISST_months$months, .fun = OISST_url_daily)
final_index <- OISST_filenames %>% 
  filter(!grepl("prelim", files))
prelim_index <- OISST_filenames %>% 
  filter(grepl("prelim", files),
         t > max(prelim_dates))
OISST_new <- rbind(final_index, prelim_index)

# Download the new data
if(nrow(OISST_new) > 0){
  print(paste0("Downloading new data at ", Sys.time()))
  OISST_dat <- plyr::ldply(OISST_new$full_name, .fun = OISST_url_daily_dl)
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
## NB: -NEVER- run this in RStudio Server!
  ## It breaks the NetCDF write privileges
# doMC::registerDoMC(cores = 50)
# doParallel::registerDoParallel(cores = 50)
if(nrow(OISST_dat) > 2){
  print(paste0("Adding new data to NetCDF files at ", Sys.time()))
  ## NB: 50 cores uses too much RAM if more than a few days are being added
  # doMC::registerDoMC(cores = 50)
  doParallel::registerDoParallel(cores = 50)
  plyr::l_ply(lon_OISST, .fun = OISST_merge, .parallel = TRUE, df = OISST_dat)
  print(paste0("Finished at ", Sys.time()))
  
  # Create date indexes
  # Get range of complete dates from the above `OISST_merge()` runs
  ncdf_dates <- as.Date(tidync("../data/OISST/avhrr-only-v2.ts.1440.nc")$transforms$time$time, origin = "1970-01-01")

  # final_dates index
  final_dates <- ncdf_dates[ncdf_dates <= max(final_index$t)]
  # tester...
  # final_dates <- final_dates[1:length(final_dates-2)]
  # tail(final_dates)
  save(final_dates, file = "metadata/final_dates.Rdata")

  # prelim_dates index
  prelim_dates <- ncdf_dates[ncdf_dates > max(final_index$t)]
  # tester...
  # prelim_dates <- prelim_dates[1:length(prelim_dates-2)]
  # tail(prelim_dates)
  save(prelim_dates, file = "metadata/prelim_dates.Rdata")
}


# Fix files that didn't run correctly
# This happens every few months, usaually due to a core slipping
  # NB: These fixes have not been updated since the tidync framework was implemented on 2019-10-29

# The easiest way to fix this is actually to load the 
# `final_dates` and `prelim_dates` objects,
# alter them to require re-downloading the affected data,
# and then save the files.
# One then runs source() on this script IN A TERMINAL AND NOT RSTUDIO
# Check the MHW Tracker in a few minutes after this finishes running again
# to see if the correction propogated through successfully

# Fix one longitude slice
# fix_lon_step <- which(lon_OISST == 92.375)
# OISST_ncdf_fix(fix_lon_step, end_date = "2019-01-22")

# Fix many
# fix_lon_steps <- which(lon_OISST %in% seq(-173.875, -11.375, by = 12.5))
# plyr::ldply(fix_lon_steps, .fun = OISST_ncdf_fix, .parallel = TRUE, end_date = "2019-08-21")

# Dates/reasons this needed to be run:
# August 29th, 2019: Core 45 slipped, affecting every 12.5th longitude value
# from -173.875 to -11.375 and 136.125 to 173.625 for 08-09 to 08-11


# 2: Update MHW event and category data -----------------------------------

# Prep guide info for this section
# doMC::registerDoMC(cores = 25)
doParallel::registerDoParallel(cores = 50)
# load("metadata/final_dates.Rdata")
# load("metadata/prelim_dates.Rdata")

# This takes roughly 35 minutes and is by far the largest time requirement
if(nrow(OISST_dat) > 2){
  print(paste0("Updating MHW results at ", Sys.time()))
  # system.time(
    plyr::l_ply(lon_OISST, .fun = MHW_event_cat_update, .parallel = TRUE)
  # ) # ~ 40 seconds per cycle
  print(paste0("Updated MHW results at ", Sys.time()))
}

# Occasionaly the cat_lon files don't come right
# One can usually tell if the size is under 400 kb
# This function can fix a specific file

# Run one
# MHW_event_cat_fix(lon_OISST[20])

# Run many
# plyr::l_ply(lon_OISST[1117:1182], .fun = MHW_event_cat_fix, .parallel = TRUE)

# Run ALL
# plyr::l_ply(lon_OISST[1:1440], .fun = MHW_event_cat_fix, .parallel = TRUE) # ~1.5 hours on 50 cores


# 3: Create daily global files --------------------------------------------

# doMC::registerDoMC(cores = 25)
doParallel::registerDoParallel(cores = 50)

# Get most current processed OISST dates
time_index <- as.Date(tidync("../data/OISST/avhrr-only-v2.ts.1440.nc")$transforms$time$time, origin = "1970-01-01")
# max(time_index)

# Get the range of dates that need to be run
# The function `cat_clim_global_daily()` uses dplyr so a for loop is used here
  # Manually control dates as desired
# update_dates <- seq(as.Date("2019-11-01"), as.Date("2020-01-07"), by = "day")
update_dates <- time_index[which(time_index >= min(final_index$t))]
if(length(update_dates) > 0) {
  print(paste0("Updating global MHW slices from ",min(update_dates)," to ",max(update_dates)))
  # system.time(
  cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)))
  # ) # ~28 seconds
  print(paste0("Updated global MHW slices at ", Sys.time()))
}


# 4: Update current_dates -------------------------------------------------

# Indexes all files throughout the cat_clim sub-folders to create the current_dates index
current_dates <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim",
                                  full.names = TRUE, recursive = TRUE))
current_dates <- sapply(strsplit(current_dates, "cat.clim."), "[[", 3)
current_dates <- as.Date(as.vector(sapply(strsplit(current_dates, ".Rda"), "[[", 1)))
# tail(current_dates)
# save(current_dates, file = "shiny/current_dates.RData")

# Check that no days are missing
possible_dates <- seq(as.Date("1982-01-01"), max(current_dates), by = "day")
if(length(possible_dates) > length(current_dates)){
  error_dates <- possible_dates[!possible_dates %in% current_dates]
  stop(paste0("The following dates are missing: ",error_dates))
}

