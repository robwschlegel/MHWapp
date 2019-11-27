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

# Firs check that the desired data are indeed present
# Sometimes the NOAA OISST data are not listed up on the ERDDAP server
print("Checking server availability")
server_data <- rerddap::ed_datasets(which = "griddap", "https://www.ncei.noaa.gov/erddap/")$Dataset.ID
if(!"ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon" %in% server_data)
  stop("Final data are not currently up on the ERDDAP server")
if(!"ncdc_oisst_v2_avhrr_prelim_by_time_zlev_lat_lon" %in% server_data)
  stop("Prelim data are not currently up on the ERDDAP server")


# The final data
# rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
print("Fetching final data info")
final_info <- rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
final_time_range <- final_info$alldata$time$value[3]
final_time_start <- NOAA_date(final_time_range, 1)
final_time_end <- NOAA_date(final_time_range, 2)


# The preliminary data
# rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_prelim_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
print("Fetching prelim data info")
prelim_info <- rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_prelim_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
prelim_time_range <- prelim_info$alldata$time$value[3]
prelim_time_start <- NOAA_date(prelim_time_range, 1)
prelim_time_end <- NOAA_date(prelim_time_range, 2)


# Check if newer final data need to/can be downloaded
print("Checking if new final data need downloading")
load("metadata/final_dates.Rdata")
if(max(final_dates) < final_time_end) {
  final_date_start <- max(final_dates)+1
  final_date_end <- final_time_end
} else {
  final_date_start <- FALSE
}


# Check if newer final data need to/can be downloaded
print("Checking if new prelim data need downloading")
load("metadata/prelim_dates.Rdata")
if(max(prelim_dates) < prelim_time_end) {
  prelim_date_start <- max(prelim_dates)+1
  prelim_date_end <- prelim_time_end
  prelim_date_blank <- FALSE
  if(max(prelim_dates)+1 < prelim_time_start){
    prelim_date_blank <- seq(max(prelim_dates)+1, prelim_time_start-1, by = "day")
    prelim_date_start <- prelim_time_start
    print(paste0("The day(s) ",prelim_date_blank," are not available in the prelim data and are being filled with ",prelim_time_start))
  }
} else {
  prelim_date_start <- FALSE
  prelim_date_blank <- FALSE
}


# If there are no new data, say so
if(final_date_start == FALSE & prelim_date_start == FALSE)
  print("No new data to download")


# Download data not in 'final_dates' if necessary/possible
if(final_date_start != FALSE){
  print(paste0("Downloading final data from ",final_date_start," to ",final_date_end))
  OISST_final_1 <- OISST_dl(c(final_date_start, final_date_end),
                            "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon")
} else {
  OISST_final_1 <- data.frame(lon = NA, lat = NA, t = NA, temp = NA)
}


# Download data not in 'prelim_dates' if necessary/possible
if(prelim_date_start != FALSE){
  print(paste0("Downloading prelim data from ",prelim_date_start," to ",prelim_date_end))
  OISST_prelim_1 <- OISST_dl(c(prelim_date_start, prelim_date_end),
                            "ncdc_oisst_v2_avhrr_prelim_by_time_zlev_lat_lon")
} else {
  OISST_prelim_1 <- data.frame(lon = NA, lat = NA, t = NA, temp = NA)
}


# Fill blank days from prelim data with earliest day available
# NB: This should occur very rarely
if(prelim_date_blank != FALSE){
  prelim_blank_plug <- filter(OISST_prelim_1, t == prelim_time_start)
  for(i in length(prelim_date_blank):1){
    prelim_blank_plug_step <- prelim_blank_plug
    prelim_blank_plug_step$t <- prelim_date_blank[i]
    OISST_prelim_1 <- rbind(prelim_blank_plug_step ,OISST_prelim_1)
  }
}


# Catch the dates when the data may be incorrect and remove anything from there forward
if(nrow(OISST_final_1) > 1){
  if(max(OISST_final_1$temp, na.rm = T) > 100){
    final_date_error <- min(filter(OISST_final_1, temp > 100)$t)
    OISST_final_1 <- filter(OISST_final_1, t < final_date_error)
    print(paste0("There was an error in the final OISST data on", final_date_error))
  }
}
if(nrow(OISST_prelim_1) > 1){
  if(max(OISST_prelim_1$temp, na.rm = T) > 100){
    prelim_date_error <- min(filter(OISST_prelim_1, temp > 100)$t)
    OISST_prelim_1 <- filter(OISST_prelim_1, t < prelim_date_error)
    print(paste0("There was an error in the prelim OISST data on", prelim_date_error))
  }
}


# Add new prelim and final data to the files
## NB: -NEVER- run this in RStudio Server!
  ## It breaks the NetCDF write privileges
# doMC::registerDoMC(cores = 50)
# doParallel::registerDoParallel(cores = 50)
if(nrow(OISST_final_1) > 1 | nrow(OISST_prelim_1) > 1){
  print("Adding new data to NetCDF files")
  ## NB: 50 cores uses too much RAM if more than a few days are being added
  # doMC::registerDoMC(cores = 50)
  doParallel::registerDoParallel(cores = 50)
  plyr::l_ply(lon_OISST, .fun = OISST_merge, .parallel = TRUE,
  df_prelim = OISST_prelim_1, df_final = OISST_final_1)
  print("Added new data to NetCDF files")
  
  # Create date indexes
  # Get range of complete dates from the above `OISST_merge()` runs
  ncdf_dates <- as.Date(tidync("../data/OISST/avhrr-only-v2.ts.1440.nc")$transforms$time$time, origin = "1970-01-01")

  # final_dates index
  final_dates <- ncdf_dates[ncdf_dates <= final_time_end]
  # tester...
  # final_dates <- final_dates[1:length(final_dates-2)]
  # tail(final_dates)
  save(final_dates, file = "metadata/final_dates.Rdata")

  # prelim_dates index
  prelim_dates <- ncdf_dates[ncdf_dates > max(final_dates)]
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

# stop("Brake before MHW calcs as they still need to be updated to work with new R parallelism")


# 2: Update MHW event and category data -----------------------------------

# Prep guide info for this section
# doMC::registerDoMC(cores = 25)
doParallel::registerDoParallel(cores = 50)
load("metadata/final_dates.Rdata")
load("metadata/prelim_dates.Rdata")

# This takes roughly 35 minutes and is by far the largest time requirement
if(final_date_start != FALSE | prelim_date_start != FALSE){
  print("Updating MHW results")
  # system.time(
    plyr::l_ply(lon_OISST, .fun = MHW_event_cat_update, .parallel = TRUE)
  # ) # ~ 40 seconds per cycle
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

# Get the range of dates that need to be run
# The function `cat_clim_global_daily()` uses dplyr so a for loop is used here
  # Manually control dates as desired
  # update_dates <- seq(as.Date("2017-01-01"), as.Date("2019-12-31"), by = "day")
if (final_date_start != FALSE) {
  update_dates <- time_index[which(time_index >= final_date_start)]
  if (length(update_dates) > 0) {
    print(paste0("Updating global MHW slices from ",min(update_dates)," to ",max(update_dates)))
    # system.time(
    cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)))
    # ) # ~28 seconds
  }
} else if (prelim_date_start != FALSE) {
  update_dates <- time_index[which(time_index >= prelim_date_start)]
  if (length(update_dates) > 0) {
    print(paste0("Updating global MHW slices from ",min(update_dates)," to ",max(update_dates)))
    cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)))
  }
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

