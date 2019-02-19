# This script combines all previous work towards the MHWapp into one location
  # The functions that this script draws on are found in "MHW_daily_functions.R"
# This script is designed to be run autonomously once per day by a chronjob
# It performs the following tasks:
## 1: Downloads the most recent NOAA OISST data available
##    and updates the local NetCDF files
## 2: Updates MHW event and category data
## 3: Creates daily global MHW category file(s)
## 4: Updates the current_dates index

source("../MHWapp/MHW_daily_functions.R")
# source("../MHWapp/MHW_daily_fixes.R")

## NB: Don't run any of this code manually
## NB: only run it via source("MHW_daily.R") in an R terminal
## NB: The NetCDF files seem to stop cooperating if they are opened manually via RStudio Server
## NB: My thinking is this somehow confuses the write privileges for the files

# 1: Update OISST data ----------------------------------------------------

doMC::registerDoMC(cores = 25) # 50 cores uses up too much RAM

# The information for the NOAA OISST data
# rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")

# Find most up-to-date NOAA data
NOAA_info <- rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
NOAA_time_range <- NOAA_info$alldata$time$value[3]
NOAA_time_start <- as.Date(as.POSIXct(as.numeric(sapply(strsplit(as.character(NOAA_time_range), ", "), "[[", 1)),
                                      origin = "1970-01-01 00:00:00"))
NOAA_time_end <- as.Date(as.POSIXct(as.numeric(sapply(strsplit(as.character(NOAA_time_range), ", "), "[[", 2)),
                                     origin = "1970-01-01 00:00:00"))

# Check if newer data need to/can be downloaded
if(max(current_dates) < NOAA_time_end) {
  download_date_start <- paste0(max(current_dates)+1,"T00:00:00Z")
  download_date_end <- paste0(NOAA_time_end,"T00:00:00Z")
  # tester...
  # download_date_end <- paste0(max(current_dates)+1,"T00:00:00Z")
  #
  if(download_date_end > NOAA_time_end) stop("Trying to download NOAA data that are not yet available.")
  # The real line of code
  # download_date_end <- paste0(NOAA_time_end,"T00:00:00Z")
} else {
  download_date_start <- FALSE
}

# Download data not in 'current_dates' if necessary/possible
if(download_date_start != FALSE){
  OISST_update_1 <- OISST_dl(c(download_date_start, download_date_end))
}

# Prep for inclusion in NetCDF files
if(download_date_start != FALSE){
  OISST_update_2 <- OISST_prep(OISST_update_1)
}

# Update the files
if(download_date_start != FALSE){
  plyr::ldply(lon_OISST, .fun = OISST_merge, .parallel = TRUE, df = OISST_update_2)
}

# Fix files that didn't run correctly
# This happens occassionally and appears to be an almost
# random access rights issue.
# It may have something to do with plyr accessing multiple files at once
# But that is just a guess

# which(lon_OISST == 92.375)

  # Run one
# OISST_ncdf_fix(370, end_date = "2019-01-22")

  # Run many
# plyr::ldply(c(2, 21, 370), .fun = OISST_ncdf_fix, .parallel = TRUE, end_date = "2019-01-22")


# 2: Update MHW event and category data -----------------------------------

# doMC::registerDoMC(cores = 50)
# 
# This takes roughly 15 minutes and is by far the largest time requirement
if(download_date_start != FALSE){
  # system.time(
    plyr::ldply(lon_OISST, .fun = MHW_event_cat_update, .parallel = TRUE)
  # ) # ~ 26 seconds per cycle
}
  # Occasionaly the cat_clim files don't come right
# One can usually tell if the size is under 400 kb
# This function can fix a specific file
  
  # Run one
# MHW_event_cat_fix(lon_OISST[370])

  # Run many
# plyr::ldply(lon_OISST[c(2, 21, 370)], .fun = MHW_event_cat_fix, .parallel = TRUE)


# 3: Create daily global files --------------------------------------------

doMC::registerDoMC(cores = 50)

# Get most current processed OISST dates
nc_OISST <- nc_open(OISST_files[1])
time_index <- as.Date(ncvar_get(nc_OISST, "time"), origin = "1970-01-01")
nc_close(nc_OISST)

# Get the range of dates that need to be run
# Manually control dates as desired
# current_dates <- seq(as.Date("1982-01-01"), as.Date("2018-12-31"), by = "day")
update_dates <- time_index[time_index > max(current_dates)]

# Process the lot of them
# The function uses dplyr so a for loop is used here
if(length(update_dates) > 0){
  for(i in 1:length(update_dates)){
    cat_clim_global_daily(update_dates[i])
  } # ~15 seconds for one
}

# 4: Update current_dates -------------------------------------------------

# Indexes all files throughout the cat_clim sub-folders to create the current_dates index
current_dates <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim", 
                                  full.names = TRUE, recursive = TRUE))
current_dates <- sapply(strsplit(current_dates, "cat.clim."), "[[", 3)
current_dates <- as.Date(as.vector(sapply(strsplit(current_dates, ".Rda"), "[[", 1)))
# tail(current_dates)
save(current_dates, file = "shiny/current_dates.RData")

# Check that no days are missing
possible_dates <- seq(as.Date("1982-01-01"), max(current_dates), by = "day")
if(length(possible_dates) > length(current_dates)){
  error_dates <- possible_dates[!possible_dates %in% current_dates]
  stop(paste0("The following dates are missing: ",error_dates))
}

