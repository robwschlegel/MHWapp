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


# 1: Update OISST data ----------------------------------------------------

# doMC::registerDoMC(cores = 25) # 50 cores uses up too much RAM
# 
# # The information for the NOAA OISST data
rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
# 
# # Find most up-to-date NOAA data
# NOAA_info <- info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
# NOAA_time_range <- NOAA_info$alldata$time$value[3]
# NOAA_time_start <- as.Date(as.POSIXct(as.numeric(sapply(strsplit(as.character(NOAA_time_range), ", "), "[[", 1)),
#                                       origin = "1970-01-01 00:00:00"))
# NOAA_time_end <- as.Date(as.POSIXct(as.numeric(sapply(strsplit(as.character(NOAA_time_range), ", "), "[[", 2)),
#                                      origin = "1970-01-01 00:00:00"))
# 
# # Check if newer data need to/can be downloaded
# if(max(current_dates) < NOAA_time_end) {
#   download_date_start <- paste0(max(current_dates)+1,"T00:00:00Z")
#   # tester...
#   download_date_end <- paste0(max(current_dates)+1,"T00:00:00Z")
#   if(download_date_end > NOAA_time_end) stop("Trying to download NOAA data that are not yet available.")
#   # The real line of code
#   # download_date_end <- paste0(NOAA_time_end,"T00:00:00Z")
#   } else {
#     download_date_start <- FALSE
# }
# 
# # Download data not in 'current_dates' if necessary/possible
# if(download_date_start != FALSE){
#   OISST_update_1 <- OISST_dl(c(download_date_start, download_date_end))
# }
# 
# # Prep for inclusion in NetCDF files
# OISST_update_2 <- OISST_prep(OISST_update_1)
# 
# # Update the files
# plyr::ldply(lon_OISST, .fun = OISST_merge, .parallel = TRUE,
#             df = OISST_update_2, current_dates = current_dates)


# 2: Update MHW event and category data -----------------------------------

doMC::registerDoMC(cores = 50)

# system.time(
# plyr::ldply(lon_OISST, .fun = MHW_event_cat_update, .parallel = TRUE,
#             current_dates = current_dates)
# ) # This takes roughly 15 minutes and is by far the largest time requirement


# 3: Create daily global files --------------------------------------------

doMC::registerDoMC(cores = 50)

# Get most current processed OISST dates
# nc_OISST <- nc_open(OISST_files[1])
# time_index <- as.Date(ncvar_get(nc_OISST, "time"), origin = "1970-01-01")
# nc_close(nc_OISST)

# Get the range of dates that need to be run
# update_dates <- time_index[time_index > max(current_dates)]

update_dates <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by = "day")

# update_dates <- seq(as.Date("1984-01-01"), as.Date("2017-12-31"), by = "day")

# Process the lot of them
# print(paste0("Began creating daily slices at ",Sys.time()))
plyr::ldply(update_dates, .fun = cat_clim_global_daily, .parallel = TRUE)
# print(paste0("Finished creating daily slices at ",Sys.time()))


# 4: Update current_dates -------------------------------------------------

# Indexes all files throughout the cat_clim sub-folders to create the current_dates index
current_dates <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim", 
                                  full.names = TRUE, recursive = TRUE))
current_dates <- sapply(strsplit(current_dates, "cat.clim."), "[[", 3)
current_dates <- as.Date(as.vector(sapply(strsplit(current_dates, ".Rda"), "[[", 1)))
save(current_dates, file = "current_dates.RData")

# Check that no days are missing
possible_dates <- seq(as.Date("1982-01-01"), max(current_dates), by = "day")
if(length(possible_dates) > length(current_dates)){
  error_dates <- possible_dates[!possible_dates %in% current_dates]
  stop(paste0("The following dates are missing: ",error_dates))
}

