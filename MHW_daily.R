# This script combines all previous work towards the MHWapp into one location
  # The functions that this script draws on are found in "MHW_daily_functions.R"
# This script is designed to be run autonomously once per day by a chronjob
# It performs the following tasks:
## 1: Downloads the most recent final and prelim NOAA OISST 
##    data available and updates the local NetCDF files
## 2: Updates MHW event and category data
## 3: Creates daily global MHW category file(s)
## 4: Updates the `final_dates`, `prelim_dates`, and `current_dates` indexes

source("../MHWapp/MHW_daily_functions.R")
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
  final_date_start <- paste0(max(final_dates)+1,"T00:00:00Z")
  final_date_end <- paste0(final_time_end,"T00:00:00Z")
} else {
  final_date_start <- FALSE
}


# Check if newer final data need to/can be downloaded
print("Checking if new prelim data need downloading")
load("metadata/prelim_dates.Rdata")
if(max(prelim_dates) < prelim_time_end) {
  prelim_date_start <- paste0(max(prelim_dates)+1,"T00:00:00Z")
  prelim_date_end <- paste0(prelim_time_end,"T00:00:00Z")
  prelim_date_blank <- FALSE
  if(max(prelim_dates)+1 < prelim_time_start){
    prelim_date_blank <- seq(max(prelim_dates)+1, prelim_time_start-1, by = "day")
    prelim_date_start <- paste0(prelim_time_start, "T00:00:00Z")
    print(paste0("The day(s) ",prelim_date_blank," are not available in the prelim data and are being filled with ",prelim_time_start))
  }
} else {
  prelim_date_start <- FALSE
  prelim_date_blank <- FALSE
}


# If there are no new data, stop the process
if(final_date_start == FALSE & prelim_date_start == FALSE)
  stop("No new data to download")


# Download data not in 'final_dates' if necessary/possible
if(final_date_start != FALSE){
  print(paste0("Downloading final data from ",final_date_start," to ",final_date_end))
  OISST_final_1 <- OISST_dl(c(final_date_start, final_date_end),
                            "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon")
}


# Download data not in 'prelim_dates' if necessary/possible
if(prelim_date_start != FALSE){
  print(paste0("Downloading prelim data from ",prelim_date_start," to ",prelim_date_end))
  OISST_prelim_1 <- OISST_dl(c(prelim_date_start, prelim_date_end),
                            "ncdc_oisst_v2_avhrr_prelim_by_time_zlev_lat_lon")
}


# Prep final data for NetCDF files
if(final_date_start != FALSE){
  print("Prepping new final data")
  OISST_final_2 <- OISST_prep(OISST_final_1)
} else {
  OISST_final_2 <- data.frame(lon = NA, lat = NA, t = NA, temp = NA)
}


# Prep prelim data for NetCDF files
if(prelim_date_start != FALSE){
  print("Prepping new prelim data")
  OISST_prelim_2 <- OISST_prep(OISST_prelim_1)
} else {
  OISST_prelim_2 <- data.frame(lon = NA, lat = NA, t = NA, temp = NA)
}


# Fill blank days from prelim data with earliest day available
# NB: This should only occur very rarely
if(prelim_date_blank != FALSE){
  prelim_blank_plug <- filter(OISST_prelim_2, t == prelim_time_start)
  for(i in length(prelim_date_blank):1){
    prelim_blank_plug_step <- prelim_blank_plug
    prelim_blank_plug_step$t <- prelim_date_blank[i]
    OISST_prelim_2 <- rbind(prelim_blank_plug_step ,OISST_prelim_2)
  }
}


# Catch the dates when the data may be incorrect and remove anything from there forward
if(nrow(OISST_final_2) > 1){
  if(max(OISST_final_2$temp, na.rm = T) > 100){
    final_date_error <- min(filter(OISST_final_2, temp > 100)$t)
    OISST_final_2 <- filter(OISST_final_2, t < final_date_error)
  }
}
if(nrow(OISST_prelim_2) > 1){
  if(max(OISST_prelim_2$temp, na.rm = T) > 100){
    prelim_date_error <- min(filter(OISST_prelim_2, temp > 100)$t)
    OISST_prelim_2 <- filter(OISST_prelim_2, t < prelim_date_error)
  }
}


# Add new prelim and final data to the files
## NB: -NEVER- run this in RStudio Server!
  ## It breaks the NetCDF write privileges
if(nrow(OISST_final_2) > 1 | nrow(OISST_prelim_2) > 1){
  print("Adding new data to NetCDF files")
  ## NB: 50 cores uses too much RAM
  doMC::registerDoMC(cores = 25)
  plyr::l_ply(lon_OISST, .fun = OISST_merge, .parallel = TRUE,
              df_prelim = OISST_prelim_2, df_final = OISST_final_2)
  print("Added new data to NetCDF files")

  # Create date indexes
  # Get range of complete dates from the above `OISST_merge()` runs
  nc <- nc_open("../data/OISST/avhrr-only-v2.ts.1440.nc")
  ncdf_dates <- as.Date(nc$dim$time$vals, origin = "1970-01-01")
  # tail(current_dates)
  nc_close(nc)

  # final_dates index
  final_dates <- ncdf_dates[ncdf_dates <= final_time_end]
  # tail(final_dates)
  save(final_dates, file = "metadata/final_dates.Rdata")

  # prelim_dates index
  prelim_dates <- ncdf_dates[ncdf_dates <= prelim_time_end]
  # tail(prelim_dates)
  save(prelim_dates, file = "metadata/prelim_dates.Rdata")
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

doMC::registerDoMC(cores = 50)

# This takes roughly 15 minutes and is by far the largest time requirement
if(final_date_start != FALSE){
  print("Updating MHW results based on new final+prelim data")
  # system.time(
    plyr::l_ply(lon_OISST, .fun = MHW_event_cat_update, .parallel = TRUE,
                final_start = gsub("T00:00:00Z", "", final_date_start))
                # final_start = "2019-02-10")
  # ) # ~ 26 seconds per cycle
} else if(prelim_date_start != FALSE) {
  print("Updating MHW results based on new prelim data only")
  plyr::l_ply(lon_OISST, .fun = MHW_event_cat_update, .parallel = TRUE,
              final_start = gsub("T00:00:00Z", "", prelim_date_start))
}

  # Occasionaly the cat_lon files don't come right
# One can usually tell if the size is under 400 kb
# This function can fix a specific file

  # Run one
# MHW_event_cat_fix(lon_OISST[20])

  # Run many
# plyr::ldply(lon_OISST[1117:1182], .fun = MHW_event_cat_fix, .parallel = TRUE)


# 3: Create daily global files --------------------------------------------

doMC::registerDoMC(cores = 50)

# Get most current processed OISST dates
nc_OISST <- nc_open(OISST_files[1440])
time_index <- as.Date(ncvar_get(nc_OISST, "time"), origin = "1970-01-01")
nc_close(nc_OISST)

# Get the range of dates that need to be run
# The function `cat_clim_global_daily()` uses dplyr so a for loop is used here
  # Manually control dates as desired
  # update_dates <- seq(as.Date("2019-05-10"), as.Date("2019-05-16"), by = "day")
if(final_date_start != FALSE) {
  update_dates <- time_index[which(time_index >= gsub("T00:00:00Z", "", final_date_start))]
  if(length(update_dates) > 0) {
    print(paste0("Updating global MHW slices from ",min(update_dates)," to ",max(update_dates)))
    for(i in 1:length(update_dates)) {
      cat_clim_global_daily(update_dates[i])
    } # ~11 seconds for one
    }
  } else if(prelim_date_start != FALSE) {
    update_dates <- time_index[which(time_index >= gsub("T00:00:00Z", "", prelim_date_start))]
    if(length(update_dates) > 0) {
      print(paste0("Updating global MHW slices from ",min(update_dates)," to ",max(update_dates)))
      for(i in 1:length(update_dates)) {
        cat_clim_global_daily(update_dates[i])
      } # ~11 seconds for one
    }
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

