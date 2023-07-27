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
# final_dates <- seq(as.Date("1982-01-01"), as.Date("2023-05-01"), by = "day")
# save(final_dates, file = "metadata/final_dates.Rdata")
# prelim_dates <- seq(as.Date("2022-03-20"), as.Date("2022-03-20"), by = "day")
# save(prelim_dates, file = "metadata/prelim_dates.Rdata")
load("metadata/final_dates.Rdata")
load("metadata/prelim_dates.Rdata")
if(length(final_dates) == 0) stop("Final date indexing has broken.")
if(length(prelim_dates) == 0) stop("Prelim date indexing has broken.")


# Get the source index monthly file folders with new data
print(paste0("Fetching OISST folder names at ",Sys.time()))
OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
OISST_url_month_get <- getURL(OISST_url_month)
OISST_months <- data.frame(months = readHTMLTable(OISST_url_month_get, skip.rows = 1:2)[[1]]$Name) %>%
  mutate(months = lubridate::as_date(str_replace(as.character(months), "/", "01"))) %>%
  filter(months >= max(lubridate::floor_date(final_dates, unit = "month"))) %>%
  mutate(months = gsub("-", "", substr(months, 1, 7)))


# Uncomment to manually control downloads
# final_dates <- as.Date("1981-12-31")
# OISST_months <- data.frame(months = apply(expand.grid(2023, stringr::str_pad(seq(1:5), 2, pad = "0")), 
#                                           1, paste, collapse = "")) |> arrange(months)


# Check if new data need downloading
# final_dates <- as.Date("2023-04-01") # tester...
OISST_filenames <- plyr::ldply(OISST_months$months, .fun = OISST_url_daily, final_dates, .parallel = T)

# Download and save files locally 
print(paste0("Saving new data locally at ", Sys.time()))
plyr::l_ply(OISST_filenames$full_name, OISST_url_daily_save, .parallel = T)

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
if(nrow(OISST_new) > 50) stop("A suspicious amount of new files are attempting to be downloaded.")
if(nrow(OISST_new) > 0){
  print(paste0("Prepping new data at ", Sys.time()))
  OISST_dat <- plyr::ldply(OISST_new$file_name, .fun = OISST_prep, .parallel = F)
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
## NB: -NEVER- run the following if statement in RStudio Server!
  ## It breaks the NetCDF write privileges
if(nrow(OISST_dat) > 2){
  print(paste0("Adding new data to NetCDF files at ", Sys.time()))
  ## NB: 50 cores uses too much RAM if more than a few days are being added
  doParallel::registerDoParallel(cores = 25) # NB: Change this back to 25
  plyr::l_ply(lon_OISST, .fun = OISST_merge, .parallel = TRUE, df = OISST_dat)
  print(paste0("Finished at ", Sys.time()))
  
  # Create date indexes
  ncdf_dates <- as.Date(tidync("../data/OISST/oisst-avhrr-v02r01.ts.1440.nc")$transforms$time$time, origin = "1970-01-01")

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
# return()

# Fix files that didn't run correctly:
# This happens every few months, usually due to a core slipping
# The easiest way to fix this is to load the `final_dates` and `prelim_dates` objects,
# alter them to require re-downloading the affected data, and then save the files.
# One then runs source() on this script IN A TERMINAL AND NOT RSTUDIO
# Check the MHW Tracker in a few minutes after this finishes running again
# to see if the correction propagated through successfully.

# If a NetCDF file breaks, re-create it with:
# OISST_lon_NetCDF(1197, tail(final_dates)[6])
# It is then necessary to ensure that all of the final data up to the most recent date are downloaded
# This will likely require that one manually edits the final_dates object at the start of this section
# And then re-run the full NetCDF merging process


# 2: Update MHW event and category data -----------------------------------

# Prep guide info for this section
doParallel::registerDoParallel(cores = 25)
ncdf_date <- max(as.Date(tidync("../data/OISST/oisst-avhrr-v02r01.ts.1440.nc")$transforms$time$time, origin = "1970-01-01"))
cat_lon_date <- max(readRDS("../data/cat_lon/MHW.cat.1440.Rda")$t)

# This takes roughly 300 minutes and is by far the largest time requirement
if(ncdf_date > cat_lon_date){
  print(paste0("Updating MHW/MCS results at ", Sys.time()))
  # system.time(
  plyr::l_ply(lon_OISST, .fun = event_cat_update, .parallel = TRUE); gc()
  # ) # ~ 258 seconds per cycle
  print(paste0("Finished MHW/MCS results at ", Sys.time()))
}

# Occasionally the cat_lon files don't come right
# One can usually tell if the size is under 400 kb
# This function can fix a specific file

# Run one
# event_cat_update(lon_OISST[1116], full = TRUE)

# Run many
# plyr::l_ply(lon_OISST[1300:1365], .fun = event_cat_update, .parallel = TRUE, full = TRUE)

# Run ALL
# plyr::l_ply(lon_OISST, .fun = event_cat_update, .parallel = TRUE, full = TRUE) # ~4.5 hours on 50 cores

# Find files that haven't been run since a certain date
# file_dates <- file.info(dir("../data/cat_lon/MCS", full.names = T)) %>%
#   mutate(file_name = sapply(strsplit(row.names(.), "/"), "[[", 5)) %>%
#   mutate(file_num = as.integer(sapply(strsplit(file_name, "[.]"), "[[", 3))) %>%
#   filter(ctime < Sys.Date()-1)
#   #filter(size < 1000000)
# plyr::l_ply(lon_OISST[file_dates$file_num], .fun = event_cat_update, .parallel = TRUE, full = TRUE)


# 3: Create daily global files --------------------------------------------

# Get most current processed OISST dates
time_index <- as.Date(tidync("../data/OISST/oisst-avhrr-v02r01.ts.1440.nc")$transforms$time$time, origin = "1970-01-01")
load("metadata/final_dates.Rdata")

# Get the range of dates that need to be run
  # Manually control dates as desired
# update_dates <- seq(as.Date("2009-01-01"), as.Date("2009-06-30"), by = "day")
update_dates <- time_index[which(time_index >= max(final_dates)-5)]
if(length(update_dates) > 0) {
  print(paste0("Updating global files from ",min(update_dates)," to ",max(update_dates)))
  print(paste0("Updating daily MHW/MCS cat files at ", Sys.time()))
  doParallel::registerDoParallel(cores = 50)
  # system.time(
  cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)))
  # ) # ~28 seconds
  print(paste0("Updating daily anom files at ", Sys.time()))
  doParallel::registerDoParallel(cores = 50)
  # system.time(
  anom_global_daily(date_range = c(min(update_dates), max(update_dates)))
  # ) # 455 seconds
  print(paste0("Finished global daily MHW/MCS files at ", Sys.time()))
}

# If any of the files created in this section break for any reason,
# simply change the 'update_dates' object manually and re-run the section.

# For loop to reprocess large sets of data one year at a time in 6 month batches
# doParallel::registerDoParallel(cores = 25) # Don't update more than 6 months at once on 40 cores
# for(i in 1982:2023){
#   update_dates <- seq(as.Date(paste0(i,"-01-01")), as.Date(paste0(i,"-06-30")), by = "day")
#   print(paste0("Updating from ",min(update_dates)," to ",max(update_dates)," at ", Sys.time()))
#   cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)))
#   print(paste0("Finished at ", Sys.time()))
#   gc(); Sys.sleep(10)
#   update_dates <- seq(as.Date(paste0(i,"-07-01")), as.Date(paste0(i,"-12-31")), by = "day")
#   print(paste0("Updating from ",min(update_dates)," to ",max(update_dates)," at ", Sys.time()))
#   cat_clim_global_daily(date_range = c(min(update_dates), max(update_dates)))
#   print(paste0("Finished at ", Sys.time()))
#   gc(); Sys.sleep(10)
# }


# 4: Check current dates --------------------------------------------------

# Indexes all files throughout the OISST/daily sub-folders to create the current_dates index
current_dates <- as.Date(sapply(str_split(list.files("../data/OISST/daily/", recursive = T, pattern = "daily"), "[.]"), "[[", 2))

# Check that no days are missing
possible_dates <- seq(as.Date("1982-01-01"), max(current_dates), by = "day")
if(length(possible_dates) > length(current_dates)){
  error_dates <- possible_dates[!possible_dates %in% current_dates]
  stop(paste0("The following dates are missing: ",error_dates))
}


# 5: Run annual summary ---------------------------------------------------

source("MHW_annual_summary.R")


# 6: Push to GitHub -------------------------------------------------------

system("git commit -a -m 'Daily run'")
system("git pull")
system("git push")

