# This script houses all of the functions that may be used when,
# for whatever reason, something goes sideways with "MHW_daily.R"
# These functions are not meant to be run in mutiples,
# rather they should be run one at a time as spot fixes

source("MHW_daily_functions.R")
# source("../tikoraluk/MHW_prep.R")


# Meta-data ---------------------------------------------------------------

# File locations
base_files <- dir("../data/MHW", pattern = "MHW.calc.", full.names = T)
MHW_event_files <- dir("../data/event", pattern = "MHW.event.", full.names = T)
# seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)
cat_lon_files <- dir("../data/cat_lon", full.names = T)
# cat_clim_files <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim", 
# full.names = TRUE, recursive = TRUE))

# This is not saved in an object as this script is not designed to be autonomous
# The human running this script must look at this output and act accordingly
# rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon",
#               url = "https://www.ncei.noaa.gov/erddap/")


# Download a specific OISST lon slice -------------------------------------

# This downloads the data
OISST_lon_dl <- function(times, lon_step, product){
  lon_val <- ifelse(lon_OISST[lon_step] < 0, lon_OISST[lon_step]+ 360, lon_OISST[lon_step])
  oisst_res <- griddap(x = product, 
                       url = "https://www.ncei.noaa.gov/erddap/", 
                       time = times, 
                       depth = c(0, 0),
                       latitude = c(-89.875, 89.875),
                       longitude = rep(lon_val, 2),
                       fields = "sst")$data %>% 
    dplyr::rename(temp = sst, t = time) %>% 
    mutate(temp = round(temp, 2),
           lon = ifelse(lon > 180, lon-360, lon),
           t = as.Date(str_remove(t, "T00:00:00Z"))) %>%
    select(lon, lat, t, temp) %>% 
    na.omit()
}


# Fix a NetCDF file -------------------------------------------------------

# Prep a single lon download
# nc_file <- sst_new
# OISST_lon_prep <- function(nc_file){
#   
#   # Open the NetCDF connection
#   nc <- nc_open(nc_file$summary$filename)
#   
#   # Extract the SST values and add the lon/lat/time dimension names
#   res <- ncvar_get(nc, varid = "sst")
#   dimnames(res) <- list(lat = nc$dim$latitude$vals,
#                         t = nc$dim$time$vals)
#   
#   # Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
#   res <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>% 
#     mutate(t = as.Date(as.POSIXct(t, origin = "1970-01-01 00:00:00")),
#            temp = round(temp, 2))
#   
#   # Close the NetCDF connection and finish
#   nc_close(nc)
#   return(res)
# }

# Function for creating NetCDF files from OISST data
OISST_ncdf <- function(df){
  
  # Determine lon slice
  lon_row <- which(lon_OISST == df$lon[1])
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  # Set file name
  ncdf_file_name <- paste0("../data/OISST/avhrr-only-v2.ts.",lon_row_pad,".nc")
  # tester...
  # ncdf_file_name <- paste0("proc/test/test.",lon_row_pad,".nc")
  
  # Set the dataframe in question
  dataset <- df %>% 
    mutate(t = as.integer(t),
           lon = ifelse(lon > 180, lon-360, lon))
  
  # lon
  xvals <- unique(dataset$lon)
  if(length(xvals) > 1) stop("Too many lon values. Should only be one.")
  # xvals_df <- data.frame(lon = lon_lat_OISST$lon)
  nx <- length(xvals)
  lon_def <- ncdim_def("lon", "degrees_east", xvals)
  
  # lat
  yvals <- lat_OISST
  # yvals_df <- data.frame(lat = lon_lat_OISST$lat)
  ny <- length(yvals)
  lat_def <- ncdim_def("lat", "degrees_north", yvals)
  
  # time
  tunits <- "days since 1970-01-01 00:00:00"
  tvals <- seq(min(dataset$t), max(dataset$t))
  nt <- length(tvals)
  time_def <- ncdim_def("time", tunits, tvals, unlim = TRUE)
  
  dfa <- dataset %>%
    mutate(t2 = t) %>% 
    group_by(t2) %>%
    nest() %>%
    mutate(data2 = purrr::map(data, OISST_acast)) %>%
    select(-data)
  
  dfa_temp <- abind(dfa$data2, along = 3)
  
  temp_def <- ncvar_def(name = "sst", units = "deg_C", 
                        dim = list(lat_def, lon_def, time_def), 
                        longname = "Sea Surface Temperature",
                        missval = -999, prec = "float")
  
  ncout <- nc_create(filename = ncdf_file_name, vars = list(temp_def), force_v4 = T)
  
  ncvar_put(nc = ncout, varid = temp_def, vals = dfa_temp)
  
  ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"time","axis","T")

  # close the file, writing data to disk
  nc_close(ncout)
}

# Currently it does not seem reliable to "fix" a NetCDF file in place
# Rather the 1982 - 2017 data are fetched from the old MHW results,
# and the most up-to-date data are fetched and merged into a brand new file
# lon_step <- 745
# end_date <- "2019-08-21"
OISST_ncdf_fix <- function(lon_step, end_date){
  
  # Load existing data
  load(base_files[lon_step])
  sst_old <- MHW_clim(MHW_res) %>% 
    select(lon, lat, t, temp)
  rm(MHW_res)
  
  # Download all available final data from 2018 onwards
  final_info <- rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
  final_time_range <- final_info$alldata$time$value[3]
  final_time_start <- NOAA_date(final_time_range, 1)
  final_time_end <- NOAA_date(final_time_range, 2)
  dl_times_final <- c("2018-01-01", final_time_end)
  sst_final <- OISST_lon_dl(dl_times_final, lon_step, "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon")

  # Prep final data
  # sst_final_prep <- sst_final
    
  # Download all available prelim data up to the given date
  prelim_info <- rerddap::info(datasetid = "ncdc_oisst_v2_avhrr_prelim_by_time_zlev_lat_lon", url = "https://www.ncei.noaa.gov/erddap/")
  prelim_time_range <- prelim_info$alldata$time$value[3]
  prelim_time_start <- NOAA_date(prelim_time_range, 1)
  prelim_time_end <- NOAA_date(prelim_time_range, 2)
  if(end_date > prelim_time_end) stop("The end date provided is after the current end date of the prelim data")
  dl_times_prelim <- c(prelim_time_start, end_date)
  sst_prelim <- OISST_lon_dl(dl_times_prelim, lon_step, "ncdc_oisst_v2_avhrr_prelim_by_time_zlev_lat_lon") %>% 
    filter(!t %in% sst_final_prep$t) %>% 
    na.omit() %>% 
    group_by(t) %>% 
    filter(temp > 100) %>% 
    ungroup()

  # Prep prelim data
  # sst_prelim_prep <- sst_prelim %>% 
  #   # mutate(lon = lon_OISST[lon_step]) %>% 
  #   # select(lon, lat, t, temp) %>% 
  #   filter(!t %in% sst_final_prep$t) %>% 
  #   na.omit() %>% 
  #   group_by(t) %>% 
  #   filter(temp > 100) %>% 
  #   ungroup()
  
  # Combine and prep
  sst_all <- rbind(sst_old, sst_final) %>%
    rbind(., sst_prelim) %>% 
  mutate(temp = ifelse(is.na(temp), NA, temp),
         t = as.integer(t)) %>% 
    na.omit()
  
  # Re-create the NetCDF file and finish
  OISST_ncdf(sst_all)
  return(paste0("Finished ",lon_OISST[lon_step]))
}


# Fix data at the event/cat lon level -------------------------------------

# tester...
# lon_step <- lon_OISST[1]
MHW_event_cat_fix <- function(lon_step){
  
  # Determine correct lon/row/slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  # Begin the calculations
  print(paste0("Began run on ",MHW_event_files[lon_row]," at ",Sys.time()))
  
  # Extract each pixel time series based on how far back the oldest event occurred for the entire longitude slice
  sst_seas_thresh <- sst_seas_thresh_merge(lon_step, date_range = as.Date("1982-01-01"))
  
  # Calculate new event metrics with new data as necessary
  # system.time(
  MHW_event_cat <- sst_seas_thresh %>%
    mutate(lat2 = lat,
           lon = lon_step) %>% 
    group_by(lat2) %>% 
    nest() %>% 
    mutate(event_cat_res = map(data, event_calc_all)) %>% 
    ungroup() %>% 
    select(-data, -lat2) %>% 
    unnest(cols = c(event_cat_res))
  # ) # ~65 seconds to redo everything
  
  # Save results and exit
  MHW_event_new <- MHW_event_cat %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(cols = c(event_cat_res)) %>% 
    na.omit()
  if(length(MHW_event_new$lon) != 0) saveRDS(MHW_event_new, file = MHW_event_files[lon_row])
  
  MHW_cat_new <- MHW_event_cat %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(cols = c(event_cat_res)) %>% 
    na.omit()
  if(length(MHW_cat_new$lon) != 0) saveRDS(MHW_cat_new, file = cat_lon_files[lon_row])
  
  print(paste0("Finished run on MHW.event.",lon_row_pad,".Rda at ",Sys.time()))
}

# Function for extracting correct sst data based on pre-determined subsets
# It also calculates and returns corrected MHW metric results
# df <- slice(MHW_event_cat, 1) %>%
# unnest()
event_calc_all <- function(df){
  
  # Calculate events
  event_base <- detect_event(df)
  event_step_1 <- event_base$event %>% 
    mutate(lon = df$lon[1], lat = df$lat[1]) %>% 
    dplyr::select(lon, lat, event_no, duration:intensity_max, intensity_cumulative) %>%
    mutate_all(round, 3)
  if(nrow(event_step_1) == 0){
    event_step_1 <- data.frame(lon = df$lon[1], lat = df$lat[1],
                               event_no = NA, duration = NA, intensity_mean = NA,
                               intensity_max = NA, intensity_cumulative = NA)
    cat_step_1 <- data.frame(t = NA, lon = df$lon[1], lat = df$lat[1],
                             event_no = NA, intensity = NA, category = NA)
  } else {
    # Calculate categories
    cat_step_1 <- category(event_base, climatology = T)$climatology %>% 
      mutate(lon = df$lon[1],
             lat = df$lat[1]) %>% 
      select(t, lon, lat, event_no, intensity, category)
  }
  
  # Exit
  event_cat <- list(event = event_step_1,
                    cat = cat_step_1)
  return(event_cat)
}


# First run to create the OISST_pixel files -------------------------------
# NB: After this has been run once it won't need to be run again
# NB: Rather this code should replace the NetCDF creation step in the daily workflow

