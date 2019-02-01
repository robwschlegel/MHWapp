# This script houses all of the functions used in "MHW_daily.R"
# This is done to keep everything tidier and easier to read

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ncdf4)
library(abind)
library(rerddap)
library(heatwaveR, lib.loc = "../R-packages/")
cat(paste0("heatwaveR version = ",packageDescription("heatwaveR")$Version))
# doMC::registerDoMC(cores = 25)


# Meta-data ---------------------------------------------------------------

load("shiny/lon_OISST.RData")
load("../tikoraluk/metadata/lon_lat_OISST.RData")
lon_lat_OISST <- arrange(lon_lat_OISST, lon, lat)

# File locations
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)
MHW_event_files <- dir("../data/event", pattern = "MHW.event.", full.names = T)
seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)
cat_lon_files <- dir("../data/cat_lon", full.names = T)
cat_clim_files <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim", 
                                   full.names = TRUE, recursive = TRUE))

# Date range of already processed data
## NB: Thi is currently static but must be self-updating to work correctly
### A self updating file that grabs dates from somewhere...
# load("current_dates.RData")
current_dates <- seq(as.Date("1982-01-01"), as.Date("2017-12-31"), by = "day")

# The current date
current_date <- Sys.Date()


# 1: Update OISST data functions ------------------------------------------

# This downloads the data
OISST_dl <- function(times){
  oisst_res <- griddap(x = "ncdc_oisst_v2_avhrr_by_time_zlev_lat_lon", 
                       url = "https://www.ncei.noaa.gov/erddap/", 
                       time = times, 
                       depth = c(0, 0),
                       latitude = c(-89.875, 89.875),
                       longitude = c(0.125, 359.875),
                       fields = "sst")
}

# This then preps them for further use
OISST_prep <- function(nc_file){
  
  # Open the NetCDF connection
  nc <- nc_open(nc_file$summary$filename)
  
  # Extract the SST values and add the lon/lat/time dimension names
  res <- ncvar_get(nc, varid = "sst")
  if(length(dim(res)) == 2){
    res <- base::array(res, dim = c(1440, 720, 1))
    dimnames(res) <- list(lon = nc$dim$longitude$vals,
                          lat = nc$dim$latitude$vals,
                          t = nc$dim$time$vals)
  } else if(length(dim(res)) == 3){
    dimnames(res) <- list(lon = nc$dim$longitude$vals,
                          lat = nc$dim$latitude$vals,
                          t = nc$dim$time$vals)
  } else {
    stop("Something has gone horribly wrong and the NOAA sst data have more than 3 dimensions.")
  }
  
  # Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
  res <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>% 
    mutate(t = as.Date(as.POSIXct(t, origin = "1970-01-01 00:00:00")),
           temp = round(temp, 2))
  
  # Close the NetCDF connection and finish
  nc_close(nc)
  return(res)
}

# Function for creating arrays from data.frames
# df <- OISST_step_2
OISST_acast <- function(df){
  # Ensurecorrect grid size
  lon_lat_OISST_sub <- lon_lat_OISST %>% 
    filter(lon == df$lon[1])
  # Round data for massive file size reduction
  df$temp <- round(df$temp, 2)
  # Force grid
  res <- df %>%
    right_join(lon_lat_OISST_sub, by = c("lon", "lat"))
  # Create array
  res_array <- base::array(res$temp, dim = c(720,1,1))
  dimnames(res_array) <- list(lat = lon_lat_OISST_sub$lat,
                              lon = unique(lon_lat_OISST_sub$lon),
                              t = unique(na.omit(res$t)))
  return(res_array)
}

# Function for merging OISST data into existing NetCDF files
# tester...
# lon_step <- lon_OISST[1]
# df <- OISST_update_2
OISST_merge <- function(lon_step, df, current_dates){
  
  ### Determine the correct lon slice/file
  # Determine lon slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  print(paste0("Began run on avhrr-only-v2.ts.",lon_row_pad,".nc at ",Sys.time()))
  
  # Determine file name
  ncdf_file_name <- paste0("../data/OISST/avhrr-only-v2.ts.",lon_row_pad,".nc")
  # ncdf_file_name <- paste0("../data/OISST/avhrr-only-v2.ts.",lon_row_pad,"-test.nc")
  
  ### Open NetCDF and determine dates present
  nc <- nc_open(ncdf_file_name, write = T)
  
  time_vals <- as.Date(nc$dim$time$vals, origin = "1970-01-01")
  # tail(time_vals)
  
  ### Grab the lon slice and data not yet in the NetCDF file
  OISST_step_1 <- df %>% 
    filter(lon  == lon_step,
           as.integer(t) > max(time_vals))
  
  if(nrow(OISST_step_1) > 0){
    
    ### Create data arrays
    OISST_step_2 <- OISST_step_1 %>% 
      mutate(temp = ifelse(is.na(temp), NA, temp),
             t = as.integer(t),
             lon = ifelse(lon > 180, lon-360, lon)) %>% 
      na.omit()
    
    dfa <- OISST_step_2 %>%
      mutate(t2 = t) %>% 
      group_by(t2) %>%
      nest() %>%
      mutate(data2 = purrr::map(data, OISST_acast)) %>%
      select(-data)
    
    dfa_temp <- abind(dfa$data2, along = 3)
    
    ### Add data to the corresponding NetCDF file
    for(i in 1:length(unique(dfa$t2))){
      ncvar_put(nc = nc, varid = "sst", vals = dfa_temp[,,i], verbose = FALSE,
                start = c(1,1,(length(nc$dim$time$vals)+i)), count = c(720,1,1))
      ncvar_put(nc = nc, varid = "time", vals = dfa$t2[i],
                start = (length(nc$dim$time$vals)+i), verbose = FALSE)
    }
  }
  # sst <- ncvar_get(nc, "sst")
  
  ### Close file and exit
  nc_sync(nc)
  nc_close(nc)
  print(paste0("Finished run on avhrr-only-v2.ts.",lon_row_pad,".nc at ",Sys.time()))
}


# 2: Update MHW event and category data functions -------------------------

# Function that loads and merges sst/seas/thresh for a given lon_step
lon_step <- lon_OISST[340]
start_date <- as.Date("2018-01-01")
end_date <- as.Date("2018-12-31")
sst_seas_thresh_merge <- function(lon_step, start_date, end_date){
  
}

# Function for updating the MHW event metric lon slice files
# tester...
# lon_step <- lon_OISST[340]
MHW_event_cat_update <- function(lon_step, current_dates){
  
  # Determine correct lon/row/slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  # Load current lon slice for event/category
  MHW_event <- readRDS(MHW_event_files[lon_row]) #%>% 
    ## NB: Delete this filter after re-running 2018 data
    # filter(date_end <= "2017-12-31")
    ##
  if(MHW_event$lon[1] != lon_step) stop("The lon_row indexing has broken down somewhere")
  MHW_cat_lon <- readRDS(cat_lon_files[lon_row])
  if(MHW_cat_lon$lon[1] != lon_step) stop("The lon_row indexing has broken down somewhere")
  
  # Begin the calculations
  print(paste0("Began run on ",MHW_event_files[lon_row]," at ",Sys.time()))
  
  # Find pixels where the max(current_dates) is the same as MHW date_end
  # This implies that the event is still ongoing and we will need to check
  # and update the values as necessary
  date_end_index <- MHW_event %>% 
    filter(date_end == max(current_dates))
  
  # Determine how far back in time to get old data based on the occurrence of the previous MHW
  previous_event_index <- MHW_event %>% 
    # filter(#lat %in% lat_index,
           ## NB: Delete this filter line after re-rnning 2018 data
           # date_end <= as.Date("2017-12-31")) %>% 
    ##
    group_by(lat) %>% 
    filter(event_no == max(event_no))
    # mutate(previous_event = ifelse(max(date_end) == max(current_dates), max(event_no)-1, max(event_no))) %>% 
    # filter(event_no == previous_event)
  
  # Extract each pixel time series and check if the temp went above the thresh
  # since the event metrics were last calculated
  # OISST data
  nc_OISST <- nc_open(ncdf_OISST_file_name)
  lat_vals <- as.vector(nc_OISST$dim$lat$vals)
  time_index <- as.Date(ncvar_get(nc_OISST, "time"), origin = "1970-01-01")
  # time_old_index <- time_index[time_index <= max(current_dates)]
  time_extract_index <- time_index[which(min(previous_event_index$date_end) == time_index):length(time_index)]
  sst_raw <- ncvar_get(nc_OISST, "sst", start = c(1,1,(which(min(previous_event_index$date_end) == time_index))))
  if(length(time_extract_index) == 1) dim(sst_raw) <- c(720,1,1)
  dimnames(sst_raw) <- list(lat = nc_OISST$dim$lat$vals,
                        t = time_extract_index)
  nc_close(nc_OISST)
  
  # Prep SST for further use
  sst <- as.data.frame(reshape2::melt(sst_raw, value.name = "temp"), row.names = NULL) %>%
    mutate(t = as.Date(t, origin = "1970-01-01")) %>%
    na.omit() %>% 
    ## NB: Delete this filter after re-running 2018 data
    filter(t <= "2018-12-31") %>% 
    ##
    dplyr::rename(ts_x = t, ts_y = temp) %>% 
    group_by(lat) %>%
    # heatwaveR:::make_whole_fast()
    nest() %>%
    mutate(whole_data = map(data, heatwaveR:::make_whole_fast)) %>%
    select(-data) %>%
    unnest() %>% 
    dplyr::rename(t = ts_x, temp = ts_y)
  
  # seas.thresh data
  nc_seas <- nc_open(ncdf_thresh_file_name)
  seas <- ncvar_get(nc_seas, "seas")
  dimnames(seas) <- list(lat = nc_seas$dim$lat$vals,
                         doy = nc_seas$dim$time$vals)
  thresh <- ncvar_get(nc_seas, "thresh")
  dimnames(thresh) <- list(lat = nc_seas$dim$lat$vals,
                           doy = nc_seas$dim$time$vals)
  nc_close(nc_seas)
  seas <- as.data.frame(reshape2::melt(seas, value.name = "seas"), row.names = NULL) %>%
    na.omit() %>% 
    dplyr::arrange(lat, doy)
  thresh <- as.data.frame(reshape2::melt(thresh, value.name = "thresh"), row.names = NULL) %>%
    na.omit() %>% 
    dplyr::arrange(lat, doy)
  
  # Check for which pixels have temperatures above the threshold
  sst_seas_thresh <- sst %>% 
    left_join(seas, by = c("lat", "doy")) %>%
    left_join(thresh, by = c("lat", "doy")) 
  sst_index <- sst_seas_thresh %>%  
    mutate(thresh_index = ifelse(temp > thresh, TRUE, FALSE)) %>% 
    filter(thresh_index == TRUE)
  
  # Create the final lat index for calculating new events
  lat_index <- unique(c(unique(date_end_index$lat), unique(sst_index$lat)))
  
  # Extract the correct sst data matching the desired subset from previous_event_index
  # Calculate new event metrics with new data as necessary
  MHW_event_cat <- previous_event_index %>% 
    mutate(lat2 = lat) %>% 
    group_by(lat2) %>% 
    nest() %>% 
    mutate(event_cat_res = map(data, event_calc,
                               sst_seas_thresh = sst_seas_thresh,
                               MHW_event = MHW_event,
                               MHW_cat_lon = MHW_cat_lon)) %>% 
    select(-data, -lat2) %>% 
    unnest() # ~70 seconds for 474 pixels
  # Save results and exit
  MHW_event_new <- MHW_event_cat %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest()
  # MHW_event_new_test <- MHW_event_new %>% 
  #   filter(lat == df$lat)
  saveRDS(MHW_event_new, file = MHW_event_files[lon_row])
  
  MHW_cat_new <- MHW_event_cat %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest()
  # MHW_cat_new_test <- MHW_cat_new %>% 
  #   filter(lat == df$lat)
  saveRDS(MHW_cat_new, file = cat_lon_files[lon_row])
  
  print(paste0("Finished run on MHW.event.",lon_row_pad,".Rda at ",Sys.time()))
}

# Function for extracting correct sst data based on pre-determined subsets
# It also caluclates and returns corrected MHW metric results
# df <- previous_event_index[20,]
event_calc <- function(df, sst_seas_thresh, MHW_event, MHW_cat_lon){
  
  # Extract necessary SST
  sst_step_1 <- sst_seas_thresh %>% 
    filter(lat == df$lat[1],
           t >= df$date_end) %>% 
    # na.omit() %>% 
    ## NB: Delete this filter after re-running 2018 data
    filter(t <= "2018-12-31") #%>% 
    ##
  
  # Calculate events
  event_base <- detect_event(sst_step_1)
  event_step_1 <- event_base$event %>% 
    mutate(lon = df$lon, lat = df$lat) %>% 
    dplyr::select(lon, lat, event_no, duration:intensity_max, intensity_cumulative) %>%
    mutate_all(round, 3)
  event_step_2 <- MHW_event %>% 
    filter(lat == df$lat[1],
           date_end != max(current_dates)) %>% 
    rbind(event_step_1) %>% 
    mutate(event_no = seq(1:n()))
  
  # Calculate categories
  cat_step_1 <- category(event_base, climatology = T)$climatology %>% 
    mutate(event_no = event_no + df$previous_event[1],
           lon = df$lon,
           lat = df$lat) %>% 
    select(t, lon, lat, event_no, intensity, category)
  cat_step_2 <- MHW_cat_lon %>%
    filter(lat == df$lat) %>%
    filter(!event_no %in% cat_step_1$event_no) %>% 
    rbind(cat_step_1)
  
  # Exit
  event_cat <- list(event = event_step_2,
                    cat = cat_step_2)
  return(event_cat)
}

# 3: Create daily global file functions -----------------------------------

# Function for loading a cat_lon slice and extracting a single day of values
# testers...
# cat_lon_file <- cat_lon_files[1]
# date_choice <- max(current_dates)+1
# date_choice <- min(update_dates)
load_sub_cat_clim <- function(cat_lon_file, date_choice){
  cat_clim <- readRDS(cat_lon_file)
  cat_clim_sub <- cat_clim %>%
    filter(t == date_choice)
  rm(cat_clim)
  return(cat_clim_sub)
}

# Function for loading, preping, and saving the daily global category slices
# tester...
# date_choice <- max(current_dates)+1
cat_clim_global_daily <- function(date_choice){
  cat_clim_daily <- map_dfr(cat_lon_files, load_sub_cat_clim, date_choice = date_choice) %>% 
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme"))) %>% 
    select(-t)
  cat_clim_year <- lubridate::year(date_choice)
  cat_clim_dir <- paste0("../data/cat_clim/",cat_clim_year)
  dir.create(as.character(cat_clim_dir), showWarnings = F)
  cat_clim_name <- paste0("cat.clim.",date_choice,".Rda")
  saveRDS(cat_clim_daily, file = paste0(cat_clim_dir,"/",cat_clim_name))
}

