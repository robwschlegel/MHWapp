# This script houses all of the functions used in "MHW_daily.R"
# This is done to keep everything tidier and easier to read

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rerddap)
library(ncdf4)
library(abind)
library(heatwaveR)
doMC::registerDoMC(cores = 25)


# Meta-data ---------------------------------------------------------------

load("../tikoraluk/metadata/lon_lat_OISST.RData")
load("../tikoraluk/metadata/lon_OISST.RData")
lon_lat_OISST <- arrange(lon_lat_OISST, lon, lat)

# File locations
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)
MHW_event_files <- dir("../data/event", pattern = "MHW.event.", full.names = T)
seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)
cat_clim_files <- dir("../data/cat_clim", full.names = T)

# Current date range
## NB: Thi is currently static but must be self-updating to work correctly
### A self updating file that grabs dates from somewhere...
# load("current_dates.RData")
current_dates <- seq(as.Date("1982-01-01"), as.Date("2018-12-31"), by = "day")


# Functions ---------------------------------------------------------------

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
  dimnames(res) <- list(lon = nc$dim$longitude$vals,
                        lat = nc$dim$latitude$vals,
                        t = nc$dim$time$vals)
  
  # Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
  res <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>% 
    mutate(t = as.Date(as.POSIXct(t, origin = "1970-01-01 00:00:00")),
           temp = round(temp, 2))
  
  # Close the NetCDF connection and finish
  nc_close(nc)
  return(res)
}

# Function for creating arrays from data.frames
OISST_acast <- function(df, var, lon_sub = T){
  if(lon_sub){
    lon_lat_OISST_sub <- lon_lat_OISST %>% 
      filter(lon == df$lon[1])
  } else {
    lon_lat_OISST_sub <- lon_lat_OISST
  }
  if("temp" %in% colnames(df)){
    df$temp <- round(df$temp, 2)
  }
  res <- df %>%
    right_join(lon_lat_OISST_sub, by = c("lon", "lat")) %>%
    reshape2::acast(lat~lon, value.var = var)
}

# Function for merging OISST data into existing NetCDF files
# tester...
# lon_step <- lon_OISST[1]
# df <- OISST_2018_prep
OISST_merge <- function(lon_step, df){
  
  ### Determine the correct lon slice/file
  # Determine lon slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  print(paste0("Began run on ",lon_row_pad," at ",Sys.time()))
  
  # Determine file name
  ncdf_file_name <- paste0("../data/OISST/avhrr-only-v2.ts.",lon_row_pad,".nc")
  # ncdf_file_name <- paste0("../data/OISST_2018/avhrr-only-v2.ts.",lon_row_pad,"-test.nc")
  
  ### Open NetCDF and determine dates present
  nc <- nc_open(ncdf_file_name, write = T)
  
  # time_vals <- as.Date(nc$dim$time$vals, origin = "1970-01-01")
  time_vals <- seq(nc$dim$time$vals[1], length.out = length(nc$dim$time$vals))
  
  ### Grab the lon slice and data not yet in the NetCDF file
  OISST_step_1 <- df %>% 
    filter(lon  == lon_step,
           as.integer(t) > max(time_vals))
  
  if(nrow(OISST_step_1) > 0){
    
    ### Create data arrays
    OISST_step_2 <- OISST_step_1 %>% 
      mutate(temp = ifelse(is.na(temp), NA, temp),
             t = as.POSIXct(t),
             lon = ifelse(lon > 180, lon-360, lon)) %>% 
      na.omit()
    
    dfa <- OISST_step_2 %>%
      group_by(t) %>%
      nest() %>%
      mutate(data2 = purrr::map(data, OISST_acast, "temp")) %>%
      select(-data)
    
    dfa_temp <- abind(dfa$data2, along = 3)
    
    ### Add data to the corresponding NetCDF file
    for(i in 1:length(dfa$t)){
      ncvar_put(nc = nc, varid = "sst", vals = dfa_temp[,,i], verbose = FALSE,
                start = c(1,1,length(nc$dim$time$vals)+i), count = c(720,1,1))
    }
  }
  # sst <- ncvar_get(nc, "sst")
  
  ### Close file and exit
  # nc_sync(nc)
  nc_close(nc)
  print(paste0("Finished run on ",lon_row_pad," at ",Sys.time()))
}