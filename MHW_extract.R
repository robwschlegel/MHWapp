# MHW_extract.R
# The purpose of this script is to provide functions that make it
# easier/quicker to extract MHW/OISST data from tikoraluk for use offline


# Libraries ---------------------------------------------------------------

source("MHW_daily_functions.R")


# Extract SST for a lon step ----------------------------------------------

# See lon_lat_OISST for exact coordinate options
unique(lon_lat_OISST$lon)
unique(lon_lat_OISST$lat)

# Set extraction coordinates
lon_point <- 8.625
lat_point <- 58.375

# Set download name
dl_name <- "norway_2018"

## NB: Don't change this code chunk
OISST_dl <- sst_seas_thresh_merge(lon_step = lon_point,
                                  start_date = "2018-01-01") %>% 
  filter(lat == lat_point,
         t <= "2018-12-31") %>% 
  mutate(lon = lon_point) %>% 
  select(lon, everything())
saveRDS(OISST_dl, paste0("downloads/",dl_name,".Rda"))

# Extractions:
# USA  site coordinates: 41.478ºN, - 71.362º, MHW map for Aug 30 2018, time series for whole of 2018. 
# Norway site coordinate = 58,317861  8,596863, May 30, 2018, time series for whole of 2018.    

