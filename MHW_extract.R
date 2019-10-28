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


# Find the index of a range of lon steps to manually download -------------

# The longitude associated with the files
lon_seq <-  c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))

# Add this to the file directories
MHW_event_files <- data.frame(file_name = dir("../data/event", pattern = "MHW.event.", full.names = T),
                              lon_step = lon_seq, stringsAsFactors = F)
cat_lon_files <- data.frame(file_name = dir("../data/cat_lon", full.names = T),
                            lon_step = lon_seq, stringsAsFactors = F)
seas_thresh_files <- data.frame(file_name = dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T),
                                lon_step = lon_seq, stringsAsFactors = F)

# Check the contents of a file
MHW_event_file <- readRDS(MHW_event_files$file_name[1])
cat_lon_file <- readRDS(cat_lon_files$file_name[1])


# Function for combining multiple lon steps into a single file ------------

# 'lon_steps' is expected to be two numbers, the range of lon values
munge_lon_steps <- function(lon_steps, lon_files){
  file_names <- lon_files %>% 
    filter(lon_step >= min(lon_steps), lon_step <= max(lon_steps))
  df <- plyr::ldply(file_names$file_name, readRDS, .parallel = T)
  return(df)
}



# US west coast
USWC <- seq(-130, -117)
USWC_MHW_event_files <- MHW_event_files %>% 
  filter(lon_step >= min(USWC), lon_step <= max(USWC))
USWC_MHW_event_file <- plyr::ldply(USWC_MHW_event_files$file_name, readRDS, .parallel = T)
save(USWC_MHW_event_file, file = "data/USWC_MHW_event_file.RData")


