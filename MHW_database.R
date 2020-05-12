# MHW_database.R
# This script houses the code used to establish the database used in the MHW Tracker
# Note that the code in this script is only meant to be run once
# 1: Setup the environment
# 2: OISST database
# 3: CCI database
# 4: CMC database
# 5: Nog een


# 1: Setup ----------------------------------------------------------------

# Libraries
.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(tidync)
library(doParallel)

# Set cores
registerDoParallel(cores = 25)

# Metadata
source("metadata/metadata.R")


# 2: OISST database -------------------------------------------------------

# The NOAA OISST database for the MHW Tracker was created while the foundational
# code base was still being developed. For this reason one will not find the code
# here that downloads and establishes the file structure used in the Tracker. I
# do intend to write this process out at some point in the future.

# The code needed to download and prep the NOAA OISST data for MHW calculations 
# may be found here: https://theoceancode.netlify.app/post/dl_env_data_r/


# 3: CCI database ---------------------------------------------------------

# The code used to download the CCI data may also be found in the same post:
# https://theoceancode.netlify.app/post/dl_env_data_r/

# Another blog post is used for the calculation of MHWs in gridded data:
# https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html

# NCDUMP
# ncdump::NetCDF(CCI_files[1])$variable[1:5]

# Function for loading a day of CCI pixels in a chosen OISST lon slice
# chosen_lon <- lon_OISST[1]
# lon_int <- 15
# day_int <- 1318
CCI_OISST_lon_day <- function(day_int, lon_int){
  
  # Set the range of lon values
  lon_int_1 <- (lon_int*100)-99
  lon_int_2 <- (lon_int*100)
  if(lon_int_2 > 1440) lon_int_2 <- 1440
  
  # Find the range of lon values
  lon_steps <- lon_OISST[seq(lon_int_1, lon_int_2)]
  
  # Find the pixels nearest to the chosen OISST lon slice
  CCI_pixels <- filter(CCI_OISST_coords, lon_OI %in% lon_steps)
  
  # Extracts and processes a CCI lon slice
  # system.time(
  CCI_lon_day <- tidync(CCI_files[day_int]) %>%
    hyper_filter(lon = dplyr::between(lon, min(CCI_pixels$lon), max(CCI_pixels$lon))) %>%
    hyper_tibble() %>%
    right_join(CCI_pixels, by = c("lon", "lat")) %>% 
    dplyr::select(lon_OI, lat_OI, time, analysed_sst) %>% 
    dplyr::rename(t = time, temp = analysed_sst, 
                  lon = lon_OI, lat = lat_OI) %>% 
    group_by(lon, lat, t) %>% 
    summarise(temp = mean(temp, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(t = as.Date(as.POSIXct(t, origin = '1981-01-01', tz = "GMT")),
           temp = round(temp-273.15, 2))
  # )
  return(CCI_lon_day)
}

# Function for loading all data within a lon slice
# lon_int <- 1
# registerDoParallel(cores = 50)
CCI_OISST_lon_full <- function(lon_int){
  date_int_range <- 1:length(seq(as.Date("1981-09-01"), as.Date("2018-12-31"), by = "day"))
  print(paste0("Began run on ",lon_int," at ",Sys.time()))
  CCI_lon_full <- plyr::ldply(date_int_range, CCI_OISST_lon_day, .parallel = T, 
                              lon_int = lon_int) # ~45 minutes on 50 cores for 100 lon slices
  lon_int_pad <- str_pad(lon_int, width = 2, pad = "0", side = "left")
  saveRDS(CCI_lon_full, paste0("../data/CCI_lon/CCI_SST_",lon_int_pad,".Rds"))
  rm(CCI_lon_full); gc()
}

# Run them all
  # NB: Only run this to fully rectangle ALL of the CCI data
  # This takes roughly 24 hours on 25 cores
# registerDoParallel(cores = 25)
plyr::l_ply(1:15, CCI_OISST_lon_full, .parallel = F, .progress = "text")

# Calculate CCI MHWs based on the OISST clim period: 1982 - 2011

# Create daily CCI MHW clim slice results
