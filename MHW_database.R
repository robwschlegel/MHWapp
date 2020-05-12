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
library(heatwaveR)
print(paste0("heatwaveR version = ",packageDescription("heatwaveR")$Version))

# Set cores
registerDoParallel(cores = 25)

# Metadata
source("metadata/metadata.R")

# Function for loading a day of CCI pixels in a chosen OISST lon slice
# lon_int <- 15
# day_int <- 1318
load_lon_day <- function(day_int, lon_int, product){
  
  # Set product related info
  if(product == "CMC"){
    file_date <- as.Date(substr(CMC_files[day_int], start = 13, stop = 21), format = "%Y%m%d")
    if(file_date <= as.Date("2016-12-31")){
      grid_coords <- CMC0.2_OISST_coords
    } else {
      grid_coords <- CMC0.1_OISST_coords
    }
    file_name <- CMC_files[day_int]
  } else if(product == "CCI"){
    grid_coords <- CCI_OISST_coords
    file_name <- CCI_files[day_int]
  } else {
    stop("Product name is incorrect")
  }

  # Set the range of lon values
  lon_int_1 <- (lon_int*100)-99
  lon_int_2 <- (lon_int*100)
  if(lon_int_2 > 1440) lon_int_2 <- 1440
  lon_steps <- lon_OISST[seq(lon_int_1, lon_int_2)]
  
  # Find the pixels nearest to the chosen OISST lon slice
  lon_pixels <- filter(grid_coords, lon_OI %in% lon_steps)

  # Extracts and processes a lon slice
  lon_day <- tidync(file_name) %>%
    hyper_filter(lon = dplyr::between(lon, min(lon_pixels$lon), max(lon_pixels$lon))) %>%
    hyper_tibble() %>%
    right_join(lon_pixels, by = c("lon", "lat")) %>% 
    dplyr::select(lon_OI, lat_OI, time, analysed_sst) %>% 
    dplyr::rename(t = time, temp = analysed_sst, 
                  lon = lon_OI, lat = lat_OI) %>% 
    group_by(lon, lat, t) %>% 
    summarise(temp = mean(temp, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(t = as.Date(as.POSIXct(t, origin = '1981-01-01', tz = "GMT")),
           temp = round(temp-273.15, 2))
  return(lon_day)
}

# Function for loading all data within a lon slice
load_lon_full <- function(lon_int, product, date_start, date_end){
  
  # Prep info
  print(paste0("Began run on ",product," ",lon_int," at ",Sys.time()))
  date_int_range <- 1:length(seq(date_start, date_end, by = "day"))
  
  # Load all days for chosen slice
  lon_full <- plyr::ldply(date_int_range, load_lon_day, .parallel = T, 
                          lon_int = lon_int, product = product)
  
  # Save and clean up
  lon_int_pad <- str_pad(lon_int, width = 2, pad = "0", side = "left")
  saveRDS(lon_full, paste0("../data/",product,"_lon/",product,"_SST_",lon_int_pad,".Rds"))
  rm(lon_full); gc()
}

# Function for calculating MHWs for a lon slice with a chosen clim period
# lon_int <- 1
# chosen_clim <- c("1982-01-01", "2011-12-31")
# product <- "OISST"
detect_event_lon <- function(lon_int, product, chosen_clim){
  
  # Prep the needed metadata
  if(file_dir == "OISST"){
    lon_int_pad <- str_pad(lon_int, width = 4, pad = "0", side = "left")
    lon_full <- tidync(paste0("../data/OISST/avhrr-only-v2.ts.",lon_int_pad,".nc")) %>% 
      hyper_tibble() %>% 
      dplyr::rename(t = time, temp = sst) %>% 
      mutate(t = as.Date(t, origin = "1970-01-01")) %>% 
      dplyr::select(lon, lat, t, temp)
  } else {
    lon_int_pad <- str_pad(lon_int, width = 2, pad = "0", side = "left")
    lon_full <- readRDS(paste0("../data/",product,"_lon/",product,"_SST_",lon_int_pad,".Rds"))
  }
  min_year <- lubridate::year(min(as.Date(chosen_clim)))
  max_year <- lubridate::year(max(as.Date(chosen_clim)))
  
  # Detect the MHWs
  # system.time(
  res <- lon_full %>%
    group_by(lon, lat) %>%
    nest() %>%
    mutate(clims = map(data, ts2clm,
                       climatologyPeriod = chosen_clim),
           events = map(clims, detect_event),
           cats = map(events, category, S = FALSE)) %>%
    select(-data, -clims)
  # ) # 389 seconds for 1 OISST slice
  
  # Save the results
  saveRDS(res, paste0("../data/",product,"_lon/",product,"_MHW_",
                      min_year,"-",max_year,"_",lon_int_pad,".Rds"))
  rm(CCI_lon_full); gc()
}


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

# Only run this to fully rectangle ALL of the CCI data
# This takes roughly 15 hours on 25 cores
# registerDoParallel(cores = 25)
# plyr::l_ply(1:15, load_lon_full, .parallel = F, product = "CCI",
#             date_start = as.Date("1981-09-01"), date_end = as.Date("2018-12-31"))

# Calculate CCI MHWs
# Clim period 1982 - 2011
# plyr::l_ply(1:15, detect_event_lon, .parallel = F)

# Create daily CCI MHW daily clim slice results


# 4: CMC database ---------------------------------------------------------

# The code used to download these data are at: "../tikoraluk/CMC_download.R"

# Function for loading a day of CCI pixels in a chosen OISST lon slice
# lon_int <- 1
# day_int <- 100
CMC_OISST_lon_day <- function(day_int, lon_int){
  
  # Set the range of lon values
  lon_int_1 <- (lon_int*100)-99
  lon_int_2 <- (lon_int*100)
  if(lon_int_2 > 1440) lon_int_2 <- 1440
  
  # Find the range of lon values
  lon_steps <- lon_OISST[seq(lon_int_1, lon_int_2)]
  
  # Find the file date to get correct lon/lat grid
  file_date <- as.Date(substr(CMC_files[day_int], start = 13, stop = 21), format = "%Y%m%d")
  if(file_date <= as.Date("2016-12-31")){
    CCI_OISST_coords <- CMC0.2_OISST_coords
  } else {
    CCI_OISST_coords <- CMC0.1_OISST_coords
  }
  
  # Find the pixels nearest to the chosen OISST lon slice
  CMC_pixels <- filter(CCI_OISST_coords, lon_OI %in% lon_steps)
  
  # Extracts and processes a CCI lon slice
  # system.time(
  CMC_lon_day <- tidync(CMC_files[day_int]) %>%
    hyper_filter(lon = dplyr::between(lon, min(CMC_pixels$lon), max(CMC_pixels$lon))) %>%
    hyper_tibble() %>%
    right_join(CMC_pixels, by = c("lon", "lat")) %>% 
    dplyr::select(lon_OI, lat_OI, time, analysed_sst) %>% 
    dplyr::rename(t = time, temp = analysed_sst, 
                  lon = lon_OI, lat = lat_OI) %>% 
    group_by(lon, lat, t) %>% 
    summarise(temp = mean(temp, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(t = as.Date(as.POSIXct(t, origin = '1981-01-01', tz = "GMT")),
           temp = round(temp-273.15, 2))
  # )
  return(CMC_lon_day)
}

plyr::l_ply(1:15, load_lon_full, .parallel = F, product = "CMC",
            date_start = as.Date("1981-09-01"), date_end = as.Date("2018-12-31"))

# Function for loading all data within a lon slice
# lon_int <- 1
# registerDoParallel(cores = 25)
CMC_OISST_lon_full <- function(lon_int){
  date_int_range <- 1:length(seq(as.Date("1991-09-01"), as.Date("2019-12-31"), by = "day"))
  print(paste0("Began run on CMC ",lon_int," at ",Sys.time()))
  system.time(
  CMC_lon_full <- plyr::ldply(date_int_range, CMC_OISST_lon_day, .parallel = T, 
                              lon_int = lon_int) # ~790 seconds on 25 cores for 100 lon slices
  )
  lon_int_pad <- str_pad(lon_int, width = 2, pad = "0", side = "left")
  saveRDS(CMC_lon_full, paste0("../data/CMC_lon/CMC_SST_",lon_int_pad,".Rds"))
  rm(CMC_lon_full); gc()
}

# Run them all
  # NB: Only run this to fully rectangle ALL of the CCI data
  # This takes roughly XXX hours on 25 cores
registerDoParallel(cores = 5)
plyr::l_ply(7:15, CMC_OISST_lon_full, .parallel = F)

# Calculate CMC MHWs based on the shared clim period: 1992 - 2018
# plyr::l_ply(1:15, CMC_detect_event, .parallel = F)

# Create daily CMC MHW clim slice results
