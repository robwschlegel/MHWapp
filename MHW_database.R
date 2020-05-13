# MHW_database.R
# This script houses the code used to establish the database used in the MHW Tracker
# Note that the code in this script is only meant to be run once
# For best results uncomment the desired lines and run this script via source()
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
# lon_int <- 7
# day_int <- 1318
# product <- "CCI"
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

  print(paste0("Began run on ",file_name," at ",Sys.time()))
  
  # Extracts and processes a lon slice
  # system.time(
  lon_day <- tidync(file_name) %>%
    hyper_filter(lon = dplyr::between(lon, min(lon_pixels$lon), max(lon_pixels$lon))) %>%
    hyper_tibble() %>%
    left_join(lon_pixels, by = c("lon", "lat")) %>%
    na.omit() %>%
    # right_join(lon_pixels, by = c("lon", "lat")) %>%
    dplyr::select(lon_OI, lat_OI, time, analysed_sst) %>% 
    dplyr::rename(t = time, temp = analysed_sst, 
                  lon = lon_OI, lat = lat_OI) %>% 
    group_by(lon, lat, t) %>% 
    summarise(temp = mean(temp, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(t = as.Date(as.POSIXct(t, origin = '1981-01-01', tz = "GMT")),
           temp = round(temp-273.15, 2))
  # ) # 21 seconds, 65,525 rows
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
detect_MHW_lon <- function(lon_int, product, chosen_clim){
  
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
  
  registerDoParallel(cores = 25)
  system.time(
  lon_clim <- plyr::ddply(lon_full, c("lon", "lat"), ts2clm, .parallel = T,
                          climatologyPeriod = chosen_clim, 
                          .paropts = c(.inorder = FALSE))
  ) # 20 seconds for 1 OISST slice
  
  # Detect the MHWs
  system.time(
  lon_clim <- lon_full %>%
    group_by(lon, lat) %>%
    nest() %>%
    mutate(clims = map(data, ts2clm,
                       climatologyPeriod = chosen_clim)) %>% #,
           # events = map(clims, detect_event),
           # cats = map(events, category, S = FALSE)) %>%
    # select(-data, -clims)
    dplyr::select(-data) %>% 
    unnest(cols = clims)
  ) # 597 seconds for 1 OISST slice
  
  # Save the results
  res <- list(clim,
              event,
              cat)
  saveRDS(res, paste0("../data/",product,"_lon/",product,"_MHW_",
                      min_year,"-",max_year,"_",lon_int_pad,".Rds"))
  rm(CCI_lon_full); gc()
}

# Function for calculating and returning parred down MHW results
detect_event_lon <- function(df){
  event_base <- detect_event(df)
  event_step_1 <- event_base$event %>% 
    mutate(lon = df$lon, lat = df$lat) %>% 
    dplyr::select(lon, lat, event_no, duration:intensity_max, intensity_cumulative) %>%
    mutate_all(round, 3)
}

detect_cat_lon <- function(df){
  cat_step_1 <- category(df, climatology = T)$climatology %>% 
    mutate(event_no = event_no + df$event_no,
           lon = df$lon,
           lat = df$lat) %>% 
    select(t, lon, lat, event_no, intensity, category)
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
registerDoParallel(cores = 20)
plyr::l_ply(8:15, load_lon_full, .parallel = F, product = "CCI",
            date_start = as.Date("1981-09-01"), date_end = as.Date("2018-12-31"))

# Calculate CCI MHWs
# Clim period 1982 - 2011
# plyr::l_ply(1:15, detect_event_lon, .parallel = F)

# Create daily CCI MHW daily clim slice results


# 4: CMC database ---------------------------------------------------------

# The code used to download these data are at: "../tikoraluk/CMC_download.R"

# Rectangle all of the CMC data
# NB: This takes roughly 4 hours on 25 cores
# registerDoParallel(cores = 50)
# plyr::l_ply(1:15, load_lon_full, .parallel = F, product = "CMC",
#             date_start = as.Date("1991-09-01"), date_end = as.Date("2019-12-31"))


# # Calculate CMC MHWs based on the shared clim period: 1992 - 2018
# plyr::l_ply(1:15, detect_event_lon, .parallel = F, product = "CMC",
#             chosen_clim = c(as.Date("1992-01-01"), as.Date("2018-12-31")))

# Create daily CMC MHW clim slice results

