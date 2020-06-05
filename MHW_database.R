# MHW_database.R
# This script houses the code used to establish the database used in the MHW Tracker
# Note that the code in this script is only meant to be run once
# For best results uncomment the desired lines and run this script via source()
# 1: Setup the environment
# 2: OISST database
# 3: CCI database
# 4: CMC database
# 5: Test visuals


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

# The product vs. OISST grids
CCI_OISST_coords <- readRDS("metadata/CCI_OISST_coords.Rds")
CMC0.2_OISST_coords <- readRDS("metadata/CMC0.2_OISST_coords.Rds")
CMC0.1_OISST_coords <- readRDS("metadata/CMC0.1_OISST_coords.Rds")

# Function for loading a single OISST file
load_OISST <- function(file_name){
  tidync_OISST <- tidync(file_name) %>% 
    hyper_tibble() %>% 
    mutate(time = as.Date(time, origin = "1970-01-01")) %>% 
    dplyr::rename(t = time, temp = sst) %>%
    dplyr::select(lon, lat, t, temp)
}

# Function to create wider OISST slices to match the rest of the project
widen_OISST <- function(lon_int){

  # Set the range of lon values
  print(paste0("Began run on OISST wide ",lon_int," at ",Sys.time()))
  lon_int_1 <- (lon_int*100)-99
  lon_int_2 <- (lon_int*100)
  if(lon_int_2 > 1440) lon_int_2 <- 1440
  
  # Load multiple thin OISST slices
  # system.time(
  wide_OISST <- plyr::ldply(OISST_files[lon_int_1:lon_int_2], load_OISST, 
                            .parallel = T, .paropts = c(.inorder = FALSE))
  # ) # 85 seconds on 25 cores
  
  # Save and clean up
  lon_int_pad <- str_pad(lon_int, width = 2, pad = "0", side = "left")
  saveRDS(wide_OISST, paste0("../data/OISST_lon/OISST_SST_",lon_int_pad,".Rds"))
  rm(wide_OISST); gc()
}

# Function for loading a day of pixels in a chosen lon slice
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
                          lon_int = lon_int, product = product, 
                          .paropts = c(.inorder = FALSE))
  
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
  print(paste0("Began run on ",product," ",lon_int," at ",Sys.time()))
  lon_int_pad <- str_pad(lon_int, width = 2, pad = "0", side = "left")
  lon_full <- readRDS(paste0("../data/",product,"_lon/",product,"_SST_",lon_int_pad,".Rds")) %>% 
    filter(t >= as.Date("1982-01-01"), t <= as.Date("2019-12-31"))
  min_year <- lubridate::year(min(as.Date(chosen_clim)))
  max_year <- lubridate::year(max(as.Date(chosen_clim)))

  # The full MHW results
  # system.time(
  lon_res <- plyr::dlply(lon_full, c("lon", "lat"), clim_event_cat, .parallel = T,
                         chosen_clim = chosen_clim, .paropts = c(.inorder = FALSE))
  # ) # 20 seconds for 1 OISST slice
  
  # Save the results
  saveRDS(lon_res, paste0("../data/",product,"_lon/",product,"_MHW_",
                          min_year,"-",max_year,"_",lon_int_pad,".Rds"))
  rm(lon_res); gc()
}

# Function for calculating and returning parred down MHW results
clim_event_cat <- function(df, chosen_clim){
  
  # Climatology
  clim_base <- ts2clm(df, climatologyPeriod = chosen_clim)
  
  # Clean climatology
  clim_clean <- dplyr::select(clim_base, doy, seas, thresh) %>% 
    unique() %>% arrange(doy)
  
  # Base MHW detectiom
  event_base <- detect_event(clim_base)
  
  # Clean it up
  event_clean <- event_base$event %>% 
    dplyr::select(event_no, duration:intensity_max, intensity_cumulative) %>%
    mutate_all(round, 3)
  
  # Cleaned up event categories
  cat_clean <- category(event_base, climatology = T)$climatology %>% 
    select(t, event_no, intensity, category)
  
  # Exit
  res <- list(clim = clim_clean,
              event = event_clean,
              cat = cat_clean)
  return(res)
}

# Function for extracting chosen parts of the MHW results
extract_MHW <- function(list_df, list_sub){
  list_df_sub <- lapply(list_df, `[`, c(list_sub)) %>% 
    plyr::ldply(., data.frame) %>% 
    separate(.id, into = c("lon_1", "lon_2", "lat_1", "lat_2"), sep = "[.]") %>% 
    unite(lon_1, lon_2, col = "lon", sep = ".") %>% 
    unite(lat_1, lat_2, col = "lat", sep = ".") %>% 
    rename_at(.vars = vars(starts_with(paste0(list_sub,"."))),
              .funs = funs(sub(paste0(list_sub,"."), "", .))) %>% 
    mutate(lon = as.numeric(lon),
           lat = as.numeric(lat))
}

# Function for loading, prepping, and saving the daily global category slices
# tester...
# date_range <- c(as.Date("1982-01-01"), as.Date("1990-01-31"))
# clim_period <- "1982-2011"
# product <- "CCI"
proc_cat <- function(date_range, product, clim_period){
  
  # Fetch file names
  lon_files <- dir(paste0("../data/",product,"_lon"), 
                   pattern = clim_period, full.names = T)
  if(length(lon_files) < 15) stop("Lon files not fetched correctly")
  
  # Load the files
  # NB: Can't run all 15 slices at once
  registerDoParallel(cores = 8)
  print(paste0("Began loading ",product,": ",clim_period, " for ",
               date_range[1]," to ",date_range[2]," at ",Sys.time()))
  cat_sub <- plyr::ldply(lon_files, load_cat, .parallel = T, 
                         date_range = date_range, .paropts = c(.inorder = FALSE)) %>% 
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme"))) %>% 
    na.omit()
  
  # NB: Running this on too many cores may cause RAM issues
  registerDoParallel(cores = 10)
  print(paste0("Began saving ",product,": ",clim_period, " for ",
               date_range[1]," to ",date_range[2]," at ",Sys.time()))
  plyr::l_ply(seq(min(cat_sub$t), max(cat_sub$t), by = "day"), 
              save_cat, .parallel = T, df = cat_sub,
              product = product, clim_period = clim_period)
  rm(cat_sub); gc()
}

# Function for loading a cat_lon slice and extracting a single day of values
load_cat <- function(cat_lon_file, date_range){
  # system.time(
  cat_clim <- readRDS(cat_lon_file) %>% 
    extract_MHW(., "cat")
  # ) # 172 seconds
  cat_clim_sub <- cat_clim %>%
    filter(t >= date_range[1], t <= date_range[2])
  rm(cat_clim); gc()
  return(cat_clim_sub)
}

# Function for saving daily global cat files
save_cat <- function(date_choice, df, product, clim_period){
  
  # Establish flie name and save location
  cat_year <- lubridate::year(date_choice)
  cat_dir <- paste0("../data/",product,"_cat/",cat_year)
  dir.create(as.character(cat_dir), showWarnings = F)
  cat_name <- paste0(product,"_cat_",clim_period,"_",date_choice,".Rds")
  
  # Extract data and save
  df_sub <- df %>% 
    filter(t == date_choice) %>% 
    mutate(intensity = round(intensity, 2))
  saveRDS(df_sub, file = paste0(cat_dir,"/",cat_name))
  rm(df); gc()
}


# 2: OISST database -------------------------------------------------------

# The NOAA OISST database for the MHW Tracker was created while the foundational
# code base was still being developed. For this reason one will not find the code
# here that downloads and establishes the file structure used in the Tracker. I
# do intend to write this process out at some point in the future.

# The code needed to download and prep the NOAA OISST data for MHW calculations 
# may be found here: https://theoceancode.netlify.app/post/dl_env_data_r/


## Create wider lon slices to match the rest of the project
# plyr::l_ply(1:15, widen_OISST, .parallel = F)


## Detect MHWs for a given clim period
# 1982 - 2011
registerDoParallel(cores = 40)
plyr::l_ply(1:15, detect_MHW_lon, .parallel = F, product = "OISST",
            chosen_clim = c(as.Date("1982-01-01"), as.Date("2011-12-31")))
Sys.sleep(10); gc()

# 1992 - 2018
# registerDoParallel(cores = 40)
# plyr::l_ply(1:15, detect_MHW_lon, .parallel = F, product = "OISST",
#             chosen_clim = c(as.Date("1992-01-01"), as.Date("2018-12-31")))
# Sys.sleep(10); gc()


## Create daily category slices
# NB: This sets it's own core usage internally
# Clim period 1982-2011
proc_cat(date_range = c(as.Date("1982-01-01"), as.Date("1990-12-31")),
         product = "OISST", clim_period = "1982-2011")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("1991-01-01"), as.Date("2000-12-31")),
         product = "OISST", clim_period = "1982-2011")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("2001-01-01"), as.Date("2010-12-31")),
         product = "OISST", clim_period = "1982-2011")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("2011-01-01"), as.Date("2019-12-31")),
         product = "OISST", clim_period = "1982-2011")
Sys.sleep(10); gc()

# Clim period 1992-2018
  # ~ xx minutes for one decade
proc_cat(date_range = c(as.Date("1982-01-01"), as.Date("1990-12-31")),
         product = "OISST", clim_period = "1992-2018")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("1991-01-01"), as.Date("2000-12-31")), 
         product = "OISST", clim_period = "1992-2018")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("2001-01-01"), as.Date("2010-12-31")),  
         product = "OISST", clim_period = "1992-2018")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("2011-01-01"), as.Date("2019-12-31")), 
         product = "OISST", clim_period = "1992-2018")
Sys.sleep(10); gc()


# 3: CCI database ---------------------------------------------------------

# The code used to download the CCI data may also be found in the same post:
# https://theoceancode.netlify.app/post/dl_env_data_r/

# Another blog post is used for the calculation of MHWs in gridded data:
# https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html

# NCDUMP
# ncdump::NetCDF(CCI_files[1])$variable[1:5]


## Only run this to fully rectangle ALL of the CCI data
# This takes roughly 15 hours on 25 cores
# registerDoParallel(cores = 20)
# plyr::l_ply(1:15, load_lon_full, .parallel = F, product = "CCI",
            # date_start = as.Date("1981-09-01"), date_end = as.Date("2018-12-31"))


## Calculate CCI MHWs
# Clim period 1982 - 2011
# registerDoParallel(cores = 40)
  # 818 seconds on 50 cores for one slice
# plyr::l_ply(1:15, detect_MHW_lon, .parallel = F, product = "CCI",
#             chosen_clim = c(as.Date("1982-01-01"), as.Date("2011-12-31")))
# Sys.sleep(10); gc()

# Clim period 1992 - 2018
# plyr::l_ply(1:15, detect_MHW_lon, .parallel = F, product = "CCI",
#             chosen_clim = c(as.Date("1992-01-01"), as.Date("2018-12-31")))
# Sys.sleep(10); gc()


## Create daily CCI MHW daily clim slice results
# Clim period 1982-2011
proc_cat(date_range = c(as.Date("1982-01-01"), as.Date("1990-12-31")),
         product = "CCI", clim_period = "1982-2011")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("1991-01-01"), as.Date("2000-12-31")), 
         product = "CCI", clim_period = "1982-2011")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("2001-01-01"), as.Date("2010-12-31")),  
         product = "CCI", clim_period = "1982-2011")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("2011-01-01"), as.Date("2019-12-31")), 
         product = "CCI", clim_period = "1982-2011")
Sys.sleep(10); gc()

# Clim period 1992-2018
proc_cat(date_range = c(as.Date("1982-01-01"), as.Date("1990-12-31")),
         product = "CCI", clim_period = "1992-2018")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("1991-01-01"), as.Date("2000-12-31")), 
         product = "CCI", clim_period = "1992-2018")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("2001-01-01"), as.Date("2010-12-31")),  
         product = "CCI", clim_period = "1992-2018")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("2011-01-01"), as.Date("2019-12-31")), 
         product = "CCI", clim_period = "1992-2018")
Sys.sleep(10); gc()


# 4: CMC database ---------------------------------------------------------

# The code used to download these data may be found at: "../tikoraluk/CMC_download.R"

## Rectangle all of the CMC data
# NB: This takes roughly 4 hours on 25 cores
# registerDoParallel(cores = 50)
# plyr::l_ply(1:15, load_lon_full, .parallel = F, product = "CMC",
#             date_start = as.Date("1991-09-01"), date_end = as.Date("2019-12-31"))


## Calculate CMC MHWs
# clim period: 1992 - 2018
# registerDoParallel(cores = 50)
# plyr::l_ply(1:15, detect_MHW_lon, .parallel = F, product = "CMC",
#             chosen_clim = c(as.Date("1992-01-01"), as.Date("2018-12-31")))
# Sys.sleep(10); gc()


## Create daily CMC MHW clim slice results
# Clim period 1992-2018
proc_cat(date_range = c(as.Date("1992-01-01"), as.Date("2000-12-31")), 
         product = "CMC", clim_period = "1992-2018")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("2001-01-01"), as.Date("2010-12-31")),  
         product = "CMC", clim_period = "1992-2018")
Sys.sleep(10); gc()
proc_cat(date_range = c(as.Date("2011-01-01"), as.Date("2019-12-31")), 
         product = "CMC", clim_period = "1992-2018")
Sys.sleep(10); gc()


# 5: Test visuals ---------------------------------------------------------

# CCI_test <- readRDS("../data/CCI_cat/1990/CCI_cat_1982-2011_1990-01-01.Rds")
# OISST_test1 <- readRDS("../data/cat_clim/1990/cat.clim.1990-01-01.Rda")
# OISST_test2 <- readRDS("../data/OISST_cat/1990/OISST_cat_1992-2018_1990-01-01.Rds")
# 
# ggplot(data = OISST_test2, aes(x = lon, y = lat)) +
#   borders() +
#   geom_tile(aes(fill = category)) +
#   scale_fill_manual(values = MHW_colours)
