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
library(tidyverse)
library(tidync)
library(FNN)
library(dtplyr) # Sometimes data.table can be faster

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
ncdump::NetCDF(CCI_files[1])$variable[1:5]

# Regrid the CCI data to the OI grid
system.time(
CCI_regrid_dat <- left_join(CCI_dat, CCI_OISST_coords, by = c("lon", "lat")) %>% 
  dplyr::select(lon_OI, lat_OI, analysed_sst) %>% 
  group_by(lon_OI, lat_OI) %>% 
  summarise(temp = mean(analysed_sst, na.rm = T)) %>% 
  ungroup()
) # 36 seconds

# Visualise
ggplot(data = CCI_regrid_dat, aes(x = lon_OI, y = lat_OI)) +
  geom_tile(aes(fill = temp)) +
  borders(colour = "black") +
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  theme_void()

# Function for loading the CCI pixels in a chosen OISST lon slice
# chosen_lon <- lon_OISST[1]
# day_int <- 1
CCI_OISST_lon_day <- function(day_int, chosen_lon){
  
  # Find the pixels nearest to the chosen OISST lon slice
  CCI_pixels <- filter(CCI_OISST_coords, lon_OI == chosen_lon)
  
  # Extract and processes a CCI lon slice
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
  return(CCI_lon_day)
}
test <- CCI_OISST_lon_day(1,lon_OISST[1])

