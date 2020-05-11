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

# Add an index column for easier pixel comparisons below
OISST_ocean_coords$index <- seq_len(nrow(OISST_ocean_coords))

# The CCI files
CCI_files <- dir("../data/CCI", full.names = T)


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

# Load a single CCI NetCDF file
CCI_dat <- tidync(CCI_files[1]) %>% 
  hyper_tibble() %>% 
  na.omit()

# Visualise
  # NB: This is a beefy boi
# ggplot(data = CCI_dat, aes(x = lon, y = lat)) +
#   geom_tile(aes(fill = analysed_sst)) +
#   borders(colour = "black") +
#   coord_quickmap(expand = F)

# Get the coordinate system
CCI_ocean_coords <- CCI_dat %>% 
  na.omit() %>% 
  select(lon, lat)

# Find the nearest coordinates to the OISST grid
CCI_OISST_coords <- CCI_ocean_coords %>% 
  mutate(index = as.vector(knnx.index(as.matrix(OISST_ocean_coords[,c("lon", "lat")]),
                                      as.matrix(.), k = 1))) %>%
  left_join(OISST_ocean_coords, by = "index") %>% 
  dplyr::rename(lon = lon.x, lat = lat.x, 
                lon_OI = lon.y, lat_OI = lat.y) %>% 
  dplyr::select(lon, lat, lon_OI, lat_OI)
saveRDS(CCI_OISST_coords, "metadata/CCI_OISST_coords.Rds")

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
  # borders(colour = "black") +
  scale_fill_viridis_c() +
  coord_quickmap(expand = F) +
  theme_void()


