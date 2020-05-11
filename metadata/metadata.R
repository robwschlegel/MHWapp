# metadata/metadata.R
# This script is used to create or load the metadata objects used in this project
# 1: Setup environment
# 2: Create metadata
# 3: Load metadata


# 1: Setup ----------------------------------------------------------------

library(tidyverse)


# 2: Create metadata ------------------------------------------------------

lon_OISST <- c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))
lat_OISST <- seq(-89.875, 89.875, by = 0.25)
lon_lat_OISST <- base::expand.grid(lon_OISST, lat_OISST) %>% 
  dplyr::rename(lon = Var1, lat = Var2) %>% 
  arrange(lon, lat) %>% 
  data.frame()

# File locations
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)
MHW_event_files <- dir("../data/event", pattern = "MHW.event.", full.names = T)
seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)
cat_lon_files <- dir("../data/cat_lon", full.names = T)
cat_clim_files <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim", 
                                   full.names = TRUE, recursive = TRUE))
# The current date
# NB: This script is running from a server in Atlantic Canada (UTC-3)
current_date <- Sys.Date()

# Get coords for each ocean pixel 
# system.time(OISST_ocean_coords <- plyr::ldply(1:1440, extract_OISST_one, .parallel = T)) # 9 seconds
# save(OISST_ocean_coords, file = "metadata/OISST_ocean_coords.Rdata")
load("metadata/OISST_ocean_coords.Rdata")

# Visualise ocean pixels
# ggplot(OISST_ocean_coords, aes(x = lon, y = lat)) + geom_tile()

# Exclude OISST pixels with any near-ice (-1.6C) cover
# During iterations of this methodology it was found that there are pixels near
# the ice edge that don't ever quite reach -1.8C but remain in a "slush" state
# This was determined to be an artefact of the OISST process and so the new
# "ice" limit was set to -1.6C
# load_OISST_no_ice_coords <- function(nc_file){
# 
#   # OISST data
#   nc_OISST <- nc_open(nc_file)
#   lon_vals <- as.vector(nc_OISST$dim$lon$vals)
#   lat_vals <- as.vector(nc_OISST$dim$lat$vals)
#   lat_index <- c(which(lat_vals == -69.875), which(lat_vals == 79.875))
#   time_vals <- as.Date(ncvar_get(nc_OISST, "time"), origin = "1970-01-01")
#   time_index <- which(time_vals == as.Date("2018-12-31"))
#   sst_raw <- ncvar_get(nc_OISST, "sst", start = c(lat_index[1], 1, 1),
#                        count = c(lat_index[2]-lat_index[1]+1, -1, time_index))
#   dimnames(sst_raw) <- list(lat = nc_OISST$dim$lat$vals[lat_index[1]:lat_index[2]],
#                             t = time_vals[seq_len(time_index)])
#   nc_close(nc_OISST)
# 
#   # Prep SST for further use
#   # system.time(
#   res <- as.data.frame(reshape2::melt(sst_raw, value.name = "temp"), row.names = NULL) %>%
#     na.omit() %>%
#     mutate(lon = lon_vals[1],
#            t = as.Date(t, origin = "1970-01-01")) %>%
#     # Filter out pixels with any ice/slush cover during the time series
#     # Filter out pixels that don't cover the whole time series
#     group_by(lon, lat) %>%
#     filter(min(round(temp, 1)) > -1.6,
#            n() == 13514) %>%
#     ungroup() %>%
#     select(lon, lat) %>%
#     mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
#     unique() %>%
#     data.frame()
#   # ) ~1.7 seconds
#   
#   # Clear some RAM
#   rm(sst_raw); gc()
#   return(res)
# }

# Get coords for each no ice cover pixel 
# system.time(OISST_no_ice_coords <- plyr::ldply(OISST_files, load_OISST_no_ice_coords, .parallel = T)) # 448 seconds
# save(OISST_no_ice_coords, file = "metadata/OISST_no_ice_coords.Rdata")
load("metadata/OISST_no_ice_coords.Rdata")

# Create ice only layer
OISST_no_ice_coords$index <- paste(OISST_no_ice_coords$lon, OISST_no_ice_coords$lat)
OISST_ocean_coords$index <- paste(OISST_ocean_coords$lon, OISST_ocean_coords$lat)
OISST_ice_coords <- OISST_ocean_coords %>%
  filter(!(index %in% OISST_no_ice_coords$index))

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
  dplyr::rename(lon = long) %>%
  # filter(lat >= 25.6) %>%
  mutate(group = ifelse(lon > 180, group+9999, group),
         lon = ifelse(lon > 180, lon-360, lon))

# The MHW category colour palette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)


# 3: Load metadata --------------------------------------------------------



