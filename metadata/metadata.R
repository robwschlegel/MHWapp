# metadata/metadata.R
# This script is used to create or load the metadata objects used in this project
# 1: Setup environment
# 2: Create metadata
# 3: Load metadata


# 1: Setup ----------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(FNN)


# 2: Create metadata ------------------------------------------------------

# NOAA OISST lon/lat coords
lon_OISST <- c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))
lat_OISST <- seq(-89.875, 89.875, by = 0.25)
lon_lat_OISST <- base::expand.grid(lon_OISST, lat_OISST) %>% 
  dplyr::rename(lon = Var1, lat = Var2) %>%
  arrange(lon, lat) %>% data.frame()

# OISST projection
OISST_proj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Coordinates with surface area
load("metadata/lon_lat_OISST_area.RData")

# File locations
OISST_files <- dir("../data/OISST", pattern = "oisst-avhrr", full.names = T)
OISST_daily_nc_files <- dir("../data/OISST/daily", pattern = "oisst-avhrr", full.names = T, recursive = T)
MHW_event_files <- dir("../data/event", pattern = "MHW.event.", full.names = T) %>% str_filter("MCS", invert = T)
MCS_event_files <- dir("../data/event/MCS", pattern = "MCS.event.", full.names = T)
MHW_seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)
MCS_seas_thresh_files <- dir("../data/thresh/MCS", pattern = "MCS.seas.thresh.", full.names = T)
MHW_cat_lon_files <- dir("../data/cat_lon", full.names = T) %>% str_filter("MCS", invert = T)
MCS_cat_lon_files <- dir("../data/cat_lon/MCS", full.names = T)
MHW_cat_clim_files <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim", 
                                   full.names = TRUE, recursive = TRUE)) %>% str_filter("MCS", invert = T)
MCS_cat_clim_files <- as.character(dir(path = "../data/cat_clim/MCS", pattern = "cat.clim", 
                                   full.names = TRUE, recursive = TRUE))
CCI_files <- dir("../data/CCI", full.names = T)
CMC_files <- dir("../data/CMC", full.names = T)

# The current date
# NB: This script is running from a server in Atlantic Canada (UTC-3)
current_date <- Sys.Date()

# Get coords for each ocean pixel 
# system.time(OISST_ocean_coords <- plyr::ldply(1:1440, extract_OISST_one, .parallel = T)) # 9 seconds
# save(OISST_ocean_coords, file = "metadata/OISST_ocean_coords.Rdata")
load("metadata/OISST_ocean_coords.Rdata")
OISST_ocean_coords <- left_join(OISST_ocean_coords, lon_lat_OISST_area, by = c("lon", "lat"))

# Visualise ocean pixels
# ggplot(OISST_ocean_coords, aes(x = lon, y = lat)) + geom_tile(aes(fill = sq_area))

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
# OISST_no_ice_coords$index <- paste(OISST_no_ice_coords$lon, OISST_no_ice_coords$lat)
# OISST_ocean_coords$index <- paste(OISST_ocean_coords$lon, OISST_ocean_coords$lat)
# OISST_ice_coords <- OISST_ocean_coords |> 
#   filter(!(index %in% OISST_no_ice_coords$index)) |> 
#   dplyr::select(-index)

# Project between EPSG:4326 (OISST) and EPSG:leaflet
# OISST_ice_coords_XY <- mutate(OISST_ice_coords, Z = 1) |> dplyr::rename(X = lon, Y = lat) |> dplyr::select(X, Y, Z)
# OISST_ice_coords_non_proj <- raster::rasterFromXYZ(OISST_ice_coords_XY, res = c(0.25, 0.25),
#                                                    digits = 3, crs = "EPSG:4326")
# OISST_ice_coords_proj <- leaflet::projectRasterForLeaflet(OISST_ice_coords_non_proj, method = "ngb")
# raster::writeRaster(OISST_ice_coords_proj, format = "GTiff", overwrite = TRUE,
#                     filename = "metadata/OISST_ice_coords.tif")
# raster::writeRaster(OISST_ice_coords_proj, format = "GTiff", overwrite = TRUE,
#                     filename = "../data/OISST/ice_proj.tif")

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
  dplyr::rename(lon = long) %>%
  mutate(group = ifelse(lon > 180, group+9999, group),
         lon = ifelse(lon > 180, lon-360, lon))

# The MHW category colour palette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# The CSW category colour palette
MCS_colours <- c(
  "I Moderate" = "#C7ECF2",
  "II Strong" = "#85B7CC",
  "III Severe" = "#4A6A94",
  "IV Extreme" = "#111433",
  "V Ice" = "#D8BFD8"
)

# Add an index column for easier pixel comparisons below
OISST_ocean_coords$index <- seq_len(nrow(OISST_ocean_coords))

# Function for finding matching pixels between OISST and another product
X_OISST_coords <- function(file_name){
  coord_match <- tidync(file_name) %>% 
      hyper_tibble() %>%
      na.omit() %>%
      select(lon, lat) %>%
      mutate(index = as.vector(knnx.index(as.matrix(OISST_ocean_coords[,c("lon", "lat")]),
                                          as.matrix(.), k = 1))) %>%
      left_join(OISST_ocean_coords, by = "index") %>%
      dplyr::rename(lon = lon.x, lat = lat.x,
                    lon_OI = lon.y, lat_OI = lat.y) %>%
      dplyr::select(lon, lat, lon_OI, lat_OI)
}

## CCI to OISST coordinate regridding
# CCI_OISST_coords <- X_OISST_coords(CCI_files[1])
# saveRDS(CCI_OISST_coords, "metadata/CCI_OISST_coords.Rds")

## CMC to OISST coordinate regridding
# CMC0.2_OISST_coords <- X_OISST_coords(CMC_files[1])
# saveRDS(CMC0.2_OISST_coords, "metadata/CMC0.2_OISST_coords.Rds")
# CMC0.1_OISST_coords <- X_OISST_coords(CMC_files[10000])
# saveRDS(CMC0.1_OISST_coords, "metadata/CMC0.1_OISST_coords.Rds")

# Get coords between EPSG:4326 (OISST) and EPSG:leaflet
# lon_lat_OISST_XY <- mutate(lon_lat_OISST) |> dplyr::rename(X = lon, Y = lat)
# coords_non_proj <- raster::rasterFromXYZ(lon_lat_OISST_XY, res = c(0.25, 0.25),
#                                          digits = 3, crs = "EPSG:4326")
# coords_proj <- leaflet::projectRasterForLeaflet(coords_non_proj, method = "ngb")
# OISST_leaf_coords <- raster::as.data.frame(coords_proj, xy = TRUE) |>
#   cbind(lon_lat_OISST) |> dplyr::select(lon, lat, x, y) |>
#   dplyr::rename(lon_leaf = x, lat_leaf = y)
# save(OISST_leaf_coords, file = "metadata/OISST_leaf_coords.Rdata")
load("metadata/OISST_leaf_coords.Rdata")


# 3: Load metadata --------------------------------------------------------

# The pixel grids against OISST
# CCI_OISST_coords <- readRDS("metadata/CCI_OISST_coords.Rds")
# CMC0.2_OISST_coords <- readRDS("metadata/CMC0.2_OISST_coords.Rds")
# CMC0.1_OISST_coords <- readRDS("metadata/CMC0.1_OISST_coords.Rds")

