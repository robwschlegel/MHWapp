# data/washington_post.R
# The purpose of this script is to extract data around Uruguay
# from January - April of 2017 in order ri visualise an IV Extreme MHW
# These data are then converted to raster format for the convenience
# of the Washington Post illustrators


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(raster)


# Load data ---------------------------------------------------------------

# Function for loading file and including date from file name
readRDS_date <- function(file_name){
  file_date <- sapply(strsplit(file_name, "/"), "[[", 4)
  file_date <- sapply(strsplit(file_date, "[.]"), "[[", 3)
  base <- readRDS(file_name) %>% 
    mutate(t = file_date) %>% 
    dplyr::select(t, lon, lat, category)
  return(base)
}

# Load and filter out desired 2017 data around Uruguay
MHW_2017 <- map_dfr(dir("shiny/cat_clim/2017", full.names = T), readRDS_date) %>% 
  filter(t <= "2017-04-30")

# Grab a single day
MHW_2017_single <- readRDS_date("shiny/cat_clim/2017/cat.clim.2017-02-25.Rda")


# Create rasters ----------------------------------------------------------

# The two different map projections used
inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

# The colour palette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# Function for performing the full raster process
raster_MHW <- function(df){
  df <- dplyr::select(df, lon, lat, category)
  # Rename the columns to make the raster function happy
  colnames(df) <- c("X", "Y", "Z")
  # Convert the category column to numeric values
  df$Z <- as.numeric(df$Z)
  # Convert to a raster
  df_raster <- raster::rasterFromXYZ(df, res = c(0.25, 0.25), digits = 3, crs = inputProj)
}

# Create raster
MHW_2017_single_raster <- raster_MHW(MHW_2017_single)


# Visualise ---------------------------------------------------------------

plot(MHW_2017_single_raster)


# Save --------------------------------------------------------------------

writeRaster(MHW_2017_single_raster, "data/washington_post.asc")

