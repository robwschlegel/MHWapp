# MHWapp/shiny/global.R
# This script is run when the server is called and is designed to house
# all of the static information that is used by the app

# Set directory manually for testing purposes
# .libPaths(c("~/R-packages", .libPaths()))
# library(shiny)
# setwd("shiny")


# Packages ----------------------------------------------------------------

# Required up front
.libPaths(c("~/R-packages", .libPaths()))
suppressPackageStartupMessages({
library(bslib)
library(bsicons)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
library(DT)
# library(cicerone)
})

# Dependencies that are called explicitly
# library(shiny)
# library(maps)
# library(readr)
# library(tidyr)
# library(threadr)
# library(stringr)
# library(lubridate)
# library(raster)
# library(tidync)
# library(purrr)
# library(daterangepicker)
# library(heatwaveR)
# cat(packageDescription("heatwaveR")$Version)


# Functions ---------------------------------------------------------------

source("functions.R", local = TRUE)


# Meta-data ---------------------------------------------------------------

### Check that main data folders exist
# NB: These are the folders that are connected to whalemap via rsync
if(!dir.exists("cat_clim")) stop("The 'cast_clim' folder is missing.")
if(!dir.exists("event")) stop("The 'event' folder is missing.")
if(!dir.exists("OISST")) stop("The 'OISST' folder is missing.")
if(!dir.exists("thresh")) stop("The 'thresh' folder is missing.")
# if(!dir.exists("modules")) stop("The 'modules' folder is missing.")

### The file locations
OISST_files <- dir("OISST", pattern = "oisst-avhrr", full.names = T)
MHW_event_files <- dir("event", pattern = "MHW.event.", full.names = T)
MCS_event_files <- dir("event/MCS", pattern = "MCS.event.", full.names = T)
MHW_seas_thresh_files <- dir("thresh", pattern = "MHW.seas.thresh.", full.names = T)
MCS_seas_thresh_files <- dir("thresh/MCS", pattern = "MCS.seas.thresh.", full.names = T)
# cat_clim_files <- as.character(dir(path = "cat_clim", pattern = "cat.clim",
#                                    full.names = TRUE, recursive = TRUE))

### The dates currently processed
# TODO: Make this an output of MHW_daily.R to be simply loaded here
# Rather base this on the annual statistics value as this is the final calculation
# If there have been any errors along the way it will be felt there
current_dates <- dir("cat_clim", recursive = T, pattern = ".tif", full.names = T) %>% threadr::str_filter("MCS", invert = T)
current_dates <- sapply(strsplit(current_dates, split = "cat.clim."), "[[", 3)
current_dates <- as.Date(sapply(strsplit(current_dates, split = ".tif"), "[[", 1))

# Testing...
# date_menu_choice <- max(current_dates)

### Starting values
initial_lat <- 45
initial_lon <- -60
initial_zoom <- 4

### The different layer groupings
cat_layers <- c("MHW Category", "MCS Category", "MHW Summary", "MCS Summary")
rb_layers <- c("SST Anomaly", "MHW Trend: Count", "MHW Trend: Duration",
               "MHW Trend: Intensity (mean)", "MHW Trend: Intensity (max)")
trend_layers <- c("MHW Trend: Count", "MHW Trend: Duration",
                  "MHW Trend: Intensity (mean)", "MHW Trend: Intensity (max)")

### The lon/lat steps as vectors
# load("lon_OISST.RData")
lon_OISST <- c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))
lat_OISST <- seq(-89.875, 89.875, by = 0.25)

### Various OISST coordinates
load("../metadata/OISST_ocean_coords.Rdata")
load("../metadata/OISST_leaf_coords.Rdata")
load("../metadata/lon_lat_OISST_area.RData")

### Ice coords
ice_proj <- raster::raster("../data/OISST/ice_proj.tif")

### Oliver et al. 2018 data
## NB: To save time on launch this is now loaded in map-server.R when requested
# Oliver_2018 <- readRDS("../data/published/Oliver_2018.Rds")

### The empty dataframe for the legend
MHW_cat_clim_sub <- data.frame(category = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

### The category colour palettes
base_colours <- c(
  "MHW" = "salmon",
  "MCS" = "steelblue"
)

MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

MCS_colours <- c(
  "I Moderate" = "#C7ECF2",
  "II Strong" = "#85B7CC",
  "III Severe" = "#4A6A94",
  "IV Extreme" = "#111433",
  "V Ice" = "thistle1" # Not added yet as this adds quite a lot of complexity
)

### Colour palettes for leaflet
# pal_factor <- colorFactor(palette = MHW_colours, levels = levels(MHW_cat_clim_sub$category))
# pal_cat <- colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)
# pal_anom <- colorNumeric(palette = c("blue", "white", "red"), domain = c(-10, 10), na.color = NA)

### The two map projections
# inputProj <- "4326" # NB: This alone is not enough
# inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

### The shiny server instance being run
server_instance <- Sys.getenv("R_SHNYSRVINST")

### Regional website URLs
regional_NOAA <- "https://www.integratedecosystemassessment.noaa.gov/regions/california-current/cc-projects-blobtracker"
regional_TMEDNET <- "http://t-mednet.org/t-resources/marine-heatwaves"
regional_European_Northwest_Shelf <- "https://fishforecasts.dtu.dk/heatwaves/nw_shelf"

### Placeholders when invalid dates are typed into date selectors
empty_date_map <- readRDS("cat_clim/1982/cat.clim.1982-01-01.Rda") |> slice(1) |> mutate(category = NA)
# empty_summary_map <- readRDS("../data/annual_summary/MHW_cat_pixel_1982.Rds") |> slice(1) |> mutate(category = NA)
# empty_summary_ts <- readRDS("../data/annual_summary/MHW_cat_daily_1982.Rds") |> slice(1) |> mutate(category = NA)

# cat("\nglobal.R finished")

