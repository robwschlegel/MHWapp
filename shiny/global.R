# This script is run when the server is called and is designed to house
# all of the static information that is used by the app

# Set directory manually for testing purposes
# setwd("shiny")


# Packages ----------------------------------------------------------------

# .libPaths(c("~/R-packages", .libPaths()))
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(magrittr)
library(leaflet)
library(leaflet.extras)
library(raster)
library(rgdal)
library(DT)
library(shinyBS)
library(plotly)
library(ncdf4)
library(heatwaveR)
# library(akima)
# cat(packageDescription("heatwaveR")$Version)


# Modules -----------------------------------------------------------------

source("functions.R", local = TRUE)
source("modules/about-server.R", local = TRUE)
source("modules/about-ui.R", local = TRUE)
source("modules/map-server.R", local = TRUE)
source("modules/map-ui.R", local = TRUE)

# mapbox_moon <- "https://api.mapbox.com/styles/v1/sebpoisson/cjjfvxsqh1nrd2rnqabh10sx4/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoic2VicG9pc3NvbiIsImEiOiJjamk1YXBiYm4waHd0M2twNmM3ODRuZjN4In0.WKHsGJ3K7SWyqO4lObCkfA"


# Meta-data ---------------------------------------------------------------

initial_lat <- 45
initial_lon <- -60
initial_zoom <- 4
# sidepanel.width <- 400

# The lon/lat steps
load("lon_OISST.RData")
lat_OISST <- seq(-89.875, 89.875, by = 0.25)

# The empty dataframe for the legend
MHW_cat_clim_sub <- data.frame(category = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

# The file locations
OISST_files <- dir("OISST", pattern = "avhrr-only", full.names = T)
# MHW_event_files <- dir("event", pattern = "MHW.event.", full.names = T)
seas_thresh_files <- dir("thresh", pattern = "MHW.seas.thresh.", full.names = T)
# cat_clim_files <- as.character(dir(path = "cat_clim", pattern = "cat.clim",
#                                    full.names = TRUE, recursive = TRUE))

# The category colour pallette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)
# MHW_colours <- data.frame(val = c("#ffc866", "#ff6900", "#9e0000", "#2d0000"),
#                           label = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

# Colour palettes for leaflet
pal_factor <- colorFactor(palette = MHW_colours, levels = levels(MHW_cat_clim_sub$category))
pal_cat <- colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)

# Projections
inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

# The dates currently processed
nc <- nc_open("OISST/avhrr-only-v2.ts.0001.nc")
current_dates <- as.Date(nc$dim$time$vals, origin = "1970-01-01")
# tail(current_dates)
nc_close(nc)
# load("current_dates.RData")

# Placeholder xy before first click
# xy <- data.frame(lng = 0, lat = 0)

# cat("\nglobal.R finished")
