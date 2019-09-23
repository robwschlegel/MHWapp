# This script is run when the server is called and is designed to house
# all of the static information that is used by the app

# Set directory manually for testing purposes
# setwd("shiny")


# Packages ----------------------------------------------------------------

# Required up front
library(shinyBS)
library(leaflet)
library(dplyr)
library(plotly)
library(ncdf4)
library(DT)

# Dependencies that are called explicitly
# .libPaths(c("~/R-packages", .libPaths()))
# library(shiny)
# library(shinyjs)
# library(shinycssloaders)
# library(shinyWidgets)
# library(readr)
# library(tidyr)
# library(lubridate)
# library(magrittr)
# library(leaflet.extras)
# library(raster)
# library(rgdal)
# library(heatwaveR)
# cat(packageDescription("heatwaveR")$Version)


# Modules -----------------------------------------------------------------

source("functions.R", local = TRUE)
source("modules/about-server.R", local = TRUE)
source("modules/about-ui.R", local = TRUE)
source("modules/map-server.R", local = TRUE)
source("modules/map-ui.R", local = TRUE)

# mapbox_moon <- "https://api.mapbox.com/styles/v1/..."


# Meta-data ---------------------------------------------------------------

### Check that main data folders exist
if(!dir.exists("cat_clim")) stop("The 'cast_clim' folder is missing.")
if(!dir.exists("event")) stop("The 'event' folder is missing.")
if(!dir.exists("modules")) stop("The 'modules' folder is missing.")
if(!dir.exists("OISST")) stop("The 'OISST' folder is missing.")
if(!dir.exists("thresh")) stop("The 'thresh' folder is missing.")

### The dates currently processed
current_dates <- dir(dir("cat_clim", full.names = T), pattern = "cat.clim", full.names = T)
current_dates <- sapply(strsplit(current_dates, split = "cat.clim."), "[[", 3)
current_dates <- as.Date(sapply(strsplit(current_dates, split = ".Rda"), "[[", 1))

### Starting values
initial_lat <- 45
initial_lon <- -60
initial_zoom <- 4
menu_panel_top <- 60
menu_panel_right <- 10
date_menu_choice <- max(current_dates)
# sidepanel.width <- 400

### The lon/lat steps
load("lon_OISST.RData")
lat_OISST <- seq(-89.875, 89.875, by = 0.25)

### The file locations
OISST_files <- dir("OISST", pattern = "avhrr-only", full.names = T)
# MHW_event_files <- dir("event", pattern = "MHW.event.", full.names = T)
seas_thresh_files <- dir("thresh", pattern = "MHW.seas.thresh.", full.names = T)
# cat_clim_files <- as.character(dir(path = "cat_clim", pattern = "cat.clim",
#                                    full.names = TRUE, recursive = TRUE))

### The empty dataframe for the legend
MHW_cat_clim_sub <- data.frame(category = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

### The category colour pallette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)
# MHW_colours <- data.frame(val = c("#ffc866", "#ff6900", "#9e0000", "#2d0000"),
#                           label = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

### Colour palettes for leaflet
pal_factor <- colorFactor(palette = MHW_colours, levels = levels(MHW_cat_clim_sub$category))
pal_cat <- colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)

### The two map projections
inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

### Placeholders before first click
# xy <- data.frame(lng = 0, lat = 0)
begin_dl <- FALSE
# button_colour_ts <- "danger"

server_instance <- Sys.getenv("R_SHNYSRVINST")

### Regional website URLs
regional_NOAA <- "https://www.integratedecosystemassessment.noaa.gov/regions/california-current/cc-projects-blobtracker"
regional_TMEDNET <- "http://t-mednet.org/t-resources/marine-heatwaves"

# cat("\nglobal.R finished")

