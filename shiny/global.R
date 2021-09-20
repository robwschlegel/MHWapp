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
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(shinyBS)
library(leaflet)
library(plotly)
library(DT)
})

# Dependencies that are called explicitly
# library(ncdf4)
# library(shiny)
# library(shinyjs)
# library(shinycssloaders)
# library(maps)
# library(readr)
# library(tidyr)
# library(threadr)
# library(stringr)
# library(lubridate)
# library(magrittr)
# library(leaflet.extras)
# library(raster)
# library(rgdal)
# library(heatwaveR)
# library(ggpubr)
# library(tidync)
# library(plyr)
# cat(packageDescription("heatwaveR")$Version)


# Modules -----------------------------------------------------------------

source("functions.R", local = TRUE)
source("modules/about-server.R", local = TRUE)
source("modules/about-ui.R", local = TRUE)
source("modules/map-server.R", local = TRUE)
source("modules/map-ui.R", local = TRUE)
source("modules/summary-server.R", local = TRUE)
source("modules/summary-ui.R", local = TRUE)
source("modules/comparison-server.R", local = TRUE)
source("modules/comparison-ui.R", local = TRUE)

# mapbox_moon <- "https://api.mapbox.com/styles/v1/..."


# Meta-data ---------------------------------------------------------------

### Check that main data folders exist
if(!dir.exists("cat_clim")) stop("The 'cast_clim' folder is missing.")
if(!dir.exists("event")) stop("The 'event' folder is missing.")
if(!dir.exists("modules")) stop("The 'modules' folder is missing.")
if(!dir.exists("OISST")) stop("The 'OISST' folder is missing.")
if(!dir.exists("thresh")) stop("The 'thresh' folder is missing.")

### The dates currently processed
current_dates <- dir("cat_clim", recursive = T, pattern = "cat.clim", full.names = T) %>% threadr::str_filter("MCS", invert = T)
current_dates <- sapply(strsplit(current_dates, split = "cat.clim."), "[[", 3)
current_dates <- as.Date(sapply(strsplit(current_dates, split = ".Rda"), "[[", 1))

### Starting values
initial_lat <- 45
initial_lon <- -60
initial_zoom <- 4
menu_panel_top <- 60
menu_panel_left <- 10
# date_menu_choice <- max(current_dates)
# sidepanel.width <- 400

### The different layer groupings
cat_layers <- c("MHW Category", "MCS Category", "MHW Summary")
rb_layers <- c("SST Anomaly", "MHW Trend: Count", "MHW Trend: Duration",
               "MHW Trend: Intensity (mean)", "MHW Trend: Intensity (max)")
trend_layers <- c("MHW Trend: Count", "MHW Trend: Duration",
                  "MHW Trend: Intensity (mean)", "MHW Trend: Intensity (max)")

### The lon/lat steps
# load("lon_OISST.RData")
lon_OISST <- c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))
lat_OISST <- seq(-89.875, 89.875, by = 0.25)

### The file locations
OISST_files <- dir("OISST", pattern = "avhrr-only", full.names = T)
# MHW_event_files <- dir("event", pattern = "MHW.event.", full.names = T)
MHW_seas_thresh_files <- dir("thresh", pattern = "MHW.seas.thresh.", full.names = T)
MCS_seas_thresh_files <- dir("thresh_MCS", pattern = "MCS.seas.thresh.", full.names = T)
# cat_clim_files <- as.character(dir(path = "cat_clim", pattern = "cat.clim",
#                                    full.names = TRUE, recursive = TRUE))

### Oliver et al. 2018 data
Oliver_2018 <- readRDS("../data/published/Oliver_2018.Rds")

### The empty dataframe for the legend
MHW_cat_clim_sub <- data.frame(category = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

### The category colour palettes
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)
# MHW_colours <- data.frame(val = c("#ffc866", "#ff6900", "#9e0000", "#2d0000"),
#                           label = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

MCS_colours <- c(
  "I Moderate" = "#C7ECF2",
  "II Strong" = "#85B7CC",
  "III Severe" = "#4A6A94",
  "IV Extreme" = "#111433"#,
  # "V Ice" = "thistle1" # Not added yet as this adds quite a lot of complexity
)

### Colour palettes for leaflet
# pal_factor <- colorFactor(palette = MHW_colours, levels = levels(MHW_cat_clim_sub$category))
# pal_cat <- colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)
# pal_anom <- colorNumeric(palette = c("blue", "white", "red"), domain = c(-10, 10), na.color = NA)

### The two map projections
inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

### The shiny server instance beng run
server_instance <- Sys.getenv("R_SHNYSRVINST")

### Regional website URLs
regional_NOAA <- "https://www.integratedecosystemassessment.noaa.gov/regions/california-current/cc-projects-blobtracker"
regional_TMEDNET <- "http://t-mednet.org/t-resources/marine-heatwaves"
regional_Danish_Archepelago <- "https://fishforecasts.dtu.dk/heatwaves/denmark"
regional_Baltic_Sea <- "https://fishforecasts.dtu.dk/heatwaves/baltic"
regional_North_Sea <- "https://fishforecasts.dtu.dk/heatwaves/north_sea"
regional_European_Northwest_Shelf <- "https://fishforecasts.dtu.dk/heatwaves/nw_shelf"

### Placeholders when invalid dates are typed into date selectors
empty_date_map <- readRDS("cat_clim/1982/cat.clim.1982-01-01.Rda") %>% 
  slice(1) %>% 
  mutate(category = NA)
# empty_summary_map <- readRDS("../data/annual_summary/MHW_cat_pixel_1982.Rds") %>% 
#   slice(1) %>% 
#   mutate(category = NA)
# empty_summary_ts <- readRDS("../data/annual_summary/MHW_cat_daily_1982.Rds") %>% 
#   slice(1) %>% 
#   mutate(category = NA)

### The base map
# map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
#   dplyr::rename(lon = long) %>%
#   mutate(group = ifelse(lon > 180, group+9999, group),
#          lon = ifelse(lon > 180, lon-360, lon))
# save(map_base, file = "metadata/map_base.Rdata")
load("../metadata/map_base.Rdata")

### The OISST ocean coordinates
load("../metadata/OISST_ocean_coords.Rdata")

# cat("\nglobal.R finished")

