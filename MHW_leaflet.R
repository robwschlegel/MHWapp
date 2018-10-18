# The purpose of this script is to explore the possibility of a leaflet map


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(leaflet)
library(raster)
# library(rgdal)
# library(htmlwidgets)
# library(mapview)


# Data --------------------------------------------------------------------

# load("data/MHW_res.RData")
load("data/MHW_cat.RData")

# The event categories
MHW_cat_event <- MHW_cat %>% 
  unnest(cat) %>% 
  filter(row_number() %% 2 == 0) %>% 
  unnest(cat)

# The event clims
MHW_cat_clim <- MHW_cat %>% 
  unnest(cat) %>% 
  filter(row_number() %% 2 == 1) %>% 
  unnest(cat)

# The category colour pallette
fillColCat <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# Base map ----------------------------------------------------------------

# start basemap
map <- leaflet() %>% 
  
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  
  # add graticules from a NOAA webserver
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL,group = 'Graticules') %>%
  
  # focus map in a certain area / zoom level
  setView(lng = 25, lat = -35, zoom = 5) %>%
  
  # add layers control
  # addLayersControl(overlayGroups = c('Place names',
  #                                    'Graticules',
  #                                    'Points',
  #                                    'Lines',
  #                                    'Polygons'),
  #                  options = layersControlOptions(collapsed = FALSE),
  #                  position = 'topright') %>%
  
  # list groups to hide on startup
  hideGroup(c('Place names'))

# show map
# map


# Add data ----------------------------------------------------------------

MHW_cat_clim_sub <- MHW_cat_clim %>% 
  dplyr::filter(t == as.Date("2000-01-01")) %>%
  # dplyr::rename(value = category) %>%
  dplyr::select(lon, lat, category)


MHW_raster <- MHW_cat_clim_sub 
colnames(MHW_raster) <- c("X","Y","Z")
MHW_raster$Z <- as.numeric(factor(MHW_raster$Z, levels = c("I Moderate", "II Strong")))
rast <- rasterFromXYZ(MHW_raster, res = c(0.25, 0.25), digits = 2, 
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
                      # crs = "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=km +no_defs" )
plot(rast)


# Set up the colors
# val <-  c("I Moderate", "II Strong", "III Severe", "IV Extreme")
# pal <-  colorFactor(c("#ffc866", "#ff6900", "#9e0000", "#2d0000"), levels = val,
#                    na.color = "transparent", ordered = T)
val <- c(1, 2)
pal <-  colorFactor(c("#ffc866", "#ff6900"), levels = val,
                    na.color = "transparent", ordered = T)

map_data <- map %>% 
  addRasterImage(rast, colors = pal, opacity = 0.5, method = "ngb") %>%
  # leaflet::
  addCircleMarkers(data = MHW_cat_clim_sub, ~lon, ~lat,
                   weight = 1.0,
                   color = "grey",
                   fillColor = "white",
                   radius = 10,
                   fillOpacity = 0.0,
                   stroke = T,
                   label = ~paste0('Event at: ',
                                   as.character(round(lat,3)), ', ',
                                   as.character(round(lon,3))),
                   group = 'Points') %>%
  addScaleBar() %>%
  # tileOptions(minZoom = 10) %>% 
  addLegend(pal = pal, values = val, title = "MHW Category", labels = c("I Moderate", "II Strong"))
map_data  

# Save --------------------------------------------------------------------

# save a stand-alone, interactive map as an html file
saveWidget(widget = map_data, file = 'MHW_map.html', selfcontained = T)

# # # save a snapshot as a png file
mapshot(map_data, file = 'MHW_map.png')
