# The purpose of this script is to explore the possibility of a leaflet map


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(leaflet)
library(raster)
library(rgdal)


# Data --------------------------------------------------------------------

load("data/MHW_res.RData")
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
  setView(lng = 25, lat = -35, zoom = 4) %>%
  
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
map


# Add data ----------------------------------------------------------------

MHW_cat_clim_sub <- MHW_cat_clim %>% 
  dplyr::filter(t == as.Date("2000-01-01")) %>%
  dplyr::rename(value = category) %>%
  dplyr::select(lon, lat, value)
  # dplyr::rename(lng = lon)

MHW_cat_clim_grid <- expand.grid(MHW_cat_clim_sub$lon, MHW_cat_clim_sub$lat) %>% 
  rename(lon = Var1, lat = Var2) %>% 
  left_join(MHW_cat_clim_sub, by = c("lon", "lat")) %>% 
  mutate(value = ifelse(is.na(value), "III Severe", value))


MHW_raster <- MHW_cat_clim_sub 
colnames(MHW_raster) <- c("X","Y","Z")
MHW_raster$Z <- as.numeric(factor(MHW_raster$Z, levels = c("I Moderate", "II Strong")))
rast <- rasterFromXYZ(MHW_raster, res = c(0.25, 0.25), digits = 2, 
                      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
                      # crs = "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=km +no_defs" )
plot(rast)


MHW_raster <- MHW_cat_clim_sub %>% 
  dplyr::rename(x = lon, y = lat, z = value) %>% 
  rasterFromXYZ()
plot(MHW_raster)

#Did this....
# s = SpatialPixelsDataFrame(df[,c('lng', 'lat')], data = df)
s <- SpatialPixelsDataFrame(MHW_raster[,c('X', 'Y')], data = MHW_raster)
crs(s) <-  sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

r <- raster(s)

# Set up the colors
# val <-  c("I Moderate", "II Strong", "III Severe", "IV Extreme")
# pal <-  colorFactor(c("#ffc866", "#ff6900", "#9e0000", "#2d0000"), levels = val,
#                    na.color = "transparent", ordered = T)
val <- c(1, 2)
pal <-  colorFactor(c("#ffc866", "#ff6900"), levels = val,
                    na.color = "transparent", ordered = T)

# Made the map
leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addRasterImage(r, colors = pal, opacity = 0.5) %>%
  addLegend(pal = pal, values = val, title = "MHW Category")

map_data <- map %>% 
  addRasterImage(rast, colors = pal, opacity = 0.5) %>%
  addLegend(pal = pal, values = val, title = "MHW Category")
  # addCircleMarkers(data = MHW_cat_clim_sub, ~lon, ~lat,
  #                  weight = 0.5,
  #                  color = "grey", 
  #                  fillColor = MHW_cat_clim_sub$value,
  #                  radius = 4, 
  #                  fillOpacity = 0.9, 
  #                  stroke = T, 
  #                  label = ~paste0('Event at: ', 
  #                                  as.character(round(lat,3)), ', ', 
  #                                  as.character(round(lon,3))), 
  #                  group = 'Points')
map_data  

# Save --------------------------------------------------------------------

# # # save a stand-alone, interactive map as an html file
# library(htmlwidgets)
# saveWidget(widget = map, file = 'map.html', selfcontained = T)

# # # save a snapshot as a png file
# library(mapview)
# mapshot(map, file = 'map.png')
