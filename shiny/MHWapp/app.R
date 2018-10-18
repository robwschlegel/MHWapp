
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(shiny)
library(leaflet)
library(raster)
library(rgdal)
# library(rasterVis)
# library(htmlwidgets)
# library(mapview)
# library(RColorBrewer)


# Load data ---------------------------------------------------------------

### The event clims
## SA only
# load("data/MHW_cat.RData")
# MHW_cat_clim <- MHW_cat %>%
#   unnest(cat) %>%
#   filter(row_number() %% 2 == 1) %>%
#   unnest(cat) %>%
#   mutate(category = factor(category, levels = c("I Moderate", "II Strong",
#                                                 "III Severe", "IV Extreme")),
#          intensity = round(intensity, 2)) %>%
#   dplyr::select(lon, lat, t, intensity, category)
# save(MHW_cat_clim, file = "shiny/MHWapp/MHW_cat_clim.RData")
## Global data
# load("data/MHW_cat_clim_sub.RData")
# MHW_cat_clim <- MHW_cat_clim_sub %>%
#   mutate(category = factor(category, levels = c("I Moderate", "II Strong",
#                                                 "III Severe", "IV Extreme")),
#          lon = ifelse(lon > 180, lon-360, lon)) %>%
#   dplyr::select(lon, lat, t, intensity, category)
# save(MHW_cat_clim, file = "shiny/MHWapp/MHW_cat_clim.RData")

# The event categories
# load("shiny/MHWapp/MHW_cat_clim.RData")
load("MHW_cat_clim.RData")

# The category colour pallette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# Testing...
# filename <- system.file("external/test.grd", package="raster")
# rast <- raster(filename)
# leaflet() %>% addRasterImage(rast)


# Base map ----------------------------------------------------------------

# start basemap
# map <- leaflet() %>% 
#   # add ocean basemap
#   addProviderTiles(providers$Esri.OceanBasemap) %>%
#   # add another layer with place names
#   addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
#   # add graticules from a NOAA webserver
#   addWMSTiles(
#     "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
#     layers = c("1-degree grid", "5-degree grid"),
#     options = WMSTileOptions(format = "image/png8", transparent = TRUE),
#     attribution = NULL,group = 'Graticules') %>%
#   # focus map in a certain area / zoom level
#   setView(lng = 25, lat = -35, zoom = 5) %>%
#   hideGroup(c('Place names'))
# 
# val <- c(1, 2)
# pal <-  colorFactor(c("#ffc866", "#ff6900"), levels = val,
#                     na.color = "transparent", ordered = T)

pal_circ <- colorFactor(palette = MHW_colours, levels = levels(MHW_cat_clim$category))
pal_rast <- colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)


# UI ----------------------------------------------------------------------


# Define UI for map controls
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                #             value = range(quakes$mag), step = 0.1
                # ),
                # selectInput("colors", "Color Scheme",
                #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                # ),
                dateInput("date_choice", "Date", 
                          # value = MHW_cat_clim$t[MHW_cat_clim$intensity == max(MHW_cat_clim$intensity)][1], 
                          value = as.Date("2017-12-01"),
                          min = min(MHW_cat_clim$t), max = max(MHW_cat_clim$t)
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)


# Server ------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  circleData <- reactive({
    MHW_cat_clim[MHW_cat_clim$t == input$date_choice,]
  })
  rasterData <- reactive({
    MHW_raster <- MHW_cat_clim %>% 
      # tester...
      # filter(t == as.Date("1984-06-08")) %>%
      filter(t == input$date_choice) %>%
      dplyr::select(lon, lat, category) %>% 
      rename(X = lon, Y = lat, Z = category)
    MHW_raster$Z <- as.numeric(MHW_raster$Z)
    MHW_raster <- rasterFromXYZ(MHW_raster, res = c(0.25, 0.25), digits = 2,
                          crs = "+proj=longlat +datum=WGS84 +no_defs")
    return(MHW_raster)
    # rasterFromXYZ(MHW_raster, res = c(0.25, 0.25), digits = 2,
    #               crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorFactor(MHW_colours, levels(MHW_cat_clim$category))
  # })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(MHW_cat_clim) %>%
      setView(-30, 30, zoom = 4, options = tileOptions(minZoom = 0, maxZoom = 8)) %>%
      # addProviderTiles("Stamen.TonerLite",
      #                  group = "Toner", 
      #                  options = tileOptions(minZoom = 0, maxZoom = 16)) #%>%
      addTiles(group = "OSM", 
               options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.7)) #%>%
      # addProviderTiles("Esri.WorldTopoMap",    
      #                  group = "Topo") #%>% 
      # fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    leafletProxy("map") %>%
      # clearShapes() %>%
      clearImages() %>% 
      addRasterImage(rasterData(), colors = pal_rast, method = "ngb", opacity = 0.7) %>%
      # addCircles(data = circleData(), radius = 5000, weight = 1, color = "grey80",
      #            fillColor = ~pal_circ(category), fillOpacity = 0.7, 
      #            popup = ~paste0("Daily intensity: ", intensity,
      #                            "\nCategory:", category)) %>%
      addScaleBar(position = "bottomright")
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = MHW_cat_clim)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position = "bottomright",
                          pal = pal_circ, values = ~category
      )
    }
  })
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

