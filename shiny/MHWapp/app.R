
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(shiny)
library(leaflet)
library(raster)
# library(rgdal)
# library(htmlwidgets)
# library(mapview)
library(RColorBrewer)


# Load data ---------------------------------------------------------------

# Prep the global event clims
# load("data/MHW_cat_clim_sub.RData")
# MHW_cat_clim <- MHW_cat_clim_sub %>%
#   mutate(category = factor(category, levels = c("I Moderate", "II Strong",
#                                                 "III Severe", "IV Extreme"))) %>%
#   dplyr::select(lon, lat, t, intensity, category)
# save(MHW_cat_clim, file = "shiny/MHWapp/MHW_cat_clim.RData")

# The event categories
# load("shiny/MHWapp/MHW_cat_clim.RData")
load("MHW_cat_clim.RData")

# The category colour pallette
fillColCat <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)


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


# UI ----------------------------------------------------------------------


# Define UI for map controls
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                #             value = range(quakes$mag), step = 0.1
                # ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                dateInput("date_choice", "Date", 
                          value = MHW_cat_clim$t[MHW_cat_clim$intensity == max(MHW_cat_clim$intensity)][1], 
                          min = min(MHW_cat_clim$t), max = max(MHW_cat_clim$t)
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)


# Server ------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    MHW_cat_clim[MHW_cat_clim$t == input$date_choice,]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, MHW_cat_clim$intensity)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(MHW_cat_clim) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = 200, weight = 1, color = "#777777",
                 fillColor = ~pal(intensity), fillOpacity = 0.7, 
                 popup = ~paste0("Daily intensity: ", intensity)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = MHW_cat_clim)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~intensity
      )
    }
  })
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

