
# Load libraries ----------------------------------------------------------

# library(tidyverse)
library(dplyr)
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
#   dplyr::select(lon, lat, t, intensity, category) %>% 
#   filter(t >= as.Date("2017-12-01"))
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


# Prep --------------------------------------------------------------------

pal_circ <- colorFactor(palette = MHW_colours, levels = levels(MHW_cat_clim$category))
pal_rast <- colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)

#Set projections
inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"


# UI ----------------------------------------------------------------------

# Define UI for map controls
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, draggable = TRUE,
                radioButtons(inputId = "Pixels",
                             label = "Display",
                             choices = list("Categories", "Events", "Anomalies"),
                             inline = TRUE,
                             selected = "Categories"),
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
  
  # circleData <- reactive({
  #   MHW_cat_clim[MHW_cat_clim$t == input$date_choice,]
  # })
  
  rasterData <- reactive({
    MHW_raster <- MHW_cat_clim %>% 
      # tester...
      # filter(t == as.Date("2017-12-01")) %>%
      filter(t == input$date_choice) %>%
      dplyr::select(lon, lat, category) %>% 
      rename(X = lon, Y = lat, Z = category)
    MHW_raster$Z <- as.numeric(MHW_raster$Z)
    MHW_raster <- rasterFromXYZ(MHW_raster, res = c(0.25, 0.25), digits = 2,
                          crs = "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    # MHW_raster <- projectRaster(MHW_raster, crs = leafletProj)
    MHW_raster <- projectRasterForLeaflet(MHW_raster, method = "ngb")
    return(MHW_raster)
    # rasterFromXYZ(MHW_raster, res = c(0.25, 0.25), digits = 2,
    #               crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    # crs = "+proj=longlat +datum=NAD83"
    # +init=epsg:3411
    # +init=epsg:4326
  })
  
  lldepth <- reactive({
    lldepth <- MHW_cat_clim %>%
      # tester...
      # filter(t == as.Date("2017-12-01")) %>%
      filter(t == input$date_choice) %>%
      dplyr::select(lon, lat, category) %>%
      rename(X = lon, Y = lat, Z = category)
    lldepth$Z <- as.numeric(lldepth$Z)
    lldepth <- rasterFromXYZ(lldepth, res = c(0.25, 0.25), digits = 2,
                             crs = "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    # lldepth <- projectRasterForLeaflet(lldepth, method = "ngb")
  })

  depth <- reactive({
    redepth <- MHW_cat_clim %>%
      # tester...
      # filter(t == as.Date("2017-12-01")) %>%
      filter(t == input$date_choice) %>%
      dplyr::select(lon, lat, category) %>%
      rename(X = lon, Y = lat, Z = category)
    redepth$Z <- as.numeric(redepth$Z)
    redepth <- rasterFromXYZ(redepth, res = c(0.25, 0.25), digits = 2,
                             crs = "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    depth <- projectRasterForLeaflet(redepth, method = "ngb")
  })
  
  output$map <- renderLeaflet({
    leaflet(MHW_cat_clim) %>%
      setView(-30, 30, zoom = 4, options = tileOptions(minZoom = 0, maxZoom = 8)) %>%
      # addProviderTiles("Stamen.TonerLite",
      #                  group = "Toner", 
      #                  options = tileOptions(minZoom = 0, maxZoom = 16)) #%>%
      addTiles(group = "OSM", 
               options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5)) #%>%
      # addProviderTiles("Esri.WorldTopoMap",    
      #                  group = "Topo") #%>% 
      # fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
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
  
  #Observer to show Popups on click
  observe({
    click <- input$map_click
    if (!is.null(click)) {
      showpos(x = click$lng, y = click$lat)
    }
  })
  
  #Show popup on clicks
  showpos <- function(x = NULL, y = NULL) {
    lldepth <- lldepth()
    depth <- depth()
    # Translate Lon-Lat to cell number using the unprojected raster
    # This is because the projected raster is not in degrees, we cannot use it!
    cell <- cellFromXY(lldepth, c(x, y))
    #If the click is inside the raster...
    if (!is.na(cell)) {
      xy <- xyFromCell(lldepth, cell) #Get the center of the cell
      x <- xy[1]
      y <- xy[2]
      xy <- SpatialPoints(data.frame(x,y))
      proj4string(xy) <- inputProj
      # proj4string(xy) <- leafletProj
      xy <- as.data.frame(spTransform(xy, leafletProj))
      #Get the cell number from the newly transformed metric X and Y.
      cell <- cellFromXY(depth, c(xy$x, xy$y))
      #At this point, you can also retrace back the center of the cell in
      #leaflet coordinates, starting from the cell number!
      xy <- SpatialPoints(xyFromCell(depth, cell))
      proj4string(xy) <- leafletProj
      xy <- as.data.frame(spTransform(xy, inputProj))
      #Get row and column, to print later
      rc <- rowColFromCell(lldepth, cell)
      #Get value of the given cell
      val <- depth[cell]
      content <- paste0("Lon = ", round(x, 3),
                        ";\n Lat = ", round(y, 3),
                        "; Category = ", names(MHW_colours)[val])
      proxy <- leafletProxy("map")
      #add Popup
      proxy %>% clearPopups() %>% addPopups(x, y, popup = content)
      #add rectangles for testing
      # proxy %>% clearShapes() %>% addRectangles(x-0.25/2, y-0.25/2, x+0.25/2, y+0.25/2)
      proxy %>% clearMarkers() %>% addAwesomeMarkers(x, y)
    }
  }
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = MHW_cat_clim)
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position = "bottomright",
                          pal = pal_circ, 
                          values = ~category
      )
    }
  })
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

