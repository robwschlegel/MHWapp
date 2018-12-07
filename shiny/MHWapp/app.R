
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
# MHW_cat_clim_sub <- MHW_cat %>%
#   unnest(cat) %>%
#   filter(row_number() %% 2 == 1) %>%
#   unnest(cat) %>%
#   mutate(category = factor(category, levels = c("I Moderate", "II Strong",
#                                                 "III Severe", "IV Extreme")),
#          intensity = round(intensity, 2)) %>%
#   dplyr::select(lon, lat, t, intensity, category)
# save(MHW_cat_clim_sub, file = "shiny/MHWapp/MHW_cat_clim_sub.RData")
## Global data
# load("data/MHW_cat_clim_sub_sub.RData")
# MHW_cat_clim_sub <- MHW_cat_clim_sub_sub %>%
#   mutate(category = factor(category, levels = c("I Moderate", "II Strong",
#                                                 "III Severe", "IV Extreme")),
#          lon = ifelse(lon > 180, lon-360, lon)) %>%
#   dplyr::select(lon, lat, t, intensity, category) %>% 
#   filter(t >= as.Date("2017-12-01"))
# save(MHW_cat_clim_sub, file = "shiny/MHWapp/MHW_cat_clim_sub.RData")

# The event categories
# load("shiny/MHWapp/MHW_cat_clim.RData")
# load("shiny/MHWapp/MHW_event.RData")
load("MHW_cat_clim.RData")
load("MHW_event.RData")


# Prep --------------------------------------------------------------------

# The category colour pallette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# Colour palettes for different metrics etc.
pal_factor <- colorFactor(palette = MHW_colours, levels = levels(MHW_cat_clim_sub$category))
pal_cat <- colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)
pal_duration <- colorNumeric(palette = c("white", "green"), na.color = NA, 
                             domain = c(0, 100))
pal_intMean <- colorNumeric(palette = c("white", "pink"), na.color = NA, 
                            domain = c(0, max(MHW_event_sub$intensity_mean, na.rm = T)))
pal_intMax <- colorNumeric(palette = c("white", "purple"), na.color = NA, 
                           domain = c(0, max(MHW_event_sub$intensity_max, na.rm = T)))
pal_intCum <- colorNumeric(palette = c("white", "brown"), na.color = NA, 
                           domain = c(0, 500))
# pal_rateOn <- colorNumeric(palette = c("white", "red"), na.color = NA, 
#                            domain = c(0, max(MHW_event_sub$rate_onset, na.rm = T)))
# pal_rateDe <- colorNumeric(palette = c("white", "blue"), na.color = NA, 
#                            domain = c(0, max(MHW_event_sub$rate_decline, na.rm = T)))
# pal_anom <- colorNumeric(palette = c("blue", "red"), domain = c(-5, 5), na.color = NA)

# Establish projection choices
inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"


# UI ----------------------------------------------------------------------

# Define UI for map controls
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, draggable = TRUE,
                radioButtons(inputId = "Dataset",
                             label = "Data",
                             choices = "OISST",
                             inline = TRUE,
                             selected = "OISST"),
                radioButtons(inputId = "Pixels",
                             label = "Display",
                             choices = list("Categories", "Events"),
                             inline = TRUE,
                             selected = "Categories"),
                uiOutput(outputId = "Metric"),
                dateInput(inputId = "date_choice", 
                          label = "Date", 
                          # value = MHW_cat_clim_sub$t[MHW_cat_clim_sub$intensity == max(MHW_cat_clim_sub$intensity)][1], 
                          value = as.Date("2017-12-01"),
                          min = min(MHW_cat_clim_sub$t), max = max(MHW_cat_clim_sub$t)
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)


# Server ------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
  
  # Reactive UI for chosing MHW metrics
  output$Metric <-  renderUI({
    if(input$Pixels == "Events"){
      selectInput(inputId = 'metrics', 
                  label = 'Metric', 
                  choices = c("Duration", "Mean Intensity", "Maximum Intensity", "Cumulative Intensity"),
                  selected = "Maximum Intensity",
                  multiple = F, selectize = T)
  } else {
    }
  })
  
  # Reactive expression for the data subsetted to what the user selected
  pal_reactive <- reactive({
    if(input$Pixels == "Categories"){
      pal_reactive <- pal_cat
      } else if(input$Pixels == "Events"){
        # pal_reactive <- pal_intMax
        if(is.null(input$metrics)){
          pal_reactive <- pal_intMax
        } else{
          if(input$metrics == "Duration"){
            pal_reactive <- pal_duration
          }
          if(input$metrics == "Maximum Intensity"){
            pal_reactive <- pal_intMax
          }
          if(input$metrics == "Mean Intensity"){
            pal_reactive <- pal_intMean
          }
          if(input$metrics == "Cumulative Intensity"){
            pal_reactive <- pal_intCum
          }
          # if(input$metrics == "Rate of Onset"){
          #   pal_reactive <- pal_rateOn
          # }
          # if(input$metrics == "Rate of Decline"){
          #   pal_reactive <- pal_rateDe
          # }
        }
        return(pal_reactive)
      }
    
    #else if(input$Pixels == "Anomalies"){
            # pal_reactive <- pal_anom
            # }
    })
  
  baseData <- reactive({
    if(input$Pixels == "Categories"){
      baseData <- MHW_cat_clim_sub %>% 
        filter(t == input$date_choice) %>%
        dplyr::select(lon, lat, category) %>% 
        dplyr::rename(X = lon, Y = lat, Z = category)
      } else if(input$Pixels == "Events"){
        baseData <- MHW_event_sub %>% 
          filter(date_start <= input$date_choice, 
                 date_end >= input$date_choice) #%>% 
          # dplyr::select(lon, lat, intensity_max) %>%
          # dplyr::rename(X = lon, Y = lat, Z = intensity_max)
        # metric_choice <- as.character(input$metrics)
        if(is.null(input$metrics)){
          baseData <- baseData[1,which(colnames(baseData) %in% c("lon", "lat", "intensity_max"))]
          colnames(baseData) <- c("X", "Y", "Z")
        } else {
          if(input$metrics == "Duration"){
            baseData <- baseData[,which(colnames(baseData) %in% c("lon", "lat", "duration"))]
            # Reduce large durations so that colour scale still shows range for normal events
            # The actual duration is still shown in the popup info
            baseData <- baseData %>% 
              mutate(duration = ifelse(duration > 100, 100, duration))
          }
          if(input$metrics == "Maximum Intensity"){
            baseData <- baseData[,which(colnames(baseData) %in% c("lon", "lat", "intensity_max"))]
          }
          if(input$metrics == "Mean Intensity"){
            baseData <- baseData[,which(colnames(baseData) %in% c("lon", "lat", "intensity_mean"))]
          }
          if(input$metrics == "Cumulative Intensity"){
            baseData <- baseData[,which(colnames(baseData) %in% c("lon", "lat", "intensity_cumulative"))]
            # Reduce large cumulative intensities so that colour scale still shows range for normal events
            # The actual cumulative intensity is still shown in the popup info
            baseData <- baseData %>% 
              mutate(intensity_cumulative = ifelse(intensity_cumulative > 500, 500, intensity_cumulative))
          }
          # if(input$metrics == "Rate of Onset"){
          #   baseData <- baseData[,which(colnames(baseData) %in% c("lon", "lat", "rate_onset"))]
          # }
          # if(input$metrics == "Rate of Decline"){
          #   baseData <- baseData[,which(colnames(baseData) %in% c("lon", "lat", "rate_decline"))]
          # }
          colnames(baseData) <- c("X", "Y", "Z")
        }
      }
    #else if(input$Pixels == "Anomalies"){
          # baseData <- MHW_clim %>%
          #   filter(t == input$date_choice) %>%
          #   dplyr::select(lon, lat, anom) %>%
          #   dplyr::rename(X = lon, Y = lat, Z = anom)
          # }
    return(baseData)
    })
  
  rasterData <- reactive({
    MHW_raster <- baseData() #%>% 
      # tester...
      # filter(t == as.Date("2017-12-01")) %>%
      # filter(t == input$date_choice) %>%
      # dplyr::select(lon, lat, category) %>% 
      # dplyr::rename(X = lon, Y = lat, Z = category)
    MHW_raster$Z <- as.numeric(MHW_raster$Z)
    MHW_raster <- rasterFromXYZ(MHW_raster, res = c(0.25, 0.25), digits = 3,
                          crs = inputProj)
    MHW_raster <- projectRasterForLeaflet(MHW_raster, method = "ngb")
    # MHW_raster <- projectRaster(MHW_raster, crs = leafletProj)
    # if(input$Pixels == "Categories"){
    #   MHW_raster <- projectRasterForLeaflet(MHW_raster, method = "ngb")
    #   } else if(input$Pixels == "Events"){
    #     MHW_raster <- projectRasterForLeaflet(MHW_raster, method = "bilinear")
    #     }
    return(MHW_raster)
    # rasterFromXYZ(MHW_raster, res = c(0.25, 0.25), digits = 2,
    #               crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    # crs = "+proj=longlat +datum=NAD83"
    # +init=epsg:3411
    # +init=epsg:4326
  })
  
  rasterNonProj <- reactive({
    rasterNonProj <- baseData()
    # rasterNonProj <- MHW_cat_clim_sub %>%
    #   # tester...
    #   # filter(t == as.Date("2017-12-01")) %>%
    #   filter(t == input$date_choice) %>%
    #   dplyr::select(lon, lat, category) %>%
    #   rename(X = lon, Y = lat, Z = category)
    rasterNonProj$Z <- as.numeric(rasterNonProj$Z)
    rasterNonProj <- rasterFromXYZ(rasterNonProj, res = c(0.25, 0.25), digits = 3,
                             crs = "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    # rasterNonProj <- projectRasterForLeaflet(rasterNonProj, method = "ngb")
  })
  
  output$map <- renderLeaflet({
    leaflet(MHW_cat_clim_sub) %>%
      setView(-60, 45, zoom = 5, options = tileOptions(minZoom = 0, maxZoom = 8)) %>%
      # addProviderTiles("Stamen.TonerLite",
      #                  group = "Toner", 
      #                  options = tileOptions(minZoom = 0, maxZoom = 16)) #%>%
      addTiles(group = "OSM", 
               options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5)) %>%
      addPopups(-60, 45, 
                popup = paste("Hello and welcome to the MHW tracker.<br>", 
                              "This is a paceholder for more information to come."))
      # addProviderTiles("Esri.WorldTopoMap",    
      #                  group = "Topo") #%>% 
      # fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  observe({
    leafletProxy("map") %>%
      clearImages() %>% 
      addRasterImage(rasterData(), colors = pal_reactive(), project = FALSE, opacity = 0.7) %>%
      addScaleBar(position = "bottomright")
  })
  
  # Observer to show Popups on click
  observe({
    click <- input$map_click
    if(!is.null(click)){
      showpos(x = click$lng, y = click$lat)
    }
  })
  
  # Show popup on clicks
  showpos <- function(x = NULL, y = NULL) {
    rasterNonProj <- rasterNonProj()
    rasterData <- rasterData()
    # depth <- depth()
    # Translate Lon-Lat to cell number using the unprojected raster
    # This is because the projected raster is not in degrees, we cannot use it!
    cell <- cellFromXY(rasterNonProj, c(x, y))
    #If the click is inside the raster...
    if(!is.na(cell)) {
      xy <- xyFromCell(rasterNonProj, cell) #Get the center of the cell
      x <- xy[1]
      y <- xy[2]
      xy <- SpatialPoints(data.frame(x,y))
      proj4string(xy) <- inputProj
      # proj4string(xy) <- leafletProj
      xy <- as.data.frame(spTransform(xy, leafletProj))
      #Get the cell number from the newly transformed metric X and Y.
      cell <- cellFromXY(rasterData, c(xy$x, xy$y))
      #At this point, you can also retrace back the center of the cell in
      #leaflet coordinates, starting from the cell number!
      xy <- SpatialPoints(xyFromCell(rasterData, cell))
      proj4string(xy) <- leafletProj
      xy <- as.data.frame(spTransform(xy, inputProj))
      #Get row and column, to print later
      rc <- rowColFromCell(rasterNonProj, cell)
      #Get value of the given cell
      val <- rasterData[cell]
      if(input$Pixels == "Categories"){
        cell_meta <- MHW_cat_clim_sub %>% 
          filter(lon == x, lat == y, t == input$date_choice)
        content <- paste0("Lon = ", round(x, 3),
                          "<br>Lat = ", round(y, 3),
                          "<br>Intensity = ", cell_meta$intensity,"°C",
                          "<br>Category = ", names(MHW_colours)[val])
        } else if(input$Pixels == "Events"){
          cell_meta <- MHW_event_sub %>% 
            filter(lon == x, lat == y,
                   date_start <= input$date_choice, 
                   date_end >= input$date_choice)
          if(is.null(input$metrics)){
            content <- paste0("Lon = ", round(x, 3),
                              "<br>Lat = ", round(y, 3))
          } else {
            content_base <- paste0("Lon = ",round(x, 3),
                                   "<br>Lat = ",round(y, 3),
                                   "<br>Start Date = ",cell_meta$date_start,
                                   "<br>Peak Date = ",cell_meta$date_peak,
                                   "<br>End Date = ",cell_meta$date_end)
            if(input$metrics == "Duration"){
              content <- paste0(content_base,
                                "<br>",input$metrics," = ",cell_meta$duration," days")
            }
            if(input$metrics %in% c("Mean Intensity", "Maximum Intensity")){
              content <- paste0(content_base,
                                "<br>",input$metrics," = ",val,"°C")
            }
            if(input$metrics == "Cumulative Intensity"){
              content <- paste0(content_base,
                                "<br>",input$metrics," = ",cell_meta$intensity_cumulative,"°C x days")
            }
            # if(input$metrics %in% c("Rate of Onset", "Rate of Decline")){
            #   content <- paste0(content_base,
            #                     "<br>",input$metrics," = ",val,"°C/day")
            # }
          }
        }
      proxy <- leafletProxy("map")
      #add Popup
      proxy %>% clearPopups() %>% addPopups(x, y, popup = content)
      #add rectangles for testing
      # proxy %>% clearShapes() %>% addRectangles(x-0.25/2, y-0.25/2, x+0.25/2, y+0.25/2)
      # proxy %>% clearMarkers() %>% addAwesomeMarkers(x, y)
    }
  }
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = MHW_cat_clim_sub)
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if(input$legend & input$Pixels == "Categories"){
      proxy %>% addLegend(position = "bottomright",
                          pal = pal_factor,
                          values = ~category
      )
    } else if(input$legend & input$Pixels == "Events"){
      if(is.null(input$metrics)){
        # proxy %>% addLegend(position = "bottomright", pal = pal_intMax,
        #                     values = c(0, 10, 20), title = "Temp. °C")
      } else{
        if(input$metrics == "Duration"){
          proxy %>% addLegend(position = "bottomright", pal = pal_duration,
                              values = c(1, 25, 50, 75, 100), labels = c("1", "25", "50", "75", ">100"), 
                              title = "Duration (days)")
        }
        if(input$metrics == "Maximum Intensity"){
          proxy %>% addLegend(position = "bottomright", pal = pal_intMax,
                              values = c(0, 10, 20), bins = 3, title = "Temp. (°C)")
        }
        if(input$metrics == "Mean Intensity"){
          proxy %>% addLegend(position = "bottomright", pal = pal_intMean,
                              values = c(0, 5, 10), bins = 3, title = "Temp. (°C)")
        }
        if(input$metrics == "Cumulative Intensity"){
          proxy %>% addLegend(position = "bottomright", pal = pal_intCum,
                              values = c(0, 250, 500), bins = 3, title = "Temp. (°C x days)")
        }
        # if(input$metrics == "Rate of Onset"){
        #   proxy %>% addLegend(position = "bottomright", pal = pal_rateOn,
        #                       values = c(0, 2, 4), bins = 3, title = "Temp. (°C/day)")
        # }
        # if(input$metrics == "Rate of Decline"){
        #   proxy %>% addLegend(position = "bottomright", pal = pal_rateDe,
        #                       values = c(0, 2, 4), bins = 3, title = "Temp. (°C/day)")
        # }
      }
    }
  })
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

