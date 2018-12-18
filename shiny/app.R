
# Load libraries ----------------------------------------------------------

# library(tidyverse)
library(dplyr)
library(shiny)
library(leaflet)
library(raster)
library(rgdal)
# library(ncdf4)
# library(rasterVis)
# library(htmlwidgets)
# library(mapview)
# library(RColorBrewer)


# Load data ---------------------------------------------------------------

# For testing only
# setwd("shiny")

# The event categories
load("MHW_cat_clim.RData")
# load("MHW_event.RData")

# Prep --------------------------------------------------------------------

# The category colour pallette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# MHW_categories <- factor(levels = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))

# Colour palettes for different metrics etc.
pal_factor <- colorFactor(palette = MHW_colours, levels = levels(MHW_cat_clim_sub$category))
pal_cat <- colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)
pal_duration <- colorNumeric(palette = c("white", "green"), na.color = NA, 
                             domain = c(0, 100))
pal_intMean <- colorNumeric(palette = c("white", "pink"), na.color = NA, 
                            domain = c(0, 10))
pal_intMax <- colorNumeric(palette = c("white", "purple"), na.color = NA, 
                           domain = c(0, 20))
pal_intCum <- colorNumeric(palette = c("white", "brown"), na.color = NA, 
                           domain = c(0, 500))

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
                             choices = list("Categories", "Metrics"),
                             inline = TRUE,
                             selected = "Categories"),
                uiOutput(outputId = "Metric"),
                dateInput(inputId = "date_choice", 
                          label = "Date", 
                          # value = MHW_cat_clim_sub$t[MHW_cat_clim_sub$intensity == max(MHW_cat_clim_sub$intensity)][1], 
                          value = as.Date("2017-02-14"),
                          min = as.Date("2017-01-01"), max = as.Date("2017-12-31")
                          # min = min(MHW_cat_clim_sub$t), max = max(MHW_cat_clim_sub$t)
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)


# Server ------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
  
  # Reactive UI for chosing MHW metrics
  output$Metric <-  renderUI({
    if(input$Pixels == "Metrics"){
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
    } else if(input$Pixels == "Metrics"){
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
      }
      return(pal_reactive)
    }
  })
  
  baseData <- reactive({
    MHW_db <- DBI::dbConnect(RSQLite::SQLite(), "../data/MHW_db.sqlite")
    # dbplyr::src_dbi(MHW_db)
    # date_filter <- as.integer(as.Date("2017-06-17"))
    date_filter <- as.integer(input$date_choice)
    if(input$Pixels == "Categories"){
      baseData <- tbl(MHW_db, "MHW_cat_clim_sub") %>% 
        filter(t == date_filter) %>% 
        collect() %>% 
        mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                      "III Severe", "IV Extreme"))) %>%
        na.omit()
    } else if(input$Pixels == "Metrics"){
      baseData <- tbl(MHW_db, "MHW_event_sub") %>% 
        filter(date_start <= date_filter, 
               date_end >= date_filter) %>% 
        collect()
    }
    DBI::dbDisconnect(MHW_db)
    return(baseData)
  })
  
  rasterNonProj <- reactive({
    baseData <- baseData()
    if(input$Pixels == "Categories"){
      MHW_raster <- baseData %>% 
        dplyr::select(lon, lat, category)
    } else if(input$Pixels == "Metrics"){
      # MHW_raster <- baseData
      if(is.null(input$metrics)){
        MHW_raster <- baseData %>% 
          dplyr::select(lon, lat, intensity_max) %>% 
          slice(1)
      } else {
        if(input$metrics == "Duration"){
          MHW_raster <- baseData %>% 
            dplyr::select(lon, lat, duration) %>%
            mutate(duration = ifelse(duration > 100, 100, duration))
        }
        if(input$metrics == "Maximum Intensity"){
          MHW_raster <- baseData %>%
            dplyr::select(lon, lat, intensity_max) %>% 
            mutate(intensity_max = ifelse(intensity_max > 20, 20, intensity_max))
        }
        if(input$metrics == "Mean Intensity"){
          MHW_raster <- baseData %>%
            dplyr::select(lon, lat, intensity_mean) %>% 
            mutate(intensity_mean = ifelse(intensity_mean > 10, 10, intensity_mean))
        }
        if(input$metrics == "Cumulative Intensity"){
          MHW_raster <- baseData %>%
            dplyr::select(lon, lat, intensity_cumulative) %>% 
            mutate(intensity_cumulative = ifelse(intensity_cumulative > 500, 500, intensity_cumulative))
        }
      }
    }
    colnames(MHW_raster) <- c("X", "Y", "Z")
    MHW_raster$Z <- as.numeric(MHW_raster$Z)
    MHW_raster <- rasterFromXYZ(MHW_raster, res = c(0.25, 0.25), digits = 3,
                                crs = inputProj)
    return(MHW_raster)
  })
  
  rasterProj <- reactive({
    rasterProj <- rasterNonProj()
    rasterProj <- projectRasterForLeaflet(rasterProj, method = "ngb")
    return(rasterProj)
  })
  
  output$map <- renderLeaflet({
    leaflet(MHW_cat_clim_sub) %>%
      setView(-60, 45, zoom = 5, options = tileOptions(minZoom = 3, maxZoom = 6)) %>%
      # addProviderTiles("Stamen.TonerLite",
      #                  group = "Toner", 
      #                  options = tileOptions(minZoom = 0, maxZoom = 16)) #%>%
      addTiles(group = "OSM", 
               options = tileOptions(minZoom = 3, maxZoom = 6, opacity = 0.5)) %>%
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
      addRasterImage(rasterProj(), colors = pal_reactive(), project = FALSE, opacity = 0.7) %>%
      # setView(options = tileOptions(minZoom = 3, maxZoom = 6)) %>% 
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
    rasterProj <- rasterProj()
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
      cell <- cellFromXY(rasterProj, c(xy$x, xy$y))
      #At this point, you can also retrace back the center of the cell in
      #leaflet coordinates, starting from the cell number!
      xy <- SpatialPoints(xyFromCell(rasterProj, cell))
      proj4string(xy) <- leafletProj
      xy <- as.data.frame(spTransform(xy, inputProj))
      #Get row and column, to print later
      rc <- rowColFromCell(rasterNonProj, cell)
      #Get value of the given cell
      val <- rasterProj[cell]
      if(input$Pixels == "Categories"){
        content <- paste0("Lon = ", round(x, 3),
                          "<br>Lat = ", round(y, 3),
                          # "<br>Intensity = ", cell_meta$intensity,"°C",
                          "<br>Category = ", names(MHW_colours)[val])
      } else if(input$Pixels == "Metrics"){
        cell_meta <- baseData()
        cell_meta <- filter(cell_meta, lon == x, lat == y)
        if(is.null(input$metrics)){
          content <- paste0("Lon = ", round(x, 3),
                            "<br>Lat = ", round(y, 3))
        } else {
          content_base <- paste0("Lon = ",round(x, 3),
                                 "<br>Lat = ",round(y, 3),
                                 "<br>Start Date = ",as.Date(cell_meta$date_start, origin = "1970-01-01"),
                                 "<br>Peak Date = ",as.Date(cell_meta$date_peak, origin = "1970-01-01"),
                                 "<br>End Date = ",as.Date(cell_meta$date_end, origin = "1970-01-01"))
          if(input$metrics == "Duration"){
            content <- paste0(content_base,
                              "<br>",input$metrics," = ",cell_meta$duration," days")
          }
          if(input$metrics == "Mean Intensity"){
            content <- paste0(content_base,
                              "<br>",input$metrics," = ",cell_meta$intensity_mean,"°C")
          }            
          if(input$metrics == "Maximum Intensity"){
            content <- paste0(content_base,
                              "<br>",input$metrics," = ",cell_meta$intensity_max,"°C")
          }
          if(input$metrics == "Cumulative Intensity"){
            content <- paste0(content_base,
                              "<br>",input$metrics," = ",cell_meta$intensity_cumulative,"°C x days")
          }
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
    } else if(input$legend & input$Pixels == "Metrics"){
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
      }
    }
  })
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

