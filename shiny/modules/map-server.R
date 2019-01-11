map <- function(input, output, session) {
  ns <- session$ns


# Functions ---------------------------------------------------------------
  

# Reactives ---------------------------------------------------------------

  ### base map data
  baseData <- reactive({
    date_filter <- input$date_choice
    year_filter <- year(date_filter)
    sub_dir <- paste0("cat_clim/",year_filter)
    sub_file <- paste0("cat.clim.",date_filter,".Rda")
    baseData <- readRDS(paste0(sub_dir,"/",sub_file))
    return(baseData)
  })
  
  ### Non-shiny-projected raster data
  rasterNonProj <- reactive({
    baseData <- baseData()
    MHW_raster <- baseData %>%
      dplyr::select(lon, lat, category)
    colnames(MHW_raster) <- c("X", "Y", "Z")
    MHW_raster$Z <- as.numeric(MHW_raster$Z)
    MHW_raster <- rasterFromXYZ(MHW_raster, res = c(0.25, 0.25), digits = 3,
                                crs = inputProj)
    return(MHW_raster)
  })
  
  ### Shiny-projected raster data
  rasterProj <- reactive({
    rasterProj <- rasterNonProj()
    rasterProj <- projectRasterForLeaflet(rasterProj, method = "ngb")
    return(rasterProj)
  })
  
  ### Pixel data
  pixelData <- reactive({
      click <- input$map_click
      if(!is.null(click)){
        rasterNonProj <- rasterNonProj()
        cell <- cellFromXY(rasterNonProj, c(click$lng, click$lat))
        #If the click is inside the raster...
        # xy <- c(-42.125, 20.125)
        xy <- xyFromCell(rasterNonProj, cell)
        
        # Grab time series data
        nc <- nc_open(as.character(OISST_index$file_name)[OISST_index$lon == xy[1]])
        ts_data <- data.frame(t = as.Date(nc$dim$time$vals, origin = "1970-01-01"),
                              temp = ncvar_get(nc, varid = "sst")[lat_OISST == xy[2],])
        nc_close(nc)
        
        # Grab threshold data
        nc <- nc_open(as.character(thresh_index$file_name)[OISST_index$lon == xy[1]])
        thresh_data <- data.frame(doy = as.vector(nc$dim$time$vals),
                                  seas = ncvar_get(nc, varid = "seas")[nc$dim$lat$vals == xy[2],],
                                  thresh = ncvar_get(nc, varid = "thresh")[nc$dim$lat$vals == xy[2],])
        nc_close(nc)
        
        # Grab event data
        event_file <- dir("event", full.names = T)[OISST_index$lon == xy[1]]
        event_data <- readRDS(event_file) %>% 
          filter(lat == xy[2]) %>%
          mutate(date_start = as.Date(date_start, origin = "1970-01-01"),
                 date_peak = as.Date(date_peak, origin = "1970-01-01"),
                 date_end = as.Date(date_end, origin = "1970-01-01"))
        pixelData <- list(ts = ts_data,
                          event = event_data,
                          thresh = thresh_data,
                          lon = xy[1],
                          lat = xy[2])
        return(pixelData)
      } else {
      }
  })
  
  ### reactive labels
  pixelLabel <- reactive({
    paste0("Chosen pixel; lon = ",pixelData()$lon[1],", lat = ",pixelData()$lat[1])
  })
  
  ### Download data
  downloadData <- reactive({
    data <- pixelData()$event
    data_sub <- data %>% 
      filter(date_start >= input$from, date_start <= input$to)
  })
  
  ### Create time series plot
  # input <- data.frame(from = as.Date("2017-01-01"),
  #                     to = as.Date("2017-12-31"))
  tsPlot <- reactive({
    ts_data <- pixelData()$ts
    ts_data_sub <- ts_data %>%
      filter(t >= input$from, t <= input$to)
    event_data <- pixelData()$event
    event_data_sub <- event_data %>%
      filter(date_start >= input$from, date_end <= input$to)
    thresh_data <- pixelData()$thresh
    thresh_data_sub <- heatwaveR:::make_whole_fast(data.frame(ts_x = seq(input$from, input$to, "day"),
                                                              ts_y = 1)) %>% 
      left_join(thresh_data, by = "doy") %>% 
      select(-ts_y) %>% 
      dplyr::rename(t = ts_x)
    # p <- ggplot() +
    #   geom_line(data = ts_data_sub, aes(x = t, y = temp), colour = "grey20") +
    #   geom_rug(data = event_data_sub, sides = "b",
    #            aes(x = date_start, y = intensity_max), colour = "red") +
    #   labs(x = "", y = "Temperature (°C)") +
    #   scale_y_continuous(limits = c(min(ts_data_sub$temp)-1, max(ts_data_sub$temp)+1))
    # if(nrow(thresh_data_sub) <= 1830){
    #   p <- p +
    #     # Consider adding these as traces in plot_ly()
    #     # Or perhaps to melt by variable and use the different types as a colour aesthetic
    #     geom_line(data = thresh_data_sub, aes(x = t, y = seas), colour = "green") +
    #     geom_line(data = thresh_data_sub, aes(x = t, y = thresh), colour = "red")
    # }
    # if(nrow(thresh_data_sub) <= 366){
    #   p <- p +
    #     geom_ribbon(aes(ymin = temp, ymax = thresh_data_sub$thresh))
    # }
    # ggplotly(p)
    # pp <- ggplotly(p)
    # rangeslider(pp)
  })
  
  ### Create lolliplot
  lolliPlot <- reactive({
    event_data <- pixelData()$event
    event_data_sub <- event_data %>%
      filter(date_start >= input$from, date_start <= input$to)
    # thresh_data_sub <- thresh_data %>%
    # filter(t >= input$from, t <= input$to)
    p <- ggplot(data = event_data_sub, aes(x = date_start, y = intensity_max)) +
      geom_lolli() +
      # geom_point() +
      labs(x = "", y = "Max. Intensity (°C)")
    ggplotly(p)
  })
  
  ### Create data table
  tsTable <- reactive({
    event_data <- pixelData()$event
    event_data_sub <- event_data %>% 
      filter(date_start >= input$from, date_start <= input$to)
    # data$Time <- strftime(data$DateTime, format = "%H:%M %p")
    # data$Date <- strftime(data$DateTime, format = "%B %d, %Y")
    # data[,c("Date", "Time", "TideHeight")] %>%
    #   setNames(c("Date", "Time", unit_label()))
  })
  

# Observers ---------------------------------------------------------------
  
  # Show raster image
  observe({
    leafletProxy("map") %>%
      clearImages() %>%
      addRasterImage(rasterProj(), colors = pal_cat, layerId = "map_raster",
                     project = FALSE, opacity = 0.7) %>%
      addScaleBar(position = "bottomright") #%>%
      # addMouseCoordinates() %>%
      # addImageQuery(rasterProj(), type = "mousemove", layerId = "map_raster")
      # fitBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90,
      #           options = tileOptions(minZoom = 3, maxZoom = 6))
  })
  
  ### Recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = MHW_cat_clim_sub)
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    proxy %>% addLegend(position = "bottomright",
                        pal = pal_factor,
                        values = ~category
    )
  })
  
  observeEvent(input$map_click, {
    # req(!is.null(pixelData()))
    toggleModal(session, "modal", "open")
  })
  

# Leaflet -----------------------------------------------------------------
  
  output$map <- renderLeaflet({
    leaflet(MHW_cat_clim_sub) %>%
      setView(-60, 45, zoom = 5, options = tileOptions(minZoom = 1, maxZoom = 7, noWrap = T)) %>%
      addTiles(group = "OSM",
               options = tileOptions(minZoom = 1, maxZoom = 7, opacity = 0.5, noWrap = T)) #%>%
      # addMouseCoordinates() #%>%
      # fitBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90,
      #           options = tileOptions(minZoom = 1, maxZoom = 7))
  })
  
  
  # map
  # output$leaflet <- leaflet::renderLeaflet({
  #   leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  #     
  #     setView(lat = initial_lat, lng = initial_long, zoom = initial_zoom) %>%
  #     
  #     addProviderTiles("Esri.WorldImagery", options = providerTileOptions(opacity = 1), group = "Satelite") %>%
  #     addTiles(urlTemplate = mapbox_moon, group = "Basemap") %>%
  #     
  #     addLayersControl(
  #       baseGroups = c("Basemap", "Satelite"),
  #       options = layersControlOptions(collapsed = TRUE),
  #       position = leaf.pos) %>%
  #     
  #     addMarkers(
  #       data = sites,
  #       lng = sites$X, 
  #       lat = sites$Y,
  #       label = sites$Station,
  #       layerId = sites$Station,
  #       icon = makeIcon(
  #         iconUrl = "input/marker.png",
  #         iconWidth = 3.8*3, iconHeight = 5.1*3
  #       ),
  #       group = 'sites',
  #       clusterOptions = markerClusterOptions(showCoverageOnHover = F)
  #     ) %>%
  #     addEasyButton(easyButton(icon = "ion-arrow-shrink", position = leaf.pos,
  #                              title = "Reset View", onClick = JS(paste0("function(btn, map){ map.setView(new L.LatLng(", initial_lat, ", ", initial_long, "), ", initial_zoom, ", { animation: true });}")))) 
    # leaflet::addMiniMap(position = "bottomleft",
    #                     zoomLevelOffset = -8,
    #                     toggleDisplay = T, 
    #                     autoToggleDisplay = T, aimingRectOptions = list(weight = 1),
    #                     tiles =  mapbox_moon)  %>%
  # })
  

# Outputs -----------------------------------------------------------------

  # Time series plot
  output$tsPlot <- renderPlotly({
    tsPlot()
  })
  
  # Lolli plot
  output$lolliPlot <- renderPlotly({
    lolliPlot()
  })
  
  # table
  output$table <- renderDataTable({
    datatable(tsTable(), options = list(
      pageLength = 100
    ))
  })
  
  # UI panel
  output$uiModal <- renderUI({
    bsModal(ns('modal'), title = div(id = ns('modalTitle'), pixelLabel()), trigger = 'click2', size = "large",
            div(id = ns("top_row"),
                sidebarLayout(
                  sidebarPanel(width = 2, id = ns('sidebar'),
                               div(class = 'control',
                                   div(class = 'label', p("From")),
                                   dateInput(ns("from"), NULL, format = "M d, yyyy",
                                             value = paste0(year(input$date_choice),"-01-01")),
                                   div(class = 'label', p("To")),
                                   dateInput(ns("to"), NULL, format = "M d, yyyy",
                                             value = paste0(year(input$date_choice),"-12-31"))
                               ),
                               # div(class = 'control',
                               #     div(class = 'label', p("Interval (minutes)")),
                               #     numericInput(ns("interval"), NULL,
                               #                  value = 10, min = 0, max = 60, step = 5)),
                               # div(class = 'control',
                               #     div(class = 'label', p("Units")),
                               #     selectInput(ns("units"), label = NULL,
                               #                 choices = c("meters", "feet"), selected = "meters")),
                               div(class = 'control',
                                   hr(),
                                   downloadButton(outputId = ns("download"),
                                                  label = "Download data (csv)", class = 'small-dl'))
                  ),
                  mainPanel(width = 9,
                            tabsetPanel(id = ns("tabs"),
                                        tabPanel(title = "Plot",
                                                 br(),
                                                 plotlyOutput(ns("tsPlot"))),
                                        # tabPanel(title = "Lolli",
                                        #          br(),
                                        #          plotlyOutput(ns("lolliPlot"))),
                                        tabPanel(title = "Table",
                                                 br(),
                                                 wellPanel(class = 'wellpanel',
                                                           DT::dataTableOutput(ns('table'))
                                                 )
                                        )))
                )))
  })

  # Downloading
  output$download <- downloadHandler(
    filename = function() {
      paste0("pixel.csv")
      # paste0(pretty_label(), "_", gsub("-", "", as.character(input$from)), "_", gsub("-", "", as.character(input$to)), ".csv")
    },
    content <- function(file) {
      readr::write_csv(downloadData(), file)
    }
  )
  
}