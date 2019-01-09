map <- function(input, output, session) {
  ns <- session$ns


# Functions ---------------------------------------------------------------
  

# Reactives ---------------------------------------------------------------

  ### base map data
  baseData <- reactive({
    date_filter <- as.integer(input$date_choice)
    # date_filter <- as.integer(as.Date("2017-06-17"))
    # print(date_filter)
    MHW_db <- DBI::dbConnect(RSQLite::SQLite(), "MHW_db.sqlite")
    # dbplyr::src_dbi(MHW_db)
    baseData <- tbl(MHW_db, "MHW_cat_clim_sub") %>%
      filter(t == date_filter) %>%
      collect() %>%
      mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                    "III Severe", "IV Extreme"))) %>%
      na.omit()
    DBI::dbDisconnect(MHW_db)
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
        xy <- xyFromCell(rasterNonProj, cell)
        MHW_db <- DBI::dbConnect(RSQLite::SQLite(), "MHW_db.sqlite")
        # dbplyr::src_dbi(MHW_db)
        pixelData <- tbl(MHW_db, "MHW_cat_clim_sub") %>%
          filter(lon == xy[1], lat == xy[2]) %>%
          collect() %>%
          na.omit() %>% 
          mutate(t = as.Date(t, origin = "1970-01-01"))
        DBI::dbDisconnect(MHW_db)
        return(pixelData)
      }
  })
  
  ### reactive labels
  pixelLabel <- reactive({
    paste0("Chosen pixel; lon = ",pixelData()$lon[1],", lat = ",pixelData()$lat[1])
  })
  
  ### filter data
  # filter_data <- reactive({
  #   sites[which(sites$Station == station$location), ]
  # })
  
  ### reactive labels
  # station_label <- reactive({
  #   filter_data()$Station
  # })
  
  # pretty_label <- reactive({
  #   strsplit(station_label(), ",")[[1]][1] %>%
  #     sub(" ", "", .)
  # })
  
  # unit_label <- reactive({
  #   if(input$units == "meters") {
  #     return("Tide Height (m)")
  #   }
  #   "Tide Height (ft)"
  # })
  
  # tide_data <- reactive({
  #   req(station$location)
  #   data <- filter_data()
  #   station <- station_label()
  #   tz <- data$TZ
  #   
  #   data <- rtide::tide_height(
  #     station, from = input$from, to = input$to,
  #     minutes = input$interval, tz = tz)
  #   
  #   data$TideHeight %<>% round(2)
  #   data$TimeZone <- tz
  #   
  #   if(input$units == "meters"){
  #     return(data)
  #   } 
  #   data$TideHeight <- round(data$TideHeight * 3.3333, 2)
  #   data
  # })
  
  ### Download data
  downloadData <- reactive({
    data <- pixelData()
    data_sub <- data %>% 
      filter(t >= input$from, t <= input$to)
  })
  
  ### Create time series plot
  tsPlot <- reactive({
    data <- pixelData()
    data_sub <- data %>%
      filter(t >= input$from, t <= input$to)
    p <- ggplot(data = data_sub, aes(x = t, y = intensity)) +
      geom_line() +
      geom_point(aes(colour = category)) +
      labs(x = "", y = "Intensity (°C above thresh.)")
    ggplotly(p)
  })
  
  ### Create data table
  tsTable <- reactive({
    data <- pixelData()
    data_sub <- data %>% 
      filter(t >= input$from, t <= input$to)
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
    req(!is.null(pixelData()))
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

  # plot
  output$plot <- renderPlotly({
    tsPlot()
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
                  sidebarPanel(width = 1, id = ns('sidebar'),
                               div(class = 'control',
                                   div(class = 'label', p("From")),
                                   dateInput(ns("from"), NULL, format = "M d, yyyy",
                                             value = "2017-01-01"),
                                   div(class = 'label', p("To")),
                                   dateInput(ns("to"), NULL, format = "M d, yyyy",
                                             value = "2017-12-31")
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
                                                 plotlyOutput(ns("plot"))),
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