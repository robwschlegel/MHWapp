map <- function(input, output, session) {
  ns <- session$ns
  
  # testers...
  # xy <- c(-42.125, 39.875)
  # x <- -42.125
  # y <- 39.875
  # input <- data.frame(from_to = c(as.Date("2018-01-01"),
  #                                 as.Date("2018-12-31")),
  #                     date = as.Date("2018-02-14"))#,
  #                     # pixel = "Smooth")
  #                     #categories = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))
  
  
# Reactive UI features ----------------------------------------------------
  
  ### The popup modal when starting the app
  # Open on startup
  shinyBS::toggleModal(session, "startupModal", "open")
  # The content of the welcome window
  output$uiStartupModal <- renderUI({
    shinyBS::bsModal(ns('startupModal'), title = strong("Welcome to the Marine Heatwave Tracker!"), trigger = "click2", size = "m",
                     HTML("This web application shows up to date information on where in the world marine heatwaves (MHWs) are occurring and what category they are.
                          <hr>
                          All of the <b>Controls</b> may be found to the right of the screen and can be minimised.
                          <hr>
                          Click in the <b>Date</b> box to choose a day to display on the map.
                          <hr>
                          Click on the <b>Animate</b> switch to bring up the animation options.
                          <hr>
                          Use the <b>Map data</b> interface to download the global MHW category data. There is a 31 day size limit per download request.
                          <hr>
                          Clicking on the <b>Category</b> buttons will filter those pixels from the map.
                          <hr>
                          After clicking on a pixel of interest, click the <b>Plot pixel</b> button to see more.
                          <hr>
                          For more information please click on the <b>Summary</b> or <b>About</b> tabs."))
  })
  
  ### Switch the display of the Controls on and off
  observeEvent(input$toggle, shinyjs::toggle("control_menu", anim = TRUE))
  
  ### Reactive category filters
  categories <- reactiveValues(categories = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))
  
  ### Moderate filtering button
  # The base reactive value for clicking
  button_I <- reactiveValues(clicked = FALSE)
  # Observe button click to filter category
  observeEvent(input$moderate_filter, {
    if(!button_I$clicked){
      button_I$clicked <- TRUE
      categories$categories <- categories$categories[!grepl("I Moderate", categories$categories)]
    } else{
      button_I$clicked <- FALSE
      if(!"I Moderate" %in% categories$categories){
        categories$categories <- c(categories$categories, "I Moderate")
      }
    }
  })
  # Change button icon upon click
  output$moderate <- renderUI({
    if(button_I$clicked){
      actionButton(inputId = ns("moderate_filter"), "I Moderate", icon = icon("remove", lib = "glyphicon"),
                   style = "color: black; background-color: #ffc866; border-color: black; width: 110px")     
    } else {
      actionButton(inputId = ns("moderate_filter"), "I Moderate", icon = icon("ok", lib = "glyphicon"),
                   style = "color: black; background-color: #ffc866; border-color: black; width: 110px")
    }
  })
  
  ### Strong filtering button
  # The base reactive value for clicking
  button_II <- reactiveValues(clicked = FALSE)
  # Observe button click to filter category
  observeEvent(input$strong_filter, {
    if(!button_II$clicked){
      button_II$clicked <- TRUE
      categories$categories <- categories$categories[!grepl("II Strong", categories$categories)]
    } else{
      button_II$clicked <- FALSE
      if(!"II Strong" %in% categories$categories){
        categories$categories <- c(categories$categories, "II Strong")
      }
    }
  })
  # Change button icon upon click
  output$strong <- renderUI({
    if(button_II$clicked){
      actionButton(inputId = ns("strong_filter"), "II Strong", icon = icon("remove", lib = "glyphicon"),
                   style = "color: black; background-color: #ff6900; border-color: black; width: 110px")     
    } else {
      actionButton(inputId = ns("strong_filter"), "II Strong", icon = icon("ok", lib = "glyphicon"),
                   style = "color: black; background-color: #ff6900; border-color: black; width: 110px")
    }
  })
  
  ### Severe filtering button
  # The base reactive value for clicking
  button_III <- reactiveValues(clicked = FALSE)
  # Observe button click to filter category
  observeEvent(input$severe_filter, {
    if(!button_III$clicked){
      button_III$clicked <- TRUE
      categories$categories <- categories$categories[!grepl("III Severe", categories$categories)]
    } else{
      button_III$clicked <- FALSE
      if(!"III Severe" %in% categories$categories){
        categories$categories <- c(categories$categories, "III Severe")
      }
    }
  })
  # Change button icon upon click
  output$severe <- renderUI({
    if(button_III$clicked){
      actionButton(inputId = ns("severe_filter"), "III Severe", icon = icon("remove", lib = "glyphicon"),
                   style = "color: white; background-color: #9e0000; border-color: black; width: 110px")     
    } else {
      actionButton(inputId = ns("severe_filter"), "III Severe", icon = icon("ok", lib = "glyphicon"),
                   style = "color: white; background-color: #9e0000; border-color: black; width: 110px")
    }
  })
  
  ### Extreme filtering button
  # The base reactive value for clicking
  button_IV <- reactiveValues(clicked = FALSE)
  # Observe button click to filter category
  observeEvent(input$extreme_filter, {
    if(!button_IV$clicked){
      button_IV$clicked <- TRUE
      categories$categories <- categories$categories[!grepl("IV Extreme", categories$categories)]
    } else{
      button_IV$clicked <- FALSE
      if(!"IV Extreme" %in% categories$categories){
        categories$categories <- c(categories$categories, "IV Extreme")
      }
    }
  })
  # Change button icon upon click
  output$extreme <- renderUI({
    if(button_IV$clicked){
      actionButton(inputId = ns("extreme_filter"), "IV Extreme", icon = icon("remove", lib = "glyphicon"),
                   style = "color: white; background-color: #2d0000; border-color: black; width: 110px")     
    } else {
      actionButton(inputId = ns("extreme_filter"), "IV Extreme ", icon = icon("ok", lib = "glyphicon"),
                   style = "color: white; background-color: #2d0000; border-color: black; width: 110px")
    }
  })
  
  ### Button for opening main modal
  output$button_ts <- renderUI({
    click <- input$map_click
    if(is.null(click)){
      shinyWidgets::actionBttn(inputId = ns("does_nothing"), label = "Plot pixel", #icon = icon("map-marked"),
                               style = "pill", color = "danger", size = "md")
    } else {
      shinyWidgets::actionBttn(inputId = ns("open_modal"), label = "Plot pixel", #icon = icon("map-marked"),
                               style = "pill", color = "success", size = "md")
    }
  })
  
  ### Date range animation menu
  output$date_animator <- renderUI({
    if(input$check_animate){
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['date']])) {
        date_anim_choice <- as.Date(as.character(query[['date']]))
      } else{
        date_anim_choice <- max(current_dates)
      }
      absolutePanel(id = ns("controls"), class = "panel panel-default", draggable = T, cursor = "move",
                    top = menu_panel_top, right = menu_panel_right+10+150, width = "350px",
                    h2("Animate"),
                    fixedRow(
                      column(width = 8,
                             dateRangeInput(inputId = ns("date_choice_slider"), 
                                            label = "Date range",
                                            start = date_anim_choice-7, 
                                            end = date_anim_choice, 
                                            min = "1982-01-01", 
                                            max = max(current_dates)))
                      # NB: Disabled the seconds choice as it was confusing
                      # column(width = 4,
                      #        numericInput(inputId = ns("slider_time_step"),
                      #                     label = "Seconds", value = 3, min = 1, max = 30))
                    ),
                    uiOutput(ns('date_animator_slider'))
      )
    }
  })
  
  ### Animation date slider
  # NB: This is rendered separately due to date generation order issues
  output$date_animator_slider <- renderUI({
    if(input$check_animate){
      shinyWidgets::sliderTextInput(
        inputId = ns("date_slider"),
        label = NULL,
        grid = TRUE,
        force_edges = TRUE,
        choices = seq(input$date_choice_slider[1],
                      input$date_choice_slider[2], by = "day"),
        selected = input$date_choice_slider[1],
        animate = animationOptions(interval = 3*1000)
      )
    }
  })
  
  ### Reactive start date to catch HTML queries
  output$date_reactive <- renderUI({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['date']])) {
      date_menu_choice <- as.Date(as.character(query[['date']]))
      } else{
        date_menu_choice <- max(current_dates)
      }
    dateInput(inputId = ns("date"),
              label = NULL,
              value = date_menu_choice,
              min = "1982-01-01",
              max = max(current_dates))
  })
  
  ### Observe the changing of dates in the animation slider
  observeEvent(input$date_slider, {
    req(input$date_slider)
    date <- as.Date(input$date_slider)
    updateDateInput(session = session, 
                    inputId = "date",
                    value = date)
  })
  
  ### Download map data interface
  output$download_map_UI <- renderUI({
    shinyWidgets::dropdownButton(
      h3("Download"),
      dateRangeInput(
        inputId = ns("map_download_date"),
        label = h5("Date range"),
        start = max(current_dates), end = max(current_dates),
        min = "1982-01-01", max = max(current_dates)),
      fluidRow(
        column(6,
               # h5("Format"),
               shinyWidgets::prettyRadioButtons(inputId = ns("map_download_type"),
                                                label = h5("Format"),
                                                inline = TRUE,
                                                choiceNames = list(".csv", ".Rds"),
                                                choiceValues = list("csv", "Rds"),
                                                selected = "csv",
                                                status = 'primary', shape = "curve", animation = "tada")
               ),
        column(6,
               h5("Data"),
               downloadButton(outputId = ns("download_map"),
                              label = NULL,
                              class = 'small-dl')
               )
        ),
      circle = FALSE, status = "primary",
      icon = icon("download"), width = "300px", right = TRUE, up = FALSE,
      label = "Map data", tooltip = FALSE)
  })
  
  ### Disable map download button if dates are inccorect
  observe({
    shinyjs::toggleState(id = "download_map", 
                         condition = input$map_download_date[1] <= input$map_download_date[2] & (as.integer(input$map_download_date[2]) - as.integer(input$map_download_date[1])) <= 31) 

  })

  
# Map projection data -----------------------------------------------------
  
  ### Base map data before screening categories
  baseDataPre <- reactive({
    req(lubridate::is.Date(input$date))
    date_filter <- input$date
    year_filter <- lubridate::year(date_filter)
    sub_dir <- paste0("cat_clim/",year_filter)
    sub_file <- paste0(sub_dir,"/cat.clim.",date_filter,".Rda")
    if(file.exists(sub_file)){
      baseDataPre <- readRDS(sub_file)
    } else {
      baseDataPre <- empty_date_map
    }
    return(baseDataPre)
  })
  
  ### Base map data after screening categories
  baseData <- reactive({
    baseDataPre <- baseDataPre()
    if(nrow(baseDataPre) > 1){
      baseData <- baseDataPre %>%
        dplyr::filter(category %in% categories$categories)
    } else{
      baseData <- empty_date_map
    }
    # Fix for the issue caused by de-slecting all of the cateogries
    if(length(baseData$category) == 0){
      baseData <- empty_date_map
    }
    return(baseData)
  })
  
  ### Non-shiny-projected raster data
  rasterNonProj <- reactive({
    baseData <- baseData()
    MHW_raster <- baseData %>%
      dplyr::select(lon, lat, category)
    colnames(MHW_raster) <- c("X", "Y", "Z")
    MHW_raster$Z <- as.numeric(MHW_raster$Z)
    rasterNonProj <- raster::rasterFromXYZ(MHW_raster, res = c(0.25, 0.25),
                                           digits = 3, crs = inputProj)
    return(rasterNonProj)
  })
  
  ### Shiny-projected raster data
  rasterProj <- reactive({
    rasterNonProj <- rasterNonProj()
    # NB: Smoothing the pixels is easy, but causes massive artifacts
    # if(input$pixels == "Smooth"){
    # rasterProj <- projectRasterForLeaflet(rasterNonProj, method = "bilinear")
    # } else {
    rasterProj <- projectRasterForLeaflet(rasterNonProj, method = "ngb")
    # }
    return(rasterProj)
  })
  
  
# Time series data --------------------------------------------------------
  
  ### Pixel data
  pixelData <- reactive({
    xy <- input$map_click
    if(!is.null(xy)){
      while(xy$lng > 180){
        xy$lng <- xy$lng - 360
      }
    }
    if(!is.null(xy)){
      while(xy$lng < -180){
        xy$lng <- xy$lng + 360
      }
    }
    if(!is.null(xy)){
      rasterNonProj <- rasterNonProj()
      cell <- raster::cellFromXY(rasterNonProj, c(xy$lng, xy$lat))
      xy <- raster::xyFromCell(rasterNonProj, cell)
      # Grab time series data
      ts_data <- sst_seas_thresh_ts(lon_step = xy[1], lat_step = xy[2]) %>% 
        dplyr::mutate(diff = thresh - seas,
                      thresh_2x = thresh + diff,
                      thresh_3x = thresh_2x + diff,
                      thresh_4x = thresh_3x + diff)
      # Grab event data
      event_file <- dir("event", full.names = T)[which(lon_OISST == xy[1])]
      event_data <- readRDS(event_file) %>% 
        dplyr::filter(lat == xy[2]) %>%
        mutate(date_start = as.Date(date_start, origin = "1970-01-01"),
               date_peak = as.Date(date_peak, origin = "1970-01-01"),
               date_end = as.Date(date_end, origin = "1970-01-01"))
      pixelData <- list(ts = ts_data,
                        event = event_data,
                        lon = xy[1],
                        lat = xy[2])
      return(pixelData)
    } else {
    }
  })
  
  
# Pop-ups -----------------------------------------------------------------
  
  # testers...
  # xy <- c(-48.375, 46.875)
  # xy <- c(-42.125, 39.875)
  # xy <- c(-402.125, 39.875)
  # x <- -402.125
  # x <- -42.125
  # y <- 39.875
  
  ### Observer to show pop-ups on click
  observeEvent(c(input$map_click, input$open_modal), {
    click <- input$map_click
    if(!is.null(click)){
      showpos(x = click$lng, y = click$lat)
    }
  })
  
  ### Show popup on clicks
  showpos <- function(x = NULL, y = NULL) {
    baseData <- baseData()
    rasterNonProj <- rasterNonProj()
    rasterProj <- rasterProj()
    xy_click <- c(x,y)
    xy_wrap <- lon_wrap(xy_click)
    # Translate lon/lat to cell number using the unprojected raster
      # This is done because the projected raster is not in degrees
    cell <- raster::cellFromXY(rasterNonProj, c(xy_wrap))
    # If the click is inside the raster...
    if(!is.na(cell)) {
      xy <- raster::xyFromCell(rasterNonProj, cell) # Get the center of the cell
      if(xy[1] >= 0) xy_lon <- paste0(abs(xy[1]),"°E")
      if(xy[1] < 0) xy_lon <- paste0(abs(xy[1]),"°W")
      if(xy[2] >= 0) xy_lat <- paste0(abs(xy[2]),"°N")
      if(xy[2] < 0) xy_lat <- paste0(abs(xy[2]),"°S")
      val <- baseData %>% 
        dplyr::filter(lon == xy[1],
                      lat == xy[2]) %>% 
        dplyr::select(category) %>% 
        as.numeric()
      if(xy[1] >= -160 & xy[1] <= -110 & xy[2] >= 25 & xy[2] <= 60){
        regional_link <- paste0("<hr>",
                                "<a target='_blank' rel='noopener noreferrer' href=",
                                regional_NOAA,">Regional website (NOAA)</a>")
      } else if(xy[1] >= -7 & xy[1] <= 37 & xy[2] >= 27 & xy[2] <= 47){
        regional_link <- paste0("<hr>",
                                "<a target='_blank' rel='noopener noreferrer' href=",
                                regional_TMEDNET,">Regional website (T-MEDNet)</a>")
      } else{
        regional_link <- ""
      }
      content <- paste0("Lon = ", xy_lon,
                        "<br>Lat = ", xy_lat,
                        "<br>Category = ", names(MHW_colours)[val],
                        regional_link,
                        "<hr>",
                        "<i>Please click the<br><b>Time series</b><br>button in the<br><b>Controls</b> panel<br>for more info</i>")
      
    } else{
      content <- ""
    }
    
    ### Add Popup to leaflet
    leafletProxy("map") %>%
      clearPopups() %>%
      addPopups(lng = xy_click[1], lat = xy_click[2],
                popup = paste(content))
  }
  
  
# Leaflet -----------------------------------------------------------------
  
  ### The leaflet base
  output$map <- renderLeaflet({
    # Check HTML string for startup coords
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[['lat']])){
      map_lat <- query[['lat']]
    } else {
      map_lat <- initial_lat
    }
    if(!is.null(query[['lon']])){
      map_lon <-  query[['lon']]
    } else {
      map_lon <- initial_lon
    }
    if(!is.null(query[['zoom']])){
      map_zoom <-  query[['zoom']]
    } else {
      map_zoom <- initial_zoom
    }
    # The base 
    leaflet(data = MHW_cat_clim_sub, options = leafletOptions(zoomControl = FALSE)) %>%
      setView(lng = map_lon, lat = map_lat, zoom = map_zoom,
              options = tileOptions(minZoom = 0, maxZoom = 8, noWrap = F)) %>%
      addScaleBar(position = "bottomright")
  })

  ### The raster layer
  observeEvent(c(input$date,
                 input$moderate_filter, input$strong_filter,
                 input$severe_filter, input$extreme_filter), {
                   leafletProxy("map") %>%
                     addRasterImage(rasterProj(), colors = pal_cat, layerId = ns("map_raster"),
                                    project = FALSE, opacity = 0.8)
  })
  
  ### Change map background
  observeEvent(input$map_back, {
    if(input$map_back == "Grey"){
      leafletProxy("map") %>%
        clearTiles() %>% 
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite,
                         options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F)) %>%
        addRasterImage(rasterProj(), colors = pal_cat, layerId = ns("map_raster"),
                       project = FALSE, opacity = 0.8)
    } else if(input$map_back == "Land features"){
      leafletProxy("map") %>%
        clearTiles() %>% 
        addProviderTiles(providers$Esri.WorldTopoMap,
                         options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F)) %>%
        addRasterImage(rasterProj(), colors = pal_cat, layerId = ns("map_raster"),
                       project = FALSE, opacity = 0.8)
    } else if(input$map_back == "Ocean features"){
      leafletProxy("map") %>%
        clearTiles() %>% 
        addProviderTiles(providers$Esri.OceanBasemap,
                         options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F)) %>%
        addRasterImage(rasterProj(), colors = pal_cat, layerId = ns("map_raster"),
                       project = FALSE, opacity = 0.8)
    } else{
      leafletProxy("map") %>%
        clearTiles() %>% 
        addTiles(options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F)) %>%
        addRasterImage(rasterProj(), colors = pal_cat, layerId = ns("map_raster"),
                       project = FALSE, opacity = 0.8)
    }
  })
  
  
# Figures/tables ----------------------------------------------------------
  
  ### Create static time series plot
  tsPlot <- reactive({
    req(input$from_to[1])
    req(input$from_to[2])
    
    # Get time series
    ts_data <- pixelData()$ts
    
    # Check that it's not empty
    if(length(ts_data$temp) == 1){
      p <- ggplot() + geom_text(aes(x = 0, y = 0,label = "Please select an ocean pixel.")) +
        labs(x = "", y = "Temperature (°C)")
      return(p)
    }
    
    # Check that date selection isn't wonky
    if(!input$from_to[2] %in% current_dates | !input$from_to[1] %in% current_dates | input$from_to[1] >= input$from_to[2] | is.null(input$from_to[1]) | is.null(input$from_to[2])){
      p <- ggplot() + geom_text(aes(x = 0, y = 0,label = "Please select a valid date range below.")) +
        labs(x = "", y = "Temperature (°C)")
      return(p)
    }
    
    # Filter time series based on dates
    ts_data_sub <- ts_data %>%
      dplyr::filter(t >= input$from_to[1], t <= input$from_to[2])
    
    # Event data prep
    event_data <- pixelData()$event
    event_data_sub <- event_data %>%
      dplyr::filter(date_start >= input$from_to[1], date_end <= input$from_to[2])
    
    # The base
    suppressWarnings( # text aes for plotly
      p <- ggplot(data = ts_data_sub, aes(x = t, y = temp)) +
        geom_segment(aes(x = input$date[1], 
                         xend = input$date[1],
                         y = min(ts_data_sub$temp), 
                         yend = max(ts_data_sub$temp),
                         text = "Date shown"), colour = "limegreen") +
        labs(x = "", y = "Temperature (°C)") +
        scale_x_date(expand = c(0, 0), date_labels = "%b %Y", limits = c(input$from_to[1], input$from_to[2])) 
      )
    # Add flame categories as needed
    if(any(ts_data_sub$temp > ts_data_sub$thresh)){
      p <- p + heatwaveR::geom_flame(aes(y2 = thresh), fill = "#ffc866", n = 5, n_gap = 2)
      }
    if(any(ts_data_sub$temp > ts_data_sub$thresh_2x)){
      p <- p + heatwaveR::geom_flame(aes(y2 = thresh_2x), fill = "#ff6900")
      }
    if(any(ts_data_sub$temp > ts_data_sub$thresh_3x)){
      p <- p + heatwaveR::geom_flame(aes(y2 = thresh_3x), fill = "#9e0000")
      }
    if(any(ts_data_sub$temp > ts_data_sub$thresh_4x)){
      p <- p + heatwaveR::geom_flame(aes(y2 = thresh_4x), fill = "#2d0000")
    }
    # Add SST, seas, clim lines
    suppressWarnings( # text aes pltoly
      p <- p + geom_line(colour = "grey20",
                         aes(group = 1, text = paste0("Date: ",t,
                                                      "<br>Temperature: ",temp,"°C"))) +
        geom_line(linetype = "dashed", colour = "steelblue3",
                  aes(x = t, y = seas, group = 1,
                      text = paste0("Date: ",t,
                                    "<br>Climatology: ",seas,"°C"))) +
        geom_line(linetype = "dotted", colour = "tomato3",
                  aes(x = t, y = thresh, group = 1,
                      text = paste0("Date: ",t,
                                    "<br>Threshold: ",thresh,"°C")))
    )
    # Add rug as needed
    if(length(event_data_sub$date_start) > 0){
      suppressWarnings( # text aes pltoly
      p <- p + geom_rug(data = event_data_sub, sides = "b", colour = "salmon", size = 2,
                        aes(x = date_peak, y = min(ts_data_sub$temp),
                            text = paste0("Event: ",event_no,
                                          "<br>Duration: ",duration," days",
                                          "<br>Start Date: ", date_start,
                                          "<br>Peak Date: ", date_peak,
                                          "<br>End Date: ", date_end,
                                          "<br>Mean Intensity: ",intensity_mean,"°C",
                                          "<br>Max. Intensity: ",intensity_max,"°C",
                                          "<br>Cum. Intensity: ",intensity_cumulative,"°C")))
      )
    }
    p
  })
    
  ### Create interactive time series plot
  tsPlotly <- reactive({
    # Grab static plot
    p <- tsPlot()
    # Convert to plotly
    # NB: Setting dynamicTicks = T causes the flames to be rendered incorrectly
    pp <- ggplotly(p, tooltip = "text", dynamicTicks = F) %>% 
      layout(hovermode = 'compare') 
    pp
  })
  
  ### Create lolliplot
  lolliPlot <- reactive({
    
    # Prep data
    event_data <- pixelData()$event
    event_data_sub <- event_data %>%
      dplyr::filter(date_start >= input$from_to[1], date_start <= input$from_to[2])
    
    # Base figure
    if(length(event_data_sub$date_start) > 0){
      suppressWarnings(
        p <- ggplot(data = event_data_sub, aes(x = date_peak, y = intensity_max)) +
          geom_segment(aes(xend = date_peak, yend = 0)) +
          geom_point(fill = "salmon", shape = 21, size = 4,
                     aes(text = paste0("Event: ",event_no,
                                       "<br>Duration: ",duration," days",
                                       "<br>Start Date: ", date_start,
                                       "<br>Peak Date: ", date_peak,
                                       "<br>End Date: ", date_end,
                                       "<br>Mean Intensity: ",intensity_mean,"°C",
                                       "<br>Max. Intensity: ",intensity_max,"°C",
                                       "<br>Cum. Intensity: ",intensity_cumulative,"°C"))) +
          scale_x_date(expand = c(0, 0), date_labels = "%b %Y", limits = c(input$from_to[1], input$from_to[2])) +
          scale_y_continuous(expand = c(0,0), limits = c(0, max(event_data_sub$intensity_max)*1.1)) +
          labs(x = "", y = "Max. Intensity (°C)")
      )
    } else{
      suppressWarnings(
        p <- ggplot() + geom_text(aes(x = input$from_to[1] + ((input$from_to[2] - input$from_to[1])/2),
                                      y = 0.5, label = "?", text = "No marine heatwave peaks in date range.")) +
          scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
          scale_x_date(expand = c(0, 0), date_labels = "%b %Y", limits = c(input$from_to[1], input$from_to[2])) +
          labs(x = "", y = "Max. Intensity (°C)")
      )
    }
    pp <- ggplotly(p, tooltip = "text", dynamicTicks = F)
    pp
  })
  
  ### Create data table
  tsTable <- reactive({
    event_data <- pixelData()$event %>% 
      dplyr::filter(date_start >= input$from_to[1], date_start <= input$from_to[2]) %>% 
      dplyr::rename(Lon = lon,
                    Lat = lat,
                    Event = event_no,
                    Duration = duration,
                    'Start Date' = date_start,
                    'Peak Date' = date_peak,
                    'End Date' = date_end,
                    'Mean Intensity' = intensity_mean,
                    'Max. Intensity' = intensity_max,
                    'Cum. Intensity' = intensity_cumulative)
  })
  
  
# Modal panel -------------------------------------------------------------
  
  ### Label/title for modal panel
  pixelLabel <- reactive({
    xy <- input$map_click
    if(!is.null(xy)){
      while(xy$lng > 180){
        xy$lng <- xy$lng - 360
      }
    }
    if(!is.null(xy)){
      while(xy$lng < -180){
        xy$lng <- xy$lng + 360
      }
    }
    if(!is.null(xy)){
      rasterNonProj <- rasterNonProj()
      cell <- raster::cellFromXY(rasterNonProj, c(xy$lng, xy$lat))
      xy <- raster::xyFromCell(rasterNonProj, cell)
      if(xy[1] >= 0) xy_lon <- paste0(abs(xy[1]),"°E")
      if(xy[1] < 0) xy_lon <- paste0(abs(xy[1]),"°W")
      if(xy[2] >= 0) xy_lat <- paste0(abs(xy[2]),"°N")
      if(xy[2] < 0) xy_lat <- paste0(abs(xy[2]),"°S")
      paste0("Pixel: lon = ",xy_lon,", lat = ",xy_lat)
    }
  })
  
  ### Open the modal panel
  observeEvent(input$open_modal, {
    click <- input$map_click
    if(!is.null(click)){
      shinyBS::toggleModal(session, "modal", "open")
    } else {
      showModal(modalDialog(
        title = "Pixel: Lon = NA, Lat = NA",
        "Please first click on a pixel in order to view more information about it."
      ))
    }
  })
  
  ### Open modal panel through direct leaflet clicking
  ### NB: This allows crashing if the user clicks twice quickly
  # observeEvent(input$map_click, {
  #   toggleModal(session, "modal", "open")
  # })
  
  ### Static time series plot
  output$tsPlot <- renderPlot({
    tsPlot()
  })
  
  ### Interactive time series plot
  output$tsPlotly <- renderPlotly({
    tsPlotly()
  })
  
  ### Lolli plot
  output$lolliPlot <- renderPlotly({
    lolliPlot()
  })
  
  ### Event metrics table
  output$table <- renderDataTable({
    DT::datatable(tsTable(),
                  options = list(pageLength = 10))
  })
  
  ### UI panel
  output$uiModal <- renderUI({
    req(lubridate::is.Date(input$date))
    # To date
    to_date <- ifelse(lubridate::year(input$date) >= lubridate::year(max(current_dates)),
                      input$date, as.Date(paste0(lubridate::year(as.Date(input$date)),"-12-31")))
    to_date <- as.Date(to_date, origin = "1970-01-01")
    # From date
    from_date <- ifelse(lubridate::year(input$date) >= lubridate::year(max(current_dates)),
                        input$date-365, as.Date(paste0(lubridate::year(as.Date(input$date)),"-01-01")))
    from_date <- as.Date(from_date, origin = "1970-01-01")
    # The main modal
    shinyBS::bsModal(ns('modal'), title = div(id = ns('modalTitle'), pixelLabel()), trigger = 'click2', size = "large",
                     fluidPage(
                       tabsetPanel(id = ns("tabs"),
                                   tabPanel(title = "Static",
                                            br(),
                                            shinycssloaders::withSpinner(plotOutput(ns("tsPlot")), type = 6, color = "#b0b7be"),
                                            hr()),
                                   tabPanel(title = "Interactive",
                                            br(),
                                            shinycssloaders::withSpinner(plotlyOutput(ns("tsPlotly")), type = 6, color = "#b0b7be"),
                                            hr()),
                                   tabPanel(title = "Lolliplot",
                                            br(),
                                            shinycssloaders::withSpinner(plotlyOutput(ns("lolliPlot")), type = 6, color = "#b0b7be"),
                                            hr()),
                                   tabPanel(title = "Table",
                                            br(),
                                            shinycssloaders::withSpinner(DT::dataTableOutput(ns('table')), type = 6, color = "#b0b7be"),
                                            hr())
                                   ),
                                   fluidRow(
                                     column(width = 4,
                                            dateRangeInput(
                                              inputId = ns("from_to"),
                                              label = h3("Date range"),
                                              start = from_date, end = to_date,
                                              min = "1982-01-01", max = max(current_dates))),
                                     column(width = 8,
                                            h3("Downloads"),
                                            downloadButton(outputId = ns("download_clim"),
                                                           label = "Climatology & Threshold (csv)", class = 'small-dl'),
                                            downloadButton(outputId = ns("download_event"),
                                                           label = "MHW data (csv)", class = 'small-dl'))
                                     )
                       )
    )
  })
  
  
# Downloads ---------------------------------------------------------------
  
  ### Prep event data
  downloadEventData <- reactive({
    data <- pixelData()$event
    lon <- pixelData()$lon[1]
    lat <- pixelData()$lat[1]
    data_sub <- data %>% 
      mutate(lon = lon,
             lat = lat) %>% 
      dplyr::select(lon, lat, event_no, date_start, date_peak, date_end, everything())
    return(data_sub)
  })
  
  ### Prep clim/thresh data
  downloadClimData <- reactive({
    data <- pixelData()$ts
    lon <- pixelData()$lon[1]
    lat <- pixelData()$lat[1]
    data_sub <- data %>% 
      mutate(lon = lon,
             lat = lat) %>% 
      dplyr::select(lon, lat, doy, seas, thresh) %>% 
      dplyr::distinct() %>% 
      arrange(doy)
    return(data_sub)
  })
  
  ### Download event data
  output$download_event <- downloadHandler(
    filename = function() {
      paste0("event_lon_",downloadEventData()$lon[1],"_lat_",downloadEventData()$lat[1],".csv")
    },
    content <- function(file) {
      readr::write_csv(downloadEventData(), file)
    }
  )
  
  ### Download clim/thresh data
  output$download_clim <- downloadHandler(
    filename = function() {
      paste0("clim_lon_",downloadClimData()$lon[1],"_lat_",downloadClimData()$lat[1],".csv")
    },
    content <- function(file) {
      readr::write_csv(downloadClimData(), file)
    }
  )
  
  ### Prep map data
  # Testing...
  # date_filter_from <- as.Date("2019-06-01")
  # date_filter_to <- as.Date("2019-06-07")
  downloadMapData <- reactive({
    req(lubridate::is.Date(input$map_download_date[1]))
    req(lubridate::is.Date(input$map_download_date[2]))
    date_filter_from <- as.Date(input$map_download_date[1])
    date_filter_to <- as.Date(input$map_download_date[2])
    date_seq <- seq(date_filter_from, date_filter_to, by = "day")
    date_seq_years <- sapply(strsplit(as.character(date_seq), "-"), "[[", 1)
    date_seq_files <- paste0("cat_clim/",date_seq_years,"/cat.clim.",date_seq,".Rda")
    map_data <- plyr::ldply(date_seq_files, readRDS_date)
    return(map_data)
  })
  
  ### Download map data
  output$download_map <- downloadHandler(
    filename = function() {
      if(input$map_download_type == "Rds"){
        paste0("MHW_map_data.Rds")
      } else if(input$map_download_type == "csv"){
        paste0("MHW_map_data.csv")
      }
    },
    content <- function(file) {
      if(input$map_download_type == "Rds"){
        saveRDS(downloadMapData(), file = file)
      } else if(input$map_download_type == "csv"){
        readr::write_csv(downloadMapData(), file)
      }
    }
  )
  
}

# cat("\nmap_server.R finished")

