map <- function(input, output, session) {
  ns <- session$ns
  
  # testers...
  # xy <- c(-42.125, 39.875)
  # x <- -42.125
  # y <- 39.875
  # input <- data.frame(date = as.Date("2019-07-19"),
  #                     layer = "Trend")#,
  #                     # pixel = "Smooth")
  #                     #categories = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))
  
  
# Reactive UI features ----------------------------------------------------
  
  ### The popup modal when starting the app
  # Open on startup
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(length(query) < 1){
      shinyBS::toggleModal(session, "startupModal", "open")
    }
  })
  # The content of the welcome window
  output$uiStartupModal <- renderUI({
    shinyBS::bsModal(ns('startupModal'), title = strong("Welcome to the Marine Heatwave Tracker!"), trigger = "click2", size = "m",
                     HTML("This web application shows up to date information on where in the world marine heatwaves (MHWs) are occurring and what category they are.
                          <hr>
                          All of the <b>Controls</b> may be found on the left of the screen and can be minimised.
                          <hr>
                          Click in the <b>Date</b> box to choose a day to display on the map.
                          <hr>
                          Click on the <b>Animate</b> switch to bring up the animation options.
                          <hr>
                          Click on the <b>Map layer</b> button to choose between the different data options.
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
  observeEvent(input$toggle, {
    shinyjs::toggle("control_menu", anim = TRUE)
    })
  
  ### Toggle animation switch on and off
  output$check_animate_UI <- renderUI({
    req(input$layer)
    if(input$layer %in% c("Category", "Anomaly")){
      materialSwitch(inputId = ns("check_animate"), 
                     label = "Animate", status = "primary")
    }
  })
  
  ### Change map layer interface
  output$layer_UI <- renderUI({
    dropdownButton(
      h3("Select map layer"),
      prettyRadioButtons(inputId = ns("layer"), label = NULL,
                         choices = c(cat_layers, rb_layers), 
                         selected = "Category", 
                         status = "primary", 
                         shape = "curve", 
                         inline = T),
      circle = FALSE, status = "primary",
      icon = icon("map"), width = "300px",
      right = FALSE, up = FALSE,
      label = "Map layer", tooltip = FALSE)
  })
  
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
    } else {
      button_I$clicked <- FALSE
      if(!"I Moderate" %in% categories$categories){
        categories$categories <- c(categories$categories, "I Moderate")
      }
    }
  })
  # Change button icon upon click
  output$moderate <- renderUI({
    req(input$layer)
    if(input$layer %in% rb_layers){
      # No button when anomaly layer is chosen
    } else if(button_I$clicked){
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
    req(input$layer)
    if(input$layer %in% rb_layers){
      # No button when anomaly layer is chosen
    } else if(button_II$clicked){
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
    req(input$layer)
    if(input$layer %in% rb_layers){
      # No button when anomaly layer is chosen
    } else if(button_III$clicked){
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
    req(input$layer)
    if(input$layer %in% rb_layers){
      # No button when anomaly layer is chosen
    } else if(button_IV$clicked){
      actionButton(inputId = ns("extreme_filter"), "IV Extreme", icon = icon("remove", lib = "glyphicon"),
                   style = "color: white; background-color: #2d0000; border-color: black; width: 110px")     
    } else {
      actionButton(inputId = ns("extreme_filter"), "IV Extreme ", icon = icon("ok", lib = "glyphicon"),
                   style = "color: white; background-color: #2d0000; border-color: black; width: 110px")
    }
  })
  
  ### Button for opening main modal
  output$button_ts <- renderUI({
    req(input$layer)
    # req(input$map_click)
    if(input$layer %in% c("Category", "Anomaly")){
      if(is.null(input$map_click)){
        actionBttn(inputId = ns("does_nothing"), label = "Plot pixel", #icon = icon("map-marked"),
                   style = "pill", color = "danger", size = "md")
      } else {
        actionBttn(inputId = ns("open_modal"), label = "Plot pixel", #icon = icon("map-marked"),
                   style = "pill", color = "success", size = "md")
      }
    } else {
    }
  })
  
  ### Date range animation menu
  output$date_animator <- renderUI({
    req(input$check_animate)
    if(input$check_animate){
      query <- parseQueryString(session$clientData$url_search)
      if(!is.null(query[['date']])){
        date_anim_choice <- as.Date(as.character(query[['date']]))
      } else {
        date_anim_choice <- max(current_dates)
      }
      absolutePanel(id = ns("controls"), class = "panel panel-default", draggable = T, cursor = "move",
                    top = menu_panel_top, left = menu_panel_left+10+150, width = "350px",
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
      sliderTextInput(
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
    req(input$layer)
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['date']])) {
      date_menu_choice <- as.Date(as.character(query[['date']]))
      } else{
        date_menu_choice <- max(current_dates)
      }
      if(input$layer %in% c("Category", "Anomaly")){
        dateInput(inputId = ns("date"),
                  label = NULL, width = '100%',
                  value = date_menu_choice,
                  min = "1982-01-01",
                  max = max(current_dates))
      } else if(input$layer == "Summary"){
        selectInput(inputId = ns("date"), label = NULL,
                    choices = seq(1982, lubridate::year(Sys.time())), 
                    selected = lubridate::year(Sys.time()), multiple = F)
      } else{
        selectInput(inputId = ns("date"), 
                    label = NULL, choices = "NA", 
                    selected = "NA", multiple = F)
      }

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
    req(input$layer)
    
    # Reactive bits based on the map layer chosen
    if(input$layer %in% c("Category", "Anomaly")){
      date_input <- dateRangeInput(
        inputId = ns("map_download_date"),
        label = h5("Date range"),
        start = max(current_dates), end = max(current_dates),
        min = "1982-01-01", max = max(current_dates))
      sum_type <- NULL
      hem_choice <- NULL
    } else if(input$layer == "Summary"){
      date_input <- sliderInput(inputId = ns("map_download_date"), sep = "",
                                label = h5("Date range"), value = c(2020, 2020),
                                min = 1982, max = lubridate::year(Sys.time()))
      sum_type <- prettyRadioButtons(inputId = ns("summary_type"),
                                     label = h5("Summary"),
                                     choices = list("Max", "Count"),
                                     selected = "Max", inline = TRUE,
                                     status = 'primary', shape = "curve", animation = "tada")
      hem_choice <- prettyRadioButtons(inputId = ns("hemisphere_choice"),
                                       label = h5("Year"),
                                       choices = list("Boreal", "Austral"),
                                       selected = "Boreal", inline = TRUE,
                                       status = 'primary', shape = "curve", animation = "tada")
    } else{
      date_input <- NULL
      sum_type <- NULL
      hem_choice <- NULL
    }
    
    dropdownButton(
      h3("Download"),
      date_input,
      fluidRow(
        column(width = 6, sum_type),
        column(width = 6, hem_choice)),
      fluidRow(
        column(width = 6,
               prettyRadioButtons(inputId = ns("map_download_type"),
                                  label = h5("Format"),
                                  choiceNames = list(".csv", ".Rds"),
                                  choiceValues = list("csv", "Rds"),
                                  selected = "csv", inline = TRUE,
                                  status = 'primary', shape = "curve", animation = "tada")),
        column(width = 6,
               h5("Data"), # Putting the label in the button looks bad
               downloadButton(outputId = ns("download_map"),
                              label = NULL, class = 'small-dl'))
        ),
      circle = FALSE, status = "primary",
      icon = icon("download"), width = "300px",
      right = FALSE, up = FALSE, 
      label = "Map data", tooltip = FALSE)
  })
  
  ### Disable map download button if dates are inccorect
  observe({
    shinyjs::toggleState(id = "download_map", 
                         condition = input$map_download_date[1] <= input$map_download_date[2] & (as.integer(input$map_download_date[2]) - as.integer(input$map_download_date[1])) <= 31) 

  })

  ### Observe the changing of date, lon, lat, zoom, in the URL
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['date']])) {
      updateDateInput(session = session, 
                      inputId = "date", 
                      value = as.Date(as.character(query[['date']])))
    }
    if (!is.null(query[['lat']])) {
      updateNumericInput(session, "lat", value = query[['lat']])
    }
    if (!is.null(query[['lon']])) {
      updateNumericInput(session, "lon", value = query[['lon']])
    }
    if (!is.null(query[['zoom']])) {
      updateNumericInput(session, "zoom", value = query[['zoom']])
    }
  })
  
  ### Update zoom level when user zooms
  observe({
      if(!is.null(input$map_zoom)) updateNumericInput(session, "zoom", value = input$map_zoom)
  })
  
  
# Map projection data -----------------------------------------------------
  
  ### Base map data before screening categories
  baseDataPre <- reactive({
    req(input$date)
    if(input$layer == "Category"){
      req(lubridate::is.Date(input$date))
      sub_dir <- paste0("cat_clim/",lubridate::year(input$date))
      sub_file <- paste0(sub_dir,"/cat.clim.",input$date,".Rda")
    } else if(input$layer == "Summary"){
      sub_dir <- "../data/annual_summary"
      sub_file <- paste0(sub_dir,"/MHW_cat_pixel_",input$date,".Rds")
    } else if(input$layer == "Anomaly"){
      req(lubridate::is.Date(input$date))
      sub_dir <- paste0("OISST/daily/",lubridate::year(input$date))
      sub_file <- paste0(sub_dir,"/daily.",input$date,".Rda")
    } else if(input$layer == "Trend: Count"){
      baseDataPre <- Oliver_2018 %>% 
        filter(var == "MHW_cnt_tr")
    } else if(input$layer == "Trend: Duration"){
      baseDataPre <- Oliver_2018 %>% 
        filter(var == "MHW_dur_tr")
    } else if(input$layer == "Trend: Intensity (mean)"){
      baseDataPre <- Oliver_2018 %>% 
        filter(var == "MHW_mean_tr")
    } else if(input$layer == "Trend: Intensity (max)"){
      baseDataPre <- Oliver_2018 %>% 
        filter(var == "MHW_max_tr")
    }
    if(input$layer %in% c("Category", "Anomaly", "Summary")){
      if(file.exists(sub_file)){
        baseDataPre <- readRDS(sub_file)
      } else {
        baseDataPre <- empty_date_map
      }
    }
    return(baseDataPre)
  })
  
  ### Base map data after screening categories
  baseData <- reactive({
    baseDataPre <- baseDataPre()
    if(input$layer %in% cat_layers){
      baseData <- baseDataPre %>%
        dplyr::filter(category %in% categories$categories)
      # Fix for the issue caused by de-slecting all of the cateogries
      if(length(baseData$category) == 0){
        baseData <- empty_date_map
      }
    }
    if(input$layer %in% rb_layers){
      baseData <- baseDataPre
    }
    return(baseData)
  })
  
  ### Non-shiny-projected raster data
  rasterNonProj <- reactive({
    baseData <- baseData()
    if(input$layer %in% c("Category", "Summary")){
      MHW_raster <- baseData %>%
        dplyr::select(lon, lat, category) 
    } else if(input$layer == "Anomaly"){
      MHW_raster <- baseData %>%
        dplyr::select(lon, lat, anom) 
    } else if(input$layer %in% trend_layers){
      MHW_raster <- baseData %>%
        dplyr::select(lon, lat, val)
    } else {
      MHW_raster <- baseData
    }
    colnames(MHW_raster) <- c("X", "Y", "Z")
    MHW_raster$Z <- as.numeric(MHW_raster$Z)
    suppressWarnings(
    rasterNonProj <- raster::rasterFromXYZ(MHW_raster, res = c(0.25, 0.25),
                                           digits = 3, crs = inputProj)
    )
    return(rasterNonProj)
  })
  
  ### Shiny-projected raster data
  rasterProj <- reactive({
    rasterNonProj <- rasterNonProj()
    # NB: Smoothing the pixels is easy, but causes massive artifacts
    # if(input$pixels == "Smooth"){
    # rasterProj <- projectRasterForLeaflet(rasterNonProj, method = "bilinear")
    # } else {
    suppressWarnings(
    rasterProj <- projectRasterForLeaflet(rasterNonProj, method = "ngb")
    )
    # }
    return(rasterProj)
  })
  
  
# Time series data --------------------------------------------------------
  
  ### Pixel data
  pixelData <- reactive({
    req(input$map_click)
    xy <- input$map_click
    while(xy$lng > 180){
      xy$lng <- xy$lng - 360
      }
    while(xy$lng < -180){
      xy$lng <- xy$lng + 360
      }
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
  })
  
  
# Pop-ups -----------------------------------------------------------------
  
  ### Observer to show pop-ups on click
  observeEvent(c(input$map_click, input$open_modal), {
    req(input$map_click)
    click <- input$map_click
    showpos(x = click$lng, y = click$lat)
  })
  
  ### Show popup on clicks
  showpos <- function(x = NULL, y = NULL) {
    baseDataPre <- baseDataPre()
    baseData <- baseData()
    rasterNonProj <- rasterNonProj()
    rasterProj <- rasterProj()
    xy_click <- c(x,y)
    xy_wrap <- lon_wrap(xy_click)
    # Translate lon/lat to cell number using the unprojected raster
      # This is done because the projected raster is not in degrees
    cell <- raster::cellFromXY(rasterNonProj, c(xy_wrap))
    # If the click is inside the raster...
    xy <- raster::xyFromCell(rasterNonProj, cell) # Get the center of the cell
    if(xy[1] >= 0) xy_lon <- paste0(abs(xy[1]),"°E")
    if(xy[1] < 0) xy_lon <- paste0(abs(xy[1]),"°W")
    if(xy[2] >= 0) xy_lat <- paste0(abs(xy[2]),"°N")
    if(xy[2] < 0) xy_lat <- paste0(abs(xy[2]),"°S")
    if(xy[1] >= -160 & xy[1] <= -110 & xy[2] >= 25 & xy[2] <= 60){
      regional_link <- paste0("<hr>",
                              "<a target='_blank' rel='noopener noreferrer' href=",
                              regional_NOAA,">Regional website (NOAA)</a>")
    } else if(xy[1] >= -7 & xy[1] <= 37 & xy[2] >= 27 & xy[2] <= 47){
      regional_link <- paste0("<hr>",
                              "<a target='_blank' rel='noopener noreferrer' href=",
                              regional_TMEDNET,">Regional website (T-MEDNet)</a>")
    } else {
      regional_link <- ""
    }
    if(input$layer %in% cat_layers){
      val <- baseDataPre %>% 
        dplyr::filter(lon == xy[1],
                      lat == xy[2]) %>% 
        dplyr::select(category) %>% 
        as.numeric()
      content_sub <- paste0("<br>Category = ", names(MHW_colours)[val])
    } else if(input$layer == "Anomaly"){
      val <- baseDataPre %>% 
        dplyr::filter(lon == xy[1],
                      lat == xy[2]) 
      content_sub <- paste0("<br>Climatology = ", round(val$seas, 2),
                            "<br>Threshold = ", round(val$thresh, 2),
                            "<br>Anomaly = ", val$anom)
    } else if(input$layer %in% trend_layers){
      val <- baseDataPre %>% 
        dplyr::filter(lon == xy[1],
                      lat == xy[2]) 
      content_sub <- paste0("<br>",input$layer," = ", round(val$val, 2))
      
    } 
    content <- paste0("Lon = ", xy_lon,
                      "<br>Lat = ", xy_lat,
                      content_sub,
                      regional_link,
                      "<hr>",
                      "<i>Please click the<br><b>Plot pixel</b><br>button in the<br><b>Controls</b> panel<br>for more info</i>")
    
    # Update lon lat
    updateNumericInput(session, "lon", value = round(xy_click[1], 2))
    updateNumericInput(session, "lat", value = round(xy_click[2], 2))
    
    # Add Popup to leaflet
    leafletProxy("map") %>%
      clearPopups() %>%
      addPopups(lng = xy_click[1], lat = xy_click[2],
                popup = paste(content))
  }
  
  
# Leaflet -----------------------------------------------------------------
  
  ### Observer to change colour palette accordingly
  pal_react <- reactive({
    baseDataPre <- baseDataPre()
    if(input$layer == "Anomaly"){
      domain_low <- min(baseDataPre$anom, na.rm = T)
      domain_high <- max(baseDataPre$anom, na.rm = T)
    }
    if(input$layer %in% trend_layers){
      domain_low <- min(baseDataPre$val, na.rm = T)
      domain_high <- max(baseDataPre$val, na.rm = T)
    }
    if(input$layer %in% cat_layers){
      domain_low <- 1; domain_high <- 1
    }
    if(abs(domain_high) > abs(domain_low)){
      domain_low <- -domain_high
    } else{
      domain_high <- -domain_low
    }
    if(input$layer %in% cat_layers){
      colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)
    } else {
      colorNumeric(palette = c( "blue", "white", "red"), na.color = NA, 
                   domain = c(domain_low, domain_high))
    }
  })
  
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
  observeEvent(c(input$date, input$layer,
                 input$moderate_filter, input$strong_filter,
                 input$severe_filter, input$extreme_filter), {
                   leafletProxy("map") %>%
                     addRasterImage(rasterProj(), colors = pal_react(), layerId = ns("map_raster"),
                                    project = FALSE, opacity = 0.8)
  })
  
  ### Shift when new lon/lat are entered
  observeEvent(c(input$lon, input$lat), {
    leafletProxy("map") %>%
      setView(lng = input$lon, lat = input$lat, zoom = input$zoom,
              options = tileOptions(minZoom = 0, maxZoom = 8, noWrap = F))
    })
  
  ### Change map background
  observeEvent(input$map_back, {
    if(input$map_back == "Grey"){
      leafletProxy("map") %>%
        clearTiles() %>% 
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite,
                         options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F)) %>%
        addRasterImage(rasterProj(), colors = pal_react(), layerId = ns("map_raster"),
                       project = FALSE, opacity = 0.8)
    } else if(input$map_back == "Land features"){
      leafletProxy("map") %>%
        clearTiles() %>% 
        addProviderTiles(providers$Esri.WorldTopoMap,
                         options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F)) %>%
        addRasterImage(rasterProj(), colors = pal_react(), layerId = ns("map_raster"),
                       project = FALSE, opacity = 0.8)
    } else if(input$map_back == "Ocean features"){
      leafletProxy("map") %>%
        clearTiles() %>% 
        addProviderTiles(providers$Esri.OceanBasemap,
                         options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F)) %>%
        addRasterImage(rasterProj(), colors = pal_react(), layerId = ns("map_raster"),
                       project = FALSE, opacity = 0.8)
    } else{
      leafletProxy("map") %>%
        clearTiles() %>% 
        addTiles(options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F)) %>%
        addRasterImage(rasterProj(), colors = pal_react(), layerId = ns("map_raster"),
                       project = FALSE, opacity = 0.8)
    }
  })
  
  ### Recreate the legend as needed
  observe({
    baseDataPre <- baseDataPre()
    if(input$layer %in% cat_layers){
      leafletProxy("map", data = MHW_cat_clim_sub) %>% 
        clearControls()
    } else if(input$layer %in% c(rb_layers)){
      if(input$layer == "Anomaly"){
        domain_low <- min(baseDataPre$anom, na.rm = T)
        domain_high <- max(baseDataPre$anom, na.rm = T)
        domain_label <- "Anomaly"
      } else if(input$layer %in% trend_layers){
        domain_low <- min(baseDataPre$val, na.rm = T)
        domain_high <- max(baseDataPre$val, na.rm = T)
        domain_label <- "Annual\nTrend"
      }
      if(abs(domain_high) > abs(domain_low)){
        domain_low <- -domain_high
      } else {
        domain_high <- -domain_low
      }
      domain_low_high <- data.frame(domain = seq(domain_low, domain_high, length.out = 10))
      pal_rev <- colorNumeric(palette = c("red", "white", "blue"), na.color = NA,
                              domain = domain_low_high)
      leafletProxy("map", data = domain_low_high) %>%
        clearControls() %>%
        addLegend(position = "bottomright",  pal = pal_rev,
                  bins = 5, values = ~domain,
                  title = domain_label,
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    } 
    # proxy %>% clearControls()
    # if(input$layer %in% rb_layers)
    # if(input$layer %in% rb_layers){
    # 
    #   } else if(input$layer %in% cat_layers){
    #     proxy %>% clearControls()
    #   }
    # if(input$layer %in% cat_layers){
      # proxy %>% clearControls()
    # }
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
      dplyr::filter(date_peak >= input$from_to[1], date_peak <= input$from_to[2])
    
    # The base
    suppressWarnings( # text aes for plotly
      p <- ggplot(data = ts_data_sub, aes(x = t, y = temp)) +
        # geom_segment(aes(x = input$date[1], 
        #                  xend = input$date[1],
        #                  y = min(ts_data_sub$temp), 
        #                  yend = max(ts_data_sub$temp),
        #                  text = "Date shown"), colour = "limegreen") +
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
    p <- p +
      geom_segment(aes(x = input$date[1], 
                       xend = input$date[1],
                       y = min(temp), 
                       yend = max(temp),
                       text = "Date shown"), colour = "limegreen")
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
      dplyr::filter(date_peak >= input$from_to[1], date_peak <= input$from_to[2])
    
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
    p
  })
  
  ### Interactive lolliplot
  lolliPlotly <- reactive({
    p <- lollilot()
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
                                   tabPanel(title = "Time series",
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
                                            downloadButton(outputId = ns("download_TS"),
                                                           label = "Time series (png)", class = 'small-dl'),
                                            downloadButton(outputId = ns("download_lolli"),
                                                           label = "Lolliplot (png)", class = 'small-dl'),
                                            downloadButton(outputId = ns("download_clim"),
                                                           label = "Climatology & Threshold (csv)", class = 'small-dl'),
                                            downloadButton(outputId = ns("download_event"),
                                                           label = "MHW data (csv)", class = 'small-dl'))
                                     )
                       )
    )
  })
  
  
# Downloads ---------------------------------------------------------------
  
  ### Download time series figure
  output$download_TS <- downloadHandler(
    filename = function() {
      paste0("MHW_TS.png")
    },
    content <- function(file) {
      ggsave(plot = tsPlot(), filename =  file, width = 8, height = 4)
    }
  )
  
  ### Download lolliplot
  output$download_lolli <- downloadHandler(
    filename = function() {
      paste0("MHW_lolli.png")
    },
    content <- function(file) {
      ggsave(plot = lolliPlot(), filename =  file, width = 8, height = 4)
    }
  )
  
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
  
  ### Download event data
  output$download_event <- downloadHandler(
    filename = function() {
      paste0("event_lon_",downloadEventData()$lon[1],"_lat_",downloadEventData()$lat[1],".csv")
    },
    content <- function(file) {
      readr::write_csv(downloadEventData(), file)
    }
  )
  
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
  # date_filter_from <- as.Date("2020-02-27")
  # date_filter_to <- as.Date("2020-02-29")
  downloadMapData <- reactive({
    # req(lubridate::is.Date(input$map_download_date[1]))
    # req(lubridate::is.Date(input$map_download_date[2]))
    if(input$layer %in% c("Category", "Anomaly")){
      date_filter_from <- as.Date(input$map_download_date[1])
      date_filter_to <- as.Date(input$map_download_date[2])
      date_seq <- seq(date_filter_from, date_filter_to, by = "day")
      date_seq_years <- sapply(strsplit(as.character(date_seq), "-"), "[[", 1)
    } else if(input$layer == "Summary"){
      date_seq_years <- input$map_download_date[1]:input$map_download_date[2]
    } else{
     # Blank 
    }
    if(input$layer == "Category"){
      date_seq_files <- paste0("cat_clim/",date_seq_years,"/cat.clim.",date_seq,".Rda")
      map_data <- plyr::ldply(date_seq_files, readRDS_date)
    } else if(input$layer == "Anomaly") {
      date_seq_files <- paste0("OISST/daily/",date_seq_years,"/daily.",date_seq,".Rda")
      map_data <- plyr::ldply(date_seq_files, readRDS_date)
    } else if(input$layer == "Summary"){
      if(input$summary_type == "Count"){
        if(input$hemisphere_choice == "Boreal"){
          date_seq_files <- paste0("../data/annual_summary/MHW_cat_count_N_",date_seq_years,".Rds")
          map_data <- plyr::ldply(date_seq_files, readRDS_year)
        } else{
          # This wil need to be updated in July 2020
          # date_seq_years <- 2018:2018
          if(2020 %in% date_seq_years) date_seq_years <- date_seq_years[-which(date_seq_years == 2020)]
          if(length(date_seq_years) < 1) date_seq_years <- 2019
          date_seq_files <- paste0("../data/annual_summary/MHW_cat_count_S_",date_seq_years,".Rds")
          map_data <- plyr::ldply(date_seq_files, readRDS_year)
        }
      } else{
        date_seq_files <- paste0("../data/annual_summary/MHW_cat_pixel_",date_seq_years,".Rds")
        map_data <- plyr::ldply(date_seq_files, readRDS_year)
      }
    } else if(input$layer %in% trend_layers){
      map_data <- baseDataPre()
    }
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

