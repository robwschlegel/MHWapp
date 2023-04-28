map <- function(input, output, session) {
  ns <- session$ns
  
  # testers...
  # xy <- c(-42.125, 39.875)
  # x <- -42.125
  # y <- 39.875
  # input <- data.frame(date = 2019,
  #                     layer = "MHW Summary")
  # input <- data.frame(date = as.Date("2019-07-19"),
  # layer = "MCS Category",
  # layer = "Trend",
  # pixel = "Smooth",
  # from = as.Date("2020-09-10"),
  # to = as.Date("2021-09-10"))
  # input <- data.frame(from_to = c(as.Date("2011-05-06"), as.Date("2012-05-05")))
  # categories = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))
  # categories <- data.frame(categories = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))


# Guided tour -------------------------------------------------------------
  
  guide <- Cicerone$
    new()$
    step(
      el = ns("toggle"),
      title = "Control panel",
      description = HTML("This web application shows near-real-time information on where in the world marine heatwaves (MHW)
                         and marine cold-spells (MCS) are occurring and what category they are.
                         <hr>
                         This is where all of the controls are contained. Click <b>Controls</b> to minimise this panel.")
    )$
    step(
      ns("controls"),
      "Control panel",
      HTML("Click in the <b>Date</b> box to choose a day to display on the map.
           <hr>
           Flip the <b>Animate</b> switch to bring up the animation menu.
           <hr>
           Access the different datasets in the MHW Tracker via the <b>Map layer</b> button.
           <hr>
           Use the <b>Map data</b> menu to download the global MHW category data. There is a 31 day size limit per download request.
           <hr>
           Change the map with <b>Background</b>. If the map appears grey on startup that can be changed here.
           <hr>
           Clicking on the <b>Category</b> buttons will filter those pixels from the map.
           <hr>
           After clicking a pixel of interest on the map the <b>Plot pixel</b> button will turn green. 
           One may then click on this button to access the time series data for the chosen pixel."),
      position = "right"
    )$
    step(
      "[data-value='map_tab']",
      "Map tab",
      HTML("This tab provides an interactive map of global MHW/MCS in near-real-time.
           <hr>
           This is the tab that is currently shown."),
      is_id = FALSE, position = "left"
    )$
    # Deactivated primarily for speed, but also because the underlying packages change too often
    # step(
    #   "[data-value='comp_tab']",
    #   "Comparison tab",
    #   "Click here to access a dashboard that allows one to compare MHW results for different remotely sensed products.",
    #   is_id = FALSE, position = "left"
    # )$
    # step(
    #   "[data-value='sum_tab']",
    #   "Summary tab",
    #   "Click here to access a dashboard designed for more thoroughly investigating annual MHW summaries.",
    #   is_id = FALSE, position = "left"
    # )$
    step(
      "[data-value='about_tab']",
      "About tab",
      "Click here for more detailed information about this app.",
      is_id = FALSE, position = "left"
    )


# Reactive UI -------------------------------------------------------------

  ### The popup modal when starting the app
  # Open on startup
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(length(query) < 1){
      # req(input$layer)
      guide$init()$start()
    }
  })
  
  ### Switch the display of the Controls on and off
  observeEvent(input$toggle, {
    shinyjs::toggle("control_menu", anim = TRUE)
    })
  
  ### Toggle animation switch on and off
  output$check_animate_UI <- renderUI({
    req(input$layer)
    if(input$layer %in% c("MHW Category", "SST Anomaly", "MCS Category")){
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
                         selected = "MHW Category",
                         # selected = "MCS Category", # For testing
                         status = "primary",
                         shape = "curve",
                         inline = F),
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
    if(input$layer %in% c("MCS Category", "MCS Summary")){
      style_react <- "color: black; background-color: #C7ECF2; border-color: black; width: 110px"
    } else {
      style_react <- "color: black; background-color: #ffc866; border-color: black; width: 110px"
    }
    if(input$layer %in% rb_layers){
      # No button when anomaly layer is chosen
    } else if(button_I$clicked){
      actionButton(inputId = ns("moderate_filter"), "I Moderate", icon = icon("remove", lib = "glyphicon"), style = style_react)
    } else {
      actionButton(inputId = ns("moderate_filter"), "I Moderate", icon = icon("ok", lib = "glyphicon"), style = style_react)
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
    if(input$layer %in% c("MCS Category", "MCS Summary")){
      style_react <- "color: black; background-color: #85B7CC; border-color: black; width: 110px"
    } else {
      style_react <- "color: black; background-color: #ff6900; border-color: black; width: 110px"
    }
    if(input$layer %in% rb_layers){
      # No button when anomaly layer is chosen
    } else if(button_II$clicked){
      actionButton(inputId = ns("strong_filter"), "II Strong", icon = icon("remove", lib = "glyphicon"), style = style_react)     
    } else {
      actionButton(inputId = ns("strong_filter"), "II Strong", icon = icon("ok", lib = "glyphicon"), style = style_react)
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
    if(input$layer %in% c("MCS Category", "MCS Summary")){
      style_react <- "color: black; background-color: #4A6A94; border-color: black; width: 110px"
    } else {
      style_react <- "color: black; background-color: #9e0000; border-color: black; width: 110px"
    }
    if(input$layer %in% rb_layers){
      # No button when anomaly layer is chosen
    } else if(button_III$clicked){
      actionButton(inputId = ns("severe_filter"), "III Severe", icon = icon("remove", lib = "glyphicon"), style = style_react)     
    } else {
      actionButton(inputId = ns("severe_filter"), "III Severe", icon = icon("ok", lib = "glyphicon"), style = style_react)
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
    if(input$layer %in% c("MCS Category", "MCS Summary")){
      style_react <- "color: black; background-color: #111433; border-color: black; width: 110px"
    } else {
      style_react <- "color: black; background-color: #2d0000; border-color: black; width: 110px"
    }
    if(input$layer %in% rb_layers){
      # No button when anomaly layer is chosen
    } else if(button_IV$clicked){
      actionButton(inputId = ns("extreme_filter"), "IV Extreme", icon = icon("remove", lib = "glyphicon"), style = style_react)     
    } else {
      actionButton(inputId = ns("extreme_filter"), "IV Extreme ", icon = icon("ok", lib = "glyphicon"), style = style_react)
    }
  })
  
  ### Button for opening main modal
  output$button_ts <- renderUI({
    req(input$layer)
    # req(input$map_click)
    if(input$layer %in% c("MHW Category", "SST Anomaly", "MCS Category")){
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
      # date_menu_choice <- as.Date("2019-12-31") # For testing
    }
    if(input$layer %in% c("MHW Category", "SST Anomaly", "MCS Category")){
        dateInput(inputId = ns("date"),
                  label = NULL, width = '100%',
                  value = date_menu_choice,
                  min = "1982-01-01",
                  max = max(current_dates))
      } else if(input$layer %in% c("MHW Summary", "MCS Summary")){
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
    if(input$layer %in% c("MHW Category", "SST Anomaly", "MCS Category")){
      date_input <- dateRangeInput(
        inputId = ns("map_download_date"),
        label = h5("Date range"),
        start = max(current_dates), end = max(current_dates),
        min = "1982-01-01", max = max(current_dates))
      sum_type <- NULL
      hem_choice <- NULL
    } else if(input$layer == "MHW Summary"){
      # date_input <- sliderInput(inputId = ns("map_download_date"), sep = "",
      #                           label = h5("Date range"), 
      #                           value = lubridate::year(Sys.time()),
      #                           min = 1982, max = lubridate::year(Sys.time()))
      date_input <- selectInput(inputId = ns("map_download_date"), 
                                label = h5("Date range"), 
                                choices = seq(1982, lubridate::year(Sys.time())),
                                selected = lubridate::year(Sys.time()), multiple = F)
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
    } else if(input$layer == "MCS Summary"){
      date_input <- selectInput(inputId = ns("map_download_date"), 
                                label = h5("Date range"), 
                                choices = seq(1982, lubridate::year(Sys.time())),
                                selected = lubridate::year(Sys.time()), multiple = F)
      sum_type <- hem_choice <- NULL
    } else {
      date_input <- sum_type <- hem_choice <- NULL
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
  
  ### Disable map download button if dates are incorrect
  observe({
    shinyjs::toggleState(id = "download_map", 
                         condition = input$map_download_date[1] <= input$map_download_date[2] & (as.integer(input$map_download_date[2]) - as.integer(input$map_download_date[1])) <= 31) 

  })

  ### Observe the changing of date, lon, lat, zoom, in the URL
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[['date']])){
      updateDateInput(session = session, 
                      inputId = "date", 
                      value = as.Date(as.character(query[['date']])))
    }
    if(!is.null(query[['lat']])){
      updateNumericInput(session, "lat", value = query[['lat']])
    }
    if(!is.null(query[['lon']])){
      updateNumericInput(session, "lon", value = query[['lon']])
    }
    if(!is.null(query[['zoom']])){
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
    if(input$layer == "MHW Category"){
      req(lubridate::is.Date(input$date))
      sub_dir <- paste0("cat_clim/",lubridate::year(input$date))
      sub_file <- paste0(sub_dir,"/cat.clim.",input$date,".Rda")
    } else if(input$layer == "MCS Category"){
      req(lubridate::is.Date(input$date))
      sub_dir <- paste0("cat_clim/MCS/",lubridate::year(input$date))
      sub_file <- paste0(sub_dir,"/cat.clim.MCS.",input$date,".Rds")
    } else if(input$layer == "MHW Summary"){
      if(lubridate::is.Date(input$date)){
        file_year <- lubridate::year(input$date)
      } else {
        file_year <- input$date
      }
      sub_dir <- "../data/annual_summary"
      sub_file <- paste0(sub_dir,"/MHW_cat_pixel_",file_year,".Rds")
    } else if(input$layer == "MCS Summary"){
      if(lubridate::is.Date(input$date)){
        file_year <- lubridate::year(input$date)
      } else {
        file_year <- input$date
      }
      sub_dir <- "../data/annual_summary"
      sub_file <- paste0(sub_dir,"/MCS_cat_pixel_",file_year,".Rds")
    } else if(input$layer == "SST Anomaly"){
      req(lubridate::is.Date(input$date))
      sub_dir <- paste0("OISST/daily/",lubridate::year(input$date))
      sub_file <- paste0(sub_dir,"/daily.",input$date,".Rda")
    } else if(input$layer == "MHW Trend: Count"){
      if(!exists("Oliver_2018")) Oliver_2018 <- readRDS("../data/published/Oliver_2018.Rds")
      baseDataPre <- Oliver_2018 %>% 
        filter(var == "MHW_cnt_tr")
    } else if(input$layer == "MHW Trend: Duration"){
      if(!exists("Oliver_2018")) Oliver_2018 <- readRDS("../data/published/Oliver_2018.Rds")
      baseDataPre <- Oliver_2018 %>% 
        filter(var == "MHW_dur_tr")
    } else if(input$layer == "MHW Trend: Intensity (mean)"){
      if(!exists("Oliver_2018")) Oliver_2018 <- readRDS("../data/published/Oliver_2018.Rds")
      baseDataPre <- Oliver_2018 %>% 
        filter(var == "MHW_mean_tr")
    } else if(input$layer == "MHW Trend: Intensity (max)"){
      if(!exists("Oliver_2018")) Oliver_2018 <- readRDS("../data/published/Oliver_2018.Rds")
      baseDataPre <- Oliver_2018 %>% 
        filter(var == "MHW_max_tr")
    }
    if(input$layer %in% c("MHW Category", "MCS Category", "MHW Summary", "MCS Summary", "SST Anomaly")){
      if(file.exists(sub_file)){
        baseDataPre <- readRDS(sub_file) %>% ungroup()
      } else {
        baseDataPre <- empty_date_map
        # baseDataPre <- readRDS("cat_clim/MCS/1982/cat.clim.MCS.1982-01-01.Rds") # For testing
        # baseDataPre <- readRDS("cat_clim/1982/cat.clim.1982-01-01.Rda") # For testing
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
      # Fix for the issue caused by de-selecting all of the categories
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
    if(input$layer %in% c("MHW Category", "MHW Summary", "MCS Category", "MCS Summary")){
      MHW_raster <- baseData %>%
        dplyr::select(lon, lat, category) 
    } else if(input$layer == "SST Anomaly"){
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
    # suppressWarnings(
    rasterNonProj <- raster::rasterFromXYZ(MHW_raster, res = c(0.25, 0.25),
                                           digits = 3, crs = inputProj)
    # )
    return(rasterNonProj)
  })
  
  ### Shiny-projected raster data
  rasterProj <- reactive({
    rasterNonProj <- rasterNonProj()
    # NB: Smoothing the pixels is easy, but causes massive artifacts
    # if(input$pixels == "Smooth"){
    # rasterProj <- projectRasterForLeaflet(rasterNonProj, method = "bilinear")
    # } else {
    # suppressWarnings(
    rasterProj <- projectRasterForLeaflet(rasterNonProj, method = "ngb")
    # )
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
                    thresh_4x = thresh_3x + diff,
                    diff_MCS = thresh_MCS - seas,
                    thresh_MCS_2x = thresh_MCS + diff_MCS,
                    thresh_MCS_3x = thresh_MCS_2x + diff_MCS,
                    thresh_MCS_4x = thresh_MCS_3x + diff_MCS)
    
    # Grab event data
    event_data <- readRDS(MHW_event_files[which(lon_OISST == xy[1])]) %>% 
      dplyr::filter(lat == xy[2]) %>%
      mutate(date_start = as.Date(date_start, origin = "1970-01-01"),
             date_peak = as.Date(date_peak, origin = "1970-01-01"),
             date_end = as.Date(date_end, origin = "1970-01-01"))
    event_MCS_data <- readRDS(MCS_event_files[which(lon_OISST == xy[1])]) %>% 
      dplyr::filter(lat == xy[2]) %>%
      mutate(date_start = as.Date(date_start, origin = "1970-01-01"),
             date_peak = as.Date(date_peak, origin = "1970-01-01"),
             date_end = as.Date(date_end, origin = "1970-01-01"))
    
    # Return the pixel data
    pixelData <- list(ts = ts_data,
                      event = event_data,
                      event_MCS = event_MCS_data,
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
                              regional_NOAA,
                              ">Regional website (NOAA)</a>")
    } else if(xy[1] >= -3 & xy[1] <= 37 & xy[2] >= 27 & xy[2] <= 47){
      regional_link <- paste0("<hr>",
                              "<a target='_blank' rel='noopener noreferrer' href=",
                              regional_TMEDNET,
                              ">Regional website (T-MEDNet)</a>")
    } else if(xy[1] >= 7 & xy[1] <= 16 & xy[2] >= 53 & xy[2] <= 59){
      regional_link <- paste0("<hr>",
                              "<a target='_blank' rel='noopener noreferrer' href=",
                              regional_Danish_Archepelago,
                              ">Regional website (Fishforecasts)</a>")
    } else if(xy[1] >= 13 & xy[1] <= 30 & xy[2] >= 53 & xy[2] <= 66){
      regional_link <- paste0("<hr>",
                              "<a target='_blank' rel='noopener noreferrer' href=",
                              regional_Baltic_Sea,
                              ">Regional website (Fishforecasts)</a>")
    } else if(xy[1] >= -4 & xy[1] <= 10 & xy[2] >= 50 & xy[2] <= 61){
      regional_link <- paste0("<hr>",
                              "<a target='_blank' rel='noopener noreferrer' href=",
                              regional_North_Sea,
                              ">Regional website (Fishforecasts)</a>")
    } else if(xy[1] >= -15 & xy[1] <= 2 & xy[2] >= 43 & xy[2] <= 61){
      regional_link <- paste0("<hr>",
                              "<a target='_blank' rel='noopener noreferrer' href=",
                              regional_European_Northwest_Shelf,
                              ">Regional website (Fishforecasts)</a>")
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
    } else if(input$layer == "SST Anomaly"){
      val <- baseDataPre %>% 
        dplyr::filter(lon == xy[1],
                      lat == xy[2]) 
      content_sub <- paste0("<br>Climatology = ", round(val$seas, 2),
                            "<br>Anomaly = ", val$anom)
    } else if(input$layer %in% trend_layers){
      val <- baseDataPre %>% 
        dplyr::filter(lon == xy[1],
                      lat == xy[2]) 
      content_sub <- paste0("<br>",input$layer," = ", round(val$val, 2))
    } 
    if(input$layer %in% c(trend_layers, "MHW Summary", "MCS Summary")){
      button_text <- ""
    } else {
      button_text <- "<hr><i>Please click the<br><b>Plot pixel</b><br>button in the<br><b>Controls</b> panel<br>for more info</i>"
    }
    content <- paste0("Lon = ", xy_lon,
                      "<br>Lat = ", xy_lat,
                      content_sub,
                      regional_link,
                      # "<hr>",
                      button_text)
    
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
    if(input$layer == "SST Anomaly"){
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
    if(input$layer %in% c("MCS Category", "MCS Summary")){
      colorNumeric(palette = MCS_colours, domain = c(1,2,3,4), na.color = NA)
    } else if(input$layer %in% cat_layers){
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
  observeEvent(c(input$layer, input$date,
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
    req(input$map_back)
    if(input$map_back == "Grey"){
      leafletProxy("map") %>%
        clearTiles() %>% 
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite,
                         options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F))
    } else if(input$map_back == "Land features"){
      leafletProxy("map") %>%
        clearTiles() %>% 
        addProviderTiles(providers$Esri.WorldTopoMap,
                         options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F))
    } else if(input$map_back == "Ocean features"){
      leafletProxy("map") %>%
        clearTiles() %>% 
        addProviderTiles(providers$Esri.OceanBasemap,
                         options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F))
    } else{
      leafletProxy("map") %>%
        clearTiles() %>% 
        addTiles(options = tileOptions(minZoom = 0, maxZoom = 8, opacity = 0.5, noWrap = F))
    }
    leafletProxy("map") %>%
      addRasterImage(rasterProj(), colors = pal_react(), layerId = ns("map_raster"),
                     project = FALSE, opacity = 0.8)
  })
  
  ### Recreate the legend as needed
  observe({
    baseDataPre <- baseDataPre()
    if(input$layer %in% cat_layers){
      leafletProxy("map", data = MHW_cat_clim_sub) %>% 
        clearControls()
    } else if(input$layer %in% c(rb_layers)){
      if(input$layer == "SST Anomaly"){
        domain_low <- min(baseDataPre$anom, na.rm = T)
        domain_high <- max(baseDataPre$anom, na.rm = T)
        domain_label <- "SST Anomaly"
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
  })
  
  
# Figures/tables ----------------------------------------------------------
  
  ### Create static time series plot
  tsPlot <- reactive({
    req(input$from_to[1]); req(input$from_to[2])
    
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
    event_MCS_data <- pixelData()$event_MCS
    event_MCS_data_sub <- event_MCS_data %>%
      dplyr::filter(date_peak >= input$from_to[1], date_peak <= input$from_to[2])
    
    # The base
    p <- ggplot(data = ts_data_sub, aes(x = t, y = temp)) +
      labs(x = "", y = "Temperature (°C)") +
      scale_x_date(expand = c(0, 0), date_labels = "%b %Y", limits = c(input$from_to[1], input$from_to[2])) +
      theme(panel.border = element_rect(colour = "black", fill = NA))
    
    # Add flame categories as needed
    ## MHWs
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
    ## MCSs
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS)){
      p <- p + heatwaveR::geom_flame(aes(y = thresh_MCS, y2 = temp), fill = "#C7ECF2", n = 5, n_gap = 2)
    }
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS_2x)){
      p <- p + heatwaveR::geom_flame(aes(y = thresh_MCS_2x, y2 = temp), fill = "#85B7CC")
    }
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS_3x)){
      p <- p + heatwaveR::geom_flame(aes(y = thresh_MCS_3x, y2 = temp), fill = "#4A6A94")
    }
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS_4x)){
      p <- p + heatwaveR::geom_flame(aes(y = thresh_MCS_4x, y2 = temp), fill = "#111433")
    }
    # Add SST, seas, clim lines
    suppressWarnings( # text aes plotly
      p <- p + geom_line(colour = "grey20",
                         aes(group = 1, text = paste0("Date: ",t,
                                                      "<br>Temperature: ",temp,"°C"))) +
        geom_line(linetype = "dashed", colour = "forestgreen",
                  aes(x = t, y = seas, group = 1,
                      text = paste0("Date: ",t,
                                    "<br>Climatology: ",seas,"°C"))) +
        geom_line(linetype = "dotted", colour = "tomato3",
                  aes(x = t, y = thresh, group = 1,
                      text = paste0("Date: ",t,
                                    "<br>Threshold: ",thresh,"°C"))) +
        geom_line(linetype = "dotted", colour = "darkorchid",
                  aes(x = t, y = thresh_MCS, group = 1,
                      text = paste0("Date: ",t,
                                    "<br>Threshold: ",thresh_MCS,"°C")))
    )
    # Add rug as needed
    if(length(event_data_sub$date_start) > 0){
      suppressWarnings( # text aes plotly
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
    if(length(event_MCS_data_sub$date_start) > 0){
      suppressWarnings( # text aes plotly
        p <- p + geom_rug(data = event_MCS_data_sub, sides = "b", colour = "steelblue1", size = 2,
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
    suppressWarnings( # text aes plotly
    p <- p +
      geom_segment(aes(x = input$date[1], 
                       xend = input$date[1],
                       y = min(temp), 
                       yend = max(temp),
                       text = "Date shown"), 
                   colour = "limegreen")
    )
    
    # Convert to plotly
    # NB: Setting dynamicTicks = T causes the flames to be rendered incorrectly
    pp <- ggplotly(p, tooltip = "text", dynamicTicks = F) %>% 
      layout(hovermode = 'compare') 
    pp
  })
  
  ### Create lolliplot
  lolliPlot <- reactive({
    
    # Filter time series based on dates
    ts_data <- pixelData()$ts
    ts_data_sub <- ts_data %>%
      dplyr::filter(t >= input$from_to[1], t <= input$from_to[2])
    
    # Prep data
    event_data <- pixelData()$event
    event_data_sub <- event_data %>%
      dplyr::filter(date_peak >= input$from_to[1], date_peak <= input$from_to[2])
    event_MCS_data <- pixelData()$event_MCS
    event_MCS_data_sub <- event_MCS_data %>%
      dplyr::filter(date_peak >= input$from_to[1], date_peak <= input$from_to[2])
    
    # Get y_lims
    y_lims <- c(0, 1)
    if(length(event_data_sub$date_start) > 0) y_lims[2] <- max(event_data_sub$intensity_max)*1.1
    if(length(event_MCS_data_sub$date_start) > 0) y_lims[1] <- min(event_MCS_data_sub$intensity_max)*1.1
    if(length(event_MCS_data_sub$date_start) > 0 & length(event_data_sub$date_start) == 0) y_lims[2] <- 0
      
    # The base figure
    p <- ggplot() +
      labs(x = "", y = "Max. Intensity (°C)") +
      scale_y_continuous(limits = y_lims, expand = c(0, 0)) +
      scale_x_date(expand = c(0, 0), date_labels = "%b %Y", limits = c(input$from_to[1], input$from_to[2])) +
      theme(panel.border = element_rect(colour = "black", fill = NA))
    
    # Add lollis as necessary
    if(length(event_data_sub$date_start) > 0){
      suppressWarnings(
        p <- p + geom_segment(data = event_data_sub,
                              aes(x = date_peak, xend = date_peak, y = intensity_max, yend = 0)) +
          geom_hline(yintercept = 0) +
          geom_point(data = event_data_sub, fill = "salmon", shape = 21, size = 4,
                     aes(x = date_peak, y = intensity_max,
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
    if(length(event_MCS_data_sub$date_start) > 0){
      suppressWarnings(
        p <- p + geom_segment(data = event_MCS_data_sub,
                              aes(x = date_peak, xend = date_peak, y = intensity_max, yend = 0)) +
          geom_hline(yintercept = 0) +
          geom_point(data = event_MCS_data_sub, fill = "steelblue1", shape = 21, size = 4,
                     aes(x = date_peak, y = intensity_max,
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
    if(length(event_data_sub$date_start) == 0 & length(event_MCS_data_sub$date_start) == 0){
      suppressWarnings(
        p <- p + geom_text(aes(x = input$from_to[1] + ((input$from_to[2] - input$from_to[1])/2),
                               y = 0.5, label = "?", text = "No marine heatwaves or marine cold-spells detected in date range."))
      )
    }
    p
  })
  
  ### Interactive lolliplot
  lolliPlotly <- reactive({
    # p <- lollilot()
    p <- lolliPlot()
    pp <- ggplotly(p, tooltip = "text", dynamicTicks = F) #%>%
      # style(hoverinfo = "none", traces = c(3, 4))
    pp
  })
  
  ### Create data table
  tsTable <- reactive({
    event_data <- pixelData()$event %>% 
      dplyr::filter(date_start >= input$from_to[1], date_start <= input$from_to[2]) %>% 
      mutate(Event = "MHW") %>% 
      dplyr::rename(Lon = lon,
                    Lat = lat,
                    '#' = event_no,
                    Duration = duration,
                    'Start Date' = date_start,
                    'Peak Date' = date_peak,
                    'End Date' = date_end,
                    'Mean Intensity' = intensity_mean,
                    'Max. Intensity' = intensity_max,
                    'Cum. Intensity' = intensity_cumulative)
    event_MCS_data <- pixelData()$event_MCS %>% 
      dplyr::filter(date_start >= input$from_to[1], date_start <= input$from_to[2]) %>% 
      mutate(Event = "MCS") %>% 
      dplyr::rename(Lon = lon,
                    Lat = lat,
                    '#' = event_no,
                    Duration = duration,
                    'Start Date' = date_start,
                    'Peak Date' = date_peak,
                    'End Date' = date_end,
                    'Mean Intensity' = intensity_mean,
                    'Max. Intensity' = intensity_max,
                    'Cum. Intensity' = intensity_cumulative)
    event_res <- rbind(event_data, event_MCS_data) %>% 
      dplyr::arrange(`Peak Date`) %>% 
      dplyr::select(Event, everything())
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
  output$lolliPlotly <- renderPlotly({
    lolliPlotly()
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
                                            # shinycssloaders::withSpinner(plotlyOutput(ns("lolliPlotly")), 
                                            # shinycssloaders::withSpinner(plotOutput(ns("tsPlot")),
                                            shinycssloaders::withSpinner(plotlyOutput(ns("tsPlotly")),
                                                                         type = 6, color = "#b0b7be"),
                                            hr()),
                                   tabPanel(title = "Lolliplot",
                                            br(),
                                            shinycssloaders::withSpinner(plotlyOutput(ns("lolliPlotly")), 
                                                                         type = 6, color = "#b0b7be"),
                                            hr()),
                                   tabPanel(title = "Table",
                                            br(),
                                            shinycssloaders::withSpinner(DT::dataTableOutput(ns('table')), 
                                                                         type = 6, color = "#b0b7be"),
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
                                                           label = "Event data (csv)", class = 'small-dl'))
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
    data <- pixelData()$event %>% mutate(event = "MHW")
    data_MCS <- pixelData()$event_MCS  %>% mutate(event = "MCS")
    lon <- pixelData()$lon[1]
    lat <- pixelData()$lat[1]
    data_sub <- rbind(data, data_MCS) %>% 
      mutate(lon = lon, lat = lat) %>% 
      dplyr::select(event, lon, lat, event_no, date_start, date_peak, date_end, everything()) %>% 
      dplyr::arrange(date_peak)
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
      dplyr::select(lon, lat, doy, seas, thresh, thresh_MCS) %>% 
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
    if(input$layer %in% c("MHW Category", "MCS Category", "SST Anomaly")){
      date_filter_from <- as.Date(input$map_download_date[1])
      date_filter_to <- as.Date(input$map_download_date[2])
      date_seq <- seq(date_filter_from, date_filter_to, by = "day")
      date_seq_years <- sapply(strsplit(as.character(date_seq), "-"), "[[", 1)
    } else if(input$layer %in% c("MHW Summary", "MCS Summary")){
      date_seq_years <- input$map_download_date
    } else{
     # Blank 
    }
    if(input$layer == "MHW Category"){
      date_seq_files <- paste0("cat_clim/",date_seq_years,"/cat.clim.",date_seq,".Rda")
      map_data <- plyr::ldply(date_seq_files, readRDS_date)
    } else if(input$layer == "MCS Category"){
      date_seq_files <- paste0("cat_clim/MCS/",date_seq_years,"/cat.clim.",date_seq,".Rds")
      map_data <- plyr::ldply(date_seq_files, readRDS_date)
    } else if(input$layer == "SST Anomaly") {
      date_seq_files <- paste0("OISST/daily/",date_seq_years,"/daily.",date_seq,".Rda")
      map_data <- plyr::ldply(date_seq_files, readRDS_date)
    } else if(input$layer == "MHW Summary"){
      if(input$summary_type == "Count"){
        if(input$hemisphere_choice == "Boreal"){
          date_seq_files <- paste0("../data/annual_summary/MHW_cat_count_N_",date_seq_years,".Rds")
          map_data <- plyr::ldply(date_seq_files, readRDS_year)
        } else{
          # This will need to be updated in July 2020
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
    } else if(input$layer == "MCS Summary"){
      date_seq_files <- paste0("../data/annual_summary/MCS_cat_pixel_",date_seq_years,".Rds")
      map_data <- plyr::ldply(date_seq_files, readRDS_year)
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

