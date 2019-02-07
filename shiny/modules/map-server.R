map <- function(input, output, session) {
  ns <- session$ns
  
  
# Reactives ---------------------------------------------------------------

  # testers...
  # xy <- c(-42.125, 39.875)
  # x <- -42.125
  # y <- 39.875
  # input <- data.frame(from = as.Date("2018-01-01"),
  #                     to = as.Date("2018-12-31"),
  #                     date_choice = as.Date("2018-02-14"))#,
  #                     #categories = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))
  
  ### base map data
  baseData <- reactive({
    date_filter <- input$date_choice
    year_filter <- year(date_filter)
    sub_dir <- paste0("cat_clim/",year_filter)
    sub_file <- paste0(sub_dir,"/cat.clim.",date_filter,".Rda")
    if(file.exists(sub_file)){
      baseData <- readRDS(sub_file)
      baseData <- baseData %>% 
        filter(category %in% input$categories)
      # Temporary fix for the issue caused by de-slecting all of the cateogries
      if(length(baseData$category) == 0){
        baseData <- readRDS("cat_clim/1982/cat.clim.1982-01-01.Rda") %>% 
          slice(1) %>% 
          mutate(category = NA)
      }
    } else {
      baseData <- readRDS("cat_clim/1982/cat.clim.1982-01-01.Rda") %>% 
        slice(1) %>% 
        mutate(category = NA)
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
    # Leaflet click
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
    # Plotly click
    # xy <- input$plotly_click
    if(!is.null(xy)){
      rasterNonProj <- rasterNonProj()
      cell <- cellFromXY(rasterNonProj, c(xy$lng, xy$lat))
      xy <- xyFromCell(rasterNonProj, cell)
      # Grab time series data
      ts_data <- sst_seas_thresh_ts(lon_step = xy[1], lat_step = xy[2])
      # Grab event data
      event_file <- dir("event", full.names = T)[which(lon_OISST == xy[1])]
      event_data <- readRDS(event_file) %>% 
        filter(lat == xy[2]) %>%
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
  
  ### reactive labels
  pixelLabel <- reactive({
    # Leaflet click
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
      cell <- cellFromXY(rasterNonProj, c(xy$lng, xy$lat))
      xy <- xyFromCell(rasterNonProj, cell)
      if(xy[1] >= 0) xy_lon <- paste0(abs(xy[1]),"°E")
      if(xy[1] < 0) xy_lon <- paste0(abs(xy[1]),"°W")
      if(xy[2] >= 0) xy_lat <- paste0(abs(xy[2]),"°N")
      if(xy[2] < 0) xy_lat <- paste0(abs(xy[2]),"°S")
    paste0("Pixel: lon = ",xy_lon,", lat = ",xy_lat)
    }
  })
  
  ### Download data
  downloadEventData <- reactive({
    data <- pixelData()$event
    data_sub <- data #%>% 
    # filter(date_start >= input$from, date_start <= input$to)
  })
  
  downloadClimData <- reactive({
    data <- pixelData()$ts
    lon <- pixelData()$lon[1]
    lat <- pixelData()$lat[1]
    data_sub <- data %>% 
      mutate(lon = lon,
             lat = lat) %>% 
      select(lon, lat, doy, seas, thresh) %>% 
      dplyr::distinct() %>% 
      arrange(doy)
    # filter(date_start >= input$from, date_start <= input$to)
  })
  
  ### Create time series plot
  tsPlot <- reactive({
    # Time series data prep
    ts_data <- pixelData()$ts
    if(length(ts_data$temp) == 1){
      p <- ggplot() + geom_text(aes(x = 0, y = 0,label = "Please select an ocean pixel")) +
        labs(x = "", y = "Temperature (°C)")
    } else {
      if(!input$to %in% current_dates | !input$from %in% current_dates | input$from >= input$to | is.null(input$from) | is.null(input$to)){
        p <- ggplot() + geom_text(aes(x = 0, y = 0,label = "Please select valid dates")) +
          labs(x = "", y = "Temperature (°C)")
      } else {
        
        ts_data_sub <- ts_data %>%
          filter(t >= input$from, t <= input$to)
        # Event data prep
        event_data <- pixelData()$event
        event_data_sub <- event_data %>%
          filter(date_start >= input$from, date_end <= input$to)
        
        suppressWarnings(
          p <- ggplot(data = ts_data_sub, aes(x = t, y = temp)) +
            geom_flame(aes(y2 = thresh)) +
            geom_line(colour = "grey20",
                      aes(group = 1, text = paste0("Date: ",t,
                                                   "<br>Temperature: ",temp,"°C"))) +
            geom_line(linetype = "dashed", colour = "steelblue3",
                      aes(x = t, y = seas, group = 1,
                          text = paste0("Date: ",t,
                                        "<br>Climatology: ",seas,"°C"))) +
            geom_line(linetype = "dotted", colour = "tomato3",
                      aes(x = t, y = thresh, group = 1,
                          text = paste0("Date: ",t,
                                        "<br>Threshold: ",thresh,"°C"))) +
            geom_segment(aes(x = input$date_choice, 
                             xend = input$date_choice,
                             y = min(ts_data_sub$temp), 
                             yend = max(ts_data_sub$temp),
                             text = "Map date"), colour = "bisque") +
            labs(x = "", y = "Temperature (°C)") +
          scale_x_date(expand = c(0, 0), limits = c(min(ts_data_sub$t), max(ts_data_sub$t)))
        )
        if(length(event_data_sub$date_start) > 0){
          suppressWarnings(
            p <- p +
              geom_rug(data = event_data_sub, sides = "b", colour = "red3", size = 2,
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
      }
    }
    
    # Convert to plotly
    pp <- ggplotly(p, tooltip = "text", dynamicTicks = T) %>% 
      layout(hovermode = 'compare') #%>% 
      # style(traces = 2, hoverlabel = list(bgcolor = "grey10"))# %>% 
      # style(traces = 3, hoverlabel = list(bgcolor = "tomato2")) %>% 
      # style(traces = 4, hoverlabel = list(bgcolor = "red2"))
    pp
    # rangeslider(pp, start = ts_data_sub$t[1],
    #             end = ts_data_sub$t[100])
    # rangeslider(pp, start = as.Date("2017-01-01"),
    #             end = as.Date("2017-12-31"))
    # rangeslider(pp, start = as.numeric(as.Date(paste0(lubridate::year(as.Date(input$date_choice)),"-01-01"))),
    #             end = as.numeric(as.Date(paste0(lubridate::year(as.Date(input$date_choice)),"-12-31"))))
    # rangeslider(pp, start = as.integer(as.Date(paste0(lubridate::year(as.Date(input$date_choice)),"-01-01"))), 
    #             end = as.integer(as.Date(paste0(lubridate::year(as.Date(input$date_choice)),"-12-31"))))
  })
  
  ### Create lolliplot
  # lolliPlot <- reactive({
  #   event_data <- pixelData()$event
  #   event_data_sub <- event_data %>%
  #     filter(date_start >= input$from, date_start <= input$to)
  #   # thresh_data_sub <- thresh_data %>%
  #   # filter(t >= input$from, t <= input$to)
  #   p <- ggplot(data = event_data_sub, aes(x = date_start, y = intensity_max)) +
  #     geom_lolli() +
  #     # geom_point() +
  #     labs(x = "", y = "Max. Intensity (°C)")
  #   ggplotly(p)
  # })
  
  ### Create data table
  tsTable <- reactive({
    event_data <- pixelData()$event %>% 
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
    # event_data_sub <- event_data #%>% 
    # filter(date_start >= input$from, date_start <= input$to)
    # event_data$date_start <- strftime(event_data$date_start, format = "%H:%M %p")
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
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position = "bottomright",
                          pal = pal_factor,
                          values = ~category,
                          title = "Category", 
                          opacity = 0.75
      )
    }
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
    # Translate Lon-Lat to cell number using the unprojected raster
    # This is because the projected raster is not in degrees
    cell <- cellFromXY(rasterNonProj, c(x, y))
    #If the click is inside the raster...
    if(!is.na(cell)) {
      xy <- xyFromCell(rasterNonProj, cell) # Get the center of the cell
      if(xy[1] >= 0) xy_lon <- paste0(abs(xy[1]),"°E")
      if(xy[1] < 0) xy_lon <- paste0(abs(xy[1]),"°W")
      if(xy[2] >= 0) xy_lat <- paste0(abs(xy[2]),"°N")
      if(xy[2] < 0) xy_lat <- paste0(abs(xy[2]),"°S")
      x <- xy[1]
      y <- xy[2]
      xy <- SpatialPoints(data.frame(x,y))
      proj4string(xy) <- inputProj
      xy <- as.data.frame(spTransform(xy, leafletProj))
      cell <- cellFromXY(rasterProj, c(xy$x, xy$y))
      xy <- SpatialPoints(xyFromCell(rasterProj, cell))
      proj4string(xy) <- leafletProj
      xy <- as.data.frame(spTransform(xy, inputProj))
      rc <- rowColFromCell(rasterNonProj, cell)
      val <- rasterProj[cell]
      content <- paste0("Lon = ", xy_lon,
                        "<br>Lat = ", xy_lat,
                        "<br>Category = ", names(MHW_colours)[val])
      
    }
    #add Popup
    leafletProxy("map") %>% clearPopups() %>% addPopups(lng = x, lat = y, popup = paste(content))#, "</b></br>", 
                                                                                        # actionButton("showmodal", "Show modal", 
                                                                                                     # onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())')))
  }
  
  observeEvent(input$open_modal, {
    click <- input$map_click
    if(!is.null(click)){
      toggleModal(session, "modal", "open")
    } else {
      showModal(modalDialog(
        title = "Pixel: Lon = NA, Lat = NA",
        "Please first click on a pixel in order to view more information about it."
      ))
    }
  })
  
  # # Leaflet clicking
  # observeEvent(input$map_click, {
  #   toggleModal(session, "modal", "open")
  # })
  
# Leaflet -----------------------------------------------------------------
  
  # The leaflet option
  output$map <- renderLeaflet({
    leaflet(MHW_cat_clim_sub) %>%
      setView(-60, 45, zoom = 5, options = tileOptions(minZoom = 1, maxZoom = 7, noWrap = F)) %>%
      addTiles(group = "OSM",
               options = tileOptions(minZoom = 1, maxZoom = 7, opacity = 0.5, noWrap = F)) #%>%
      # addMouseCoordinates() #%>%
      # fitBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90,
      #           options = tileOptions(minZoom = 1, maxZoom = 7))
  })
  
  
# Outputs -----------------------------------------------------------------
  
  # Click info
  # output$click_info <- renderText({
  #   paste0("x=", input$map_click$lng, "\ny=", input$map_click$lat)
  # })
  
  # Time series plot
  output$tsPlot <- renderPlotly({
    tsPlot()
  })
  
  # Lolli plot
  # output$lolliPlot <- renderPlotly({
  #   lolliPlot()
  # })
  
  # table
  output$table <- renderDataTable({
    datatable(tsTable(), options = list(
      pageLength = 50
    ))
  })
  
  # UI panel
  output$uiModal <- renderUI({
    bsModal(ns('modal'), title = div(id = ns('modalTitle'), pixelLabel()), trigger = 'click2', size = "large",
            # div(id = ns("top_row"),
            fluidPage(
              # title = "",
              tabsetPanel(id = ns("tabs"),
                          tabPanel(title = "Plot",
                                   br(),
                                   withSpinner(plotlyOutput(ns("tsPlot")), type = 6, color = "#b0b7be"),
                                   hr(),
                                   fluidRow(
                                   column(width = 2,
                                          h4("From"),
                                          # This falls over if a user tries to type the dates manually
                                          dateInput(inputId = ns("from"), label = NULL, format = "M d, yyyy",
                                                    value = paste0(lubridate::year(as.Date(input$date_choice)),"-01-01"))),
                                   column(width = 2,
                                          h4("To"),
                                          dateInput(inputId = ns("to"), label = NULL, format = "M d, yyyy",
                                                    value = paste0(lubridate::year(as.Date(input$date_choice)),"-12-31"))),
                                   column(width = 2,
                                          h4("Download"),
                                          downloadButton(outputId = ns("download_clim"),
                                                         label = "Climatology & Threshold (csv)", class = 'small-dl')))
                          ),
                          # tabPanel(title = "Lolli",
                          #          br(),
                          #          plotlyOutput(ns("lolliPlot"))),
                          tabPanel(title = "Table",
                                   br(),
                                   wellPanel(class = 'wellpanel',
                                             DT::dataTableOutput(ns('table'))),
                                   hr(),
                                   fluidRow(
                                     column(width = 2,
                                            h4("Download"),
                                            downloadButton(outputId = ns("download_event"),
                                                           label = "MHW data (csv)", class = 'small-dl')))
                          )
              )#,
              # hr(),
              # fluidRow(
              #   column(2,
              #          h4("Download"),
              #          downloadButton(outputId = ns("download"),
              #                         label = "MHW data (csv)", class = 'small-dl'))#,
                # column(4,
                #        h4("From"),
                #        dateInput(inputId = ns("from"), label = NULL, format = "M d, yyyy",
                #                  value = paste0(lubridate::year(as.Date(input$date_choice)),"-01-01"))),
                # column(6,
                #        h4("To"),
                #        dateInput(inputId = ns("to"), label = NULL, format = "M d, yyyy",
                #                  value = paste0(lubridate::year(as.Date(input$date_choice)),"-12-31")))
                # )
            )
    )
  })
  
  # Download event data
  output$download_event <- downloadHandler(
    filename = function() {
      paste0("event_lon_",downloadEventData()$lon[1],"_lat_",downloadEventData()$lat[1],".csv")
      # paste0(pretty_label(), "_", gsub("-", "", as.character(input$from)), "_", gsub("-", "", as.character(input$to)), ".csv")
    },
    content <- function(file) {
      readr::write_csv(downloadEventData(), file)
    }
  )
  
  # Download clin/thresh
  output$download_clim <- downloadHandler(
    filename = function() {
      paste0("clim_lon_",downloadClimData()$lon[1],"_lat_",downloadClimData()$lat[1],".csv")
      # paste0(pretty_label(), "_", gsub("-", "", as.character(input$from)), "_", gsub("-", "", as.character(input$to)), ".csv")
    },
    content <- function(file) {
      readr::write_csv(downloadClimData(), file)
    }
  )
}
# cat("\nmap_server.R finished")


