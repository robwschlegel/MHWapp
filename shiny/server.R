# MHWapp/shiny/server.R

server <- function(input, output, session){
  
  # Testing...
  # input <- data.frame(date = as.Date("1989-07-15"),
  #                     layer = "MHW Category")

  # Reactive UI -------------------------------------------------------------

  ### Reactive category filters
  categories <- reactiveValues(categories = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))
  
  ### Reactive value to track which year has been selected
  #### This is meant to help avoid loading data unnecessarily
  yearReact <- reactiveValues(year = 1)#lubridate::year(Sys.Date()))
  
  ### Reactive value boxes
  output$percT <- renderUI({
    req(input$layer)
    value_box_cat("Total cover", input$layer, mapCover())
  })
  output$percI <- renderUI({
    req(input$layer)
    value_box_cat("I Moderate", input$layer, mapCover())
  })
  output$percII <- renderUI({
    req(input$layer)
    value_box_cat("II Strong", input$layer, mapCover())
  })
  output$percIII <- renderUI({
    req(input$layer)
    value_box_cat("III Severe", input$layer, mapCover())
  })
  output$percIV <- renderUI({
    req(input$layer)
    value_box_cat("IV Extreme", input$layer, mapCover())
  })
  output$percV <- renderUI({
    req(input$layer)
    if(input$layer == "MCS Category"){
      value_box_cat("V Ice", input$layer, mapCover())
    } else {
      NULL
    }
  })

  ### React to the reaction
  output$valueBoxes <- renderUI({
    req(input$layer)
    if(input$layer == "MCS Category"){
      layout_column_wrap(
        width = "200px",
        height = "130px",
        height_mobile = "100px",
        fill = FALSE, fillable = TRUE,
        uiOutput("percT"), uiOutput("percI"), uiOutput("percII"), uiOutput("percIII"), uiOutput("percIV"), uiOutput("percV"))
      } else {
        layout_column_wrap(
          width = "200px",
          height = "130px",
          height_mobile = "100px",
          fill = FALSE, fillable = TRUE,
          uiOutput("percT"), uiOutput("percI"), uiOutput("percII"), uiOutput("percIII"), uiOutput("percIV"))
    }
  })
  
  
  # Map data ----------------------------------------------------------------
  
  ### Shiny-projected raster data
  rasterProj <- reactive({
    req(input$date, input$layer)
    if(input$layer == "MCS Category"){
      rast_dir <- paste0("cat_clim/MCS/")
      rast_name <- paste0("/cat.clim.MCS.",input$date,".tif")
    } else {
      rast_dir <- paste0("cat_clim/")
      rast_name <- paste0("/cat.clim.",input$date,".tif")
    }
    rasterProj <- raster::raster(paste0(rast_dir, substr(input$date, 1, 4), rast_name))
    return(rasterProj)
  })

  ### Percent ocean coverage
  mapCover <- reactive({
    req(input$date, input$layer)
    if(input$layer == "MCS Category"){
      event_type <- "MCS"
    } else {
      event_type <- "MHW"
    }
    # if( != yearReact$year)
    mapCoverAll <- readRDS(paste0("OISST/annual_summary/",event_type,"_cat_daily_",
                             lubridate::year(input$date),".Rds"))
    mapCover <- mapCoverAll |> 
      filter(t == input$date) |> 
      dplyr::select(t, category, cat_area_prop) |>
      rbind(data.frame(t = input$date, category = "Total cover",
                       cat_area_prop = sum(filter(mapCoverAll, t == input$date)$cat_area_prop)))
    return(mapCover)
  })
  
  
  # Leaflet -----------------------------------------------------------------
  
  ### Observer to change colour palette accordingly
  pal_react <- reactive({
    req(input$layer)
    if(input$layer == "MCS Category"){
      # event_palette <- MCS_colours
      colorNumeric(palette = MCS_colours, domain = c(1,2,3,4,5), na.color = NA)
    } else {
      # event_palette <- MHW_colours
      colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)
    }
    })
  
  ### The leaflet base
  output$leaf_map <- renderLeaflet({
    req(input$layer)
    
    # The base 
    # TODO: Consider adding a hover to wake scroll functionality to prevent accidental scroll zooming
    # https://bhaskarvk.github.io/leaflet.extras/reference/sleep.html
    leaflet(options = leafletOptions(zoomControl = FALSE)) |> 
      setView(lng = initial_lon, lat = initial_lat, zoom = initial_zoom,
              options = tileOptions(minZoom = 0, maxZoom = 8, noWrap = F)) |> 
      addScaleBar(position = "topright") |> addTiles()
  })
  
  ### The raster layer
  observeEvent(c(input$layer, input$date, input$iceMask,
                 input$moderate_filter, input$strong_filter,
                 input$severe_filter, input$extreme_filter), {
                   leafletProxy("leaf_map") |> 
                     addRasterImage(rasterProj(), 
                                    colors = pal_react(), layerId = "map_raster",
                                    project = FALSE, opacity = 0.8) |> 
                 removeImage("ice")
                   if(input$iceMask){
                     leafletProxy("leaf_map") |>
                       addRasterImage(ice_proj, colors = "snow", layerId = "ice", project = FALSE, opacity = 0.7)
                   }
                 })

  
  # Pop-ups -----------------------------------------------------------------
  
  ### Observer to show pop-ups on click
  observeEvent(c(input$leaf_map_click), {
    req(input$leaf_map_click)
    click <- input$leaf_map_click
    showpos(x = click$lng, y = click$lat)
  })
  
  
  # Time series data --------------------------------------------------------
  
  ### Pixel data
  pixelData <- reactive({
    req(input$leaf_map_click)
    if(is.null(input$leaf_map_click)){
      return()
    } else {
      # tester...
      # x <- 10; y <- -5
      
      # Get OISST pixel coords
      click <- input$leaf_map_click
      x <- click$lng; y <- click$lat
      xy_click <- c(x, y)
      xy_wrap <- lon_wrap(xy_click)
      xy <- c(lon_OISST[which(abs(lon_OISST - xy_wrap[1]) == min(abs(lon_OISST - xy_wrap[1])))][1],
              lat_OISST[which(abs(lat_OISST - xy_wrap[2]) == min(abs(lat_OISST - xy_wrap[2])))][1])
      
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
    }
  })
  
  
  # Figures/tables ----------------------------------------------------------
  
  ### Create static time series plot
  tsPlot <- reactive({
    req(input$from_to[1]); req(input$from_to[2])
    
    # Get time series
    ts_data <- pixelData()$ts
    
    # Get coordinates
    xy <- c(pixelData()$lon, pixelData()$lat)
    if(xy[1] >= 0) xy_lon <- paste0(abs(xy[1]),"°E")
    if(xy[1] < 0) xy_lon <- paste0(abs(xy[1]),"°W")
    if(xy[2] >= 0) xy_lat <- paste0(abs(xy[2]),"°N")
    if(xy[2] < 0) xy_lat <- paste0(abs(xy[2]),"°S")
    
    # Labels
    x_lab <- NULL; y_lab <- "Temperature (°C)"; title_lab <- paste0("Lon: ",xy_lon,"   Lat: ",xy_lat)
    
    # Check that it's not empty
    if(length(ts_data$temp) == 1){
      p <- ggplot() + geom_text(aes(x = 0, y = 0,label = "Please select an ocean pixel.")) +
        labs(x = x_lab, y = y_lab, title = title_lab)
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
    
    # Get days during MHW/MCS
    # ts_data_MHW <- plyr::ddply(event_data_sub, c("event_no"), ts_event_join, ts_data_sub, .parallel = F)
    # ts_data_MCS <- plyr::ddply(event_MCS_data_sub, c("event_no"), ts_event_join, ts_data_sub, .parallel = F)
    
    # Get constant y-axis values
    # ts_y_min <- min(ts_data_sub$temp, na.rm = T)
    # ts_y_max <- max(ts_data_sub$temp, na.rm = T)
    
    # The base
    p <- ggplot(data = ts_data_sub, aes(x = t, y = temp)) +
      labs(x = x_lab, y = y_lab, title = title_lab) +
      scale_x_date(expand = c(0, 0), date_labels = "%b %Y", limits = c(input$from_to[1], input$from_to[2])) +
      theme(panel.border = element_rect(colour = "black", fill = NA))
    
    # Add flame categories as needed
    ## MHWs
    # if(any(ts_data_sub$temp > ts_data_sub$thresh)){
    if(any(ts_data_sub$temp > ts_data_sub$thresh)){
      p <- p + heatwaveR::geom_flame(aes(y2 = thresh), fill = "#ffc866", n = 5, n_gap = 2)
      # p <- p + geom_segment(data = ts_data_sub, linewidth = 1,
      #                   aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#ffc866")
    }
    if(any(ts_data_sub$temp > ts_data_sub$thresh_2x)){
      p <- p + heatwaveR::geom_flame(aes(y2 = thresh_2x), fill = "#ff6900")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp > thresh_2x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#ff6900")
    }
    if(any(ts_data_sub$temp > ts_data_sub$thresh_3x)){
      p <- p + heatwaveR::geom_flame(aes(y2 = thresh_3x), fill = "#9e0000")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp > thresh_3x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#9e0000")
    }
    if(any(ts_data_sub$temp > ts_data_sub$thresh_4x)){
      p <- p + heatwaveR::geom_flame(aes(y2 = thresh_4x), fill = "#2d0000")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp > thresh_4x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#2d0000")
    }
    ## MCSs
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS)){
      p <- p + heatwaveR::geom_flame(aes(y = thresh_MCS, y2 = temp), fill = "#C7ECF2", n = 5, n_gap = 2)
      # p <- p + geom_segment(data = ts_data_sub,
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#C7ECF2")
    }
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS_2x)){
      p <- p + heatwaveR::geom_flame(aes(y = thresh_MCS_2x, y2 = temp), fill = "#85B7CC")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp < thresh_MCS_2x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#85B7CC")
    }
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS_3x)){
      p <- p + heatwaveR::geom_flame(aes(y = thresh_MCS_3x, y2 = temp), fill = "#4A6A94")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp < thresh_MCS_3x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#4A6A94")
    }
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS_4x)){
      p <- p + heatwaveR::geom_flame(aes(y = thresh_MCS_4x, y2 = temp), fill = "#111433")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp < thresh_MCS_4x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#111433")
    }
    # if(any(ts_data_sub$category == "V Ice")){
    #   p <- p + geom_segment(data = filter(ts_data_sub, category == "V Ice"),
    #                         aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#D8BFD8")
    # }
    
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
    pp <- ggplotly(p, tooltip = "text", dynamicTicks = F) |> plotly::layout(hovermode = "x")
    pp
  })
  
  ### Create lolliplot
  lolliPlot <- reactive({
    
    # Filter time series based on dates
    ts_data <- pixelData()$ts
    ts_data_sub <- ts_data %>%
      dplyr::filter(t >= input$from_to[1], t <= input$from_to[2])
    
    # Get coordinates
    xy <- c(pixelData()$lon, pixelData()$lat)
    if(xy[1] >= 0) xy_lon <- paste0(abs(xy[1]),"°E")
    if(xy[1] < 0) xy_lon <- paste0(abs(xy[1]),"°W")
    if(xy[2] >= 0) xy_lat <- paste0(abs(xy[2]),"°N")
    if(xy[2] < 0) xy_lat <- paste0(abs(xy[2]),"°S")
    
    # Labels
    x_lab <- NULL; y_lab <- "Temperature (°C)"; title_lab <- paste0("Lon: ",xy_lon,"   Lat: ",xy_lat)
    
    # Prep data
    event_data <- pixelData()$event
    event_data_sub <- event_data |> 
      mutate(category = paste0("MHW_",category)) |> 
      dplyr::filter(date_peak >= input$from_to[1], date_peak <= input$from_to[2])
    event_MCS_data <- pixelData()$event_MCS
    event_MCS_data_sub <- event_MCS_data |> 
      mutate(category = paste0("MCS_",category)) |> 
      dplyr::filter(date_peak >= input$from_to[1], date_peak <= input$from_to[2])
    
    # Get y_lims
    y_lims <- c(0, 1)
    if(length(event_data_sub$date_start) > 0) y_lims[2] <- max(event_data_sub$intensity_max)*1.1
    if(length(event_MCS_data_sub$date_start) > 0) y_lims[1] <- min(event_MCS_data_sub$intensity_max)*1.1
    if(length(event_MCS_data_sub$date_start) > 0 & length(event_data_sub$date_start) == 0) y_lims[2] <- 0
    
    # The base figure
    p <- ggplot() +
      labs(x = x_lab, y = "Max. Intensity (°C)", title = title_lab) +
      scale_fill_manual(values = colours_double) +
      scale_y_continuous(limits = y_lims, expand = c(0, 0)) +
      scale_x_date(expand = c(0, 0), date_labels = "%b %Y", limits = c(input$from_to[1], input$from_to[2])) +
      theme(panel.border = element_rect(colour = "black", fill = NA), legend.position = "none")
    
    # Add lollis as necessary
    if(length(event_data_sub$date_start) > 0){
      suppressWarnings(
        p <- p + geom_segment(data = event_data_sub,
                              aes(x = date_peak, xend = date_peak, y = intensity_max, yend = 0)) +
          geom_hline(yintercept = 0) +
          geom_point(data = event_data_sub, shape = 21, size = 4, #fill = "salmon",
                     aes(x = date_peak, y = intensity_max, fill = category,
                         text = paste0("Event: ",event_no,
                                       "<br>Category: ",gsub("MHW_", "", category),
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
          geom_point(data = event_MCS_data_sub, shape = 21, size = 4, #fill = "steelblue1",
                     aes(x = date_peak, y = intensity_max, fill = category,
                         text = paste0("Event: ",event_no,
                                       "<br>Category: ",gsub("MCS_", "", category),
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
    event_data <- pixelData()$event |> 
      dplyr::filter(date_start >= input$from_to[1], date_start <= input$from_to[2]) |> 
      mutate(Event = "MHW")
    event_MCS_data <- pixelData()$event_MCS |> 
      dplyr::filter(date_start >= input$from_to[1], date_start <= input$from_to[2]) |> 
      mutate(Event = "MCS")
    event_res <- rbind(event_data, event_MCS_data) |> 
      dplyr::rename(Lon = lon,
                    Lat = lat,
                    '#' = event_no,
                    Category = category,
                    Duration = duration,
                    'Start Date' = date_start,
                    'Peak Date' = date_peak,
                    'End Date' = date_end,
                    'Mean Intensity' = intensity_mean,
                    'Max. Intensity' = intensity_max,
                    'Cum. Intensity' = intensity_cumulative) |> 
      dplyr::arrange(`Peak Date`) |> 
      dplyr::select(Event, Lon, Lat, `#`, Category, Duration, `Start Date`, `Peak Date`, `End Date`, 
                    `Mean Intensity`, `Max. Intensity`, `Cum. Intensity`)
    event_res
  })
  
  ### Static time series plot
  output$tsPlot <- renderPlot({
    if(is.null(input$leaf_map_click)){
      ggplot() +
        geom_text(aes(x = 1, y = 1,
                      label = "Please click a pixel on the map to visualise data.")) + theme_void()
    } else {
      tsPlot()
    }
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
  output$tsTable <- renderDataTable({
    DT::datatable(tsTable(),
                  options = list(pageLength = 10))
  })
  

  # Downloads ---------------------------------------------------------------

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
  # tester...
  # date_filter_from <- as.Date("2023-07-16"); date_filter_to <- as.Date("2023-07-16")
  downloadMapData <- reactive({
    date_filter_from <- as.Date(input$map_download_date[1])
    date_filter_to <- as.Date(input$map_download_date[2])
    date_seq <- seq(date_filter_from, date_filter_to, by = "day")
    date_seq_years <- sapply(strsplit(as.character(date_seq), "-"), "[[", 1)
    if(input$layer == "MHW Category"){
      date_seq_files <- paste0("cat_clim/",date_seq_years,"/cat.clim.",date_seq,".Rda")
      map_data <- purrr::map_dfr(date_seq_files, readRDS_date)
    } else if(input$layer == "MCS Category"){
      date_seq_files <- paste0("cat_clim/MCS/",date_seq_years,"/cat.clim.MCS.",date_seq,".Rds")
      map_data <- purrr::map_dfr(date_seq_files, readRDS_date)
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
  
  # During testing...
  # session$onSessionEnded(stopApp)
}

