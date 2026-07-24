# MHWapp/shiny/server.R

server <- function(input, output, session){
  
  # Testing...
  # input <- data.frame(date = as.Date("2024-07-15"),
  #                     layer = "MHW Category",
  #                     baseLine = "1982-2011")
  # input <- data.frame(from_to = c(max(current_dates)-365, max(current_dates)))
  

  # Reactive UI -------------------------------------------------------------

  ### Reactive category filters
  categories <- reactiveValues(categories = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))
  
  ### Reactive value to track which year has been selected
  #### This is meant to help avoid loading data unnecessarily
  yearReact <- reactiveValues(year = 1)#lubridate::year(Sys.Date()))
  
  observeEvent(c(input$date), {
    if(input$date+365 < max(current_dates)){
      daterangepicker::updateDaterangepicker(session, "from_to",
                                             start = as.Date(input$date)-365,
                                             end = as.Date(input$date)+365)
    }
  })
  
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
        width = 1/6,
        min_height = "130px",
        height_mobile = "100px",
        heights_equal = "all",
        fill = TRUE, fillable = FALSE,
        uiOutput("percT"), uiOutput("percI"), uiOutput("percII"), uiOutput("percIII"), uiOutput("percIV"), uiOutput("percV"))
    } else {
      layout_column_wrap(
        width = 1/5,
        min_height = "130px",
        height_mobile = "100px",
        heights_equal = "all",
        fill = TRUE, fillable = FALSE,
        uiOutput("percT"), uiOutput("percI"), uiOutput("percII"), uiOutput("percIII"), uiOutput("percIV"))
    }
  })
  
  
  # Map data ----------------------------------------------------------------
  
  ### Shiny-projected raster data
  rasterProj <- reactive({
    req(input$baseLine, input$date, input$layer)
    if(input$layer == "MCS Category"){
      rast_dir <- paste0("cat_clim/MCS/")
      rast_name <- paste0("/cat.clim.MCS.",input$date,".",input$baseLine,".tif")
    } else {
      rast_dir <- paste0("cat_clim/")
      rast_name <- paste0("/cat.clim.",input$date,".",input$baseLine,".tif")
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
                                  input$baseLine,"_",lubridate::year(input$date),".Rds"))
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
    leaflet(options = leafletOptions(zoomControl = TRUE)) |> 
      setView(lng = initial_lon, lat = initial_lat, zoom = initial_zoom,
              options = tileOptions(minZoom = 0, maxZoom = 8, noWrap = F)) |> 
      addScaleBar(position = "topright") |> addTiles()
  })
  
  ### The raster layer
  observeEvent(c(input$baseLine, input$layer, input$date, input$iceMask,
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
  
  ### Map click
  click <- reactive({
    if(is.null(input$leaf_map_click)){
      click <- data.frame(lng = NA, lat = NA)
    } else {
      click <- data.frame(lng = input$leaf_map_click$lng,
                          lat = input$leaf_map_click$lat)
    }
    })
  ### Attempted throttling not behaving as expected
  # click_d <- click |> debounce(1000)
  # click_t <- click |> throttle(1000)
  
  ### Observer to show pop-ups on click
  observeEvent(c(input$leaf_map_click), {
    req(input$leaf_map_click)
    # click <- input$leaf_map_click
    click <- click()
    showpos(x = click$lng, y = click$lat)
  })
  
  
  # Time series data --------------------------------------------------------
  
  ### Pixel data
  pixelData <- reactive({
    req(input$leaf_map_click)
    if(is.null(input$leaf_map_click)){ # Reactivate once new server is established
      return()
    } else { # Reactivate once new server is established
      # testers...
      # x <- 10; y <- -5
      # x <- -27.125; y <- 25.625
      
      # Get OISST pixel coords
      click <- input$leaf_map_click
      x <- click$lng; y <- click$lat
      xy_click <- c(x, y)
      xy_wrap <- lon_wrap(xy_click)
      xy <- c(lon_OISST[which(abs(lon_OISST - xy_wrap[1]) == min(abs(lon_OISST - xy_wrap[1])))][1],
              lat_OISST[which(abs(lat_OISST - xy_wrap[2]) == min(abs(lat_OISST - xy_wrap[2])))][1])
      
      # Grab time series data
      ts_data <- sst_seas_thresh_ts(lon_step = xy[1], lat_step = xy[2], base_years = input$baseLine) |> 
        dplyr::mutate(diff = thresh - seas,
                      thresh_2x = thresh + diff,
                      thresh_3x = thresh_2x + diff,
                      thresh_4x = thresh_3x + diff,
                      diff_MCS = thresh_MCS - seas,
                      thresh_MCS_2x = thresh_MCS + diff_MCS,
                      thresh_MCS_3x = thresh_MCS_2x + diff_MCS,
                      thresh_MCS_4x = thresh_MCS_3x + diff_MCS)
      
      # Grab event data
      MHW_event_files_base <- MHW_event_files[grepl(input$baseLine, MHW_event_files)]
      MCS_event_files_base <- MCS_event_files[grepl(input$baseLine, MCS_event_files)]
      event_data <- heatwave3::hw3_export(MHW_event_files_base[which(lon_OISST == xy[1])], lat_range = xy[2]) |> 
        mutate(date_start = as.Date(date_start, origin = "1982-01-01"),
               date_peak = as.Date(date_peak, origin = "1982-01-01"),
               date_end = as.Date(date_end, origin = "1982-01-01"),
               category = factor(category, levels = c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                                 labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")))
      event_MCS_data <- heatwave3::hw3_export(MCS_event_files_base[which(lon_OISST == xy[1])], lat_range = xy[2]) |> 
        mutate(date_start = as.Date(date_start, origin = "1982-01-01"),
               date_peak = as.Date(date_peak, origin = "1982-01-01"),
               date_end = as.Date(date_end, origin = "1982-01-01"),
               category = factor(category, levels = c("I Moderate", "II Strong", "III Severe", "IV Extreme", "V Ice"),
                                 labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme", "V Ice")))
      
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
      p <- ggplot() + geom_text(aes(x = 0, y = 0, label = "Please select an ocean pixel.")) +
        labs(x = x_lab, y = y_lab, title = title_lab) + theme_grey(base_size = 40)
      return(p)
    }
    
    # Filter time series based on dates
    ts_data_sub <- ts_data |> 
      dplyr::filter(t >= input$from_to[1], t <= input$from_to[2])
    
    # Event data prep
    event_data <- pixelData()$event
    event_data_sub <- event_data |>
      dplyr::filter(date_peak >= input$from_to[1], date_peak <= input$from_to[2])
    event_MCS_data <- pixelData()$event_MCS
    event_MCS_data_sub <- event_MCS_data |>
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
      theme_grey(base_size = 20) +
      theme(panel.border = element_rect(colour = "black", fill = NA))
    
    # Add flame categories as needed
    ## MHWs
    # if(any(ts_data_sub$temp > ts_data_sub$thresh)){
    if(any(ts_data_sub$temp > ts_data_sub$thresh)){
      p <- p + heatwave3::geom_flame3(aes(y2 = thresh), fill = "#ffc866", n = 5, n_gap = 2)
      # p <- p + geom_segment(data = ts_data_sub, linewidth = 1,
      #                   aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#ffc866")
    }
    if(any(ts_data_sub$temp > ts_data_sub$thresh_2x)){
      p <- p + heatwave3::geom_flame3(aes(y2 = thresh_2x), fill = "#ff6900")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp > thresh_2x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#ff6900")
    }
    if(any(ts_data_sub$temp > ts_data_sub$thresh_3x)){
      p <- p + heatwave3::geom_flame3(aes(y2 = thresh_3x), fill = "#9e0000")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp > thresh_3x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#9e0000")
    }
    if(any(ts_data_sub$temp > ts_data_sub$thresh_4x)){
      p <- p + heatwave3::geom_flame3(aes(y2 = thresh_4x), fill = "#2d0000")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp > thresh_4x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#2d0000")
    }
    ## MCSs
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS)){
      p <- p + heatwave3::geom_flame3(aes(y = thresh_MCS, y2 = temp), fill = "#C7ECF2", n = 5, n_gap = 2)
      # p <- p + geom_segment(data = ts_data_sub,
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#C7ECF2")
    }
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS_2x)){
      p <- p + heatwave3::geom_flame3(aes(y = thresh_MCS_2x, y2 = temp), fill = "#85B7CC")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp < thresh_MCS_2x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#85B7CC")
    }
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS_3x)){
      p <- p + heatwave3::geom_flame3(aes(y = thresh_MCS_3x, y2 = temp), fill = "#4A6A94")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp < thresh_MCS_3x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#4A6A94")
    }
    if(any(ts_data_sub$temp < ts_data_sub$thresh_MCS_4x)){
      p <- p + heatwave3::geom_flame3(aes(y = thresh_MCS_4x, y2 = temp), fill = "#111433")
      # p <- p + geom_segment(data = filter(ts_data_sub, temp < thresh_MCS_4x),
      #                       aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#111433")
    }
    if(any(ts_data_sub$category == "V Ice")){
      p <- p + geom_segment(data = filter(ts_data_sub, category == "V Ice"),
                            aes(x = t, xend = t, y = ts_y_min, yend = ts_y_max), colour = "#D8BFD8")
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
    # NB: Removed for now as these are most interesting with plotly
    # if(length(event_data_sub$date_start) > 0){
    #   suppressWarnings( # text aes plotly
    #     p <- p + geom_rug(data = event_data_sub, sides = "b", colour = "salmon", size = 2,
    #                       aes(x = date_peak, y = min(ts_data_sub$temp),
    #                           text = paste0("Event: ",event_no,
    #                                         "<br>Duration: ",duration," days",
    #                                         "<br>Start Date: ", date_start,
    #                                         "<br>Peak Date: ", date_peak,
    #                                         "<br>End Date: ", date_end,
    #                                         "<br>Mean Intensity: ",intensity_mean,"°C",
    #                                         "<br>Max. Intensity: ",intensity_max,"°C",
    #                                         "<br>Cum. Intensity: ",intensity_cumulative,"°C")))
    #   )
    # }
    # if(length(event_MCS_data_sub$date_start) > 0){
    #   suppressWarnings( # text aes plotly
    #     p <- p + geom_rug(data = event_MCS_data_sub, sides = "b", colour = "steelblue1", size = 2,
    #                       aes(x = date_peak, y = min(ts_data_sub$temp),
    #                           text = paste0("Event: ",event_no,
    #                                         "<br>Duration: ",duration," days",
    #                                         "<br>Start Date: ", date_start,
    #                                         "<br>Peak Date: ", date_peak,
    #                                         "<br>End Date: ", date_end,
    #                                         "<br>Mean Intensity: ",intensity_mean,"°C",
    #                                         "<br>Max. Intensity: ",intensity_max,"°C",
    #                                         "<br>Cum. Intensity: ",intensity_cumulative,"°C")))
    #   )
    # }
    p
  })
  
  ### Create interactive time series plot
  # tsPlotly <- reactive({
  #   
  #   # Grab static plot
  #   p <- tsPlot()
  #   suppressWarnings( # text aes plotly
  #     p <- p +
  #       geom_segment(aes(x = input$date[1],
  #                        xend = input$date[1],
  #                        y = min(temp),
  #                        yend = max(temp),
  #                        text = "Date shown"),
  #                    colour = "limegreen")
  #   )
  #   
  #   # Convert to plotly
  #   # NB: Setting dynamicTicks = T causes the flames to be rendered incorrectly
  #   pp <- ggplotly(p, tooltip = "text", dynamicTicks = F) |> plotly::layout(hovermode = "x")
  #   pp
  # })
  
  ### Create lolliplot
  lolliPlot <- reactive({
    
    # Filter time series based on dates
    ts_data <- pixelData()$ts
    ts_data_sub <- ts_data |>
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
      theme_grey(base_size = 15) +
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
  # Reactivate once server is updated
  lolliPlotly <- reactive({
    pp <- ggplotly(lolliPlot(), tooltip = "text", dynamicTicks = F)
    pp
  })
  
  ### Create data table
  # Reactivate once server is updated
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
    if(is.null(input$leaf_map_click)){ # Reactivate once server is updated
      ggplot() +
        geom_text(aes(x = 1, y = 1,
                      label = "Please click a pixel on the map to visualise data.")) +  # Reactivate once server is updated
                      # label = "Currently under construction. Check back soon!")) +
        theme_void(base_size = 20)
    } else {  # Reactivate once server is updated
      tsPlot()  # Reactivate once server is updated
    }  # Reactivate once server is updated
  })
  
  ### Interactive time series plot
  # output$tsPlotly <- renderPlotly({
  #   tsPlotly()
  # })

  ### Lolli plot
  output$lolliPlotly <- renderPlotly({
    lolliPlotly()
  })
  
  ### Event metrics table
  output$tsTable <- renderDataTable({
    DT::datatable(tsTable(),
                  options = list(pageLength = 10))
  })
  

  # About -------------------------------------------------------------------

  observeEvent(input$about_btn, {
    showModal(modalDialog(
      title = NULL, size = "xl",
      h2(tags$b("Welcome!")),
      br(),
      p("You have arrived at the Marine Heatwave (MHW) Tracker. This web application shows the occurrence of MHWs 
                    around the world in near-real-time (roughly a one-two day delay). The Tracker also shows the historic daily records
                    for the whole planet going back to January 1st, 1982. It is also possible to look at the current and historic records
                    for Marine Cold-Spells (MCS). Please see the 'How do I use this?' section below for details."),
      br(),
      h2(tags$b("What is a Marine Heatwave (MHW)?")),
      br(),
      p("A MHW is generally defined as when the temperature in a given location is in the top 10% of temperatures 
                    recorded during that time of year for at least 5 straight days. For example, if the coastal waters off
                    Durban in South Africa are roughly 22°C on any given April 1st, if temperatures in excess of perhaps 28°C 
                    are recorded there in 2019 over March 28th to April 10th, this could be flagged as a MHW. This is a definition
                    for MHWs first put forward by ", 
        a(target = '_blank', rel = 'noopener noreferrer', 
          href = "https://www.sciencedirect.com/science/article/pii/S0079661116000057", "Hobday et al. (2016)"),
        ". For a more detailed explanation with visuals please follow this ", 
        a(target = '_blank', rel = 'noopener noreferrer',
          href = "http://www.marineheatwaves.org/all-about-mhws.html", "link"), 
        ". An interactive demo for how to detect a MHW is available at this ",
        a(target = '_blank', rel = 'noopener noreferrer',
          href = "http://193.50.85.71:3838/demoMHW/", "link"),"."),
      br(),
      h2(tags$b("What do these colours mean?")),
      br(),
      p("In the map panel of the Marine Heatwave Tracker we can see that there is a particular colour palette being used. Each of 
                      these four colours corresponds to increasing categories of MHWs as first proposed in ", 
        a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.jstor.org/stable/26542662", "Hobday et al. (2018)"), 
        ". The first category, 'I Moderate', is somewhat common and no category one MHWs have yet been recorded as causing lasting 
                      ecological or financial damage. The second category of MHWs, 'II Strong', are increasing in occurrence the most rapidly 
                      of the four categories and are on course to become as common as category one MHWs were when record keeping began in the 80's. 
                      Fortunately, category two MHWs have rarely been documented to cause lasting damage. The third category, 'III Severe', 
                      are less common but can be devastating when they persist for more than a month. The last category, 'IV Extreme', 
                      is thankfully a rare occurrence. It is known that less than three months of a persistent category four MHW can wipe 
                      out entire coastal ecosystems. If we switch the map layer to MCSs we will see a fifth category 'V Ice'. This was an 
                      addition to the category naming convention from ",
        a(target = '_blank', rel = 'noopener noreferrer', 
          href = "https://www.sciencedirect.com/science/article/abs/pii/S0079661121001683", "Schlegel et al. (2021)"),".
                      This category shows when a cold event is being detected at a threshold below -1.7°C. This basically means we are 
                      just looking at ice that is a bit colder than normal. Not really a proper MCS by the intended spirit of the definition."),
      br(),
      h2(tags$b("Why should I care?")),
      br(),
      p("If you've found your way to this website then you are likely interested in the effects we are having on the worlds 
                    oceans and probably have your own personal reasons to care about their health. There are many anthropogenic (human caused) 
                    threats to the health of the oceans, which include but are not limited to: over-fishing, chemical run-off from land, 
                    and climate change in the form of extreme warm water events known as MHWs. All of these different threats may impact 
                    the ocean in different ways, but one of the main concerns is that we are changing the oceans so much that we will not 
                    be able to repair them ourselves. MHWs are not new to our oceans, but our ability to quantify them and put them up on 
                    a website like this is. Thanks to this tool we can now see for ourselves in near-real-time where in the world extreme 
                    temperatures may be threatening the parts of the oceans where we work, live, and play. It is our hope that this website 
                    can be used around the world to help anyone that is interested to see if the changes they have noticed in their ocean are
                    due to a MHW or not."),
      br(),
      h2(tags$b("How do I use this?")),
      br(),
      p("This site works similarly to the Google maps we use in day-to-day life. Click and drag to move around the world. 
                    Use the mouse scroll wheel, or +/- buttons, to zoom in or out. The 'Date' box in the 'Controls' panel tells us which 
                    date is being shown on the map. Clicking in this box we can choose any date from the near present back to January 1st, 1982.
                    Click on 'Map layer' to bring up the options for which type of data to visualise. Curently one may choose between daily MHW or 
                    MCS data. Please see the 'What are these different map layers?' section below for more detail. Click on the 'Map data' button 
                    to download data for the chosen map layer over a range of dates. Note that downloads are limited to 62 days of data. To disable
                    white ice mask on the map, click on 'Ice mask' and toggle the switch off."),
      p("If one of the pixels on the map catches your eye, clicking on it will give more information in the window below the map. 
                    There we can find three panels. The first, 'Time series', shows the daily data for the pixel we clicke don. Using the 
                    'Date range' selector we can extend the time period we want to display. The second tab, 'Lolliplot', shows all of the 
                    MHW/MCS in the selected date range, but focuses on just the events. This plot is interactive and we can move our mouse
                    over the points to see more info. Lastly, the 'Table' tab shows the metrics for all of the events in the chosen date range. 
                    We can sort the events by clicking on the different columns. We can download the climatology & threshold data as well as 
                    the MHW/MCS data for the chosen pixel with the download buttons at the bottom of the control panel. 
                    To download the temperature data please go to the", 
        a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.ncdc.noaa.gov/oisst", "NOAA website"),"."),
      br(),
      h2(tags$b("Where do these data come from?")),
      br(),
      p("The global satellite product used in the Marine Heatwave Tracker is the daily Optimally Interpolated Sea Surface Temperature
                    (OISST) v2.1 data on an even 1/4 degree grid that may be downloaded from the National Oceanic and Atmospheric Administration (NOAA). 
                    The daily values go back as far as September 1st, 1981, but the MHW Tracker only hosts results starting on January 1st, 1982 
                    as this is the first full year of data. More information about these data may be found ", 
        a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.ncdc.noaa.gov/oisst", "here"),"."),
      p("Please note that the data are released in near-real-time, and then go through a second layer of quality control that
                    takes roughly two weeks. Therefore any MHW results shown in this app within two weeks of the current date may be 
                    subject to minor changes. All MHW results older than two weeks may be taken as final. In practice, the difference between
                    the preliminary results and the final results are almost always negligible."),
      p("A tutorial for how to download these data in R may be found ", 
        a(target = '_blank', rel = 'noopener noreferrer',
          href = "https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html", "here"),
        ", and a tutorial on how to calculate MHWs from these data in R may be found ",
        a(target = '_blank', rel = 'noopener noreferrer',
          href = "https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html", "here"),"."),
      p("The MHW/MCS results on the Marine Heatwave Tracker are calculated with the R version of the Hobday et al. (2016, 2018) definition 
                      briefly outlined above. Extensive documentation on the R code may be found ",
        a(target = '_blank', rel = 'noopener noreferrer',
          href = "https://robwschlegel.github.io/heatwaveR/index.html", "here"),". This algorithm is also available for", 
        a(target = '_blank', rel = 'noopener noreferrer',
          href = "https://github.com/ecjoliver/marineHeatWaves", "python ")," and ",
        a(target = '_blank', rel = 'noopener noreferrer',
          href = "https://github.com/ZijieZhaoMMHW/m_mhw1.0", "MATLAB"),
        ". The climatology period used for calculating MHWs and MCS is the current recommended WMO 
                      climate normal of 1991-01-01 to 2020-12-31."),
      br(),
      h2(tags$b("What are these different map layers?")),
      br(),
      p("The default map layer, 'MHW Category', shows the categories of the MHW occurring at each pixel on the chosen day. 
                      One may also choose 'MCS Category', which shows the same, but for cold events. Annual summaries and daily anomaly values
                      also exist in the MHW Tracker database, but this functionality has not yet been reintroduced to v2.0 of the Tracker."),
      ### To add back in once these layers have been reactivated. Also add ability to download annual summary .png files
      # p("The 'Summary' layer shows the highest category MHW that occurred at each pixel over the course of the chosen year.
      # The 'Anomaly' layer shows the temperature anomalies for the chosen day against the daily climatology from 
      # 1982-01-01 to 2011-12-31. The various 'Trend' layers show the annual trend in the change of the MHW metric referred to.
      # For example, a value of 0.2 at a pixel when looking at the 'Trend: Count' layer would mean that for the last 30+ years
      # MHWs at that pixel have been increasing by a count of 0.2 every year. Or in other words, every five years (1/0.2) there is
      # one additional MHW occurring in that pixel per year. For another example let’s look at the 'Trend: Duration' layer.
      # If we see a pixel here with a value of one, this means that, on average, MHWs in this pixel are becoming one day longer each year.
      # Conversely, a value of negative one, as seen throughout the eastern Pacific, would mean that MHWs are shortening by one day each year.
      # The data for these trends are the underlying data for Figure 1 in ",
      #   a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.nature.com/articles/s41467-018-03732-9/", "Oliver et al., 2018"),
      #   " and a more detailed description for them may be found in that publication."),
      br(),
      h2(tags$b("Who made this?")),
      br(),
      p("The Marine Heatwave Tracker was developed by ", 
        a(target = '_blank', rel = 'noopener noreferrer', href = "https://theoceancode.netlify.app/", "Robert Schlegel"),
        " and is an outcome of the ", 
        a(target = '_blank', rel = 'noopener noreferrer', 
          href = "http://www.marineheatwaves.org/", "Marine Heatwaves International Working Group"),
        ". Therefore, this work has been directly and indirectly supported by several governmental, academic, and private
                      organisations/funding bodies. The full list with links to further information is provided below in alphabetical order:"),
      column(1), p("AU - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.aber.ac.uk/en/", 
                              "Aberystwyth University")),
      column(1), p("AIMS - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.aims.gov.au/", 
                                "Australian Institute of Marine Science")),
      column(1), p("CLEX - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://climateextremes.org.au/", 
                                "ARC Centre of Excellence for Climate Extremes")),
      column(1), p("CSIRO - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.csiro.au/", 
                                 "Commonwealth Scientific and Industrial Research Organisation")),
      column(1), p("DAL - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.dal.ca/", 
                               "Dalhousie University")),
      column(1), p("MBA - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.mba.ac.uk/", 
                               "Marine Biological Association")),
      column(1), p("MEOPAR - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://meopar.ca/",
                                  "Marine Environmental Observation, Prediction and Response Network")),
      column(1), p("NESP - ", a(target = '_blank', rel = 'noopener noreferrer', href = "http://nespclimate.com.au/", 
                                "National Environmental Science Programme")),
      column(1), p("OFI - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.oceanfrontierinstitute.com/", 
                               "Ocean Frontier Insitute")),
      column(1), p("SAMS - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.sams.ac.uk/", 
                                "The Sottish Association for Marine Science")),
      column(1), p("UC - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.canterbury.ac.nz/", 
                              "University of Canterbury")),
      column(1), p("UNSW - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.unsw.edu.au/", 
                                "University of New South Wales")),
      column(1), p("UTAS - ", a(target = '_blank', rel = 'noopener noreferrer', href = "http://www.utas.edu.au/", 
                                "University of Tasmania")),
      column(1), p("UW - ", a(target = '_blank', rel = 'noopener noreferrer', href = "http://www.washington.edu/", 
                              "University of Washington")),
      column(1), p("UWA - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.uwa.edu.au/", 
                               "The University of Western Australia")),
      column(1), p("UWC - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.uwc.ac.za", 
                               "University of the Western Cape")),
      br(),
      h2(tags$b("Bugs?")),
      br(),
      p("This app was developed for the Firefox web browser. Therefore the first fix for any observed bugs
                      is to re-open the Marine Heatwave Tracker in Firefox."),
      p("The Tracker is visually heavy and may appear clumsy on smaller screens (e.g. cell phones).
                      The app has been optimised for use on mobile devices as much as is possible."),
      p("Occasionally the marine heatwave polygons in the time series plots do not render correctly. 
                      Changing the date selection range will allow the figure to re-render correctly."),
      p("Very rarely when the app starts up no MHW pixels will be displayed. Refreshing the website will fix this."),
      p("The map panel may not show up on some computeres running on the Windows operating systems. This is usually remedied
                      by accessing the Tracker in Internet Explorer / Microsoft Edge."),
      p("If the MHW pixels appear, but the map remains grey, this is usually due to internet speed. Refreshing often allows the map
                      tiles to render correctly."),
      p("If problems with viewing the map panel persist, one is encouraged to update the javascript plugins on one's browser of choice."),
      br(),
      p("To report any bugs or to provide any other feedback on the app please contact the developer at:
                      robwschlegel@gmail.com"),
      br(),
      h2(tags$b("How do I cite this app?")),
      br(),
      p("To ",
        a(target = '_blank', rel = 'noopener noreferrer', 
          href = "https://github.com/robwschlegel/MHWapp/blob/master/CITATION", "cite"),
        " the app itself please use:"),
      p("Schlegel, R. W. (2020). Marine Heatwave Tracker. http://www.marineheatwaves.org/tracker. doi: 10.5281/zenodo.3787872"),
      br(),
      h2(tags$b("Is the code for the Tracker open source?")),
      br(),
      p("Yes. The source code for the MHW Tracker is freely available in a ",
        a(target = '_blank', rel = 'noopener noreferrer', href = "https://github.com/robwschlegel/MHWapp", "GitHub repo"), 
        "and is protected under the terms of the",
        a(target = '_blank', rel = 'noopener noreferrer', 
          href = "https://github.com/robwschlegel/MHWapp/blob/master/LICENSE.md","MIT License"), ". For questions about the
                      use or adaptation of the source code please contact the developer at: robert.schlegel@imev-mer.fr"),
      br(),
      h2(tags$b("In the press")),
      br(),
      p("A press release was issued for the Marine Heatwave Tracker on May 27th, 2019. A link to the initial release on
                      the Ocean Frontier Institute (OFI) page may be found ", 
        a(target = '_blank', rel = 'noopener noreferrer',
          href = "https://oceanfrontierinstitute.com/news/news/the-ocean-feels-the-heat", "here"), "."),
      br(),
      h2(tags$b("References")),
      br(),
      p("Any use of the NOAA OISST data should be accompanied by the following two reference:"),
      p("Reynolds, R. W., Smith, T. M., Liu, C., Chelton, D. B., Casey, K. S., and Schlax, M. G. (2007). 
                      Daily high-resolution-blended analyses for sea surface temperature. J. Clim. 20, 5473–5496. doi: 10.1175/2007JCLI1824.1"),
      p("Huang, B., Liu, C., Freeman, E., Graham, G., Smith, T., & Zhang, H. M. (2021). Assessment and intercomparison of 
                      NOAA daily optimum interpolation sea surface temperature (DOISST) version 2.1. Journal of Climate, 34(18), 7421-7441."),
      br(),
      p("The marine heatwave data displayed in this app were calculated in R with the package heatwaveR. To cite
                      heatwaveR in publications please use:"),
      p("Schlegel, R. W., and Smit, A. J. (2018). heatwaveR: a central algorithm for the detection of heatwaves 
                      and cold-spells. J. Open Sour. Softw. 3:821. doi: 10.21105/joss.00821"),
      br(),
      p("The definition and categorisation of marine heatwaves may be found in the following two papers:"),
      p("Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C. J., et al. (2016). 
                      A hierarchical approach to defining marine heatwaves. Progr. Oceanogr. 141, 227–238. doi: 10.1016/j.pocean.2015.12.014"),
      p("Hobday, A. J., Oliver, E. C. J., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., et al. (2018). 
                      Categorizing and naming marine heatwaves. Oceanography 31, 162–173. doi: 10.5670/oceanog.2018.5205"),
      br(),
      h3(tags$b("Shiny server instance: ")),
      h5(paste0(Sys.getenv("R_SHNYSRVINST"))),
      br(),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Downloads ---------------------------------------------------------------

  ### Prep event data
  downloadEventData <- reactive({
    data <- pixelData()$event |> mutate(event = "MHW")
    data_MCS <- pixelData()$event_MCS  |> mutate(event = "MCS")
    lon <- pixelData()$lon[1]
    lat <- pixelData()$lat[1]
    data_sub <- rbind(data, data_MCS) |> 
      mutate(lon = lon, lat = lat) |> 
      dplyr::select(event, lon, lat, event_no, date_start, date_peak, date_end, everything()) |> 
      dplyr::arrange(date_peak)
    return(data_sub)
  })
  
  ### Download event data
  output$download_event <- downloadHandler(
    filename = function() {
      paste0("event_lon_",downloadEventData()$lon[1],"_lat_",downloadEventData()$lat[1],"_",input$baseLine,".csv")
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
    data_sub <- data |> 
      mutate(lon = lon,
             lat = lat) |> 
      dplyr::select(lon, lat, doy, seas, thresh, thresh_MCS) |> 
      dplyr::distinct() |> 
      arrange(doy)
    return(data_sub)
  })
  
  ### Download clim/thresh data
  output$download_clim <- downloadHandler(
    filename = function() {
      paste0("clim_lon_",downloadClimData()$lon[1],"_lat_",downloadClimData()$lat[1],"_",input$baseLine,".csv")
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
      date_seq_files <- paste0("cat_clim/",date_seq_years,"/cat.clim.",date_seq,".",input$baseLine,".Rda")
      map_data <- purrr::map_dfr(date_seq_files, readRDS_date)
    } else if(input$layer == "MCS Category"){
      date_seq_files <- paste0("cat_clim/MCS/",date_seq_years,"/cat.clim.MCS.",date_seq,".",input$baseLine,".Rds")
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

