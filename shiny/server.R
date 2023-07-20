# MHWapp/shiny/server.R

server <- function(input, output, session){
  
  # Testing...
  # input <- data.frame(date = as.Date("2023-07-15"),
  #                     layer = "MHW Category")

  # Reactive UI -------------------------------------------------------------

  ### Reactive category filters
  categories <- reactiveValues(categories = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))
  
  ### Reactive value to track which year has been selected
  #### This is meant to help avoid loading data unnecessarily
  yearReact <- reactiveValues(year = 1)#lubridate::year(Sys.Date()))
  
  
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
      df_dir <- paste0("cat_clim/MCS/")
      df_name <- paste0("/cat.clim.MCS.",input$date,".Rds")
    } else {
      df_dir <- paste0("cat_clim/")
      df_name <- paste0("/cat.clim.",input$date,".Rda")
    }
    # if( != yearReact$year)
    df_map <- readRDS(paste0(df_dir, substr(input$date, 1, 4), df_name))
    df_perc_cat <- df_map |> 
      left_join(lon_lat_OISST_area, by = c("lon", "lat")) |> 
      summarise(sum_area = sum(sq_area, na.rm = T), .by = "category") |> 
      mutate(perc_area = round(sum_area/511208164, 3)*100)
  })
  
  
  # Leaflet -----------------------------------------------------------------
  
  ### Observer to change colour palette accordingly
  # TODO: This could potentially be bundled up into rasterProj()
  pal_react <- reactive({
    req(input$layer)
    if(input$layer == "MCS Category"){
      event_palette <- MCS_colours
    } else {
      event_palette <- MHW_colours
    }
    colorNumeric(palette = event_palette, domain = c(1,2,3,4), na.color = NA)
    })
  
  ### The leaflet base
  output$leaf_map <- renderLeaflet({
    
    # The base 
    leaflet(options = leafletOptions(zoomControl = FALSE)) |> 
      setView(lng = initial_lon, lat = initial_lat, zoom = initial_zoom,
              options = tileOptions(minZoom = 0, maxZoom = 8, noWrap = F)) |> 
      addScaleBar(position = "topright") |> addTiles()
  })
  
  ### The raster layer
  observeEvent(c(input$layer, input$date,
                 input$moderate_filter, input$strong_filter,
                 input$severe_filter, input$extreme_filter), {
                   leafletProxy("leaf_map") %>%
                     addRasterImage(rasterProj(), 
                                    colors = pal_react(), layerId = "map_raster",
                                    project = FALSE, opacity = 0.8)
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
                        lon = xy_wrap[1],
                        lat = xy_wrap[2])
      return(pixelData)
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
    pp <- ggplotly(p, tooltip = "text", dynamicTicks = F) |> plotly::layout(hovermode = "x")
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
    event_res
  })
  
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
  output$tsTable <- renderDataTable({
    DT::datatable(tsTable(),
                  options = list(pageLength = 10))
  })
  
  # During testing...
  session$onSessionEnded(stopApp)
}

