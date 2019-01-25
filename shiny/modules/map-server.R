map <- function(input, output, session) {
  ns <- session$ns
  
  
# Functions ---------------------------------------------------------------
  
  
# Reactives ---------------------------------------------------------------

  ### Find where clicking is happening
  # clickData <- reactive({
  #   print(input$click)
  # })
  
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
    
    # testers...
    # xy <- c(-42.125, 20.125)
    
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
      
      # Leaflet processing
      rasterNonProj <- rasterNonProj()
      cell <- cellFromXY(rasterNonProj, c(xy$lng, xy$lat))
      #If the click is inside the raster...
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
    xy <- pixelData()
    # xy <- input$map_click
    # if(xy$lng > 180){
    #   while(xy$lng > 180){
    #     xy$lng <- xy$lng - 360
    #   }
    # }
    # if(xy$lng < -180){
    #   while(xy$lng < -180){
    #     xy$lng <- xy$lng + 360
    #   }
    # }
    paste0("Chosen pixel; lon = ",xy$lon[1],", lat = ",xy$lat[1])
  })
  
  ### Download data
  downloadData <- reactive({
    data <- pixelData()$event
    data_sub <- data #%>% 
    # filter(date_start >= input$from, date_start <= input$to)
  })
  
  ### Create time series plot
  # input <- data.frame(from = as.Date("2017-01-01"),
  #                     to = as.Date("2017-12-31"),
  #                     date_choice = as.Date("2017-02-14"))
  tsPlot <- reactive({
    # Time series data prep
    ts_data <- pixelData()$ts
    ts_data_sub <- ts_data %>%
      mutate(temp = round(temp, 2)) %>% 
      filter(t >= input$from, t <= input$to) #%>%
    
    # Event data prep
    event_data <- pixelData()$event
    event_data_sub <- event_data %>%
    filter(date_start >= input$from, date_end <= input$to)
    
    # Threshold data prep
    thresh_data <- pixelData()$thresh
    thresh_data_sub <- heatwaveR:::make_whole_fast(data.frame(ts_x = seq(min(ts_data_sub$t), max(ts_data_sub$t), "day"),
                                                              ts_y = 1)) %>% 
      left_join(thresh_data, by = "doy") %>% 
      select(-ts_y) %>% 
      dplyr::rename(t = ts_x) %>% 
      mutate(seas = round(seas, 2),
             thresh = round(thresh, 2))
    
    # Merge ts and thresh data for plotly
    # ts_data_sub <- ts_data_sub %>%
    #   left_join(thresh_data_sub, by = "t") %>%
    #   gather(key = "var", value = "temp", -t, -doy) %>%
    #   group_by(var)
    
    # plot_ly code
    # p <- plot_ly(data = ts_data_sub, x = ~t, y = ~temp, color = ~var, linetype = ~var) %>%
    #   add_lines(colors = c("seagreen", "black", "firebrick"), linetypes = c("dotted", "solid", "dashed"))
    # rangeslider(p, start = paste0(lubridate::year(as.Date(input$date_choice)),"-01-01"),
    #             end = paste0(lubridate::year(as.Date(input$date_choice)),"-12-31"))
    # plotly_json(p)
    
    
    # subplot(
    #   add_lines(p, color = ~city),
    #   add_lines(p, linetype = ~city),
    #   shareX = TRUE, nrows = 2
    # )
    
    # Merge ts and thresh data for ggplot
    # ts_data_sub <- ts_data %>%
    #   dplyr::left_join(thresh_data_sub, by = "t")
    
    # ggplot2 code
      # Adding aes(text) to geom_line makes ggplot angry, but it is passed to plotly correctly
        # This is a bit of a hack but is the best practice...
    suppressWarnings(
    p <- ggplot(data = ts_data_sub, aes(x = t, y = temp)) +
      geom_flame(aes(y2 = thresh_data_sub$thresh)) +
      geom_line(colour = "grey20",
                aes(group = 1, text = paste0("Date: ",t,
                                  "<br>Temp.: ",temp,"°C"))) +
      geom_line(data = thresh_data_sub, linetype = "dashed", colour = "green",
                aes(x = t, y = seas, group = 1,
                    text = paste0("Date: ",t,
                                  "<br>Seas.: ",seas,"°C"))) +
      geom_line(data = thresh_data_sub,
                linetype = "dotted", colour = "red",
                aes(x = t, y = thresh, group = 1,
                    text = paste0("Date: ",t,
                                  "<br>Thresh.: ",thresh,"°C"))) +
      geom_rug(data = event_data_sub, sides = "b", colour = "red",
               aes(x = date_peak, y = min(ts_data$temp), 
                   text = paste0("Event: ",event_no,
                                 "<br>Duration: ",duration," days",
                                 "<br>Start Date: ", date_start,
                                 "<br>Peak Date: ", date_peak,
                                 "<br>End Date: ", date_end,
                                 "<br>Mean Intensity: ",intensity_mean,"°C",
                                 "<br>Max. Intensity: ",intensity_max,"°C",
                                 "<br>Cum. Intensity: ",intensity_cumulative,"°C"))) +
      labs(x = "", y = "Temperature (°C)") +
      scale_x_date(expand = c(0, 0))
    )
      # scale_y_continuous(limits = c(min(ts_data_sub$temp)-1, max(ts_data_sub$temp)+1))
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
    pp <- ggplotly(p, tooltip = "text") #%>%
      # layout(xaxis = list(range = c(as.integer(as.Date(paste0(lubridate::year(as.Date(input$date_choice)),"-01-01"))),
                                    # as.integer(as.Date(paste0(lubridate::year(as.Date(input$date_choice)),"-12-31")))),
                          # autorange = FALSE,
                          # tickmode = "array",
                          # tickvals = as.integer(as.Date(paste0(base::unique(lubridate::year(ts_data_sub$t)),"-01-01"))),
                          # type = "date"))
    pp
    # rangeslider(start = ts_data$t[1],
      #             end = ts_data$t[100])
      # rangeslider(start = as.Date("2017-01-01"),
      #             end = as.Date("2017-12-31"))
      # rangeslider(start = as.numeric(paste0(lubridate::year(as.Date(input$date_choice)),"-01-01")),
      #             end = as.numeric(paste0(lubridate::year(as.Date(input$date_choice)),"-12-31")))
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
  
  ### Find where clicking is happening
  # observeEvent(input$plot_click, {
  #   print(input$plot_click)
  # })
  
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
  
  # Leaflet clicking
  observeEvent(input$map_click, {
    # req(!is.null(pixelData()))
    toggleModal(session, "modal", "open")
  })
  
  # Plotly clicking
  # observeEvent(event_data("plotly_click", source = "map"), {
  #   showModal(modalDialog(
  #     renderPlotly({
  #       plot_ly(baseDate(), x = ~lon, y = ~lat)
  #       })
  #     ))
  #   # toggleModal(session, "modal", "open")
  # })
  
  # Plotly clicking
  # observeEvent(event_data("plotly_click", source = "map"), {
  #   toggleModal(session, modalId = "uiModal", toggle = "toggle")
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
  
  # The plotly option
  # output$map <- renderPlotly({
  #   
  #   breaks <- c(1,2,3,4)
  #   colors <- MHW_colours
  #   colorscale <- data.frame(breaks, colors)
  #   
  #   plot_ly(data = baseData(), x = ~lon, y = ~lat, z = ~as.integer(category)) %>%
  #     add_heatmap(colorscale = colorscale) %>% 
  #     layout(xaxis = list(title = ""), 
  #            yaxis = list(title = ""))
    
  # The map_box option
    # plot_mapbox(map_base, x = ~lon, y = ~lat) %>%
    # add_paths(size = I(2)) #%>%
    # add_heatmap(baseData(), x = ~lon, y = ~lat, z = ~as.integer(category))
    
  # An possible sf overlay
    # 
    # plot_ly() %>%
    #   add_sf(
    #     data = sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)), 
    #     color = I("black"), fillcolor = "transparent", hoverinfo = "none", size = I(1)
    #   ) %>%
    #   add_contour(
    #     z = dat$air, 
    #     x = dat$lon, 
    #     y = dat$lat,
    #     name = "precipitation",
    #     zauto = FALSE,
    #     # this censors extreme values -- I would have gone with a a log scale myself ;)
    #     zmin = -3,      
    #     zmax = 3,
    #     colorscale = colorscale,
    #     colorbar = list(
    #       borderwidth = 0, 
    #       outlinewidth = 0, 
    #       thickness = 15, 
    #       tickfont = list(size = 14), 
    #       title = "mm/day"
    #     ),
    #     contours = list(
    #       end = 2.5, 
    #       showlines = FALSE, 
    #       size = 0.25,
    #       start = -2.5
    #     )
    #   ) %>%
    #   layout(
    #     title = "Surface Precipitation Rate Anomalies<br>Dec 2017-Jan 2018",
    #     showlegend = FALSE,
    #     annotations = list(
    #       text = "Data courtesy of <a href='http://www.esrl.noaa.gov/psd/data/composites/day/'>NOAA Earth System Research Laboratory</a>",
    #       xref = 'paper',
    #       yref = 'paper',
    #       x = 0,
    #       y = 0,
    #       yanchor = 'top',
    #       showarrow = FALSE
    #     ),
    #     yaxis = list(title = ""),
    #     xaxis = list(title = "", range = range(dat$lon))
    #   )
  # })
  
  
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
                                   plotlyOutput(ns("tsPlot")),
                                   hr(),
                                   fluidRow(
                                   column(width = 2,
                                          h4("From"),
                                          dateInput(inputId = ns("from"), label = NULL, format = "M d, yyyy",
                                                    value = paste0(lubridate::year(as.Date(input$date_choice)),"-01-01"))),
                                   column(width = 2,
                                          h4("To"),
                                          dateInput(inputId = ns("to"), label = NULL, format = "M d, yyyy",
                                                    value = paste0(lubridate::year(as.Date(input$date_choice)),"-12-31"))))
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
                                            downloadButton(outputId = ns("download"),
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
  
  # Downloading
  output$download <- downloadHandler(
    filename = function() {
      paste0("lon_",downloadData()$lon[1],"_lat_",downloadData()$lat[1],".csv")
      # paste0(pretty_label(), "_", gsub("-", "", as.character(input$from)), "_", gsub("-", "", as.character(input$to)), ".csv")
    },
    content <- function(file) {
      readr::write_csv(downloadData(), file)
    }
  )
  
}