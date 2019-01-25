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
    baseData <- baseData %>% 
      filter(category %in% input$categories)
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
    check <- input$map_click
    if(!is.null(check)){
    xy <- pixelData()
    if(xy$lon[1] >= 0) xy_lon <- paste0(abs(xy$lon[1]),"°E")
    if(xy$lon[1] < 0) xy_lon <- paste0(abs(xy$lon[1]),"°W")
    if(xy$lat[1] >= 0) xy_lat <- paste0(abs(xy$lat[1]),"°N")
    if(xy$lat[1] < 0) xy_lat <- paste0(abs(xy$lat[1]),"°S")
    paste0("Chosen pixel; lon = ",xy_lon,", lat = ",xy_lat)
    }
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
    
    suppressWarnings(
    p <- ggplot(data = ts_data_sub, aes(x = t, y = temp)) +
      geom_flame(aes(y2 = thresh_data_sub$thresh)) +
      geom_line(colour = "grey20",
                aes(group = 1, text = paste0("Date: ",t,
                                  "<br>Temperature: ",temp,"°C"))) +
      geom_line(data = thresh_data_sub, linetype = "dashed", colour = "steelblue3",
                aes(x = t, y = seas, group = 1,
                    text = paste0("Date: ",t,
                                  "<br>Climatology: ",seas,"°C"))) +
      geom_line(data = thresh_data_sub,
                linetype = "dotted", colour = "tomato3",
                aes(x = t, y = thresh, group = 1,
                    text = paste0("Date: ",t,
                                  "<br>Threshold: ",thresh,"°C"))) +
      geom_rug(data = event_data_sub, sides = "b", colour = "red3", size = 2,
               aes(x = date_peak, y = min(ts_data_sub$temp),
                   text = paste0("Event: ",event_no,
                                 "<br>Duration: ",duration," days",
                                 "<br>Start Date: ", date_start,
                                 "<br>Peak Date: ", date_peak,
                                 "<br>End Date: ", date_end,
                                 "<br>Mean Intensity: ",intensity_mean,"°C",
                                 "<br>Max. Intensity: ",intensity_max,"°C",
                                 "<br>Cum. Intensity: ",intensity_cumulative,"°C"))) +
      geom_segment(aes(x = input$date_choice, 
                       xend = input$date_choice,
                       y = min(ts_data_sub$temp), 
                       yend = max(ts_data_sub$temp),
                       text = "Map date"), colour = "bisque") +
      labs(x = "", y = "Temperature (°C)") +
      scale_x_date(expand = c(0, 0))
    )
    
    # Convert to plotly
    pp <- ggplotly(p, tooltip = "text", dynamicTicks = T) %>% 
      layout(hovermode = 'compare') #%>% 
      # style(traces = 2, hoverlabel = list(bgcolor = "grey10"))# %>% 
      # style(traces = 3, hoverlabel = list(bgcolor = "tomato2")) %>% 
      # style(traces = 4, hoverlabel = list(bgcolor = "red2"))
    pp
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
                          title = "Category"
      )
    }
  })
  
  # Leaflet clicking
  observeEvent(input$map_click, {
    toggleModal(session, "modal", "open")
  })
  
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
                                   plotlyOutput(ns("tsPlot"), height = "600px"),
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