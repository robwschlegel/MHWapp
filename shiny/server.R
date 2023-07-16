server <- function(input, output, session){
  
  # Testing...
  # input <- data.frame(date = as.Date("2023-07-01"))

  # Reactive UI -------------------------------------------------------------

  ### Reactive category filters
  categories <- reactiveValues(categories = c("I Moderate", "II Strong", "III Severe", "IV Extreme"))
  
  
  # Map projection data -----------------------------------------------------
  
  ### Base map data before screening categories
  baseDataPre <- reactive({
    req(input$date)
    sub_dir <- paste0("cat_clim/",lubridate::year(input$date))
    sub_file <- paste0(sub_dir,"/cat.clim.",input$date,".Rda")
    baseDataPre <- readRDS(sub_file)
    return(baseDataPre)
  })
  
  ### Base map data after screening categories
  # baseData <- reactive({
  #   baseDataPre <- baseDataPre()
  #   if(input$layer %in% cat_layers){
  #     baseData <- baseDataPre %>%
  #       dplyr::filter(category %in% categories$categories)
  #     # Fix for the issue caused by de-selecting all of the categories
  #     if(length(baseData$category) == 0){
  #       baseData <- empty_date_map
  #     }
  #   }
  #   if(input$layer %in% rb_layers){
  #     baseData <- baseDataPre()
  #   }
  #   return(baseData)
  # })
  
  ### Non-shiny-projected raster data
  rasterNonProj <- reactive({
    # baseData <- baseData()
    baseData <- baseDataPre()
    MHW_raster <- dplyr::select(baseData, lon, lat, category)
    colnames(MHW_raster) <- c("X", "Y", "Z")
    MHW_raster$Z <- as.numeric(MHW_raster$Z)
    # suppressWarnings(
    # rasterNonProj <- raster::rasterFromXYZ(MHW_raster, res = c(0.25, 0.25),
    #                                        digits = 3, crs = inputProj)
    # )
    rasterNonProj <- terra::rast(MHW_raster, digits = 3, crs = inputProj)
    # rasterProj <- raster::projectRaster(rasterNonProj, crs = "EPSG:3857")
    return(rasterNonProj)
  })
  
  ### Shiny-projected raster data
  rasterProj <- reactive({
    rasterNonProj <- rasterNonProj()
    rasterProj <- projectRasterForLeaflet(rasterNonProj, method = "ngb")
    return(rasterProj)
  })
  
  
  # Leaflet -----------------------------------------------------------------
  
  ### Observer to change colour palette accordingly
  pal_react <- reactive({
    baseDataPre <- baseDataPre()
    colorNumeric(palette = MHW_colours, domain = c(1,2,3,4), na.color = NA)
    })
  
  ### The leaflet base
  output$leaf_map <- renderLeaflet({
    
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
    leaflet(data = MHW_cat_clim_sub, options = leafletOptions(zoomControl = FALSE)) |> 
      setView(lng = map_lon, lat = map_lat, zoom = map_zoom,
              options = tileOptions(minZoom = 0, maxZoom = 8, noWrap = F)) |> 
      addScaleBar(position = "topright") |> addTiles() |> 
      addRasterImage(rasterNonProj, 
      # addRasterImage(rasterProj(), 
                     # colors = pal_react(), layerId = "map_raster",
                     project = FALSE, opacity = 0.8)
  })
  
  ### The raster layer
  # observeEvent(c(input$layer, input$date,
  #                input$moderate_filter, input$strong_filter,
  #                input$severe_filter, input$extreme_filter), {
  #                  leafletProxy("map") %>%
  #                    addRasterImage(rasterProj(), colors = pal_react(), layerId = "map_raster",
  #                                   project = FALSE, opacity = 0.8)
  #                })
  
  ### Shift when new lon/lat are entered
  # observeEvent(c(input$lon, input$lat), {
  #   leafletProxy("map") %>%
  #     setView(lng = input$lon, lat = input$lat, zoom = input$zoom,
  #             options = tileOptions(minZoom = 0, maxZoom = 8, noWrap = F))
  # })
  
  ### Recreate the legend as needed
  # observe({
  #   baseDataPre <- baseDataPre()
  #   if(input$layer %in% cat_layers){
  #     leafletProxy("map", data = MHW_cat_clim_sub) |> clearControls()
  #   } else if(input$layer %in% c(rb_layers)){
  #     if(input$layer == "SST Anomaly"){
  #       domain_low <- min(baseDataPre$anom, na.rm = T)
  #       domain_high <- max(baseDataPre$anom, na.rm = T)
  #       domain_label <- "SST Anomaly"
  #     } else if(input$layer %in% trend_layers){
  #       domain_low <- min(baseDataPre$val, na.rm = T)
  #       domain_high <- max(baseDataPre$val, na.rm = T)
  #       domain_label <- "Annual\nTrend"
  #     }
  #     if(abs(domain_high) > abs(domain_low)){
  #       domain_low <- -domain_high
  #     } else {
  #       domain_high <- -domain_low
  #     }
  #     domain_low_high <- data.frame(domain = seq(domain_low, domain_high, length.out = 10))
  #     pal_rev <- colorNumeric(palette = c("red", "white", "blue"), na.color = NA,
  #                             domain = domain_low_high)
  #     leafletProxy("map", data = domain_low_high) %>%
  #       clearControls() %>%
  #       addLegend(position = "bottomright",  pal = pal_rev,
  #                 bins = 5, values = ~domain,
  #                 title = domain_label,
  #                 labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
  #   }
  # })
  
  # During testing...
  session$onSessionEnded(stopApp)
}

