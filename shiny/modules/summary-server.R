summary <- function(input, output, session) {
  ns <- session$ns
  
  ### Load chosen year of data 
  summaryMapData <- reactive({
    year_choice <- summary_year
    annual_map <- readRDS(paste0("../data/annual_summary/MHW_cat_pixel_",year_choice,".Rds"))
    return(annual_map)
  })
  summaryTsData <- reactive({
    year_choice <- summary_year
    annual_ts <- readRDS(paste0("../data/annual_summary/MHW_cat_daily_",year_choice,".Rds"))
    return(annual_ts)
  })
  
  ### The map viusal
  summaryMapGG <- reactive({
    # Get map pixels
    annual_map <- summaryMapData()
    annual_map_coarse <- annual_map %>% 
      filter(lon > 100, lon < 110,
             lat > 0, lat < 10)
      # mutate(lon = round(lon), 
      #        lat = round(lat)) %>% 
      # group_by(lon, lat) %>% 
      # summarise()
    # Global map of MHW occurrence
    suppressWarnings( # Suppress text labels to plotly warning
    fig_map <- ggplot(annual_map_coarse, aes(x = lon, y = lat)) +
      # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
      geom_tile(aes(fill = category, text = paste0("Longitude: ",lon,
                                                   "<br>Latitude: ",lat,
                                                   "<br>Date: ",t,
                                                   "<br>Intensity: ",intensity,"°C",
                                                   "<br>Sum of Intensity: ",intensity_sum,"°C"))) +
      # geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
      scale_fill_manual("Category", values = MHW_colours) +
      coord_cartesian(expand = F) +#, ylim = c(min(OISST_ocean_coords$lat),
                                    #       max(OISST_ocean_coords$lat))) +
      theme_void() +
      guides(fill = guide_legend(override.aes = list(size = 10))) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            panel.background = element_rect(fill = "grey90"))
    )
    fig_map_p <- ggplotly(fig_map, tooltip = "text", dynamicTicks = F) %>% 
      layout(hovermode = 'compare')
  })
  
  ### Plotly able map
  output$summaryMapPlotly <- renderPlotly({
    summaryMapGG()
  })
}