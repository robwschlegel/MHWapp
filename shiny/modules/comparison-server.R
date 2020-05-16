comparison <- function(input, output, session) {
  
  ns <- session$ns
  
  # Map figures -------------------------------------------------------------
  
  # The map
  output$compOne <- renderPlot({
    
    MHW_cat_pixel_one <- readRDS("../data/annual_summary/OISST_cat_pixel_1982-2011_1992.Rds")
    
    fig_map_one <- ggplot(MHW_cat_pixel_one, aes(x = lon, y = lat)) +
      # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
      geom_tile(aes(fill = category), colour = NA) +
      geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
      scale_fill_manual("Category", values = MHW_colours) +
      coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
                                           max(OISST_ocean_coords$lat))) +
      theme_void() +
      guides(fill = guide_legend(override.aes = list(size = 10))) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            panel.background = element_rect(fill = "grey90"))
    return(fig_map_one)
  })
  
  output$compTwo <- renderPlot({
    
    MHW_cat_pixel_two <- readRDS("../data/annual_summary/CCI_cat_pixel_1982-2011_1992.Rds")
    
    fig_map_two <- ggplot(MHW_cat_pixel_two, aes(x = lon, y = lat)) +
      # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
      geom_tile(aes(fill = category), colour = NA) +
      geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
      scale_fill_manual("Category", values = MHW_colours) +
      coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
                                           max(OISST_ocean_coords$lat))) +
      theme_void() +
      guides(fill = guide_legend(override.aes = list(size = 10))) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            panel.background = element_rect(fill = "grey90"))
    return(fig_map_two)
  })
  
  output$compThree <- renderPlot({
    
    MHW_cat_pixel_three <- readRDS("../data/annual_summary/CMC_cat_pixel_1992-2018_1992.Rds")
    
    fig_map_three <- ggplot(MHW_cat_pixel_three, aes(x = lon, y = lat)) +
      # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
      geom_tile(aes(fill = category), colour = NA) +
      geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
      scale_fill_manual("Category", values = MHW_colours) +
      coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
                                           max(OISST_ocean_coords$lat))) +
      theme_void() +
      guides(fill = guide_legend(override.aes = list(size = 10))) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            panel.background = element_rect(fill = "grey90"))
    return(fig_map_three)
  })
  
}