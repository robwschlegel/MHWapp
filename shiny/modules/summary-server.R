summary <- function(input, output, session) {
  ns <- session$ns
  
  ### Load chosen year of data 
  summaryMapData <- reactive({
    # year_choice <- summary_year
    year_choice <- input$summary_year
    annual_map <- readRDS(paste0("../data/annual_summary/MHW_cat_pixel_",year_choice,".Rds"))
    return(annual_map)
  })
  summaryTsData <- reactive({
    # year_choice <- summary_year
    year_choice <- input$summary_year
    annual_ts <- readRDS(paste0("../data/annual_summary/MHW_cat_daily_",year_choice,".Rds"))
    return(annual_ts)
  })
  
  ### The map viusal
  summaryMapFig <- reactive({
    # Get map pixels
    annual_map <- summaryMapData()
    # Global map of MHW occurrence
    fig_map <- ggplot(annual_map, aes(x = lon, y = lat)) +
      # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
      geom_tile(aes(fill = category)) +
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
    fig_map
  })
  
  ### ggplot map
  output$summaryMapGG <- renderPlot({
    summaryMapFig()
  })
  

  output$hist <- renderPlot({
    hist(rnorm(input$n))
  })
  
  # ### The full normal R figure code
  # # Extract small data.frame for easier labelling
  # MHW_cat_daily_labels <- MHW_cat_daily %>% 
  #   filter(t == max(t)) %>% 
  #   ungroup() %>% 
  #   mutate(label_first_n_cum = cumsum(first_n_cum))
  # 
  # ## Create figures
  # print("Creating figures")
  # 
  # 
  # # Stacked barplot of global daily count of MHWs by category
  # fig_count <- ggplot(MHW_cat_daily, aes(x = t, y = cat_n)) +
  #   geom_bar(aes(fill = category), stat = "identity", show.legend = F,
  #            position = position_stack(reverse = TRUE), width = 1) +
  #   scale_fill_manual("Category", values = MHW_colours) +
  #   scale_y_continuous(limits = c(0, nrow(OISST_ocean_coords)),
  #                      breaks = seq(0, nrow(OISST_ocean_coords), length.out = 11),
  #                      labels = paste0(seq(0, 100, by = 10), "%")) +
  #   scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  #   labs(y = "Global MHW count\n(non-cumulative)", x = "Day of the year") +
  #   coord_cartesian(expand = F) +
  #   theme(axis.title = element_text(size = 14),
  #         axis.text = element_text(size = 12))
  # # fig_count
  # 
  # # Stacked barplot of cumulative percent of ocean affected by MHWs
  # fig_cum <- ggplot(MHW_cat_daily, aes(x = t, y = first_n_cum)) +
  #   geom_bar(aes(fill = category), stat = "identity", show.legend = F,
  #            position = position_stack(reverse = TRUE), width = 1) +
  #   geom_hline(data = MHW_cat_daily_labels, show.legend = F,
  #              aes(yintercept = label_first_n_cum, colour = category)) +
  #   scale_fill_manual("Category", values = MHW_colours) +
  #   scale_colour_manual("Category", values = MHW_colours) +
  #   scale_y_continuous(limits = c(0, nrow(OISST_ocean_coords)),
  #                      breaks = seq(0, nrow(OISST_ocean_coords), length.out = 11),
  #                      labels = paste0(seq(0, 100, by = 10), "%")) +
  #   scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  #   labs(y = "Top MHW category per pixel\n(cumulative)", x = "Day of first occurrence") +
  #   coord_cartesian(expand = F) +
  #   theme(axis.title = element_text(size = 14),
  #         axis.text = element_text(size = 12))
  # # fig_cum
  # 
  # # Stacked barplot of average  cumulative MHW days per pixel
  # fig_prop <- ggplot(MHW_cat_daily, aes(x = t, y = cat_n_prop)) +
  #   geom_bar(aes(fill = category), stat = "identity", show.legend = F,
  #            position = position_stack(reverse = TRUE), width = 1) +
  #   scale_fill_manual("Category", values = MHW_colours) +
  #   scale_y_continuous(labels = ) +
  #   scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +  
  #   labs(y = "Average MHW days per pixel\n(cumulative)", x = "Day of the year") +
  #   coord_cartesian(expand = F) +
  #   theme(axis.title = element_text(size = 14),
  #         axis.text = element_text(size = 12))
  # # fig_prop
  # 
  # print("Combining figures")
  # fig_ALL_sub <- ggpubr::ggarrange(fig_count, fig_cum, fig_prop, ncol = 3, align = "hv",
  #                                  labels = c("B)", "C)", "D)"), font.label = list(size = 16))
  # fig_ALL <- ggpubr::ggarrange(fig_map, fig_ALL_sub, ncol = 1, heights = c(1, 0.6),
  #                              labels = c("A)"), common.legend = T, legend = "bottom",
  #                              font.label = list(size = 16))
  # 
  # # Fancy caption technique
  # # fig_ALL_cap <-  grid::textGrob(paste0(strwrap(fig_cap, 140), sep = "", collapse = "\n"),
  # #                               x = 0.01, just = "left", gp = grid::gpar(fontsize = 10))
  # 
  # # Standard caption technique
  # fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 20))
  # fig_ALL_cap <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL, heights = c(0.07, 1), nrow = 2)
}