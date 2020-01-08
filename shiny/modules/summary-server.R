summary <- function(input, output, session) {
  ns <- session$ns
  
  ### Load chosen year of data 
  summaryMapData <- reactive({
      year_choice <- input$summary_year
      annual_map <- readRDS(paste0("../data/annual_summary/MHW_cat_pixel_",year_choice,".Rds"))
    return(annual_map)
  })
  summaryTsData <- reactive({
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
      geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
      # geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
      scale_fill_manual("Category", values = MHW_colours) +
      coord_equal(ratio = 0.7, expand = F) +#, ylim = c(min(OISST_ocean_coords$lat),
                                    #       max(OISST_ocean_coords$lat))) +
      theme_void() +
      guides(fill = guide_legend(override.aes = list(size = 10))) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            panel.background = element_rect(fill = "grey90"))
    fig_map
  })
  
  ### The daily count figure
  summaryCountFig <- reactive({
    # Get map pixels
    annual_ts <- summaryTsData()
    # Stacked barplot of global daily count of MHWs by category
    fig_count <- ggplot(annual_ts, aes(x = t, y = cat_n)) +
      geom_bar(aes(fill = category), stat = "identity", show.legend = F,
               position = position_stack(reverse = TRUE), width = 1) +
      scale_fill_manual("Category", values = MHW_colours) +
      scale_y_continuous(limits = c(0, 691150), # This is the number of ocean pixels
                         breaks = seq(0, 691150, length.out = 6),
                         labels = paste0(seq(0, 100, by = 20), "%")) +
      scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
      labs(y = "Global MHW count\n(non-cumulative)", x = "Day of the year") +
      coord_equal(ratio = 0.0003, expand = F) +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
    fig_count
  })
  
  ### The cummulative category figure
  summaryCumFig <- reactive({
    # Get map pixels
    annual_ts <- summaryTsData()
    # Extract small data.frame for easier labelling
    ts_labels <- annual_ts %>%
      filter(t == max(t)) %>%
      ungroup() %>%
      mutate(label_first_n_cum = cumsum(first_n_cum))
    # Stacked barplot of cumulative percent of ocean affected by MHWs
    fig_cum <- ggplot(annual_ts, aes(x = t, y = first_n_cum)) +
      geom_bar(aes(fill = category), stat = "identity", show.legend = F,
               position = position_stack(reverse = TRUE), width = 1) +
      geom_hline(data = ts_labels, show.legend = F,
                 aes(yintercept = label_first_n_cum, colour = category)) +
      scale_fill_manual("Category", values = MHW_colours) +
      scale_colour_manual("Category", values = MHW_colours) +
      scale_y_continuous(limits = c(0, 691150), # This is the number of ocean pixels
                         breaks = seq(0, 691150, length.out = 6),
                         labels = paste0(seq(0, 100, by = 20), "%")) +
      scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
      labs(y = "Top MHW category per pixel\n(cumulative)", x = "Day of first occurrence") +
      coord_equal(ratio = 0.0003, expand = F) +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
    fig_cum
  })

  ### The proportion of MHW days figure
  summaryPropFig <- reactive({
    # Get map pixels
    annual_ts <- summaryTsData()
    # Stacked barplot of average cumulative MHW days per pixel
    fig_prop <- ggplot(annual_ts, aes(x = t, y = cat_n_prop)) +
      geom_bar(aes(fill = category), stat = "identity", show.legend = F,
               position = position_stack(reverse = TRUE), width = 1) +
      scale_fill_manual("Category", values = MHW_colours) +
      # scale_y_continuous(labels = ) +
      scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
      labs(y = "Average MHW days per pixel\n(cumulative)", x = "Day of the year") +
      coord_equal(ratio = 3.5, expand = F) +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
    fig_prop
  })
  
  ### The fullannual summary viusal
  summaryAllFig <- reactive({
    # Get map pixels
    annual_map <- summaryMapData()
    # Global map of MHW occurrence
    fig_map <- ggplot(annual_map, aes(x = lon, y = lat)) +
      # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
      geom_tile(aes(fill = category)) +
      geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
      # geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
      scale_fill_manual("Category", values = MHW_colours) +
      coord_cartesian(expand = F) +
      # coord_equal(ratio = 0.7, expand = F) +#, ylim = c(min(OISST_ocean_coords$lat),
      #       max(OISST_ocean_coords$lat))) +
      theme_void() +
      guides(fill = guide_legend(override.aes = list(size = 10))) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            panel.background = element_rect(fill = "grey90"))
    # Get map pixels
    annual_ts <- summaryTsData()
    # Stacked barplot of global daily count of MHWs by category
    fig_count <- ggplot(annual_ts, aes(x = t, y = cat_n)) +
      geom_bar(aes(fill = category), stat = "identity", show.legend = F,
               position = position_stack(reverse = TRUE), width = 1) +
      scale_fill_manual("Category", values = MHW_colours) +
      scale_y_continuous(limits = c(0, 691150), # This is the number of ocean pixels
                         breaks = seq(0, 691150, length.out = 6),
                         labels = paste0(seq(0, 100, by = 20), "%"),
                         expand = c(0, 0)) +
      scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m", expand = c(0, 0)) +
      labs(y = "Global MHW count\n(non-cumulative)", x = "Day of the year") #+
      # coord_equal(ratio = 0.0003, expand = F) +
      # theme(axis.title = element_text(size = 14),
            # axis.text = element_text(size = 12))
    # Extract small data.frame for easier labelling
    ts_labels <- annual_ts %>%
      filter(t == max(t)) %>%
      ungroup() %>%
      mutate(label_first_n_cum = cumsum(first_n_cum))
    # Stacked barplot of cumulative percent of ocean affected by MHWs
    fig_cum <- ggplot(annual_ts, aes(x = t, y = first_n_cum)) +
      geom_bar(aes(fill = category), stat = "identity", show.legend = F,
               position = position_stack(reverse = TRUE), width = 1) +
      geom_hline(data = ts_labels, show.legend = F,
                 aes(yintercept = label_first_n_cum, colour = category)) +
      scale_fill_manual("Category", values = MHW_colours) +
      scale_colour_manual("Category", values = MHW_colours) +
      scale_y_continuous(limits = c(0, 691150), # This is the number of ocean pixels
                         breaks = seq(0, 691150, length.out = 6),
                         labels = paste0(seq(0, 100, by = 20), "%"),
                         expand = c(0, 0)) +
      scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m", expand = c(0, 0)) +
      labs(y = "Top MHW category per pixel\n(cumulative)", x = "Day of first occurrence") #+
      # coord_equal(ratio = 0.0003, expand = F) +
      # theme(axis.title = element_text(size = 14),
            # axis.text = element_text(size = 12))
    # Stacked barplot of average cumulative MHW days per pixel
    fig_prop <- ggplot(annual_ts, aes(x = t, y = cat_n_prop)) +
      geom_bar(aes(fill = category), stat = "identity", show.legend = F,
               position = position_stack(reverse = TRUE), width = 1) +
      scale_fill_manual("Category", values = MHW_colours) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_date(date_breaks = "4 months", date_labels = "%Y-%m", expand = c(0, 0)) +
      labs(y = "Average MHW days per pixel\n(cumulative)", x = "Day of the year") #+
      # coord_equal(ratio = 3.5, expand = F) +
      # theme(axis.title = element_text(size = 14),
            # axis.text = element_text(size = 12))
    # fig_All_sub <- ggpubr::ggarrange(fig_count, fig_cum, fig_prop, ncol = 3, align = "hv",
    #                                  labels = c("B)", "C)", "D)"), font.label = list(size = 16))
    # fig_All <- ggpubr::ggarrange(fig_map, fig_All_sub, ncol = 1, heights = c(1, 0.6),
    #                              labels = c("A)"), common.legend = T, legend = "bottom",
    #                              font.label = list(size = 16))
    fig_All <- ggpubr::ggarrange(fig_map, ggpubr::ggarrange(fig_count, fig_cum, fig_prop,
                                                            ncol = 3, nrow = 1), ncol = 1, nrow = 2, heights = c(10, 5))
    fig_All
    # Standard caption technique
    # fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 20))
    # fig_ALL_cap <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL, heights = c(0.07, 1), nrow = 2)
    # fig_ALL_cap
  })
  
  ### The figure rendering
  output$summaryMap <- renderPlot({
    summaryMapFig()
  })
  output$summaryCount <- renderPlot({
    summaryCountFig()
  })
  output$summaryCum <- renderPlot({
    summaryCumFig()
  })
  output$summaryProp <- renderPlot({
    summaryPropFig()
  })
  output$summaryAll <- renderPlot({
    summaryAllFig()
  })
  
}
