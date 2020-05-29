comparison <- function(input, output, session) {
  
  ns <- session$ns
  

  # Render UI ---------------------------------------------------------------

  # Radio buttons to choose clim period
  radio_clim_period <- prettyRadioButtons(inputId = ns("clim_period"), label = "Clim. period:", 
                                          choices = c("1982-2011", "1992-2018"),
                                          selected = "1992-2018", inline = F,
                                          status = "primary", fill = TRUE)
  
  # Select years from a dropdown
  picker_year <- pickerInput(inputId = ns("year"), label = "Year:",
                             choices = seq(1982, 2019), multiple = FALSE,
                             selected = 1992,    options = list(size = 5))
  
  # Select seasons from a dropdown
  picker_season <- pickerInput(inputId = ns("season"), label = "Season:",
                               choices = c("Q1", "Q2", "Q3", "Q4"), multiple = FALSE,
                               selected = "Q1")
  
  # The UI values will need to be able to react to different comp choices
  
  # The chosen controls per tab
  output$sidebar_controls <- renderUI({
    if(input$comparisonMenu == "map"){
      sidebarMenu(radio_clim_period, picker_year)
    } else if(input$comparisonMenu == "daily"){
      sidebarMenu(radio_clim_period, picker_year)
    } else if(input$comparisonMenu == "total"){
      sidebarMenu(radio_clim_period)
    } else {
      # Intentionally empty
    }
  })

  # Reactive values ---------------------------------------------------------

  ## The pixel files based on reactive years and clim period choices
  # OISST
  cat_pixel_OISST <- reactive({
    req(input$year); req(input$clim_period)
    file_name_OISST <- paste0("../data/annual_summary/OISST_cat_pixel_",
                              input$clim_period,"_",input$year,".Rds")
    if(file.exists(file_name_OISST)) cat_pixel_OISST <- readRDS(file_name_OISST)
  })
  # CCI
  cat_pixel_CCI <- reactive({
    req(input$year); req(input$clim_period)
    file_name_CCI <- paste0("../data/annual_summary/CCI_cat_pixel_",
                              input$clim_period,"_",input$year,".Rds")
    if(file.exists(file_name_CCI)) cat_pixel_CCI <- readRDS(file_name_CCI)
  })
  # CMC
  cat_pixel_CMC <- reactive({
    req(input$year); req(input$clim_period)
    file_name_CMC <- paste0("../data/annual_summary/CMC_cat_pixel_",
                              input$clim_period,"_",input$year,".Rds")
    if(file.exists(file_name_CMC)) cat_pixel_CMC <- readRDS(file_name_CMC)
  })

  ## The daily files based on reactive years and clim period choices
  # OISST
  cat_daily_OISST <- reactive({
    req(input$year); req(input$clim_period)
    file_name_OISST <- paste0("../data/annual_summary/OISST_cat_daily_",
                              input$clim_period,"_",input$year,".Rds")
    if(file.exists(file_name_OISST)) cat_daily_OISST <- readRDS(file_name_OISST)
  })
  # CCI
  cat_daily_CCI <- reactive({
    req(input$year); req(input$clim_period)
    file_name_CCI <- paste0("../data/annual_summary/CCI_cat_daily_",
                            input$clim_period,"_",input$year,".Rds")
    if(file.exists(file_name_CCI)) cat_daily_CCI <- readRDS(file_name_CCI)
  })
  # CMC
  cat_daily_CMC <- reactive({
    req(input$year); req(input$clim_period)
    file_name_CMC <- paste0("../data/annual_summary/CMC_cat_daily_",
                            input$clim_period,"_",input$year,".Rds")
    if(file.exists(file_name_CMC)) cat_daily_CMC <- readRDS(file_name_CMC)
  })
  # Combine for figures
  cat_daily_ALL <- reactive({
    req(input$year); req(input$clim_period)
    
    # testers...
    # cat_daily_OISST <- readRDS("../data/annual_summary/OISST_cat_daily_1992-2018_1992.Rds")
    # cat_daily_CCI <- readRDS("../data/annual_summary/CCI_cat_daily_1992-2018_1992.Rds")
    # cat_daily_CMC <- readRDS("../data/annual_summary/CMC_cat_daily_1992-2018_1992.Rds")
    
    cat_daily_OISST <- cat_daily_OISST()
    cat_daily_CCI <- cat_daily_CCI()
    cat_daily_CMC <- cat_daily_CMC()
    
    cat_daily_OISST$product <- "OISST"
    
    if(input$year > 2018 & input$clim_period == "1992-2018"){
      cat_daily_CMC$product <- "CMC"
      cat_daily_ALL <- rbind(cat_daily_OISST, cat_daily_CMC)
    } else if(input$year > 2018 & input$clim_period == "1982-2011"){
      cat_daily_ALL <- cat_daily_OISST
    } else if(input$year < 1992 | input$clim_period == "1982-2011"){
      cat_daily_CCI$product <- "CCI"
      cat_daily_ALL <- rbind(cat_daily_OISST, cat_daily_CCI)
    } else if(input$clim_period == "1992-2018"){
      cat_daily_CCI$product <- "CCI"
      cat_daily_CMC$product <- "CMC"
      cat_daily_ALL <- rbind(cat_daily_OISST, cat_daily_CCI, cat_daily_CMC)
    } else{
      stop("Something has gone wrong with the daily data selection")
    }
    
    cat_daily_ALL <- cat_daily_ALL %>% 
      mutate(first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4),
             cat_prop = round(cat_n/nrow(OISST_ocean_coords), 4)) %>% 
      group_by(product, t) %>% 
      mutate(cat_prop_stack = case_when(category == "I Moderate" ~ cat_prop[1],
                                        category == "II Strong" ~ cat_prop[1]+cat_prop[2],
                                        category == "III Severe" ~ cat_prop[1]+cat_prop[2]+cat_prop[3],
                                        category == "IV Extreme" ~ sum(cat_prop)),
             first_n_cum_prop_stack = case_when(category == "I Moderate" ~ first_n_cum_prop[1],
                                                category == "II Strong" ~ first_n_cum_prop[1]+first_n_cum_prop[2],
                                                category == "III Severe" ~ first_n_cum_prop[1]+first_n_cum_prop[2]+first_n_cum_prop[3],
                                                category == "IV Extreme" ~ sum(first_n_cum_prop)),
             cat_n_prop_stack = case_when(category == "I Moderate" ~ cat_n_prop[1],
                                                category == "II Strong" ~ cat_n_prop[1]+cat_n_prop[2],
                                                category == "III Severe" ~ cat_n_prop[1]+cat_n_prop[2]+cat_n_prop[3],
                                                category == "IV Extreme" ~ sum(cat_n_prop))) %>% 
      ungroup()
    return(cat_daily_ALL)
  })
  
  cat_total_ALL <- reactive({
    req(input$clim_period)
    file_name_OISST <- paste0("../data/annual_summary/OISST_cat_daily_",
                            input$clim_period,"_total.Rds")
    if(file.exists(file_name_OISST)) cat_total_OISST <- readRDS(file_name_OISST)
    file_name_CCI <- paste0("../data/annual_summary/CCI_cat_daily_",
                            input$clim_period,"_total.Rds")
    if(file.exists(file_name_CCI)) cat_total_CCI <- readRDS(file_name_CCI)
    file_name_CMC <- paste0("../data/annual_summary/CMC_cat_daily_",
                            input$clim_period,"_total.Rds")
    if(file.exists(file_name_CMC)) cat_total_CMC <- readRDS(file_name_CMC)
    
    cat_total_OISST$product <- "OISST"
    
    if(input$clim_period == "1982-2011"){
      cat_total_CCI$product <- "CCI"
      cat_total_ALL <- rbind(cat_total_OISST, cat_total_CCI)
    } else{
      cat_total_CCI$product <- "CCI"
      cat_total_CMC$product <- "CMC"
      cat_total_ALL <- rbind(cat_total_OISST, cat_total_CCI, cat_total_CMC)
    }
    
    # cat_total_ALL <- cat_total_ALL %>% 
    #   group_by(product, t) %>% 
    #   mutate(cat_prop_stack = case_when(category == "I Moderate" ~ cat_prop[1],
    #                                     category == "II Strong" ~ cat_prop[1]+cat_prop[2],
    #                                     category == "III Severe" ~ cat_prop[1]+cat_prop[2]+cat_prop[3],
    #                                     category == "IV Extreme" ~ sum(cat_prop)),
    #          first_n_cum_prop_stack = case_when(category == "I Moderate" ~ first_n_cum_prop[1],
    #                                             category == "II Strong" ~ first_n_cum_prop[1]+first_n_cum_prop[2],
    #                                             category == "III Severe" ~ first_n_cum_prop[1]+first_n_cum_prop[2]+first_n_cum_prop[3],
    #                                             category == "IV Extreme" ~ sum(first_n_cum_prop)),
    #          cat_n_prop_stack = case_when(category == "I Moderate" ~ cat_n_prop[1],
    #                                       category == "II Strong" ~ cat_n_prop[1]+cat_n_prop[2],
    #                                       category == "III Severe" ~ cat_n_prop[1]+cat_n_prop[2]+cat_n_prop[3],
    #                                       category == "IV Extreme" ~ sum(cat_n_prop))) %>% 
    #   ungroup()
    return(cat_total_ALL)
  })
    
  
  # Map figures -------------------------------------------------------------
  
  # The map
  output$compOISST <- renderPlot({
    req(input$year); req(input$clim_period)
    file_name_OISST <- paste0("../data/annual_summary/OISST_cat_pixel_",
                              input$clim_period,"_",input$year,".Rds")
    if(file.exists(file_name_OISST)){
      cat_pixel_OISST <- cat_pixel_OISST()
      fig_map_OISST <- comp_map(cat_pixel_OISST)
      return(fig_map_OISST)
    }
  })
  
  output$compCCI <- renderPlot({
    req(input$year); req(input$clim_period)
    file_name_CCI <- paste0("../data/annual_summary/CCI_cat_pixel_",
                              input$clim_period,"_",input$year,".Rds")
    if(file.exists(file_name_CCI)){
      cat_pixel_CCI <- cat_pixel_CCI()
      fig_map_CCI <- comp_map(cat_pixel_CCI)
      return(fig_map_CCI)
    }
  })
  
  output$compCMC <- renderPlot({
    req(input$year); req(input$clim_period)
    file_name_CMC <- paste0("../data/annual_summary/CMC_cat_pixel_",
                              input$clim_period,"_",input$year,".Rds")
    if(file.exists(file_name_CMC)){
      cat_pixel_CMC <- cat_pixel_CMC()
      fig_map_CMC <- comp_map(cat_pixel_CMC)
      return(fig_map_CMC)
    }
  })
  

  # Latitude figures --------------------------------------------------------
  
  output$compLat <- renderPlotly({
    req(input$year); req(input$clim_period)
    
    # testers...
    # cat_pixel_OISST <- readRDS("../data/annual_summary/OISST_cat_pixel_1992-2018_1992.Rds")
    # cat_pixel_CCI <- readRDS("../data/annual_summary/CCI_cat_pixel_1992-2018_1992.Rds")
    # cat_pixel_CMC <- readRDS("../data/annual_summary/CMC_cat_pixel_1992-2018_1992.Rds")

    cat_pixel_OISST <- cat_pixel_OISST()
    cat_pixel_CCI <- cat_pixel_CCI()
    cat_pixel_CMC <- cat_pixel_CMC()
    
    cat_pixel_OISST$product <- "OISST"
    
    if(input$year > 2018 & input$clim_period == "1992-2018"){
      cat_pixel_CMC$product <- "CMC"
      cat_pixel_ALL <- rbind(cat_pixel_OISST, cat_pixel_CMC)
    } else if(input$year > 2018 & input$clim_period == "1982-2011"){
      cat_pixel_ALL <- cat_pixel_OISST
    } else if(input$year < 1992 | input$clim_period == "1982-2011"){
      cat_pixel_CCI$product <- "CCI"
      cat_pixel_ALL <- rbind(cat_pixel_OISST, cat_pixel_CCI)
    } else if(input$clim_period == "1992-2018"){
      cat_pixel_CCI$product <- "CCI"
      cat_pixel_CMC$product <- "CMC"
      cat_pixel_ALL <- rbind(cat_pixel_OISST, cat_pixel_CCI, cat_pixel_CMC) %>% 
        group_by(product, lat) %>% 
        count(category)
    } else{
      stop("Something has gone wrong with the latitude data selection")
    }
    
    lat_plot <- ggplot(data = cat_pixel_ALL, aes(x = lat, y = n)) +
      geom_line(aes(colour = category, linetype = product, group = 1,
                    text = paste0("Lat: ",lat,"; ",product,": ",n))) +
      scale_colour_manual(values = MHW_colours) +
      labs(colour = NULL, linetype = NULL, 
           y = "Pixel count", x = "Latitude") +
      theme(legend.text = element_text(size = 6),
            legend.key.size = unit(0, 'lines')) +
      coord_cartesian(expand = FALSE)
    
    lat_plotly <- ggplotly(lat_plot, tooltip = "text") %>% #, dynamicTicks = F) %>% 
      layout(hovermode = 'compare', legend = list(font = list(size = 10), tracegroupgap = 0))
    lat_plotly
  })
  

  # Daily figures -----------------------------------------------------------
  
  # Daily count figure
  output$dailyCount <- renderPlotly({
    req(input$year); req(input$clim_period)
    
    cat_daily_ALL <- cat_daily_ALL() 
    
    daily_count_plot <- ggplot(data = cat_daily_ALL, aes(x = t, y = cat_prop)) +
      geom_line(aes(colour = category, linetype = product, group = 1,
                    text = paste0(t," - ",product,": ", round(cat_prop*100, 2),"%"))) +
      scale_y_continuous(limits = c(0, 1),
                         breaks = seq(0.2, 0.8, length.out = 4),
                         labels = paste0(seq(20, 80, by = 20), "%")) +
      scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
      scale_colour_manual(values = MHW_colours) +
      labs(colour = NULL, linetype = NULL, 
           y = "Global MHW count\n(non-cumulative)", x = "Day of the year") +
      theme(legend.text = element_text(size = 6),
            legend.key.size = unit(0, 'lines')) +
      coord_cartesian(expand = FALSE)
    
    daily_count_plotly <- ggplotly(daily_count_plot, tooltip = "text") %>% #, dynamicTicks = F) %>% 
      layout(hovermode = 'compare', legend = list(font = list(size = 10), tracegroupgap = 0))
    daily_count_plotly
  })
  
  # Daily first occurrence figure
  output$dailyFirst <- renderPlotly({
    req(input$year); req(input$clim_period)
    
    cat_daily_ALL <- cat_daily_ALL()
    
    daily_first_plot <- ggplot(data = cat_daily_ALL, aes(x = t, y = first_n_cum_prop)) +
      geom_line(aes(colour = category, linetype = product, group = 1,
                    text = paste0(t," - ",product,": ",round(first_n_cum_prop*100, 2),"%"))) +
      scale_y_continuous(limits = c(0, 1),
                         breaks = seq(0.2, 0.8, length.out = 4),
                         labels = paste0(seq(20, 80, by = 20), "%")) +
      scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
      scale_colour_manual(values = MHW_colours) +
      labs(colour = NULL, linetype = NULL, 
           y = "Top MHW category per pixel\n(cumulative)", x = "Day of first occurrence") +
      theme(legend.text = element_text(size = 6),
            legend.key.size = unit(0, 'lines')) +
      coord_cartesian(expand = FALSE)
    
    daily_first_plotly <- ggplotly(daily_first_plot, tooltip = "text") %>% #, dynamicTicks = F) %>% 
      layout(hovermode = 'compare', legend = list(font = list(size = 10), tracegroupgap = 0))
    daily_first_plotly
  })
  
  # Daily cumulative count figure
  output$dailyCum <- renderPlotly({
    req(input$year); req(input$clim_period)
    
    cat_daily_ALL <- cat_daily_ALL()
    
    daily_cum_plot <- ggplot(data = cat_daily_ALL, aes(x = t, y = cat_n_prop)) +
      geom_line(aes(colour = category, linetype = product, group = 1,
                    text = paste0(t," - ",product,": ", round(cat_n_prop, 2)))) +
      scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
      scale_colour_manual(values = MHW_colours) +
      labs(colour = NULL, linetype = NULL, 
           y = "Average MHW days per pixel\n(cumulative)", x = "Day of the year") +
      theme(legend.text = element_text(size = 6),
            legend.key.size = unit(0, 'lines')) +
      coord_cartesian(expand = FALSE)
    
    daily_cum_plotly <- ggplotly(daily_cum_plot, tooltip = "text") %>% #, dynamicTicks = F) %>% 
      layout(hovermode = 'compare', legend = list(font = list(size = 10), tracegroupgap = 0))
    daily_cum_plotly
  })
  

  # Total figures -----------------------------------------------------------

  # total count figure
  output$totalCount <- renderPlotly({
    req(input$clim_period)
    
    cat_total_ALL <- cat_total_ALL() 
    
    total_count_plot <- ggplot(data = cat_total_ALL, aes(x = t, y = cat_prop_daily_mean)) +
      geom_line(aes(colour = category, linetype = product, group = 1,
                    text = paste0(t," - ",product,": ", round(cat_prop_daily_mean*100, 2),"%"))) +
      scale_y_continuous(limits = c(0, 1),
                         breaks = seq(0.2, 0.8, length.out = 4),
                         labels = paste0(seq(20, 80, by = 20), "%")) +
      scale_x_continuous(breaks = seq(1982, 2019, 5)) +
      scale_colour_manual(values = MHW_colours) +
      labs(colour = NULL, linetype = NULL, 
           y = "Daily MHW occurrence", x = NULL) +
      theme(legend.text = element_text(size = 6),
            legend.key.size = unit(0, 'lines')) +
      coord_cartesian(expand = FALSE)
    
    total_count_plotly <- ggplotly(total_count_plot, tooltip = "text") %>% #, dynamicTicks = F) %>% 
      layout(hovermode = 'compare', legend = list(font = list(size = 10), tracegroupgap = 0))
    total_count_plotly
  })
  
  # total first occurrence figure
  output$totalFirst <- renderPlotly({
    req(input$clim_period)
    
    cat_total_ALL <- cat_total_ALL()
    
    total_first_plot <- ggplot(data = cat_total_ALL, aes(x = t, y = first_n_cum_prop)) +
      geom_line(aes(colour = category, linetype = product, group = 1,
                    text = paste0(t," - ",product,": ",round(first_n_cum_prop*100, 2),"%"))) +
      scale_y_continuous(limits = c(0, 1),
                         breaks = seq(0.2, 0.8, length.out = 4),
                         labels = paste0(seq(20, 80, by = 20), "%")) +
      scale_x_continuous(breaks = seq(1982, 2019, 5)) +
      scale_colour_manual(values = MHW_colours) +
      labs(colour = NULL, linetype = NULL, 
           y = "Total MHW occurrence", x = NULL) +
      theme(legend.text = element_text(size = 6),
            legend.key.size = unit(0, 'lines')) +
      coord_cartesian(expand = FALSE)
    
    total_first_plotly <- ggplotly(total_first_plot, tooltip = "text") %>% #, dynamicTicks = F) %>% 
      layout(hovermode = 'compare', legend = list(font = list(size = 10), tracegroupgap = 0))
    total_first_plotly
  })
  
  # total cumulative count figure
  output$totalCum <- renderPlotly({
    req(input$clim_period)
    
    cat_total_ALL <- cat_total_ALL()
    
    total_cum_plot <- ggplot(data = cat_total_ALL, aes(x = t, y = cat_n_prop)) +
      geom_line(aes(colour = category, linetype = product, group = 1,
                    text = paste0(t," - ",product,": ", round(cat_n_prop, 2)))) +
      scale_x_continuous(breaks = seq(1982, 2019, 5)) +
      scale_colour_manual(values = MHW_colours) +
      labs(colour = NULL, linetype = NULL, 
           y = "MHW days/pixel", x = NULL) +
      theme(legend.text = element_text(size = 6),
            legend.key.size = unit(0, 'lines')) +
      coord_cartesian(expand = FALSE)
    
    total_cum_plotly <- ggplotly(total_cum_plot, tooltip = "text") %>% #, dynamicTicks = F) %>% 
      layout(hovermode = 'compare', legend = list(font = list(size = 10), tracegroupgap = 0))
    total_cum_plotly
  })
  
}

