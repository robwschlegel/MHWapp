summary <- function(input, output, session) {
  
  ns <- session$ns
  
  
  # Render UI ---------------------------------------------------------------
  
  # Radio buttons to choose clim period
  summary_clim_period_radio <- prettyRadioButtons(inputId = ns("summary_clim_period"), label = "Clim. period:",
                                                  choices = c("1982-2011", "1992-2018"),
                                                  selected = "1992-2018", inline = F,
                                                  status = "primary", fill = TRUE)
  
  # Select years from a dropdown
  summary_year_picker <- pickerInput(inputId = ns("summary_year"), label = h4("Annual summary"),
                                     choices = seq(1982, lubridate::year(Sys.time())), multiple = FALSE,
                                     selected = lubridate::year(Sys.time())-1)
  
  # Select SST products from a dropdown
  summary_product_picker <- pickerInput(inputId = ns("summary_product"), label = h4("Product"),
                                        choices = c("OISST", "CCI", "CMC"), multiple = FALSE,
                                        selected = "OISST")
  
  # The caption for the annual summary figure
  annual_caption <- dropdownButton(
    p("Annual global marine heatwave (MHW) occurrence."),
    p("A) Global map showing the highest category experienced at each pixel over the course of the year estimated using
      the NOAA OI v2 data set.White shows that no MHWs occurred in a pixel over the entire year."),
    p("B) Stacked barplot showing the percent of ocean pixels experiencing a MHW on any given day of the year."), 
    p("C) Stacked barplot showing the cumulative percent of the ocean that has experienced a MHW over the year. 
      These values are based on when in the year a pixel first experiences it's highest category MHW, so no pixel 
      is counted more than once. Horizontal bars in this figure show the final percentage values for each category of MHW."), 
    p("D) Stacked barplots showing the cumulative number of MHW days averaged over all pixels in the ocean."),
    circle = FALSE,
    right = FALSE, up = FALSE, width = "500px",
    label = "Caption", tooltip = FALSE)
  
  # The caption for the total summary figure
  total_caption <- dropdownButton(
    p("Total global marine heatwave (MHW) occurrence. MHWs calculated with the Hobday et al. 2016 definition and categories calculated
      with the Hobday et al. 2018 deifnition."),
    p("A) The daily average percent of the ocean that experienced a MHW."),
    p("B) The proportion of the highest category of MHW experienced."),
    p("C) The total average of daily MHW occurrence throughout the entire ocean."),
    circle = FALSE,
    right = FALSE, up = FALSE, width = "500px",
    label = "Caption", tooltip = FALSE)
  
  # Download button for annual summary
  download_annual_summary <- downloadButton(outputId = ns("annual_summary_dl"))
  
  # Download button for annual summary
  download_total_summary <- downloadButton(outputId = ns("total_summary_dl"))
  
  # The chosen controls per tab
  output$summarySidebarControls <- renderUI({
    if(input$summaryMenu == "summary_annual"){
      sidebarMenu(summary_clim_period_radio, summary_product_picker, summary_year_picker, annual_caption, download_annual_summary)
    } else if(input$summaryMenu == "summary_total"){
      sidebarMenu(summary_clim_period_radio, summary_product_picker, total_caption, download_total_summary)
    } else {
      # Intentionally empty
    }
  })
  
  # Reactive values ---------------------------------------------------------
  
  ## Values used to inform the pickers as to the valid options based on clim period + product
  
  ## The pixel files based on reactive product, year, and clim period choices
  cat_pixel <- reactive({
    req(input$summary_year); req(input$summary_product); req(input$summary_clim_period)
    file_name <- paste0("../data/annual_summary/",input$summary_product,"_cat_pixel_",
                        input$summary_clim_period,"_",input$summary_year,".Rds")
    if(file.exists(file_name)) cat_pixel <- readRDS(file_name)
  })

  ## The daily files based on reactive years and clim period choices
  cat_daily <- reactive({
    req(input$summary_year); req(input$summary_product); req(input$summary_clim_period)
    file_name <- paste0("../data/annual_summary/",input$summary_product,"_cat_daily_",
                        input$summary_clim_period,"_",input$summary_year,".Rds")
    if(file.exists(file_name)){
      cat_daily <- readRDS(file_name)
      return(cat_daily)
    }
  })
  
  ## The total summary file
  cat_total <- reactive({
    req(input$summary_product); req(input$summary_clim_period)
    file_name <- paste0("../data/annual_summary/",input$summary_product,"_cat_daily_",
                        input$summary_clim_period,"_total.Rds")
    if(file.exists(file_name)) cat_total <- readRDS(file_name)
    return(cat_total)
  })
  
  
  # Figures -----------------------------------------------------------------
  
  ## The total display
  ### Stacked barplot of global daily count of MHWs by category
  output$total_daily_count <- renderPlot({
    
    cat_total <- cat_total()
    
    ggplot(cat_total, aes(x = t, y = cat_prop_daily_mean)) +
      geom_bar(aes(fill = category), stat = "identity", show.legend = T,
               position = position_stack(reverse = TRUE), width = 1) +
      scale_fill_manual("Category", values = MHW_colours) +
      scale_y_continuous(limits = c(0, 1),
                         breaks = seq(0.2, 0.8, length.out = 4),
                         labels = paste0(seq(20, 80, by = 20), "%")) +
      scale_x_continuous(breaks = seq(1982, 2019, 5)) +
      labs(y = "Daily MHW occurrence", x = NULL) +
      coord_cartesian(expand = F) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.position = "bottom",
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 16))
  })
  
  ### Stacked barplot of cumulative percent of ocean affected by MHWs
  output$total_cum_perc <- renderPlot({
    
    cat_total <- cat_total()
    
    ggplot(cat_total, aes(x = t, y = first_n_cum_prop)) +
      geom_bar(aes(fill = category), stat = "identity", show.legend = T,
               position = position_stack(reverse = TRUE), width = 1) +
      scale_fill_manual("Category", values = MHW_colours) +
      scale_y_continuous(limits = c(0, 1),
                         breaks = seq(0.2, 0.8, length.out = 4),
                         labels = paste0(seq(20, 80, by = 20), "%")) +
      scale_x_continuous(breaks = seq(1982, 2019, 5)) +
      labs(y = "Total MHW occurrence", x = NULL) +
      coord_cartesian(expand = F) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.position = "bottom",
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 16))
  })
  
  ### Load chosen year's summary; pre-rendered
  output$summary_all <- renderImage({
    return(list(
      src = paste0("../figures/MHW_cat_summary_",input$summary_year,".png"),
      width = 1080,
      height = 720,
      contentType = "image/png"
    ))
  }, deleteFile = FALSE)
  
  ### Load chosen year's summary; pre-rendered
  output$total_all <- renderImage({
    return(list(
      src = paste0("../figures/MHW_cat_historic.png"),
      width = 1080,
      height = 360,
      contentType = "image/png"
    ))
  }, deleteFile = FALSE)
  
  
  # Test box
  output$test_box <- renderText({
    paste0(input$summary_year, input$summary_product, input$summary_clim_period)
    })

  
  # Downloads ---------------------------------------------------------------
  
  ### Download handler for annual summary figures
  output$annual_summary_dl <- downloadHandler(
    filename = function() {
      paste0("MHW_cat_summary_",input$summary_year,".png")
    },
    content <- function(file) {
      png::writePNG(png::readPNG(paste0("../figures/MHW_cat_summary_",input$summary_year,".png")), file, dpi = 300)
    }
  )
  
  ### Download handler for total summary figures
  output$total_summary_dl <- downloadHandler(
    filename = function() {
      paste0("MHW_cat_historic.png")
    },
    content <- function(file) {
      png::writePNG(png::readPNG(paste0("../figures/MHW_cat_historic.png")), file, dpi = 300)
    }
  )
  
}

