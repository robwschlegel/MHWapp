summary <- function(input, output, session) {
  
  ns <- session$ns
  
  
  # Render UI ---------------------------------------------------------------
  
  # Select years from a dropdown
  picker_year <- pickerInput(inputId = ns("summary_year"), label = h4("Annual summary"),
                             choices = seq(1982, lubridate::year(Sys.time())), multiple = FALSE,
                             selected = lubridate::year(Sys.time())-1)
  
  # Select SST products from a dropdown
  picker_product <- pickerInput(inputId = ns("summary_product"), label = h4("Product"),
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
    req(input$summaryMenu)
    if(input$summaryMenu == "annual"){
      sidebarMenu(picker_product, picker_year, annual_caption, download_annual_summary)
    } else if(input$summaryMenu == "total"){
      sidebarMenu(picker_product, total_caption, download_total_summary)
    } else {
      # Intentionally empty
    }
  })
  
  
  # Figures -----------------------------------------------------------------
  
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

