summary <- function(input, output, session) {
  ns <- session$ns


# Reactive UI -------------------------------------------------------------

  output$summary_UI <- renderUI({
    if(input$summary_tabs == "annual_tab"){
      sidebarPanel(width = 2,
                   selectInput(inputId = ns("summary_year"), 
                               label = h4("Annual summary"),
                               choices = seq(1982, lubridate::year(Sys.time())), 
                                             selected = lubridate::year(Sys.time())-1, multiple = F),
                   # h4("Caption:"),
                   shinyWidgets::dropdownButton(
                    p("Annual global marine heatwave (MHW) occurrence.
                      A) Global map showing the highest category experienced at each pixel over the course of the year estimated using
                      the NOAA OI v2 data set.White shows that no MHWs occurred in a pixel over the entire year. B) Stacked barplot
                      showing the percent of ocean pixels experiencing a MHW on any given day of the year. C) Stacked barplot showing the
                      cumulative percent of the ocean that has experienced a MHW over the year. These values are based on when in the year a
                      pixel first experiences it's highest category MHW, so no pixel is counted more than once. Horizontal bars in this figure show
                      the final percentage values for each category of MHW. D) Stacked barplots showing the cumulative number of MHW days
                      averaged over all pixels in the ocean."),
                     circle = FALSE,
                     right = FALSE, up = FALSE, width = "500px",
                     label = "Caption", tooltip = FALSE),
                   br(),
                   downloadButton(outputId = ns("annual_summary_dl"))
                   )
    } else if(input$summary_tabs == "historic_tab"){
      sidebarPanel(width = 2,
                   h4("Historic summary"),
                   shinyWidgets::dropdownButton(
                    p("Historic global marine heatwave (MHW) occurrence. MHWs calculated with the Hobday et al. 2016 definition and categories calculated
                      with the Hobday et al. 2018 deifnition. A) The daily average percent of the ocean that experienced a MHW. The values shown are for 
                      the highest category of MHW experienced. C) The total average of daily MHW occurrence throughout the entire ocean."),
                    circle = FALSE,
                    right = FALSE, up = FALSE, width = "500px",
                    label = "Caption", tooltip = FALSE),
                   br(),
                   downloadButton(outputId = ns("historic_summary_dl"))
                   )
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
  output$historic_all <- renderImage({
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
  
  ### Download handler for historicsummary figures
  output$historic_summary_dl <- downloadHandler(
    filename = function() {
      paste0("MHW_cat_historic.png")
    },
    content <- function(file) {
      png::writePNG(png::readPNG(paste0("../figures/MHW_cat_historic.png")), file, dpi = 300)
    }
  )
  
}

