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
                   downloadButton(outputId = ns("annual_summary_dl"))
                   )
    } else if(input$summary_tabs == "historic_tab"){
      sidebarPanel(width = 2,
                   h4("Historic summary"),
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

