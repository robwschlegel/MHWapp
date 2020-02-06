summary <- function(input, output, session) {
  ns <- session$ns
  

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

  
# Downloads ---------------------------------------------------------------
  
  ### Summary figure
  # downloadAnnualSummary <- reactive({
  #   annual_fig <- png::readPNG(paste0("../figures/MHW_cat_summary_",input$summary_year,".png"))
  #   return(annual_fig)
  # })
  
  output$annual_summary_dl <- downloadHandler(
    filename = function() {
      paste0("MHW_cat_summary_",input$summary_year,".png")
    },
    content <- function(file) {
      png::writePNG(png::readPNG(paste0("../figures/MHW_cat_summary_",input$summary_year,".png")), file, dpi = 300)
    }
  )
  
}
