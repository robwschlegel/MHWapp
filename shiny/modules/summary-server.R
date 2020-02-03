summary <- function(input, output, session) {
  ns <- session$ns
  
  ### Load chosen year's summary; pre-rendered
  output$summary_all <- renderImage({
      return(list(
        src = paste0("../figures/MHW_cat_summary_",input$summary_year,".png"),
        contentType = "image/png"
      ))
  }, deleteFile = FALSE)

}
