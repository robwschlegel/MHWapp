server <- function(input, output, session){
  
  # shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade", time = 2)
  
  # callModule(map, "map")
  
  # Deactivated primarily for speed, but also because the underlying packages change too often
  # callModule(comparison, "comparison")
  # callModule(summary, "summary")
  
  # callModule(about, "about")
  
  # During testing...
  session$onSessionEnded(stopApp)
}

