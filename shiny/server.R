server <- function(input, output, session){
  
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade", time = 2)
  
  callModule(map, "map")
  
  callModule(summary, "summary")
  
  callModule(about, 'about')
  
  # During testing...
  session$onSessionEnded(stopApp)
}

