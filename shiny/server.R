function(input, output, session) {
  
  callModule(map, "map")
  
  callModule(summary, "summary")
  
  callModule(about, 'about')
  
  shinyjs::hide(id = "loading-content", anim = TRUE, animType = "fade", time = 2)
  
  # During testing...
  # session$onSessionEnded(stopApp)
}

