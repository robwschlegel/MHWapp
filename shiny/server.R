function(input, output, session) {
  
  callModule(about, 'about')
  
  callModule(map, "map")
  
  hide(id = "loading-content", anim = TRUE, animType = "fade", time = 1)
  # shinyjs::show(id = "app-content")
}

