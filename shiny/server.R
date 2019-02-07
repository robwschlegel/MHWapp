function(input, output, session) {
  
  callModule(about, 'about')
  
  callModule(map, "map")
  
  hide(id = "loading-content", anim = TRUE, animType = "fade", time = 2)
  # shinyjs::show(id = "app-content")
}

