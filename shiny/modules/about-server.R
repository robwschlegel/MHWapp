about <- function(input, output, session) {
  # observeEvent(input$contactUs, {
  #   shinyjs::toggle("feedbackForm", anim = TRUE, animType = "slide", time = 0.2)
  # })
  # 
  # observeEvent(input$info1, {
  #   shinyjs::toggle("div1", anim = TRUE, animType = "slide", time = 0.2)
  # })
  # 
  # observeEvent(input$info2, {
  #   shinyjs::toggle("div2", anim = TRUE, animType = "slide", time = 0.2)
  # })
  # 
  # ### --- feedback
  # create_feedback <- reactive({
  #   email <- input$email
  #   data.frame(Project = proj,
  #              Email = email,
  #              Comment = input$comment)
  # })
  # 
  # observe({
  #   shinyjs::toggleState("submitFeedback", condition = input$comment != "")
  # })
  # 
  # observeEvent(input$submitFeedback, {   
  #   progress <- shiny::Progress$new()
  #   on.exit(progress$close())
  #   progress$set(message = "Sending message...", value = 0.5)  
  #   feedback <- create_feedback()
  #   slackr::slackr(feedback)
  #   showModal(modalDialog(
  #     footer = modalButton("OK"),
  #     title = "",
  #     "Thanks! Your message was successfully submitted."
  #   ))
  #   shinyjs::reset('feedbackForm')
  #   shinyjs::hide('feedbackForm')})
}

