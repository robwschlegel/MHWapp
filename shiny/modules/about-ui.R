aboutUI <- function(id, label = 'About') {
  ns <- NS(id)
  
  fluidRow(
    column(1,
           HTML("")),
    column(10,
           h4(paste("Welcome!")),
           br(),
           p("Unfortunately this app was not designed for use on a mobile device (e.g. cell phone or tablet).
             It will therefore appear rather clumsy if being viewed on a small screen."),
           br()#,
           #' h5(disclaimer, a("GitHub page.", href = "https://github.com/poissonconsulting/rtide")),
           #' actionLink(ns('info2'), "Citation info"),
           #' hidden(div(id = ns("div2"),
           #'            h6(HTML("Data downloaded from this app are generated from the 'rtide' R package.<br><br>
           #'                    To cite package 'rtide' in publications use:<br><br>
           #'                    
           #'                    Joe Thorley, Luke Miller and Abram Fleishman (2018). rtide: Tide
           #'                    Heights. R package version 0.0.4.9010.
           #'                    https://github.com/poissonconsulting/rtide <br><br>
           #'                    
           #'                    To cite app: <br><br>
           #'                    Seb Dalgarno (2018). rtide: An app to download and view tide height predictions.
           #'                    https://poissonconsulting.shinyapps.io/rtide/<br><br>
           #'                    
           #'                    A BibTeX entry for LaTeX users is: <br><br>
           #'                    
           #'                    @Manual{,
           #'                    title = {rtide: Tide Heights},
           #'                    author = {Joe Thorley and Luke Miller and Abram Fleishman},
           #'                    year = {2018},
           #'                    note = {R package version 0.0.4.9010},
           #'                    url = {https://github.com/poissonconsulting/rtide},
           #'                    }")))),
           #' br(),
           #' actionLink(ns('contactUs'), label = 'Contact us'),
           #' shinyjs::hidden(div(id = ns('feedbackForm'),
           #'                     br(),
           #'                     wellPanel(div(
           #'                       textInput(ns('email'), "Email (optional)"),
           #'                       textAreaInput(ns("comment"), label = NULL, width = "100%", height = '100px'),
           #'                       class = 'error-message'),
           #'                       actionButton(ns("submitFeedback"), "Submit")))),
           #' hr(),
           #' h6('rtide R package and app were developed by Poisson Consulting.'),
           #' actionLink(inputId = 'poisson', 
           #'            label = img(src = 'poisson-logo.png',
           #'                        height = 177/5,
           #'                        width = 739/5,
           #'                        onclick = "window.open('http://www.poissonconsulting.ca', '_blank')"))
                      ),
    column(1,
           HTML("")))
}