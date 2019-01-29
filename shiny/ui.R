ui = tagList(useShinyjs(),  
             tags$head(includeCSS("style.css")),
             navbarPage(title =  "MHW Tracker",
                        selected = 'Map',
                        tabPanel(title = 'Map',
                                 br(),
                                 mapUI('map')),
                        tabPanel(title = 'About',
                                 br(),
                                 aboutUI('about')))
)
## NB: Consider shifting css file chosen based on app window width