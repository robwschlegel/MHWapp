ui = tagList(shinyjs::useShinyjs(),  
             tags$head(includeCSS("style.css")),
             # tags$style(".datepicker { z-index: 9999 !important; }"),
             # Loading message
             div(
               id = "loading-content",
               h2("Loading...")
             ),
             # hidden(
               # div(
                 # id = "app-content",
             navbarPage(title =  "Marine Heatwave Tracker",
                        selected = 'Map',
                        tabPanel(title = 'Map',
                                 # br(),
                                 mapUI('map')),
                        tabPanel(title = 'Summary',
                                 # br(),
                                 summaryUI('summary')),
                        tabPanel(title = 'About',
                                 # br(),
                                 aboutUI('about')),
                        collapsible = TRUE)
               # )
             # )
)
## NB: Consider shifting css file chosen based on app window width