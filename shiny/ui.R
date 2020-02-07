ui = tagList(shinyjs::useShinyjs(),  
             # CSS
             tags$head(includeCSS("style.css")),
             # Loading message
             div(
               id = "loading-content",
               h2("Loading...")
             ),
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
)

