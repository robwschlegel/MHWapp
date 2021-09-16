ui <- tagList(shinyjs::useShinyjs(),  
              # CSS
              tags$head(includeCSS("style.css")),
              # Loading message
              div(id = "loading-content",
                  br(), br(),
                  h2("Loading"),
                  br(), br(), br(),
                  h5("If this screen lasts for more than 10 seconds please try Firefox."),
                  br(),
                  h5("If Firefox doesn't work you may be experiencing a Mac OS Javascript bug."),
                  br(),
                  h5("Please consult the 'Bugs?' section on the 'About' page for more information.")),
              # Nav bar tabs
              navbarPage(title =  "Marine Heatwave Tracker",
                         selected = 'Map',
                         tabPanel(title = 'Map',
                                  mapUI('map')),
                         tabPanel(title = 'Comparison',
                                  comparisonUI('comparison')),
                         tabPanel(title = 'Summary',
                                  summaryUI('summary')),
                         tabPanel(title = 'About',
                                  aboutUI('about')),
                         collapsible = TRUE, fluid = TRUE)
)

