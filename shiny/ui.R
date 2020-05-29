ui <- tagList(shinyjs::useShinyjs(),  
              # CSS
              tags$head(includeCSS("style.css")),
              # Loading message
              div(id = "loading-content",
                  br(), br(),
                  h2("Loading...")),
              # Nav bar tabs
              navbarPage(title =  "Marine Heatwave Tracker",
                         selected = 'Summary',
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

