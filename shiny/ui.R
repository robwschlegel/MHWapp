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
              navbarPage(title = "Marine Heatwave Tracker",
                         selected = 'map_tab',
                         header = list(cicerone::use_cicerone()), # Start guided tour
                         # id = "nav",
                         # position = "fixed-top",
                         tabPanel(title = 'Map',
                                  value = 'map_tab',
                                  mapUI('map')),
                         tabPanel(title = 'Comparison',
                                  value = 'comp_tab',
                                  comparisonUI('comparison')),
                         tabPanel(title = 'Summary',
                                  value = 'sum_tab',
                                  summaryUI('summary')),
                         tabPanel(title = 'About',
                                  value = 'about_tab',
                                  aboutUI('about')),
                         collapsible = TRUE, fluid = TRUE)#,
              # cicerone::use_cicerone()
)

