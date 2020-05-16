comparisonUI <- function(id, label = 'comparison') {
  ns <- NS(id)
  
  ## Tracker ideas
  # - Show comparisons in two panels next to each other
  # - Preferably a dashboard
  # - High level choice on the left is which year is being compared
  # - This will also allow for an overall comparison and seasons (e.g, Q1 - Q4)
  
  dashboardPage(
    
    # The app title
    # dashboardHeader(title = "Correlation between SST and other variable anomalies during MHWs"),
    dashboardHeader(title = "MHW flux"),
    
    # The primary options
    dashboardSidebar(
      sidebarMenu(id = "mainMenu",
                  menuItem("Annual", tabName = "annual", icon = icon("map"), selected = TRUE),
                  # menuItem("Season", tabName = "season", icon = icon("chart-pie")),
                  # menuItem("Total", tabName = "total", icon = icon("chart-bar")),
                  # menuItem("Tables", tabname = "tables", icon = icon("table")),
                  menuItem("About", tabName = "about", icon = icon("question")),
                  # The reactive controls based on the primary option chosen
                  uiOutput(outputId = "sidebar_controls"))
    ),
    
    # The dashboard
    dashboardBody(
      tabItems(
        

        # Annual figures ----------------------------------------------------------

        tabItem(tabName = "annual",
                fluidRow(box(plotOutput(ns("compOne")), width = 4, title = "OISST",
                             status = "primary", solidHeader = TRUE, collapsible = TRUE),
                         box(plotOutput(ns("compTwo")), width = 4, title = "CCI",
                             status = "primary", solidHeader = TRUE, collapsible = TRUE),
                         box(plotOutput(ns("compThree")), width = 4, title = "CMC",
                             status = "primary", solidHeader = TRUE, collapsible = TRUE))),
        

        # Season figures ----------------------------------------------------------
# 
#         tabItem(tabName = "season",
#                 # The event metric table
#                 fluidRow(box(dataTableOutput("eventTable"), width = 12, title = "Event metrics", 
#                              status = "primary", solidHeader = TRUE, collapsible = TRUE))),
        # Test box
        # fluidRow(box(verbatimTextOutput("devel")))),
        

        # Total figures -----------------------------------------------------------

        # tabItem(tabName = "total", 
        #         fluidRow(
        #           # Histogram box
        #           box(width = 6, title = "Histogram", status = "primary", solidHeader = TRUE, collapsible = TRUE,
        #               dropdownButton(
        #                 h4("Histogram controls:"),
        #                 radioButtons(inputId = "position", label = "Position:", 
        #                              choices = c("stack", "dodge"),
        #                              selected = "stack", inline = T),
        #                 sliderInput(inputId = "bins", label = "Number of bins:",
        #                             min = 1, max = 20, value = 10),
        #                 circle = TRUE, status = "danger", icon = icon("gear"))))),
        
        
        # Tables ------------------------------------------------------------------
        
        # tabItem(tabname = "tables",
        #         # tableOutput("resultsKable"),
        #         h2("test")),
        
        
        # App explanation ---------------------------------------------------------
        
        tabItem(tabName = "about", 
                h2("This is where the functioning of the app will be explained..."))
      )
    )
  )
}

