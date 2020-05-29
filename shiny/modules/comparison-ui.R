comparisonUI <- function(id, label = 'comparison') {
  ns <- NS(id)
  
  ## Tracker ideas
  # - Show comparisons in two panels next to each other
  # - Preferably a dashboard
  # - High level choice on the left is which year is being compared
  # - This will also allow for an overall comparison and seasons (e.g, Q1 - Q4)
  
  dashboardPage(skin = "black",
    
    # The app title
    dashboardHeader(disable = TRUE),
    
    # The primary options
    dashboardSidebar(
      sidebarMenu(id = ns("comparisonMenu"),
                  menuItem("Total", tabName = "total", icon = icon("chart-bar"), selected = TRUE),
                  menuItem("Daily", tabName = "daily", icon = icon("chart-bar")),
                  menuItem("Map", tabName = "map", icon = icon("map")),
                  # menuItem("Season", tabName = "season", icon = icon("chart-pie")),
                  # menuItem("Tables", tabname = "tables", icon = icon("table")),
                  menuItem("About", tabName = "about", icon = icon("question")),
                  # The reactive controls based on the primary option chosen
                  uiOutput(outputId = ns("sidebar_controls")))
    ),
    
    # The dashboard
    dashboardBody(
      tabItems(
        

        # Total figures -----------------------------------------------------------
        
        tabItem(tabName = "total",
                fluidRow(box(plotlyOutput(ns("totalCount")), width = 12, title = "Average daily MHW occurrence per year (% of ocean)",
                             status = "primary", solidHeader = TRUE, collapsible = TRUE),
                         box(plotlyOutput(ns("totalFirst")), width = 12, title = "Total coverage per year (% of ocean)",
                             status = "warning", solidHeader = TRUE, collapsible = TRUE),
                         box(plotlyOutput(ns("totalCum")), width = 12, title = "Total MHW days per pixel per year",
                             status = "success", solidHeader = TRUE, collapsible = TRUE))),
        

        # Daily figures -----------------------------------------------------------

        tabItem(tabName = "daily",
                fluidRow(box(plotlyOutput(ns("dailyCount")), width = 12, title = "Daily count (% of ocean)",
                             status = "primary", solidHeader = TRUE, collapsible = TRUE),
                         box(plotlyOutput(ns("dailyFirst")), width = 12, title = "Total coverage (% of ocean)",
                             status = "warning", solidHeader = TRUE, collapsible = TRUE),
                         box(plotlyOutput(ns("dailyCum")), width = 12, title = "MHW days per pixel",
                             status = "success", solidHeader = TRUE, collapsible = TRUE))),
        
        # Map figures -------------------------------------------------------------

        tabItem(tabName = "map",
                fluidRow(box(plotOutput(ns("compOISST")), width = 4, title = "OISST", # These titles will need to be reactive
                             status = "primary", solidHeader = TRUE, collapsible = TRUE),
                         box(plotOutput(ns("compCCI")), width = 4, title = "CCI",
                             status = "success", solidHeader = TRUE, collapsible = TRUE),
                         box(plotOutput(ns("compCMC")), width = 4, title = "CMC",
                             status = "warning", solidHeader = TRUE, collapsible = TRUE)),
                fluidRow(box(plotlyOutput(ns("compLat")), width = 12, title = "Latitude",
                             status = "danger", solidHeader = TRUE, collapsible = TRUE))),


        
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

