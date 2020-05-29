summaryUI <- function(id, label = 'summary') {
  ns <- NS(id)
  
  # Side panel + main panel with tabs
  fluidPage(theme = "style.css",
            sidebarLayout(fluid = T,
                          uiOutput(outputId = ns("summary_UI")),
                          mainPanel(width = 10,
                                    tabsetPanel(id = ns("summary_tabs"), #type = "pills",
                                                tabPanel(title = "Annual", value = "annual_tab",
                                                         shinycssloaders::withSpinner(imageOutput(outputId = ns("summary_all")),
                                                                                      type = 6, color = "#b0b7be")
                                                ),
                                                tabPanel(title = "Historic", value = "historic_tab",
                                                         shinycssloaders::withSpinner(imageOutput(outputId = ns("historic_all")),
                                                                                      type = 6, color = "#b0b7be"))
                                    )
                          )
            )
  )
}

# cat("\nsummary_ui.R finished")

summaryUI <- function(id, label = 'summary') {
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
                  sidebarMenu(id = ns("summaryMenu"),
                              menuItem("Annual", tabName = "annual", icon = icon("chart-bar"), selected = TRUE),
                              menuItem("Total", tabName = "total", icon = icon("chart-bar")),
                              # menuItem("Map", tabName = "map", icon = icon("map")),
                              # menuItem("Season", tabName = "season", icon = icon("chart-pie")),
                              # menuItem("Tables", tabname = "tables", icon = icon("table")),
                              menuItem("About", tabName = "about", icon = icon("question")),
                              # The reactive controls based on the primary option chosen
                              uiOutput(outputId = ns("summarySidebarControls")))
                ),

                # The dashboard
                dashboardBody(
                  tabItems(


                    # Annual figures ----------------------------------------------------------


                    tabItem(tabName = "annual",
                            fluidRow(box(imageOutput(outputId = ns("summary_all"), type = 6, color = "#b0b7be")), 
                                     width = 12, title = "Annual summary", status = "primary", solidHeader = TRUE, collapsible = TRUE)),


                    # Total figures -----------------------------------------------------------

                  tabItem(tabName = "total",
                          fluidRow(box(shinycssloaders::withSpinner(imageOutput(outputId = ns("total_all")),
                                                                    type = 6, color = "#b0b7be")), width = 12, title = "Total summary",
                                   status = "primary", solidHeader = TRUE, collapsible = TRUE)),


                    # App explanation ---------------------------------------------------------

                    tabItem(tabName = "about",
                            h2("This is where the functioning of the app will be explained..."))
                  )
                )
  )
}
