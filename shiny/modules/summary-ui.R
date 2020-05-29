summaryUI <- function(id, label = 'summary') {
  ns <- NS(id)
  
  dashboardPage(skin = "black",
                
                # The app title
                dashboardHeader(disable = TRUE),
                
                # The primary options
                dashboardSidebar(
                  sidebarMenu(id = ns("summaryMenu"),
                              menuItem("Annual", tabName = "summary_annual", icon = icon("chart-bar")),
                              menuItem("Total", tabName = "summary_total", icon = icon("chart-bar")),
                              menuItem("About", tabName = "summary_about", icon = icon("question"), selected = TRUE),
                              # The reactive controls based on the primary option chosen
                              uiOutput(outputId = ns("summarySidebarControls"))
                              )
                ),
                
                # The dashboard
                dashboardBody(
                  tabItems(
                    
                    # Annual figures ----------------------------------------------------------
                    
                    tabItem(tabName = "summary_annual",
                            fluidRow(box(plotOutput(outputId = ns("summary_map")),
                                         width = 12, title = "Highest MHW category", status = "danger", solidHeader = TRUE, collapsible = TRUE)),
                            fluidRow(box(plotlyOutput(outputId = ns("summary_daily_count")), width = 4,
                                         title = "Daily MHW occurrence",
                                         status = "primary", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotOutput(outputId = ns("summary_cum_perc")), width = 4,
                                         title = "Total MHW coverage",
                                         status = "warning", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotOutput(ns("summary_cum_days")), width = 4,
                                         title = "Average MHW days per pixel",
                                         status = "success", solidHeader = TRUE, collapsible = TRUE))
                            ),
                    
                    
                    # Total figures -----------------------------------------------------------
                    
                    tabItem(tabName = "summary_total",
                            # fluidRow(box(imageOutput(outputId = ns("total_all")), 
                                         # width = 12, title = "Total summary", status = "primary", solidHeader = TRUE, collapsible = TRUE))
                            fluidRow(box(plotlyOutput(outputId = ns("total_daily_count")), width = 4, 
                                         title = "Daily MHW occurrence", 
                                         status = "primary", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotlyOutput(outputId = ns("total_cum_perc")), width = 4, 
                                         title = "Total MHW coverage", 
                                         status = "warning", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotlyOutput(ns("total_cum_days")), width = 4, 
                                         title = "Average MHW days per pixel",
                                         status = "success", solidHeader = TRUE, collapsible = TRUE))#,
                            # fluidRow(box(textOutput(outputId = ns("test_box"))))
                            ),
                    
                    
                    # App explanation ---------------------------------------------------------
                    
                    tabItem(tabName = "summary_about", 
                            h2("This is where the functioning of the app will be explained..."))
                  )
                )
  )
}
