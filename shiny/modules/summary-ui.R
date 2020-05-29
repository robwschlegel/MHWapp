summaryUI <- function(id, label = 'summary') {
  ns <- NS(id)
  
  dashboardPage(skin = "black",
                
                # The app title
                dashboardHeader(disable = TRUE),
                
                # The primary options
                dashboardSidebar(
                  sidebarMenu(id = ns("summaryMenu"),
                              menuItem("Total", tabName = "summary_total", icon = icon("chart-bar")),
                              # menuItem("Annual", tabName = "summary_annual", icon = icon("chart-bar")),
                              menuItem("About", tabName = "summary_about", icon = icon("question"), selected = TRUE),
                              # The reactive controls based on the primary option chosen
                              uiOutput(outputId = ns("summarySidebarControls"))
                              )
                ),
                
                # The dashboard
                dashboardBody(
                  tabItems(
                    
                    # Annual figures ----------------------------------------------------------
                    
                    # tabItem(tabName = "summary_annual",
                    #         fluidRow(box(imageOutput(outputId = ns("summary_all")), 
                    #                      width = 12, title = "Annual summary", status = "primary", solidHeader = TRUE, collapsible = TRUE))
                    #         ),
                    
                    
                    # Total figures -----------------------------------------------------------
                    
                    tabItem(tabName = "summary_total",
                            # fluidRow(box(imageOutput(outputId = ns("total_all")), 
                                         # width = 12, title = "Total summary", status = "primary", solidHeader = TRUE, collapsible = TRUE))
                            fluidRow(box(plotOutput(outputId = ns("total_daily_count")), width = 4, 
                                         title = "Average daily MHW occurrence per year (% of ocean)", 
                                         status = "primary", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotOutput(outputId = ns("total_cum_perc")), width = 4, 
                                         title = "Total coverage per year (% of ocean)", 
                                         status = "warning", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotlyOutput(ns("total_Cum")), width = 12, 
                                         title = "Total MHW days per pixel per year",
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
