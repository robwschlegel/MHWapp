summaryUI <- function(id, label = 'summary') {
  ns <- NS(id)
  
  # NB: shinydashboard doesn't work with navBarPage layout from top UI
  
  # Side panel + main panel with tabs
  fluidPage(theme = "style.css",
    sidebarLayout(fluid = T,
                  uiOutput(outputId = ns("summary_UI")),
                  mainPanel(width = 10,
                            tabsetPanel(id = ns("summary_tabs"), #type = "pills",
                                        tabPanel(title = "Annual", value = "annual_tab",
                                                 shinycssloaders::withSpinner(imageOutput(outputId = ns("summary_all")), type = 6, color = "#b0b7be")
                                                 ),
                                        tabPanel(title = "Historic", value = "historic_tab",
                                                 shinycssloaders::withSpinner(imageOutput(outputId = ns("historic_all")), type = 6, color = "#b0b7be"))
                                        )
                            )
                  )
    )
}

# cat("\nsummary_ui.R finished")

