summaryUI <- function(id, label = 'summary') {
  ns <- NS(id)
  
  # Side panel + main panel with tabs
  fluidPage(
    sidebarLayout(fluid = T,
      sidebarPanel(width = 2,
        selectInput(inputId = ns("summary_year"), label = h4("Annual summary for:"),
                    choices = seq(1982, lubridate::year(Sys.time())-1), selected = 2019, multiple = F),
        downloadButton(outputId = ns("annual_summary_dl"))
      ),
      mainPanel(width = 10,
                shinycssloaders::withSpinner(imageOutput(outputId = ns("summary_all"), height = 300), type = 6, color = "#b0b7be")
                )
      )
    )
}

# cat("\nsummary_ui.R finished")

