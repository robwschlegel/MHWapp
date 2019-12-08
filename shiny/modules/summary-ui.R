summaryUI <- function(id, label = 'summary') {
  
  # Set seesion guide
  ns <- NS(id)
  
  # fluidPage(
  #   numericInput("n", "Simulations", 10),
  #   actionButton("simulate", "Simulate")
  # )
  
  fluidPage(
    fluidRow(style = 'height:50vh',
      column(2,
             selectInput(inputId = ns("summary_year"), label = h3("Annual summary for:"),
                         choices = seq(1982, lubridate::year(Sys.time())), selected = 2019, multiple = F)
  ),
  # fillRow(
    column(10,
           shinycssloaders::withSpinner(plotOutput(outputId = ns("summaryMap")), type = 6, color = "#b0b7be")
           )
  ),
  fluidRow(style = 'height:30vh',
    column(4,
           shinycssloaders::withSpinner(plotOutput(outputId = ns("summaryCount")), type = 6, color = "#b0b7be")
    ),
    column(4,
           shinycssloaders::withSpinner(plotOutput(outputId = ns("summaryCum")), type = 6, color = "#b0b7be")
           ),
    column(4,
           shinycssloaders::withSpinner(plotOutput(outputId = ns("summaryProp")), type = 6, color = "#b0b7be")
           )
    )
  )
}

# cat("\nsummary_ui.R finished")

