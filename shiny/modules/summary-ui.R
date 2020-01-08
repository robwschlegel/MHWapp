summaryUI <- function(id, label = 'summary') {
  ns <- NS(id)
  
  # Single page with individual figures
  # fluidPage(
  #   fluidRow(style = 'height:50vh',
  #     column(2, selectInput(inputId = ns("summary_year"), label = h3("Annual summary for:"),
  #                           choices = seq(1982, lubridate::year(Sys.time())), selected = 2019, multiple = F)),
  #   column(10, shinycssloaders::withSpinner(plotOutput(outputId = ns("summaryMap")), type = 6, color = "#b0b7be"))),
  # fluidRow(style = 'height:30vh',
  #   column(4, shinycssloaders::withSpinner(plotOutput(outputId = ns("summaryCount")), type = 6, color = "#b0b7be")),
  #   column(4, shinycssloaders::withSpinner(plotOutput(outputId = ns("summaryCum")), type = 6, color = "#b0b7be")),
  #   column(4, shinycssloaders::withSpinner(plotOutput(outputId = ns("summaryProp")), type = 6, color = "#b0b7be")))
  # )
  
  # Side panel + main panel with tabs
  fluidPage(#theme = shinythemes::shinytheme()
    # titlePanel("MHW Summaries"),
    sidebarLayout(fluid = T,
      sidebarPanel(width = 2,
        selectInput(inputId = ns("summary_year"), label = h4("Annual summary for:"),
                    choices = seq(1982, lubridate::year(Sys.time())), selected = 2019, multiple = F),
        # sliderInput(inputId = ns("lon_slider"), label = "Longitude range", 
        #             min = -180, max = 180, value = c(-180, 180), step = 10, ticks = T, dragRange = T),
        # sliderInput(inputId = ns("lat_slider"), label = "Latitude range", 
        #             min = -90, max = 90, value = c(-90, 90), step = 10, ticks = T, dragRange = T),
        # actionButton(inputId = ns("summary_button"), label = "Summarise"),
        downloadButton(outputId = ns("annual_summary_dl"))#,
        # shinythemes::themeSelector()
      ),
      mainPanel(width = 10,
                tabsetPanel(
                  tabPanel("Annual",
                           shinycssloaders::withSpinner(plotOutput(outputId = ns("summaryAll"), height = "800px"), type = 6, color = "#b0b7be")
                  ),
                  tabPanel("Trends"),
                  tabPanel("Projections")
                )
      )
    )
  )

  
  
}

# cat("\nsummary_ui.R finished")

