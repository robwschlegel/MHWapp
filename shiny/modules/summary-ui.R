summaryUI <- function(id, label = 'summary') {
  ns <- NS(id)
  
  # Side panel + main panel with tabs
  dashboardPage(
    dashboardHeader(title = "", disable = TRUE),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Annual", tabName = "annual", icon = icon("dashboard"))#,
        # selectInput(inputId = ns("summary_year"), label = h4("Annual summary for:"),
        #             choices = seq(1982, lubridate::year(Sys.time())-1), selected = 2019, multiple = F),
        # downloadButton(outputId = ns("annual_summary_dl"))
      )
      
      # sidebarPanel(width = 2,

      ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "annual",
                selectInput(inputId = ns("summary_year"), label = h4("Annual summary for:"),
                            choices = seq(1982, lubridate::year(Sys.time())-1), selected = 2019, multiple = F),
                downloadButton(outputId = ns("annual_summary_dl"))#,
                # shinycssloaders::withSpinner(imageOutput(outputId = ns("summary_all"), height = "auto"), type = 6, color = "#b0b7be")
        )
      )
      # mainPanel(width = 10,
                # shinycssloaders::withSpinner(imageOutput(outputId = ns("summary_all"), height = "auto"), type = 6, color = "#b0b7be")
                # )
      )#,
    # renderText("Figure 5: (a) Global map showing the highest category experienced at each pixel over the course of the year estimated using the NOAA OI v2 data set. Light grey shows that no MHWs occurred in a pixel over the entire year. (b) Stacked barplot showing the percent of ocean pixels experiencing a MHW on any given day of the year, (c) Stacked barplot showing the cumulative percent of the ocean that has experienced a MHW over the year. These values are based on when in the year a pixel first experiences it's highest category MHW, so no pixel is counted more than once. Horizontal bars in this figure show the final percentage values for each category of MHW. (d) Stacked barplot showing the cumulative number of MHW days averaged over all pixels in the ocean. This is taken by finding the cumulative MHW days per pixel for the entire ocean and dividing that by the overall number of ocean pixels (~690,000). Source: Robert Schlegel, Woods Hole.")
    )
}

# cat("\nsummary_ui.R finished")

