ui <- page_fillable(
  # title = "Marine Heatwave Tracker",
  h1("Marine Heatwave Tracker"),
  # header = list(cicerone::use_cicerone()), # Start guided tour
  value_box(title = "Percent cover", value = "...", showcase = bs_icon("percent")),
  # selected = "map_tab",
  # sidebar = NULL,
  # nav_panel(title = "Map", value = "map_tab", mapUI("map")),
  # nav_panel(title = 'About', value = 'about_tab', aboutUI('about')),
  # nav_spacer(),
  # nav_item(tags$a("GitHub", href = "https://github.com/robwschlegel/MHWapp"))
  card(full_screen = TRUE,
       layout_sidebar(
         class = "p-0",
         sidebar = sidebar(
           title = "Earthquakes off Fiji",
           bg = "#1E1E1E",
           width = "35%",
           class = "fw-bold font-monospace",
           accordion(
             accordion_panel(
               "Dropdowns", icon = bsicons::bs_icon("menu-app")
               # !!!filters
             ),
             accordion_panel(
               "Numerical", icon = bsicons::bs_icon("sliders")
               # filter_slider("depth", "Depth", dat, ~depth),
               # filter_slider("table", "Table", dat, ~table)
             )
           )
         ),
         leaflet() |> addTiles()
       )
       ),
  card()
)
