library(leaflet)

shinyUI(navbarPage("Poulter Lab Tools",
    tabPanel("EPA Goal",



  # Add a little CSS to make the map background pure white
  tags$head(tags$style("
    #showcase-code-position-toggle, #showcase-sxs-code { display: none; }
    .floater { background-color: white; padding: 8px; opacity: 0.7; border-radius: 6px; box-shadow: 0 0 15px rgba(0,0,0,0.2); }
  ")),

  leafletMap(
    "map", width="100%", height=800,
    # By default OpenStreetMap tiles are used; we want nothing in this case
    #initialTileLayer = NULL,
    #initialTileLayerAttribution = NULL,
    options=list(
      center = c(40, -98.85),
      zoom = 4,
      maxBounds = list(list(17, -180), list(59, 180))
    )
  ),

    absolutePanel(
        left = 50, top = 10, width = 200, class = "floater", fixed = TRUE, draggable = TRUE,

        uiOutput("descrip1"),

        uiOutput("commSlider"),
        uiOutput("industSlider"),
        uiOutput("residSlider"),
        uiOutput("transSlider"),
        uiOutput("elecSlider")
    ),

    absolutePanel(
        right = "40%", top = 10, width = 300, class = "floater", fixed = TRUE, draggable = TRUE,

        uiOutput("text1")

    ),

    absolutePanel(
        right = 30, top = 10, width = 200, class = "floater", fixed = TRUE, draggable = TRUE,

        h4("State CO2 Emissions"),
        uiOutput("stateInfo")

    ),

    absolutePanel(
        right = 30, top = 10, width = 200, class = "floater", fixed = TRUE, draggable = TRUE,

        uiOutput("pieChartOut")

    ),

  absolutePanel(
    right = 30, top = 280, width = 200,style = "", class = "floater", fixed = TRUE, draggable = TRUE,
    tags$table(
      mapply(function(from, to, color) {
        tags$tr(
          tags$td(tags$div(
            style = sprintf("width: 16px; height: 16px; background-color: %s;", color)
          )),
          tags$td(from, "-", to)
        )
      }, densityRanges$from, densityRanges$to, palette, SIMPLIFY=FALSE)
    )
  )
))
)
