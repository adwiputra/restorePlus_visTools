library(leaflet)

# AD off====
# Choices for drop-downs
vars <- c(
  "Production" = "Production",
  "Productivity" = "Productivity",
  "Area" = "Area"
)
# AD off\ends----

varsType <- c(
  "Cocoa" = "Ccoa",
  "Coffee" = "Coff",
  "Corn" = "Corn",
  "Crop Land" = "CrpLnd",
  "Primary Forest" = "PriFor",
  "Rice" = "Rice",
  "Small-holder Oil Plam" = "SOpal",
  "Company Oil Palm" = "LOpal"
)

varsScen <- c(
  "Baseline" = "Baseline",
  "Conservation" = "CONS",
  "Conservation-Intensification" = "CONS_INT",
  "Intensification" = "INTENS",
  "Business as Usual" = "NoCC",
  "Restoration" = "RESTOR"
)


navbarPage("Superzip", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("IndoBiom VisTools"),
        selectInput("type", "Type", varsType),
        selectInput("scen", "Scenario", varsScen),
        selectInput("color", "Color", vars),
        sliderInput("year", "Year", 2000, 2050, value = 2000, step = 10),
        # selectInput("size", "Size", vars, selected = "adultpop"),
        # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
        #   # Only prompt for threshold when coloring or sizing by superzip
        #   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        # ),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
)
