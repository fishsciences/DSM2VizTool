#' comparativeUI
#'
#' @param id NA
#'
#' @export
#'

comparativeUI <- function(id){
  ns <- NS(id)
  tagList(
    # map fills the screen and absolutePanels are placed on top
    leaflet::leafletOutput(ns('Map')),
    
    # map tools  ----------------------------------------------------------------
    # top left
    absolutePanel(
      top = 80,
      left = 20,
      style = "opacity: 0.8;",
      wellPanel(
        shinyWidgets::materialSwitch(
          inputId = ns("map_tools"),
          label = "Show map tools",
          right = TRUE,
          status = "primary"
        ),
        conditionalPanel(
          condition = 'input.map_tools',
          ns = ns,
          selectInput(
            inputId = ns("map_back"),
            width = 150,
            label = "Background map",
            choices = c(
              "WorldGrayCanvas" = "Esri.WorldGrayCanvas",
              "WorldTopoMap" = "Esri.WorldTopoMap",
              "WorldImagery" = "Esri.WorldImagery"
            ),
            selected = "Esri.WorldGrayCanvas"
          ),
          selectInput(
            inputId = ns("map_pal"),
            width = 150,
            label = "Map color palette",
            choices = c(
              "BrBG",
              "PiYG",
              "PRGn",
              "PuOr",
              "RdBu",
              "RdGy",
              "RdYlBu",
              "RdYlGn",
              "Spectral"
            ),
            selected = "RdYlGn"
          ),
          sliderInput(
            inputId = ns("color_range"),
            label = "Map color range",
            min = 0,
            max = 1,
            value = c(0, 1),
            step = 0.01
          ),
          shinyWidgets::materialSwitch(
            ns("remove_channels"),
            "Remove channels",
            right = TRUE,
            status = "primary"
          )
        ),
        shinyjs::hidden(
          shinyWidgets::pickerInput(
            inputId = ns("map_channel"),
            width = 150,
            label = "Selected channel",
            choices = NULL,
            options = list(`live-search` = TRUE, size = 5)
          )
        )
      )
    ),
    
    # main panel ----------------------------------------------------------------
    # top right
    absolutePanel(
      top = 80,
      right = 35,
      width = 235,
      style = "opacity: 0.8;",
      shinyjs::hidden(
        wellPanel(
          id = ns("mainPanel"),
          shinyWidgets::radioGroupButtons(
            inputId = ns("metric"),
            label = "Response metric",
            choices = c(
              "Velocity" = "velocity",
              "Flow" = "flow",
              "Stage" = "stage"
            ),
            selected = "velocity"
          ),
          selectInput(
            inputId = ns("summ_stat"),
            label = "Summary statistic",
            choices = summ_stats,
            selected = "mean"
          ),
          shinyjs::hidden(helpText(id = ns("ss_help"),  "Please select a summary statistic.")),
          shinyWidgets::radioGroupButtons(
            inputId = ns("type"),
            label = "Map color variable",
            choices = c("Overlap" = "po", "Difference" = "ad"),
            selected = "po"
          ),
          selectInput(
            inputId = ns("comp_scenario"),
            label = "Comparison scenario",
            choices = NULL
          )
        )
      )
    ),
    
    # density plot ----------------------------------------------------------------
    absolutePanel(
      bottom = -105,
      left = 20,
      width = 500,
      style = "opacity: 0.95;",
      # trial-and-error to select value for bottom; not sure how that works
      plotOutput(ns("densityPlot"))
    ),
    
    # * scale axes
    absolutePanel(
      bottom = 260,
      left = 280,
      style = "opacity: 0.95;",
      shinyjs::hidden(
        checkboxInput(ns("scale_axes"), "Scale axes across comparisons", TRUE)
      )
    ),
    
    # info buttons ----------------------------------------------------------------
    
    absolutePanel(
      top = 85,
      right = 40,
      style = "opacity: 0.95;",
      shinyWidgets::actionBttn(
        ns("map_info"),
        "",
        icon("info"),
        style = "material-circle",
        size = "sm"
      )
    ),
    absolutePanel(
      bottom = 20,
      left = 480,
      style = "opacity: 0.95;",
      shinyjs::hidden(
        shinyWidgets::actionBttn(
          ns("plot_info"),
          "",
          icon("info"),
          style = "material-circle",
          size = "sm"
        )
      )
    )
  )
}

