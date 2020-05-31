#' timeseriesUI
#'
#' @param id NA
#'
#' @export
#'

timeseriesUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      # * sidebar panel -----------------------------------------------------
      sidebarPanel(
        shinyjs::hidden(dateRangeInput(ns('date_range_viz'), 
                                       label = 'Date range')),
        shinyjs::hidden(helpText(id = ns("date_range_viz_warn"), 
                                 "Please select larger date range.")),
        shinyjs::hidden(shinyWidgets::pickerInput(inputId = ns("sel_scenarios"), 
                                                  label = "Selected scenarios", 
                                                  choices = NULL, multiple = TRUE)),
        shinyjs::hidden(helpText(id = ns("files_warn"), 
                                 "Please select more than one file for comparative analysis.")),
        shinyjs::hidden(helpText(id = ns("interval_warn"), 
                                 "Comparative analysis requires that all selected scenarios 
                                 have the same time interval (see Table 1 on Metadata tab).")),
        shinyjs::hidden(shinyWidgets::pickerInput(inputId = ns("base_scenario"), 
                                                  label = "Baseline scenario", choices = NULL)),
        br(),
        p(align = "center", shinyjs::hidden(actionButton(inputId = ns("run_comp"), 
                                                         label = "Run Comparative Analysis", 
                                                         icon = icon("spinner"))))
      ),
      # * main panel -----------------------------------------------------
      mainPanel(
        br(),
        p(id = ns("ts_msg"), "Select and read HDF5 files on the Metadata tab to 
          view time series plots of velocity, flow, and stage."),
        fluidRow(
          column(1),
          column(5,
                 shinyjs::hidden(
                   shinyWidgets::checkboxGroupButtons(
                     inputId = ns("ts_plots"), 
                     label = "Plots shown", 
                     choices = c("Velocity", "Flow", "Stage"),
                     selected = c("Velocity"),
                     checkIcon = list(yes = icon("ok", lib = "glyphicon"))))
          ),
          column(3,
                 shinyjs::hidden(
                   shinyWidgets::pickerInput(
                     inputId = ns("ts_channel"), label = "Selected channel", width = '150px',
                     choices = NULL, options = list(`live-search` = TRUE, size = 10)))
          ),
          column(3)
        ),
        # should have noted where I found this code; javascript to check if input$ts_plots is both not null and selected
        conditionalPanel(condition = "input.ts_plots.indexOf('Velocity') > -1", 
                         ns = ns,
                         br(),
                         plotOutput(ns("tsVelocityPlot"))),
        conditionalPanel(condition = "input.ts_plots.indexOf('Flow') > -1",
                         ns = ns,
                         br(),
                         plotOutput(ns("tsFlowPlot"))),
        conditionalPanel(condition = "input.ts_plots.indexOf('Stage') > -1",
                         ns = ns,
                         br(),
                         plotOutput(ns("tsStagePlot")))
      )
    ),
    # * info button -----------------------------------------------------
    absolutePanel(
      top = 65, right = 40, style = "opacity: 0.95;",
      shinyjs::hidden(
        shinyWidgets::actionBttn(
          ns("ts_info"), "", icon("info"),
          style = "material-circle", size = "sm")))
  )
}

