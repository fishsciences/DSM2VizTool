#' timeseriesServer
#'
#' @param input        module
#' @param output       module
#' @param session      module
#' @param x            used to separately pass session to updateNavBarPage
#' @param metadata_rv  reactive values returned from metadata tab
#'
#' @import dplyr
#' @import ggplot2
#' @export
#'
#'

timeseriesServer <-  function(input, output, session, x, metadata_rv){
  
  # reactive values ----------------------------------------------------------------
  # AD = absolute difference; PO = proportion overlap; DN = density; SS = summary stats; 
  # B = base scenario; NB = non-base scenarios; DC = deleted channels 
  rv <- reactiveValues(AD = NULL, PO = NULL, DN = NULL, SS = NULL, B = NULL, NB = NULL, DC = NULL) 
  
  # info alert  ----------------------------------------------------------------
  observeEvent(input[["ts_info"]], {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "",             
      text = 
        tags$span(
          tags$p(align="left",
                 "The date range on the Time Series tab is initially set from the date range on the Metadata tab. 
                   Changing the date range allows for zooming in/out on the time series plots. The comparative 
                   analysis is based on the date range selected on the Time Series tab."
          ),
          tags$p(align="left",
                 "Selecting a file for use as a baseline scenario has no effect on the time series plots; 
                   it is only used in the comparative analysis."
          )
        ),
      type = "info",
      btn_labels = "OK"
    )
  })
  
  # helper functions ----------------------------------------------------------------
  
  response_list <- function(response, dates){
    # additional date filtering after loading data (input$date_range_viz)
    # selecting all channels; only one dimension for node at this point in the process
    mapply(function(response, dates)
      response[,,dates[["SubIndex"]], drop = FALSE],
      response, dates,
      SIMPLIFY = FALSE)
  }
  
  response_tibble <- function(response, dates, channels, focal_channel){
    # convert response list to tibble for selected focal channel
    # used in plotting time series
    # response, dates, and channels, are all lists with one tibble per scenario (i.e., user uploaded file)
    out <- mapply(
      function(response, dates, channels){
        ci <- channels[["Index"]][channels[["Channel"]] == focal_channel]
        tibble(Date = dates[["Date"]],
               Value = response[, ci, , drop = TRUE])},
      response, dates, channels,
      SIMPLIFY = FALSE)
    
    bind_rows(out, .id = "Scenario")
  }
  
  ts_plot <- function(data, y.lab, title, obs.check){
    # plot time series
    if (obs.check == FALSE) return(NULL)
    ggplot(data, aes(x = Date, y = Value, col = Scenario)) +
      geom_line(size = 1, alpha = 0.6) +
      labs(y = y.lab, title = title) +
      scale_colour_brewer(type = "qual", palette = "Set1") +
      theme_minimal() +
      theme_mod
  }
  
  # dates  ----------------------------------------------------------------
  
  datesList <- reactive({
    req(metadata_rv[["DLRS"]])
    drv = lubridate::ymd_hms(paste(input[["date_range_viz"]], c("00:00:00", "23:59:59")))
    lapply(metadata_rv[["DLRS"]], filter, Date >= drv[1] & Date <= drv[2])
  })
  
  # velocity  ----------------------------------------------------------------
  
  velocityList <- reactive({
    req(metadata_rv[["VELOCITY"]])
    response_list(metadata_rv[["VELOCITY"]], datesList())
  })
  
  # using velocity to test date ranges and number of observations; should be same for flow and stage
  velocityTibble <- reactive({
    req(metadata_rv[["VELOCITY"]])
    response_tibble(velocityList(), datesList(), metadata_rv[["CL"]], input[["ts_channel"]]) %>% 
      filter(Scenario %in% input[["sel_scenarios"]])
  })
  
  # check that at least two observations for at least one scenario 
  obsCheck <- reactive({
    req(metadata_rv[["VELOCITY"]])
    if (nrow(velocityTibble()) > 1){
      scenario_rows = velocityTibble() %>% 
        group_by(Scenario) %>% 
        summarise(N = n())
      out = max(scenario_rows[["N"]]) > 1
    }else{
      out = FALSE  # if 0 or 1 rows in velocityTibble(), then not enough observations and obsCheck() is false
    }
    return(out) 
  })
  
  output$tsVelocityPlot = renderPlot({
    req(metadata_rv[["VELOCITY"]])
    ts_plot(velocityTibble(), "Velocity (ft/s)", "Velocity", obsCheck())
  })
  
  # flow  ----------------------------------------------------------------
  
  flowList <- reactive({
    req(metadata_rv[["FLOW"]])
    response_list(metadata_rv[["FLOW"]], datesList())
  })
  
  output$tsFlowPlot = renderPlot({
    req(metadata_rv[["FLOW"]])
    response_tibble(flowList(), datesList(), metadata_rv[["CL"]], input[["ts_channel"]]) %>% 
      filter(Scenario %in% input[["sel_scenarios"]]) %>% 
      ts_plot("Flow (cfs)", "Flow", obsCheck())
  })
  
  # stage  ----------------------------------------------------------------
  
  stageList <- reactive({
    req(metadata_rv[["STAGE"]])
    response_list(metadata_rv[["STAGE"]], datesList())
  })
  
  output$tsStagePlot = renderPlot({
    req(metadata_rv[["STAGE"]])
    response_tibble(stageList(), datesList(), metadata_rv[["CL"]], input[["ts_channel"]]) %>% 
      filter(Scenario %in% input[["sel_scenarios"]]) %>% 
    ts_plot("Stage (ft)", "Stage", obsCheck())
  })
  
  # dynamic UI ----------------------------------------------------------------
  
  observe({
    cond = !is.null(metadata_rv[["FLOW"]])
    shinyjs::toggle("ts_msg", condition = !cond)
    shinyjs::toggle("ts_info", condition = cond)
    shinyjs::toggle("ts_plots", condition = cond)
    shinyjs::toggle("ts_channel", condition = cond)
    shinyjs::toggle("map_channel", condition = cond)
    shinyjs::toggle("date_range_viz", condition = cond)
    shinyjs::toggle("base_scenario", condition = cond)
    shinyjs::toggle("sel_scenarios", condition = cond)
  })
  
  intervalCheck <- reactive({
    req(input[["sel_scenarios"]])
    # check if all selected scenarios use the same time interval
    length(unique(filter(metadata_rv[["H5META"]], scenario %in% input[["sel_scenarios"]])[["interval_units"]])) == 1
  })
  
  observe({
    req(input[["sel_scenarios"]])
    shinyjs::toggle("date_range_viz_warn", condition = !obsCheck())
    shinyjs::toggle("interval_warn", condition = !intervalCheck())
    shinyjs::toggle("run_comp", condition = length(input[["sel_scenarios"]]) > 1 & obsCheck() & intervalCheck()) 
    shinyjs::toggle("files_warn", condition = length(input[["sel_scenarios"]]) < 2)
  })
  
  observe({
    req(metadata_rv[["ACC"]])
    acc = metadata_rv[["ACC"]][!(metadata_rv[["ACC"]] %in% metadata_rv[["DC"]])]
    shinyWidgets::updatePickerInput(session, "ts_channel", choices = acc, selected = input[["map_channel"]])
  })
  
  observe({
    req(metadata_rv[["ACC"]])
    acc = metadata_rv[["ACC"]][!(metadata_rv[["ACC"]] %in% metadata_rv[["DC"]])]
    shinyWidgets::updatePickerInput(session, "map_channel", choices = acc, selected = input[["ts_channel"]])
  })
  
  observe({
    drr = metadata_rv[["DRR"]]
    updateDateRangeInput(session, "date_range_viz",
                         start = drr[1], end = drr[2],
                         min = drr[1], max = drr[2])
  })
  
  observe({
    sc = sort(names(metadata_rv[["FLOW"]]))
    shinyWidgets::updatePickerInput(session, "sel_scenarios", choices = sc, selected = sc)
  })
  
  observe({
    shinyWidgets::updatePickerInput(session, "base_scenario", choices = input[["sel_scenarios"]])
  })
  
  return(rv)
}
