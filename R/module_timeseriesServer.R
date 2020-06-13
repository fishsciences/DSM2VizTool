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
  
  # * summary statistics  ----------------------------------------------------------------
  
  allLists <- reactive({
    list("flow" = flowList(), "stage" = stageList(), "velocity" = velocityList())
  })
  
  sumStats <- reactive({
    lapply(allLists, function(al) 
      mapply(function(al_sub, cl) calc_summary_stats(al_sub, channel.dim = 2, cl[["Channel"]]),
             al, metadata_rv[["CL"]], SIMPLIFY = FALSE))
  })
  
  nonBase <- reactive({
    # names of scenarios that were not selected as baseline scenario
    req(input[["base_scenario"]])
    input[["sel_scenarios"]][input[["sel_scenarios"]] != input[["base_scenario"]]]
  })
  
  # run comparative analysis  ----------------------------------------------------------------
  
  observeEvent(input[["run_comp"]],{
    
    rv[["DN"]] = rv[["PO"]] = rv[["AD"]] = rv[["SS"]] = rv[["B"]] = rv[["NB"]] = NULL # clear out previous results
    
    withProgress(message = 'Running analysis...', value = 0,{
      al = allLists()
      ac = metadata_rv[["CL"]][[input[["base_scenario"]]]][["Channel"]] # ac = all channels; should be same for all scenarios
      ss = sumStats()
      ss.comb = list() # combine scenarios within each hydro metric (i.e., flow, velocity, stage)
      
      dn = list()  # dn = density
      po = list()  # po = proportion overlap
      ad = list()  # ad = absolute difference
      ad.re = list() # ad.re = absolute difference rescaled to 0-1 across channels and scenarios
      for (j in c("flow", "velocity", "stage")){
        ss.comb[[j]] = bind_rows(ss[[j]], .id = "scenario")
        po.base = al[[j]][[input[["base_scenario"]]]]
        ad.base = ss[[j]][[input[["base_scenario"]]]] %>% arrange(channel)
        dn.comp.list = list()
        po.comp.list = list()
        ad.comp.list = list()
        for (i in nonBase()){
          po.comp = al[[j]][[i]]
          ad.comp = ss[[j]][[i]] %>% arrange(channel)
          # summ_stats just specifies the summary statistic columns for the comparison (i.e., ignores channel column)
          ad.comp.list[[i]] = abs(ad.comp[,summ_stats] - ad.base[,summ_stats]) %>% 
            mutate(channel = ad.comp[["channel"]])
          dn.chan.list = list()
          po.chan = vector(mode = "numeric", length = length(ac))
          for (k in 1:length(ac)){ # using channels in base scenario, but should be same in all scenarios (if filtering was correct)
            msd.chan = filter(ss.comb[[j]], channel == ac[k])
            mn = plyr::round_any(min(msd.chan[["min"]], na.rm = TRUE), accuracy = 0.001, f = floor)
            mx = plyr::round_any(max(msd.chan[["max"]], na.rm = TRUE), accuracy = 0.001, f = ceiling)

            po.out = proportion_overlap(po.base[1,k,], po.comp[1,k,], mn, mx)
            dn.chan.list[[k]] = bind_rows(tibble(scenario = input[["base_scenario"]], channel = ac[k], x = po.out[["x"]], y = po.out[["y.base"]]),
                                          tibble(scenario = i, channel = ac[k], x = po.out[["x"]], y = po.out[["y.comp"]]))
            
            po.chan[k] = po.out[["po"]]
            
            incProgress(1/(3 * length(nonBase()) * length(ac))) # 3 is for c("flow", "velocity", "stage")
          }
          dn.comp.list[[i]] = bind_rows(dn.chan.list)
          po.comp.list[[i]] = tibble(channel = ac, prop.overlap = po.chan)
        }
        dn[[j]] = bind_rows(dn.comp.list, .id = "comp") %>% 
          mutate(base = input[["base_scenario"]]) # adding base scenario to output for completeness, but not necessary for subsequent calcs (I think)
        po[[j]] = bind_rows(po.comp.list, .id = "comp") %>% 
          mutate(base = input[["base_scenario"]])
        ad[[j]] = bind_rows(ad.comp.list, .id = "comp") %>% 
          mutate(base = input[["base_scenario"]])
        ad.re[[j]] = ad[[j]] %>% 
          mutate_at(.vars = rescale_cols, .funs = scales::rescale)
      }
    })
    rv[["DN"]] = dn
    rv[["PO"]] = po
    rv[["AD"]] = ad.re              
    rv[["SS"]] = ss.comb
    rv[["B"]] = input[["base_scenario"]] # need to stash all input values at time button was clicked
    rv[["NB"]] = nonBase()
    updateNavbarPage(x, "nav_tabs", selected = "Comparative") # change tab after clicking on process output button
  })
  
  return(rv)
}
