#' metadataServer
#'
#' @param input module
#' @param output module
#' @param session module
#'
#' @import dplyr
#' @export
#'
#'

metadataServer <-  function(input, output, session){
  # reactive values ----------------------------------------------------------------
  # dlwy = date list water year; drr = date range read; cl = channel list; acc = all common channels
  rv <- reactiveValues(H5 = NULL, H5META = NULL, STAGE = NULL, FLOW = NULL, AREA = NULL, VELOCITY = NULL, DLWY = NULL, DRR = NULL, CL = NULL, ACC = NULL)
  
  # helper functions ----------------------------------------------------------------
  
  h5_read_attr <- function(data){
    # wrapper around rhdf5::h5readAttributes
    hydro_attr <- lapply(data[["datapath"]], rhdf5::h5readAttributes, "/hydro")
    time_attr <- lapply(data[["datapath"]], rhdf5::h5readAttributes, "/hydro/data/channel flow")
    rhdf5::h5closeAll()
    data.frame(scenario = data[["scenario"]],
               start_date = sapply(time_attr, "[[", "start_time"),
               interval = sapply(time_attr, "[[", "interval"),
               num_channels = sapply(hydro_attr, "[[", "Number of channels"),
               num_intervals = sapply(hydro_attr, "[[", "Number of intervals"))
  }
  
  h5_read <- function(data, node, channels, dates, name){
    # wrapper around rhdf5::h5read
    # loop through selected files to read selected data in load data step
    # loads only one of two nodes (upstream or downstream); all common channels; and selected date range (input$date_range_load)
    out = list()
    sc = names(dates) # scenarios are stored in list names; dates reflects only scenarios with valid value
    for (i in sc){
      dp = data[["datapath"]][data[["scenario"]] == i]
      out[[i]] = rhdf5::h5read(dp, paste0("/hydro", name),
                               index = list(node[[i]][["Index"]], channels[[i]][["Index"]], dates[[i]][["Index"]]))  # nodes, channels, dates for order of index; NULL indicates load all
    }
    return(out)
  }
  
  calc_end_date <- function(start_date, num_intervals, interval_vals, interval_units){
    # only accepts min, hour, and day as by argument
    # DSM2 output files don't include end date in metadata (as far as I know)
    total_time <- (num_intervals - 1) * interval_vals
    # translate units to a lubridate function
    funcs <- list("min" = lubridate::minutes, "hour" = lubridate::hours, "day" = lubridate::days)
    # using do.call to unlist without losing date formatting
    end_date <- do.call("c", mapply(function(f, start, tt) start + f(tt), 
                                    funcs[interval_units], start_date, total_time, 
                                    USE.NAMES = FALSE, SIMPLIFY = FALSE))
  }
  
  get_interval_number <- function(start, end, interval_vals, interval_units){
    # find the number of intervals in a date range
    # need plural interval_units for difftime
    interval_units <- paste0(interval_units, "s")
    1L + as.integer(difftime(end, start, units = interval_units))/interval_vals
  }
  
  get_date_indexes <- function(start_date, date_range_start, date_range_end, 
                               interval_vals, interval_units) {
    # get index of dates relative to start_date time interval
    # used to index into full HDF5 files when only using a subset of dates
    # need plural interval_units for difftime
    interval_units <- paste0(interval_units, "s")
    diff_drs <- as.integer(difftime(date_range_start, start_date, units = interval_units))
    diff_dre <- as.integer(difftime(date_range_end, start_date, units = interval_units))
    index_start = 1L + diff_drs/interval_vals
    index_end = 1L + diff_dre/interval_vals
    return(seq(index_start, index_end, 1))
  }
  
  process_nodes <- function(x){
    x = gsub(pattern = " ", replacement = "", x) # node locations (upstream and downstream) included whitespace 
    x = sapply(x, simple_cap, USE.NAMES = FALSE) # first letter capitalized to match input in ui.R
    return(x)
  }
  
  simple_cap <- function(x) {
    # used in process_nodes()
    # ?toupper
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }
  
  # select files ----------------------------------------------------------------
  
  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, "h5_files", roots = volumes, filetypes = c("h5"), session = session)
  
  filePaths <- reactive({
    shinyFiles::parseFilePaths(volumes, input[["h5_files"]])
  })
  
  observe({
    req(isTRUE(nrow(filePaths()) > 0))
    fp <- filePaths()
    fp$scenario <- gsub(pattern = ".h5", replacement = "", x = fp$name)
    rv[["H5"]] <- fp
  })
  
  # metadata ----------------------------------------------------------------
  
  h5Metadata <- reactive({
    req(rv[["H5"]])
    # extract metadata; h5_read_attr() is wrapper to h5readAttributes
    h5_read_attr(rv[["H5"]]) %>% 
      mutate(interval_vals = as.numeric(gsub('\\D','', interval)),
             interval_units = gsub('\\d','', interval),
             start_date = lubridate::ymd_hms(start_date),
             end_date = calc_end_date(start_date, num_intervals, 
                                      interval_vals, interval_units))
  })
  
  output$metadataTable = DT::renderDT({ 
    h5Metadata() %>% 
      mutate(start_date = as.character(start_date),
             end_date = as.character(end_date)) %>%
      select(Scenario = scenario, `Start Date` = start_date, `End Date` = end_date,
             Interval = interval, Channels = num_channels)
  },
  style = "bootstrap", rownames = FALSE, caption = "Table 1. Basic metadata for selected HDF5 files.",
  options = list(searching = FALSE, bPaginate = FALSE, info = FALSE, scrollX = TRUE)
  )
  
  get_date_indexes <- function(start_date, date_range_start, date_range_end, 
                               interval_vals, interval_units) {
    # get index of dates relative to start_date time interval
    # used to index into full HDF5 files when only using a subset of dates
    # need plural interval_units for difftime
    interval_units <- paste0(interval_units, "s")
    diff_drs <- as.integer(difftime(date_range_start, start_date, units = interval_units))
    diff_dre <- as.integer(difftime(date_range_end, start_date, units = interval_units))
    index_start = 1L + diff_drs/interval_vals
    index_end = 1L + diff_dre/interval_vals
    return(seq(index_start, index_end, 1))
  }
  
  process_nodes <- function(x){
    x = gsub(pattern = " ", replacement = "", x) # node locations (upstream and downstream) included whitespace 
    x = sapply(x, simple_cap, USE.NAMES = FALSE) # first letter capitalized to match input in ui.R
    return(x)
  }
  
  simple_cap <- function(x) {
    # used in process_nodes()
    # ?toupper
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }
  
  # dates ----------------------------------------------------------------
  
  datesListRead <- reactive({
    d = h5Metadata()
    out = list()
    for (i in 1:nrow(d)){
      out[[d[["scenario"]][i]]] = tibble(Date = seq(from = d[["start_date"]][i], to = d[["end_date"]][i], 
                                                    by = paste(d[["interval_vals"]][i], d[["interval_units"]][i])),
                                         Index = get_date_indexes(d[["start_date"]][i], d[["start_date"]][i], d[["end_date"]][i],
                                                                  d[["interval_vals"]][i], d[["interval_units"]][i]))
    }
    return(out)
  })
  
  datesListReadSub <- reactive({
    drr = lubridate::ymd_hms(paste(input[["date_range_read"]], c("00:00:00", "23:59:59")))
    dlr = datesListRead()
    out = list()
    for (i in names(dlr)){
      out[[i]] = filter(dlr[[i]], Date >= drr[1] & Date <= drr[2])
      # After reading data, the original index will be lost; need new index for subsetting data in visualization step 
      out[[i]][["SubIndex"]] = if (nrow(out[[i]]) > 0) 1:length(out[[i]][["Date"]]) else NULL 
    }
    return(out)
  })
  
  # intervals ----------------------------------------------------------------
  
  intervals <- reactive({
    d = h5Metadata()
    drr = lubridate::ymd_hms(paste(input[["date_range_read"]], c("00:00:00", "23:59:59")))
    for (i in 1:nrow(d)){
      first_date = if_else(d[["start_date"]][i] > drr[1], d[["start_date"]][i], drr[1])
      last_date = if_else(d[["end_date"]][i] < drr[2], d[["end_date"]][i], drr[2])
      int_num = get_interval_number(first_date, last_date, d[["interval_vals"]][i], d[["interval_units"]][i])
      d[["num_intervals_in_range"]][i] = ifelse(int_num < 0, 0, int_num)
    }
    return(d)
  })
  
  intervalSub <- reactive({
    intervals() %>% filter(num_intervals_in_range > 1)
  })
  
  output$intervalTable = DT::renderDT({
    intervals() %>% 
      select(Scenario = scenario, `Total Intervals` = num_intervals, `Intervals in Date Range` = num_intervals_in_range)},
    style = "bootstrap", rownames = FALSE, caption = "Table 2. Number of time intervals in selected HDF5 files.",
    options = list(searching = FALSE, bPaginate = FALSE, info = FALSE, scrollX = TRUE)
  )
  
  # dynamic UI ----------------------------------------------------------------
  
  observe({
    cond = !is.null(rv[["H5"]])
    shinyjs::toggle("metadata_msg", condition = !cond)
    shinyjs::toggleState("h5_files", condition = !cond)
    shinyjs::toggle("reset_app", condition = cond)
    shinyjs::toggle("date_range_read", condition = cond)
  })
  
  observe({
    req(rv[["H5"]])
    cond = max(intervals()[["num_intervals_in_range"]]) > 1
    shinyjs::toggle("node_loc", condition = cond)
    shinyjs::toggle("read_data", condition = cond)
    shinyjs::toggle("date_range_read_warn_small", condition = !cond)
  })
  
  observe({
    req(rv[["H5"]])
    cond = max(intervals()[["num_intervals_in_range"]]) > 35040 # threshold based on 1 year of 15-min intervals; b/c warning message advises selecting less than one year
    shinyjs::toggle("date_range_read_warn_large", condition = cond)
  })
  
  observe({
    d = bind_rows(datesListRead(), .id = "Scenario")
    min_date = lubridate::floor_date(min(d[["Date"]], na.rm = TRUE), unit = "day")
    max_date = lubridate::floor_date(max(d[["Date"]], na.rm = TRUE), unit = "day")
    updateDateRangeInput(session, "date_range_read",
                         start = min_date, end = max_date, 
                         min = min_date, max = max_date)
  })
  
  # channels ----------------------------------------------------------------
  
  channelList <- reactive({
    req(rv[["H5"]])
    out <- lapply(rv[["H5"]][["datapath"]], function(file)
      tibble(Channel = rhdf5::h5read(file, "/hydro/geometry/channel_number"),
             Index = 1:length(Channel))) 
    names(out) <- rv[["H5"]][["scenario"]]
    out
  })
  
  channelTibble <- reactive({
    bind_rows(channelList(), .id = "Scenario") %>% 
      # exclude scenarios that will be dropped when reading data (b/c not enough data)
      filter(Scenario %in% intervalSub()[["scenario"]]) 
  })
  
  channelTibbleWide <- reactive({
    channelTibble() %>% 
      select(-Index) %>% 
      bind_rows(tibble(Scenario = "DefaultMap", Channel = channels)) %>% 
      mutate(Value = 1L) %>% 
      tidyr::spread(key = Scenario, value = Value, fill = 0L) %>% 
      mutate(Total = rowSums(.) - Channel) # scenario columns are 0 and 1
  })
  
  output$channelTable <- DT::renderDT({
    sc <- intervalSub()[["scenario"]]
    channelTibbleWide() %>% 
      filter(Total < length(sc) + 1) %>% 
      # re-arranging columns so that Default is always the 2nd column; Channel will always be first because of spread
      select(c("Channel", "Default Map" = "DefaultMap", sc)) 
  }, 
  style = "bootstrap", rownames = FALSE,
  caption = "Table 3. Differences in channels included in default map file and selected HDF5 files with at least 2 time intervals in the date range (see Table 2). 
    1 and 0 indicate that channels are present or not present in a file, respectively. A table with no data indicates that all files have the same channels.",
  options = list(searching = FALSE, bPaginate = FALSE, info = FALSE, scrollX = TRUE)
  )
  
  allCommonChannels <- reactive({
    if (is.null(input[["h5_files"]])) {
      x = channels
    }else{
      x = channelTibbleWide() %>% 
        # filter to keep channels that are found in all scenarios and default map
        filter(Total == length(intervalSub()[["scenario"]]) + 1) %>% 
        pull(Channel)
    }
    return(x) 
  })
  
  # nodes ----------------------------------------------------------------
  
  nodeList <- reactive({
    req(rv[["H5"]])
    out <- lapply(rv[["H5"]][["datapath"]], function(file)
      tibble(NodeLoc = rhdf5::h5read(file, "/hydro/geometry/channel_location"),
             Index = 1:length(NodeLoc))) 
    names(out) <- rv[["H5"]][["scenario"]]
    out
  })
  
  # read data ----------------------------------------------------------------
  
  observeEvent(input[["read_data"]],{
    rv[["H5META"]] = rv[["STAGE"]] = rv[["FLOW"]] = rv[["AREA"]] = rv[["VELOCITY"]] = rv[["DLRS"]] = rv[["DRR"]] = rv[["CL"]] = rv[["ACC"]] = NULL # clear out previous results
    
    rv[["H5META"]] = h5Metadata()
    rv[["DLRS"]] = datesListReadSub() 
    rv[["DRR"]] = input[["date_range_read"]]
    rv[["ACC"]] = allCommonChannels()
    rv[["CL"]] = lapply(channelList(), filter, Channel %in% rv[["ACC"]])
    nl = lapply(nodeList(), filter, NodeLoc == input[["node_loc"]]) # don't need to store in rv because only two options; once one is selected don't need to track any more
    
    withProgress(message = 'Reading...', value = 0, detail = "Stage",{
      rv[["STAGE"]] = h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel stage")
      setProgress(2.5/10, detail = "Flow")
      rv[["FLOW"]] = h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel flow")
      setProgress(5/10, detail = "Area")
      rv[["AREA"]] = h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel area")
      setProgress(7.5/10, message = "Calculating...", detail = "Velocity")
      rv[["VELOCITY"]] = list()
      for (i in names(rv[["FLOW"]])){
        rv[["VELOCITY"]][[i]] = rv[["FLOW"]][[i]]/rv[["AREA"]][[i]]
      }
      setProgress(9.5/10, message = "Finishing", detail = "")
    })
    rhdf5::h5closeAll()
    updateTabsetPanel(session, "explore_tabs", selected = "Time Series") # change tab after clicking on process output button
  })#/Read Button
}
