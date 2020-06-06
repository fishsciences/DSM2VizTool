#' metadataServer
#'
#' @param input     module
#' @param output    module
#' @param session   module
#' @param x         used to separately pass session to updateNavBarPage
#'
#' @import dplyr
#' @export
#'
#'

metadataServer <-  function(input, output, session, x){
  
  # reactive values ----------------------------------------------------------------
  # dlrs = date list read sub; drr = date range read; cl = channel list; acc = all common channels
  rv <- reactiveValues(H5 = NULL, H5META = NULL, STAGE = NULL, FLOW = NULL, AREA = NULL, 
                       VELOCITY = NULL, DLRS = NULL, DRR = NULL, CL = NULL, ACC = NULL)
  
  # info alert ----------------------------------------------------------------
  
  observeEvent(input[["metadata_info"]], {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "",             
      text = 
        tags$span(
          tags$p(align="left",
                 "Selecting HDF5 files reads the metadata (but not data) for each selected file and populates the Metadata tab with 
                   tables related to the start and end dates, time intervals, and channels included in the selected files."
          ),
          tags$p(align="left",
                 "Because the HDF5 files might contain large amounts of data, the user can select a date range 
                   (based on the file metadata) to read a subset of the file to reduce the time that DSM2 Viz spends 
                   reading data. The app also requires that the user selects the upstream or downstream node for each 
                   channel. We have created an ", tags$a(href="https://fishsciences.shinyapps.io/dsm2-map/", "interactive map"), " showing DSM2 
                   channels and nodes, including the upstream and downstream node."
          ),
          tags$p(align="left",
                 "Click the 'Read Data' button to read the specified subset of the stage, flow, and channel 
                   area output; velocity is calculated by dividing flow by channel area."
          )
        ),
      type = "info",
      btn_labels = "OK"
    )
  })
  
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
  
  # dates ----------------------------------------------------------------
  
  datesListRead <- reactive({
    h5_df = h5Metadata()
    out <- mapply(function(start, end, val, unit)
      tibble(Date = seq(from = start, to = end, by = paste(val, unit)),
             Index = get_date_indexes(start, end, val, unit)),
      h5_df[["start_date"]], h5_df[["end_date"]], h5_df[["interval_vals"]], h5_df[["interval_units"]],
      SIMPLIFY = FALSE)
    names(out) <- h5_df[["scenario"]]
    out
  })
  
  datesListReadSub <- reactive({
    drr = lubridate::ymd_hms(paste(input[["date_range_read"]], c("00:00:00", "23:59:59")))
    out <- lapply(datesListRead(),
                  function(df){
                    tmp <- filter(df, Date >= drr[1] & Date <= drr[2])
                    tmp[["SubIndex"]] <- if (nrow(tmp) > 0) 1:nrow(tmp) else NULL 
                    tmp
                  })
    out
  })
  
  # intervals ----------------------------------------------------------------
  
  intervals <- reactive({
    h5_df <- h5Metadata()
    drr <- lubridate::ymd_hms(paste(input[["date_range_read"]], c("00:00:00", "23:59:59")))
    tmp <- h5_df %>% 
      mutate(first_date = if_else(start_date > drr[1], start_date, drr[1]),
             last_date = if_else(end_date < drr[2], end_date, drr[2]),
             int_num = get_interval_number(first_date, last_date, interval_vals, interval_units),
             num_intervals_in_range = ifelse(int_num < 0, 0, int_num))
    h5_df[["num_intervals_in_range"]] <- tmp[["num_intervals_in_range"]]
    h5_df
  })
  
  intervalSub <- reactive({
    filter(intervals(), num_intervals_in_range > 1)
  })
  
  output$intervalTable = DT::renderDT({
    select(intervals(), Scenario = scenario, `Total Intervals` = num_intervals, `Intervals in Date Range` = num_intervals_in_range)},
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
    # threshold based on 1 year of 15-min intervals; b/c warning message advises selecting less than one year
    cond = max(intervals()[["num_intervals_in_range"]]) > 35040 
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
      tibble(NodeLoc = process_nodes(rhdf5::h5read(file, "/hydro/geometry/channel_location")),
             Index = 1:length(NodeLoc))) 
    names(out) <- rv[["H5"]][["scenario"]]
    out
  })
  
  # read data ----------------------------------------------------------------
  
  observeEvent(input[["read_data"]],{
    # clear out values from previous reads
    rv[["H5META"]] <- rv[["STAGE"]] <- rv[["FLOW"]] <- rv[["AREA"]] <-  
      rv[["VELOCITY"]] <- rv[["DLRS"]] <- rv[["DRR"]] <- rv[["CL"]] <- rv[["ACC"]] <- NULL
    
    rv[["H5META"]] <- h5Metadata()
    rv[["DLRS"]] <- datesListReadSub() 
    rv[["DRR"]] <- input[["date_range_read"]]
    rv[["ACC"]] <- allCommonChannels()
    rv[["CL"]] <- lapply(channelList(), filter, Channel %in% rv[["ACC"]])
    # don't need to store in rv because only two options; once one is selected don't need to track any more
    nl = lapply(nodeList(), filter, NodeLoc == input[["node_loc"]]) 
    
    withProgress(message = 'Reading...', value = 0, detail = "Stage",{
      rv[["STAGE"]] <- h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel stage")
      
      setProgress(2.5/10, detail = "Flow")
      rv[["FLOW"]] <- h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel flow")
      
      setProgress(5/10, detail = "Area")
      rv[["AREA"]] <- h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel area")
      
      setProgress(7.5/10, message = "Calculating...", detail = "Velocity")
      rv[["VELOCITY"]] <- mapply(function(flow, area) flow/area,
                                 rv[["FLOW"]], rv[["AREA"]], SIMPLIFY = FALSE)
      
      setProgress(9.5/10, message = "Finishing", detail = "")
    })
    rhdf5::h5closeAll()
    # https://stackoverflow.com/questions/46075184/scoping-issue-with-updatenavbarpage-function-from-within-shiny-module
    updateNavbarPage(x, "nav_tabs", selected = "Time Series")
  })
  
  return(rv)
}
