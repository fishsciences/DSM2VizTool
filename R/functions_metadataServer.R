#' h5_read_attr
#'
#' @export
#'

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

#' h5_read
#'
#' @export
#'

h5_read <- function(data, node, channels, dates, name){
  # wrapper around rhdf5::h5read
  # loop through selected files to read selected data in load data step
  # loads only one of two nodes (upstream or downstream); all common channels; and selected date range (input$date_range_load)
  out = list()
  # scenarios are stored in list names; dates reflects only scenarios with valid value
  sc = names(dates) 
  # decided not to switch this for loop to mapply 
  # because didn't want to risk (or spend time confirming)
  # that input vectors/lists would be in same order after filtering
  for (i in sc){
    dp = data[["datapath"]][data[["scenario"]] == i]
    out[[i]] = rhdf5::h5read(dp, paste0("/hydro", name),
                             # nodes, channels, dates for order of index; NULL indicates load all
                             index = list(node[[i]][["Index"]], 
                                          channels[[i]][["Index"]], 
                                          dates[[i]][["Index"]])) 
  }
  return(out)
}

#' calc_end_date
#'
#' @export
#'

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


#' get_interval_number
#'
#' @export
#'

get_interval_number <- function(start, end, interval_vals, interval_units){
  # find the number of intervals in a date range
  # need plural interval_units for difftime
  interval_units <- paste0(interval_units, "s")
  do.call("c", 
          mapply(function(start, end, val, units)
            1L + as.integer(difftime(end, start, units = units))/val,
            start, end, interval_vals, interval_units,
            SIMPLIFY = FALSE))
}

#' get_date_indexes
#'
#' @export
#'

get_date_indexes <- function(start, end, interval_vals, interval_units) {
  # need plural interval_units for difftime
  interval_units <- paste0(interval_units, "s")
  diff_dates <- as.integer(difftime(end, start, units = interval_units))
  index_end = 1L + diff_dates/interval_vals
  1:index_end
}

#' process_nodes
#'
#' @export
#'

process_nodes <- function(x){
  x = gsub(pattern = " ", replacement = "", x) # node locations (upstream and downstream) included whitespace 
  x = sapply(x, simple_cap, USE.NAMES = FALSE) # first letter capitalized to match input in ui.R
  return(x)
}

#' simple_cap
#'
#' @export
#'

simple_cap <- function(x) {
  # used in process_nodes()
  # ?toupper
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

