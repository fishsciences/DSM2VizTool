#' response_list
#'
#' @export
#'

response_list <- function(response, dates){
  # additional date filtering after loading data (input$date_range_viz)
  # selecting all channels; only one dimension for node at this point in the process
  mapply(function(response, dates)
    response[,,dates[["SubIndex"]], drop = FALSE],
    response, dates,
    SIMPLIFY = FALSE)
}

#' response_tibble
#'
#' @export
#'

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

#' ts_plot
#'
#' @export
#'

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

#' calc_summary_stats
#'
#' @export
#'

calc_summary_stats <- function(array, channel.dim, channels){
  # calculate summary stats for use in comparative analysis
  tibble(channel = channels,
         min = apply(array, channel.dim, min, na.rm = TRUE),
         first.quart = apply(array, channel.dim, quantile, probs = 0.25, na.rm = TRUE),
         median = apply(array, channel.dim, median, na.rm = TRUE),
         mean = apply(array, channel.dim, mean, na.rm = TRUE),
         third.quart = apply(array, channel.dim, quantile, probs = 0.75, na.rm = TRUE),
         max = apply(array, channel.dim, max, na.rm = TRUE),
         prop.neg = apply(array, channel.dim, function(x) sum(x < 0, na.rm = TRUE)/length(x)))
}

#' proportion_overlap
#'
#' @export
#'

proportion_overlap <- function(base, comp, mn, mx){
  # calculate proportion overlap for use in comparative analysis
  # base and comp are equal length vectors
  # mn and mx are min and max values for that channel across all comparisons
  if (length(base) != length(comp)) stop("base and comp are not same length")
  bd = density(base, from = mn, to = mx) # bd = baseline density
  cd = density(comp, from = mn, to = mx) # cd = comparison density
  dis = MESS::auc(bd[["x"]], abs(cd[["y"]] - bd[["y"]]))/(MESS::auc(bd[["x"]], bd[["y"]]) + MESS::auc(cd[["x"]], cd[["y"]])) # dis = 0 is completely overlapping; dis = 1 is no overlap
  return(list(po = 1 - dis, 
              x = bd[["x"]], # base and comp have same x
              y.base = bd[["y"]],
              y.comp = cd[["y"]]))
}