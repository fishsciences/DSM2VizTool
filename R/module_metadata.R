#' metadataUI
#'
#' @param id NA
#'
#' @export
#'

metadataUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          br(),
          col_2(
            shinyWidgets::dropdownButton(
              actionButton(
                inputId = ns("download_1hr"),
                label = "1-hour/70 MB",
                icon = icon("download"),
                onclick = "window.open('https://s3-us-west-2.amazonaws.com/datavore/delta-hydrodynamics/ExampleH5Files_1hour.zip', '_blank')"
              ),
              actionButton(
                inputId = ns("download_15min"),
                label = "15-min/500 MB",
                icon = icon("download"),
                onclick = "window.open('https://s3-us-west-2.amazonaws.com/datavore/delta-hydrodynamics/ExampleH5Files_15min.zip', '_blank')"
              ),
              helpText("(time interval/file size)"),
              circle = TRUE, status = "primary",
              icon = icon("download"), size = "sm", 
              tooltip = shinyWidgets::tooltipOptions(title = "Example HDF5 Files")
            )
          ),
          col_10(
            shinyFiles::shinyFilesButton(
              id = ns('h5_files'),
              label = "Select HDF5 Files", # button label
              title = "Select HDF5 Files", # widget title
              icon = icon("folder-open"),
              multiple = TRUE
            )
          )
        )
      ),
      mainPanel(
        br(),
        p(id = ns("metadata_msg"), "Select HDF5 files of DSM2 HYDRO output to view metadata. Example HDF5 files are available to download by clicking on the blue download button."),
        DT::DTOutput(ns("metadataTable"))
      )
    )
  )
}

#' metadataServer
#'
#' @param input module
#' @param output module
#' @param session module
#'
#' @export
#'
#'


metadataServer <-  function(input, output, session){
  
  # Reactive values ----------------------------------------------------------------
  rv <- reactiveValues(H5 = NULL) 
  
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
    h5_df <- h5_read_attr(rv[["H5"]])
    h5_df$interval_vals = as.numeric(gsub('\\D','', h5_df$interval))
    h5_df$interval_units = gsub('\\d','', h5_df$interval)
    h5_df$start_date = lubridate::ymd_hms(h5_df$start_date)
    h5_df$end_date = calc_end_date(h5_df$start_date, h5_df$num_intervals, 
                                   h5_df$interval_vals, h5_df$interval_units)
    h5_df
  })
  
  h5MetadataDisplay <- reactive({
    h5_df <- h5Metadata()
    h5_df$start_date = as.character(h5_df$start_date)
    h5_df$end_date = as.character(h5_df$end_date)
    h5_df <- h5_df[, c("scenario", "start_date", "end_date", "interval", "num_channels")]
    names(h5_df) <- c("Scenario", "Start Date", "End Date", "Interval", "Channels")
    h5_df
  })
  #     end_date = as.character(end_date)
  #     out[i] = ifelse(nchar(end_date) > 10, end_date, paste(end_date, "00:00:00")) # time of 00:00:00 gets dropped; adding it for consistency
  output$metadataTable = DT::renderDT({ 
    h5MetadataDisplay() },
    style = "bootstrap", rownames = FALSE, caption = "Table 1. Basic metadata for selected HDF5 files.",
    options = list(searching = FALSE, bPaginate = FALSE, info = FALSE, scrollX = TRUE)
  )
}
