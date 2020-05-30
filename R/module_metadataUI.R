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
        p(align = "center", 
          shinyjs::hidden(
            actionButton(
              inputId = "reset_app",
              label = "Reset Application",
              icon = icon("redo"),
              onclick = "history.go(0)"
            )
          )
        ),
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
              id = ns("h5_files"),
              label = "Select HDF5 Files", # button label
              title = "Select HDF5 Files", # widget title
              icon = icon("folder-open"),
              multiple = TRUE
            )
          )
        ),
        br(),    
        shinyjs::hidden(dateRangeInput(ns("date_range_read"), label = 'Date range')),
        shinyjs::hidden(shinyWidgets::radioGroupButtons(inputId = ns("node_loc"), label = "Nodes",
                                                        choices = c("Upstream", "Downstream"),
                                                        selected = "Upstream", justified = TRUE)),
        shinyjs::hidden(helpText(id = ns("date_range_read_warn_small"),
                                 "Please select date range to include at least one scenario with >1 interval (see Table 2).")),
        br(),
        p(align = "center", shinyjs::hidden(actionButton(inputId = ns("read_data"), label = "Read Data", icon = icon("spinner")))),
        shinyjs::hidden(helpText(id = ns("date_range_read_warn_large"),
                                 "Warning: Date range includes large number of intervals (see Table 2); reading data will be slow and may run out of memory. 
                          It is recommended to select less than one year of data because differences between scenarios are likely to be swamped by seasonal and annual variation."))
      ),
      mainPanel(
        br(),
        p(id = ns("metadata_msg"), "Select HDF5 files of DSM2 HYDRO output to view metadata. Example HDF5 files are available to download by clicking on the blue download button."),
        DT::DTOutput(ns("metadataTable")),
        br(),
        DT::DTOutput(ns("intervalTable")),
        br(),
        DT::DTOutput(ns("channelTable")),
        br()
      )
    )
  )
}

