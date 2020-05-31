#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  # https://stackoverflow.com/questions/46075184/scoping-issue-with-updatenavbarpage-function-from-within-shiny-module
  metadata_results <- callModule(DSM2VizTool::metadataServer, "metadataUI", x = session)
  timeseries_results <- callModule(DSM2VizTool::timeseriesServer, "timeseriesUI", x = session, metadata_results)

}
