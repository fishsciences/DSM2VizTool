#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Reactive values ----------------------------------------------------------------
  # dlwy = date list water year; drr = date range read; cl = channel list; acc = all common channels
  # rv <- reactiveValues(H5 = NULL, H5META = NULL, STAGE = NULL, FLOW = NULL, AREA = NULL, VELOCITY = NULL, DLWY = NULL, DRR = NULL, CL = NULL, ACC = NULL, 
  #                      # AD = absolute difference; PO = proportion overlap; DN = density; SS = summary stats; B = base scenario; NB = non-base scenarios; DC = deleted channels 
  #                      AD = NULL, PO = NULL, DN = NULL, SS = NULL, B = NULL, NB = NULL, DC = NULL) 
  
  # List the first level callModules here
  # https://stackoverflow.com/questions/46075184/scoping-issue-with-updatenavbarpage-function-from-within-shiny-module
  callModule(DSM2VizTool::metadataServer, "metadataUI", x = session)

}
