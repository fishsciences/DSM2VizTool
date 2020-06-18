#' comparativeServer
#'
#' @param input          module
#' @param output         module
#' @param session        module
#' @param metadata_rv    reactive values returned from metadata tab
#' @param timeseries_rv  reactive values returned from timeseries tab
#'
#' @import dplyr
#' @import ggplot2
#' @import leaflet
#' @export
#'
#'

comparativeServer <-  function(input, output, session, metadata_rv, timeseries_rv){
  
  # reactive values ----------------------------------------------------------------
  # DC = deleted channels 
  rv <- reactiveValues(DC = NULL) 
  
  # dynamic UI ----------------------------------------------------------------
  
  observe({
    cond = !is.null(metadata_rv[["FLOW"]])
    shinyjs::toggle("map_channel", condition = cond)
  })
  
  observe({
    req(metadata_rv[["ACC"]])
    acc = metadata_rv[["ACC"]][!(metadata_rv[["ACC"]] %in% rv[["DC"]])]
    shinyWidgets::updatePickerInput(session, "map_channel", choices = acc)
  })
  
  observe({
    updateSelectInput(session, "comp_scenario", choices = timeseries_rv[["NB"]])
  })
  
  observe({
    cond = !is.null(timeseries_rv[["PO"]])
    shinyjs::toggle("map_pal", condition = cond)
    shinyjs::toggle("color_range", condition = cond)
    shinyjs::toggle("remove_channels", condition = cond)
    shinyjs::toggle("mainPanel", condition = cond)  
    shinyjs::toggle("plot_info", condition = cond)
  })
  
  observe({ 
    req(timeseries_rv[["NB"]])                        
    cond = length(timeseries_rv[["NB"]]) > 1 & input[["summ_stat"]] != ""
    shinyjs::toggle("scale_axes", condition = cond)
    shinyjs::toggle("comp_scenario", condition = cond)
  })
  
  observe({ 
    req(timeseries_rv[["NB"]]) 
    cond = input[["summ_stat"]] == ""
    shinyjs::toggle("ss_help", condition = cond)
  })
  
  observeEvent(input[["metric"]],{
    ch = summ_stats
    if(input[["metric"]] == "stage") ch = summ_stats[summ_stats != "prop.neg"]
    updateSelectInput(session, "summ_stat", choices = ch, selected = input[["summ_stat"]])
  })
  
  # filter ----------------------------------------------------------------
  
  poSub <- reactive({
    req(timeseries_rv[["PO"]], input[["comp_scenario"]])
    po = timeseries_rv[["PO"]][[input[["metric"]]]]
    return(po[po[["comp"]] == input[["comp_scenario"]] & !(po[["channel"]] %in% rv[["DC"]]),])
  })
  
  poSubChannel <- reactive({
    po = poSub()
    return(po[po[["channel"]] == input[["map_channel"]], ])
  })
  
  adSub <- reactive({
    req(timeseries_rv[["AD"]], input[["comp_scenario"]], input[["summ_stat"]] != "")
    ad = timeseries_rv[["AD"]][[input[["metric"]]]]
    ad.sub = ad[ad[["comp"]] == input[["comp_scenario"]] & !(ad[["channel"]] %in% rv[["DC"]]),]
    ad.sub[["value"]] = ad.sub[[input[["summ_stat"]]]]
    return(ad.sub)
  })
  
  adSubChannel <- reactive({
    ad = adSub()
    return(ad[ad[["channel"]] == input[["map_channel"]], ])
  })
  
  ssSub <- reactive({
    req(input[["comp_scenario"]], input[["summ_stat"]] != "")
    ss = timeseries_rv[["SS"]][[input[["metric"]]]]
    ss[["value"]] = ss[[input[["summ_stat"]]]] # rename summary stat column to value for plotting later
    ss = ss[ss[["channel"]] == input[["map_channel"]] & ss[["scenario"]] %in% c(timeseries_rv[["B"]], input[["comp_scenario"]]),]
    return(ss)
  })
  
  densityChannel <- reactive({
    dn = timeseries_rv[["DN"]][[input[["metric"]]]]
    dn = dn[dn[["channel"]] == input[["map_channel"]],]
  })
  
  densitySub <- reactive({ 
    req(input[["comp_scenario"]])
    dn = densityChannel()
    dn = dn[dn[["comp"]] == input[["comp_scenario"]],]
    dn[["scenario"]] = factor(dn[["scenario"]], levels = c(timeseries_rv[["B"]], input[["comp_scenario"]]))
    return(dn)
  })
  
  densityRange <- reactive({
    # x-axes were scaled the same (for each channel) when calculating density
    if (input[["scale_axes"]] == TRUE){
      df = densityChannel()
    }else{
      df = densitySub()
    }
    y.thresh = max(df[["y"]])/100  # just guessed at threshold value; might need to be adjusted; looked okay in my spot checks
    df = df[df[["y"]] > y.thresh,]
    # out = xy_range(df)
    out = c("x.min" = min(df[["x"]], na.rm = TRUE),
            "x.max" = max(df[["x"]], na.rm = TRUE),
            "y.min" = min(df[["y"]], na.rm = TRUE),
            "y.max" = max(df[["y"]], na.rm = TRUE))
    return(out)
  })
  
  # density plot ----------------------------------------------------------------
  
  output$densityPlot = renderPlot({
    po = poSubChannel()
    po.lab = tibble(X = Inf, Y = Inf, H = 1.5, V = 2, L = formatC(round(po[["prop.overlap"]], 3), format='f', digits=3)) # create a data.frame with formatted proportion overlap value for plotting in upper right corner of density plot
    
    ad = adSubChannel()
    ad.lab = tibble(X = -Inf, Y = Inf, H = -0.5, V = 2, L = formatC(ad[["value"]], format='f', digits = ifelse(ad[["value"]] < 100, 3, 0), big.mark = ",")) # create a data.frame with formatted absolute difference value for plotting in upper left corner of density plot
    
    df = densitySub()
    dr = densityRange()
    
    # lookup vectors for linking descriptive names to simple names for plotting purposes
    x_labs = c("Velocity (ft/s)" = "velocity", "Flow (cfs)" = "flow", "Stage (ft)" = "stage")
    
    p <- ggplot(df) +
      geom_density(aes(x = x, y = y, fill = factor(scenario), col = factor(scenario)), stat = "identity", alpha = 0.6) +
      labs(x = names(x_labs)[x_labs == input[["metric"]]], y = "Density") +
      geom_text(data = ad.lab, aes(x = X, y = Y, hjust = H, vjust = V, label = L), size = 6, col = ifelse(input[["type"]] == "ad", 2, 1)) +
      geom_text(data = po.lab, aes(x = X, y = Y, hjust = H, vjust = V, label = L), size = 6, col = ifelse(input[["type"]] == "ad", 1, 2)) +
      scale_fill_manual(name = "Scenario", values = c("#6a3d9a", "#ff7f00")) +
      scale_colour_manual(name = "Scenario", values = c("#6a3d9a", "#ff7f00"), guide = FALSE) +
      coord_cartesian(xlim = c(dr[["x.min"]], dr[["x.max"]]), ylim = c(0, dr[["y.max"]])) +
      ggtitle(paste("Channel", input[["map_channel"]])) +
      theme_minimal() +
      theme_mod
    if (input[["summ_stat"]] != "prop.neg"){  # doesn't make sense to plot prop.neg/reversal on these distributions
      p = p + geom_vline(data = ssSub(), aes(xintercept = value, col = factor(scenario)), linetype = "dashed", size = 1)
    }
    return(p)
  }, width = 500, height = 280)
  
  # map data ----------------------------------------------------------------
  # join map data with appropriate value for coloring channels on map
  
  shpSub <- reactive({
    subset(shp, !(channel_nu %in% rv[["DC"]]))
  })
  
  poData <- reactive({
    req(input[["type"]] == "po")
    shp = shpSub()
    cr = input[["color_range"]]
    shp@data = shp@data %>% 
      left_join(select(poSub(), channel_nu = channel, overlap = prop.overlap), by = "channel_nu") %>% 
      mutate(overlap = ifelse(overlap < cr[1], cr[1],
                              ifelse(overlap > cr[2], cr[2], overlap)))
    pal = colorNumeric(
      palette = input[["map_pal"]], 
      domain = cr)
    return(list(shp, pal))
  })
  
  adData <- reactive({
    req(input[["type"]] == "ad")
    shp = shpSub()
    cr = input[["color_range"]]
    shp@data = shp@data %>%
      left_join(select(adSub(), channel_nu = channel, value), by = "channel_nu") %>% 
      mutate(value = ifelse(value < cr[1], cr[1],
                            ifelse(value > cr[2], cr[2], value)))
    pal = colorNumeric(
      palette = input[["map_pal"]],
      domain = cr,
      reverse = TRUE  # for difference, high values indicate greater effect, which is opposite of proportion overlap; reverse the palette to reduce cognitive burden of palette flipping when moving from proportion overlap to absolute difference
    )
    return(list(shp, pal))
  })
  
  # channel map  ----------------------------------------------------------------
  
  output$Map = renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
      addProviderTiles("WorldGrayCanvas") %>% 
      setView(lng = -121.75, lat = 38.05, zoom = 10) 
  })
  
  proxyMap = leafletProxy("Map", session)
  
  observe({
   # input[["nav_tabs"]]
    proxyMap %>% 
      addProviderTiles(input[["map_back"]])
  })
  
  observe({
   # input[["nav_tabs"]]               # dependency on nav_tabs displays shapefile when tab is clicked (assuming next condition is met)
    if (is.null(timeseries_rv[["PO"]])){         # display default map until comparative results are generated
      proxyMap %>% 
        clearShapes() %>% 
        addPolylines(data = shp,
                     weight = 6,
                     layerId = ~channel_nu)
    }
  })
  
  # * draw shapefiles with color scales ----------------------------------------------------------------
  
  # draw proportion overlap map
  observeEvent(poData(),{
    pd = poData()[[1]]
    pal.po <- poData()[[2]]
    delta.map = proxyMap %>%
      clearShapes() %>% 
      addPolylines(data = pd,
                   color = ~pal.po(overlap),
                   weight = 6,
                   layerId = ~channel_nu) %>%
      clearControls() %>% 
      addLegend("bottomright", pal = pal.po, values = pd@data[["overlap"]], title = "Overlap", opacity = 0.8)
    return(delta.map)
  })
  
  # draw absolute difference map
  observeEvent(adData(),{
    ad = adData()[[1]]
    pal.ad = adData()[[2]]
    delta.map = proxyMap %>%
      clearShapes() %>% 
      addPolylines(data = ad,
                   color = ~pal.ad(value),
                   weight = 6,
                   layerId = ~channel_nu) %>%
      clearControls() %>% 
      addLegend("bottomright", pal = pal.ad, values = ad@data[["value"]], title = "Difference", opacity = 0.8)
    return(delta.map)
  })
  
  # * map interactivity  ----------------------------------------------------------------
  # zoom and place map markers for selected channels
  # based on the example here: https://uasnap.shinyapps.io/ex_leaflet/
  
  # highlight selected channel; black chosen to highlight because other choices (e.g., yellow) might be used in map (i.e., hard to see yellow channel highlighted in yellow)
  ap_sel <- function(map, channel){
    addPolylines(map,
                 data = subset(shp, channel_nu == channel),
                 color = "black",
                 weight = 12,
                 opacity = 0.4,
                 layerId = "Selected")
  }
  
  observeEvent(input[["Map_shape_click"]], {
    # update the list of deleted channels (rv[["DC"]]) on map click (when remove_channels is on)
    p <- input[["Map_shape_click"]]
    req(p[["id"]])
    if (input[["remove_channels"]] == TRUE){
      p <- input[["Map_shape_click"]]
      if (is.null(rv[["DC"]])){
        rv[["DC"]] = p[["id"]]
      }else{
        rv[["DC"]] = c(rv[["DC"]], p[["id"]])
      }
    }else{
      # update the location selectInput on map clicks
      if (p[["id"]] != "Selected"){  # 'Selected' layer placed on top of reach layer; don't want to update when 'Selected' layer clicked
        updateSelectInput(session, "map_channel", selected = p[["id"]])
        proxyMap %>% setView(lng = p[["lng"]], lat = p[["lat"]], input[["Map_zoom"]]) %>% ap_sel(p[["id"]])
      } 
    }
  })
  
  # update the map markers and view on location selectInput changes
  observeEvent(input[["map_channel"]], { 
    req(input$Map_zoom) # map zoom is initially zero until set in renderLeaflet
    if (input[["remove_channels"]] == FALSE){
      cll_sub <- filter(cll, channel_nu == input[["map_channel"]])
      if(nrow(cll_sub) == 0){  
        proxyMap %>% removeShape(layerId = "SelectedChannel")
      }else{
        proxyMap %>% setView(lng = cll_sub[["lon"]], lat = cll_sub[["lat"]], input[["Map_zoom"]]) %>% ap_sel(input[["map_channel"]])
      }
    }
  })
  
  # info alerts  ----------------------------------------------------------------
  observeEvent(input[["explore_info"]], {
    if (input[["explore_tabs"]] == "Metadata"){
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
    }else{
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
    }
  })
  
  observeEvent(input[["map_info"]], {
    if (is.null(timeseries_rv[["PO"]])){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "",             
        text = 
          tags$p(align="left",
                 "This is the default channel map. When DSM2 HYDRO output files are selected and read, and the 
                 comparative analysis is run, this map will show the color-scaled results of the comparative analysis."
          ),
        type = "info",
        btn_labels = "OK"
      )
    }else{
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "",             
        text = 
          tags$span(
            tags$p(align="left",
                   "Clicking on a channel pans to the selected channel and updates the 'Selected channel' input in 
                   the top left panel."
            ),
            tags$p(align="left",
                   "Overlap is the proportion overlap in the density distributions for the selected comparison. 
                   Proportion overlap ranges from 0 to 1."
            ),
            tags$p(align="left",
                   "Difference is the absolute difference in the selected summary statistic for the selected comparison. 
                   The absolute difference values are rescaled across channels and scenarios to values ranging from 0 to 1; 
                   except for 'Reversal' which naturally ranges from 0 to 1."
            ),
            tags$p(align="left",
                   "By default, map color is scaled from 0 to 1. To adjust the map color range, click on 
                   'Show map tools' in the top left panel."
            ),
            tags$p(align="left",
                   "When the 'Remove channels' switch is on (under 'Show map tools'), clicking on a channel removes 
                   that channel from the map."
            )
          ),
        type = "info",
        btn_labels = "OK"
      )
    }
  })
  
  observeEvent(input[["plot_info"]], {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "",          
      text = 
        tags$span(
          tags$p(align="left",
                 "The density plot shows the distribution of velocity/flow/stage values over the selected date range 
                 for the selected comparison." 
          ),
          tags$p(align="left",
                 "The rescaled absolute difference in the selected summary statistic and proportion overlap of 
                 the distributions are shown in the top left and top right corners of the plot, respectively."
          ),
          tags$p(align="left",
                 "The dashed vertical lines show the value of the selected summary statistic. Vertical lines are 
                 not displayed when Reversal is selected as the summary statistic." 
          ),
          tags$p(align="left",
                 "Reversal is the proportion of values that are negative. Reversal only applies to flow and velocity, 
                 not stage."
          ),
          tags$p(align="left",
                 "The minimum and maximum summary statistics do not always align with the minimum and maximum extent 
                 of the distributions because the density estimates are based on a smoothing kernel."
          )
        ),
      type = "info",
      btn_labels = "OK"
    )
  })

}
