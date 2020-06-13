# work through logic in shiny app; makes easier to troubleshoot problems, particularly when changing functionality

library(tidyverse)

# Metadata ----------------------------------------------------------------

rv <- list(H5 = NULL, H5META = NULL, STAGE = NULL, FLOW = NULL, AREA = NULL, 
           VELOCITY = NULL, DLRS = NULL, DRR = NULL, CL = NULL, ACC = NULL)

rv[["H5"]] <- tibble(datapath = c("inst/example/sjr12000_omr5000.h5", "inst/example/sjr1500_omr5000.h5"),
                     scenario = gsub(pattern = ".h5", replacement = "", x = datapath))

h5Metadata <- h5_read_attr(rv[["H5"]]) %>% 
  mutate(interval_vals = as.numeric(gsub('\\D','', interval)),
         interval_units = gsub('\\d','', interval),
         start_date = lubridate::ymd_hms(start_date),
         end_date = calc_end_date(start_date, num_intervals, 
                                  interval_vals, interval_units))

datesListRead <- mapply(function(start, end, val, unit)
  tibble(Date = seq(from = start, to = end, by = paste(val, unit)),
         Index = get_date_indexes(start, end, val, unit)),
  h5Metadata[["start_date"]], h5Metadata[["end_date"]], h5Metadata[["interval_vals"]], h5Metadata[["interval_units"]],
  SIMPLIFY = FALSE)
names(datesListRead) <- h5Metadata[["scenario"]]

drr <- lubridate::ymd_hms(c("2007-06-01 00:00:00", "2007-06-10 23:59:59"))

datesListReadSub <- lapply(datesListRead,
                           function(df){
                             tmp <- filter(df, Date >= drr[1] & Date <= drr[2])
                             tmp[["SubIndex"]] <- if (nrow(tmp) > 0) 1:nrow(tmp) else NULL 
                             tmp})

tmp <- h5Metadata %>% 
  mutate(first_date = if_else(start_date > drr[1], start_date, drr[1]),
         last_date = if_else(end_date < drr[2], end_date, drr[2]),
         int_num = get_interval_number(first_date, last_date, interval_vals, interval_units),
         num_intervals_in_range = ifelse(int_num < 0, 0, int_num))
h5Metadata[["num_intervals_in_range"]] <- tmp[["num_intervals_in_range"]]
intervals <- h5Metadata

intervalSub <- filter(intervals, num_intervals_in_range > 1)

channelList <- lapply(rv[["H5"]][["datapath"]], function(file)
  tibble(Channel = rhdf5::h5read(file, "/hydro/geometry/channel_number"),
         Index = 1:length(Channel))) 
names(channelList) <- rv[["H5"]][["scenario"]]

channelTibble <- bind_rows(channelList, .id = "Scenario") %>% 
  # exclude scenarios that will be dropped when reading data (b/c not enough data)
  filter(Scenario %in% intervalSub[["scenario"]]) 

channelTibbleWide <- channelTibble %>% 
  select(-Index) %>% 
  bind_rows(tibble(Scenario = "DefaultMap", Channel = channels)) %>% 
  mutate(Value = 1L) %>% 
  tidyr::spread(key = Scenario, value = Value, fill = 0L) %>% 
  mutate(Total = rowSums(.) - Channel) # scenario columns are 0 and 1

allCommonChannels <-channelTibbleWide %>% 
  # filter to keep channels that are found in all scenarios and default map
  filter(Total == length(intervalSub[["scenario"]]) + 1) %>% 
  pull(Channel)

nodeList <- lapply(rv[["H5"]][["datapath"]], function(file)
  tibble(NodeLoc = process_nodes(rhdf5::h5read(file, "/hydro/geometry/channel_location")),
         Index = 1:length(NodeLoc))) 
names(nodeList) <- rv[["H5"]][["scenario"]]


rv[["H5META"]] <- h5Metadata
rv[["DLRS"]] <- datesListReadSub
rv[["DRR"]] <- drr
rv[["ACC"]] <- allCommonChannels
rv[["CL"]] <- lapply(channelList, filter, Channel %in% rv[["ACC"]])
# don't need to store in rv because only two options; once one is selected don't need to track any more
nl = lapply(nodeList, filter, NodeLoc == "Upstream") 

rv[["STAGE"]] <- h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel stage")
rv[["FLOW"]] <- h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel flow")
rv[["AREA"]] <- h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel area")
rv[["VELOCITY"]] <- mapply(function(flow, area) flow/area,
                           rv[["FLOW"]], rv[["AREA"]], SIMPLIFY = FALSE)
rhdf5::h5closeAll()

metadata_rv <- rv

# Time series ----------------------------------------------------------------

rv <- list(AD = NULL, PO = NULL, DN = NULL, SS = NULL, B = NULL, NB = NULL, DC = NULL) 

drv <- lubridate::ymd_hms(c("2007-06-01 00:00:00", "2007-06-10 23:59:59"))

datesList <- lapply(metadata_rv[["DLRS"]], filter, Date >= drv[1] & Date <= drv[2])
velocityList <- response_list(metadata_rv[["VELOCITY"]], datesList)
velocityTibble <- response_tibble(velocityList, datesList, metadata_rv[["CL"]], 84)
flowList <- response_list(metadata_rv[["FLOW"]], datesList)
stageList <- response_list(metadata_rv[["STAGE"]], datesList)
allLists <- list("flow" = flowList, "stage" = stageList, "velocity" = velocityList)

sumStats <- lapply(allLists, function(al) 
  mapply(function(al_sub, cl) calc_summary_stats(al_sub, channel.dim = 2, cl[["Channel"]]),
         al, metadata_rv[["CL"]], SIMPLIFY = FALSE))

base <- "inst/example/sjr1500_omr5000"
nonBase <- "inst/example/sjr12000_omr5000"

al = allLists
ac = metadata_rv[["CL"]][[base]][["Channel"]] # ac = all channels; should be same for all scenarios
ss = sumStats
ss.comb = list() # combine scenarios within each hydro metric (i.e., flow, velocity, stage)

dn = list()  # dn = density
po = list()  # po = proportion overlap
ad = list()  # ad = absolute difference
ad.re = list() # ad.re = absolute difference rescaled to 0-1 across channels and scenarios
for (j in c("flow", "velocity", "stage")){
  ss.comb[[j]] = bind_rows(ss[[j]], .id = "scenario")
  po.base = al[[j]][[base]]
  ad.base = ss[[j]][[base]] %>% arrange(channel)
  dn.comp.list = list()
  po.comp.list = list()
  ad.comp.list = list()
  for (i in nonBase){
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
      dn.chan.list[[k]] = bind_rows(tibble(scenario = base, channel = ac[k], x = po.out[["x"]], y = po.out[["y.base"]]),
                                    tibble(scenario = i, channel = ac[k], x = po.out[["x"]], y = po.out[["y.comp"]]))
      
      po.chan[k] = po.out[["po"]]
    }
    dn.comp.list[[i]] = bind_rows(dn.chan.list)
    po.comp.list[[i]] = tibble(channel = ac, prop.overlap = po.chan)
  }
  dn[[j]] = bind_rows(dn.comp.list, .id = "comp") 
  po[[j]] = bind_rows(po.comp.list, .id = "comp") 
  ad[[j]] = bind_rows(ad.comp.list, .id = "comp")
  ad.re[[j]] = ad[[j]] %>% 
    mutate_at(.vars = rescale_cols, .funs = scales::rescale)
}

rv[["DN"]] = dn
rv[["PO"]] = po
rv[["AD"]] = ad.re              
rv[["SS"]] = ss.comb
rv[["B"]] = base
rv[["NB"]] = nonBase

