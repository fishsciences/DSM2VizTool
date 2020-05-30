

shp = rgdal::readOGR(dsn = "./data-raw", layer = "FlowlinesLatLong")
usethis::use_data(shp, overwrite = TRUE)

cll = read.csv("data-raw/ChannelLatLong.csv")   # used for placing map markers for selected channels
usethis::use_data(cll, overwrite = TRUE)

channels = sort(cll$channel_nu) 
usethis::use_data(channels, overwrite = TRUE)


