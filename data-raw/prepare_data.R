

shp = rgdal::readOGR(dsn = "./data-raw", layer = "FlowlinesLatLong")
usethis::use_data(shp, overwrite = TRUE)

cll = read.csv("data-raw/ChannelLatLong.csv")   # used for placing map markers for selected channels
usethis::use_data(cll, overwrite = TRUE)

channels = sort(cll$channel_nu) 
usethis::use_data(channels, overwrite = TRUE)

# custom ggplot2 theme
library(ggplot2)
theme_mod = theme(
  plot.title = element_text(size = 16),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 14),
  strip.text.x = element_text(size = 12),
  strip.text.y = element_text(size = 12),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12)
)
usethis::use_data(theme_mod, overwrite = TRUE)


summ_stats = c(
  "Min" = "min",
  "1st quartile" = "first.quart",
  "Median" = "median",
  "Mean" = "mean",
  "3rd quartile" = "third.quart",
  "Max" = "max",
  "Reversal" = "prop.neg"
)
usethis::use_data(summ_stats, overwrite = TRUE)

# don't need to rescale prop.neg; need unnamed vector for mutate_at (i.e., can't use summ_stats)
rescale_cols = c("min", "first.quart", "median", "mean", "third.quart", "max")
usethis::use_data(rescale_cols, overwrite = TRUE)


