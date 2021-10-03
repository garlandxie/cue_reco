# libraries ----
library(readr)
library(here)
library(assertr)
library()

# import ----
mapbox <- read_csv(
  here("data/raw", "0302231.wdwe-2h.csv")
)

# check packaging ----
head(mapbox, n = 10)
str(mapbox)

# check names ----
names(mapbox)

# validate ----

# QA/QC b/c there's a lot of data (~ 1.3 million rows)
mapbox |>
  verify(xlat >= 43) |>
  verify(xlon <= 79) |>
  verify(agg_day_period >= 0) |>
  verify(agg_time_period >= 0) |>
  verify(activity_index_total >= 0) |>
  verify(month %in% c("2020-06", "2020-07", "2020-08")) |>
  assert(within_bounds(0, 1), agg_day_period) |>
  assert(within_bounds(0, 11), agg_time_period)




  