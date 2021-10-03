# libraries ----
library(readr)
library(here)
library(assertr)
library(data.table)  # for manipulating data faster than dplyr
library(magrittr)

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
mapbox %>%
  verify(xlat >= 43) %>%
  verify(xlon <= 79) %>%
  verify(agg_day_period >= 0) %>%
  verify(agg_time_period >= 0) %>% 
  verify(activity_index_total >= 0) %>% 
  verify(month %in% c("2020-06", "2020-07", "2020-08")) %>% 
  assert(within_bounds(0, 1), agg_day_period) %>% 
  assert(within_bounds(0, 11), agg_time_period)

# data cleaning: number of visitors per weekday --------------------------------

set.seed(1L)

num_visitors_weekday <-
  mapbox %>%
  as.data.table() %>%
  
  # Mapbox omits quadkeys (geographic column) if total absolute mobile activity
  # fall below a certain threshold prior to calculating the index
  # In that case, it's safer to take the sum rather than the arithmetic means
  # AF also requested median too, so calculate that too 
  
  .[, keyby = .(agg_day_period, month, geography, xlon, xlat, bounds), 
    .(sum_activity = sum(activity_index_total))] %>%
  
  # weekday only, exclude weekends
  .[agg_day_period == 0, ] %>%
  .[, list(month, geography, xlon, xlat, bounds, sum_activity)]

# data cleaning: number of visitors per weekend --------------------------------

num_visitors_weekend <-
  mapbox %>%
  as.data.table() %>%
  
  # Mapbox omits quadkeys (geographic column) if total absolute mobile activity
  # fall below a certain threshold prior to calculating the index
  # In that case, it's safer to take the sum rather than the arithmetic means
  # AF also requested median too, so calculate that too 
  
  .[, keyby = .(agg_day_period, month, geography, xlon, xlat, bounds), 
    .(sum_activity = sum(activity_index_total))] %>%
  
  # weekday only, exclude weekends
  .[agg_day_period == 1, ] %>%
  .[, list(month, geography, xlon, xlat, bounds, sum_activity)]

# data cleaning: number of visitors per week -----------------------------------

num_visitors_week <-
  mapbox %>%
  as.data.table() %>%
  
  # Mapbox omits quadkeys (geographic column) if total absolute mobile activity
  # fall below a certain threshold prior to calculating the index
  # In that case, it's safer to take the sum rather than the arithmetic means
  # AF also requested median too, so calculate that too 
  
  # Combine weekday and weekends per quadkey, which means a weekly activity
  # So, omit agg_day_period as a grouping variable 
  .[, keyby = .(month, geography, xlon, xlat, bounds), 
    .(sum_activity = sum(activity_index_total))] %>%
  
  .[, list(month, geography, xlon, xlat, bounds, sum_activity)]
