# libraries -------------------------------------------------------------------- 
library(readr)       # for uploading csv very quickly
library(here)        # for creating relative file paths 
library(data.table)  # for manipulating data faster than dplyr
library(magrittr)    # for 'piping' data table code 
library(ggplot2)     # for visualizing data 
library(patchwork)

# import ----
mapbox_dt <- readRDS(here("data", "working", "mapbox_dt.RDS"))

# data cleaning: number of visitors per weekday --------------------------------

set.seed(1L)

mobile_act_weekday <-
 
  mapbox_dt %>%
  
  # Mapbox omits quadkeys (geographic column) if total absolute mobile activity
  # fall below a certain threshold prior to calculating the index
  # In that case, it's safer to take the sum rather than the arithmetic means
  # AF also requested median too, so calculate that too 
  
  .[, 
    list(
      sum_activity = sum(activity_index_total), 
      median_activity = median(activity_index_total), 
      num_quadkeys = .N
      ),
    keyby = 
      list(agg_day_period, month, geography, xlon, xlat, bounds)] %>%
  
  # weekday only, exclude weekends
  .[agg_day_period == 0, ] %>%
  .[, 
    list(
      month, 
      geography, 
      xlon, 
      xlat, 
      bounds, 
      sum_activity, 
      median_activity, 
      num_quadkeys)]

# data cleaning: number of visitors per weekend --------------------------------

mobile_act_weekend <-
  
  mapbox_dt %>%
  
  # Mapbox omits quadkeys (geographic column) if total absolute mobile activity
  # fall below a certain threshold prior to calculating the index
  # In that case, it's safer to take the sum rather than the arithmetic means
  # AF also requested median too, so calculate that too 
  
  .[, 
    list(
      sum_activity = sum(activity_index_total), 
      median_activity = median(activity_index_total), 
      num_quadkeys = .N
    ),
    keyby = 
      list(agg_day_period, month, geography, xlon, xlat, bounds)] %>%
  
  # weekday only, exclude weekends
  .[agg_day_period == 1, ] %>%
  .[, 
    list(
      month, 
      geography, 
      xlon, 
      xlat, 
      bounds,
      sum_activity,
      median_activity, 
      num_quadkeys)]

# data cleaning: number of visitors per week -----------------------------------

mobile_act_week <-
 
  mapbox_dt %>%
  
  # Mapbox omits quadkeys (geographic column) if total absolute mobile activity
  # fall below a certain threshold prior to calculating the index
  # In that case, it's safer to take the sum rather than the arithmetic means
  # AF also requested median too, so calculate that too 
  
  # Combine weekday and weekends per quadkey, which means a weekly activity
  # So, omit agg_day_period as a grouping variable 
  .[, 
    list(sum_activity = sum(activity_index_total),
         median_activity = median(activity_index_total),
         num_quadkeys = .N), 
    keyby = list(month, geography, xlon, xlat, bounds)] %>%
  
  .[, 
    list(
      month, 
      geography, 
      xlon, 
      xlat, 
      bounds, 
      sum_activity, 
      median_activity, 
      num_quadkeys)]

# data cleaning: number of visitors during peak hours --------------------------

# Important note!
# According to AF, the time windows for agg_time_period
# are 2 hrs apart

# To make it easier to read, convert the existing time windows
# 0: 12 am - 2 am
# 1: 2 am - 4 am 
# 2: 4 am - 6 am
# 3: 6 am - 8 am
# 4: 8 am - 10 am
# 5: 10 am - 12 pm
# 6: 12 pm - 2 pm
# 7: 2 pm - 4pm
# 8: 4 pm - 6 pm
# 9: 6 pm - 8 pm
# 10: 8 pm - 10 pm
# 11: 10 pm - 12 am

mobile_act_peak_hours <-
  
  mapbox_dt %>%
  
  # Peak hours are 10 am - 4 pm 
  # so need to filter the dataset through
  # 5-7 for agg_time_period 
  .[agg_time_period %in% c(5, 6, 7)] %>%
  
  # Mapbox omits quadkeys (geographic column) if total absolute mobile activity
  # fall below a certain threshold prior to calculating the index
  # In that case, it's safer to take the sum rather than the arithmetic means
  # AF also requested median too, so calculate that too 
  
  # Combine weekday and weekends per quadkey, which means a weekly activity
  # So, omit agg_day_period as a grouping variable 
  .[, 
    list(sum_activity = sum(activity_index_total),
         median_activity = median(activity_index_total),
         num_quadkeys = .N), 
    keyby = list(month, geography, xlon, xlat, bounds)] %>%
  
  .[, 
    list(
      month, 
      geography, 
      xlon, 
      xlat, 
      bounds, 
      sum_activity, 
      median_activity, 
      num_quadkeys)]

# data cleaning: number of visitors during off-peak hours ----------------------

# Important note!
# According to AF, the time windows for agg_time_period
# are 2 hrs apart

# To make it easier to read, convert the existing time windows
# 0: 12 am - 2 am
# 1: 2 am - 4 am 
# 2: 4 am - 6 am
# 3: 6 am - 8 am
# 4: 8 am - 10 am
# 5: 10 am - 12 pm
# 6: 12 pm - 2 pm
# 7: 2 pm - 4pm
# 8: 4 pm - 6 pm
# 9: 6 pm - 8 pm
# 10: 8 pm - 10 pm
# 11: 10 pm - 12 am

mobile_act_off_hours <-
  
  mapbox_dt %>%
  
  # Peak hours are 10 am - 4 pm 
  # so need to filter the dataset that are NO in the
  # 5-7 for agg_time_period time window
  .[!(agg_time_period %in% c(5, 6, 7))] %>%
  
  # Mapbox omits quadkeys (geographic column) if total absolute mobile activity
  # fall below a certain threshold prior to calculating the index
  # In that case, it's safer to take the sum rather than the arithmetic means
  # AF also requested median too, so calculate that too 
  
  # Combine weekday and weekends per quadkey, which means a weekly activity
  # So, omit agg_day_period as a grouping variable 
  .[, 
    list(sum_activity = sum(activity_index_total),
         median_activity = median(activity_index_total),
         num_quadkeys = .N), 
    keyby = list(month, geography, xlon, xlat, bounds)] %>%
  
  .[, 
    list(
      month, 
      geography, 
      xlon, 
      xlat, 
      bounds, 
      sum_activity, 
      median_activity, 
      num_quadkeys)]

# plots ------------------------------------------------------------------------

theme_set(theme_bw())

mobile_act_weekday %>%
  ggplot(aes(x = sum_activity)) +
  geom_histogram() +
  facet_wrap(~month) + 
  labs(title = "Number of visitors per weekday") + 
  labs(x = "Total Activity Index")

mobile_act_weekend %>%
  ggplot(aes(x = sum_activity)) +
  geom_histogram() + 
  facet_wrap(~month) + 
  labs(title = "Number of visitors per weekend") + 
  labs(x = "Total Activity Index")

mobile_act_week %>%
  ggplot(aes(x = sum_activity)) +
  geom_histogram() +
  facet_wrap(~month) + 
  labs(title = "Number of visitors per week") + 
  labs(x = "Total Activity Index")

# plots: off vs peak hours -----------------------------------------------------

hist_peak_hours <- mobile_act_peak_hours %>%
  ggplot(aes(x = sum_activity)) +
  geom_histogram() +
  facet_wrap(~month) + 
  labs(
    title = "Peaks Hours (10 am - 4pm)",
    x     = "Total Activity Index"
    )

hist_off_hours <- mobile_act_off_hours %>%
  ggplot(aes(x = sum_activity)) +
  geom_histogram() +
  facet_wrap(~month) + 
  labs(
    title = "Off Hours (not between 10am - 4pm)",
    x     = "Total Activity Index")

hist_off_hours + hist_peak_hours

# save to disk ----------------------------------------------------------------

# mobile activity per week 
saveRDS(
  object = mobile_act_week, 
  file = here("data", "working", "mobile_activity_week.rds")
)

# mobile activity per weekend 
saveRDS(
  object = mobile_act_weekend, 
  file = here("data", "working", "mobile_activity_weekend.rds")
)

# mobile activity per weekday 
saveRDS(
  object = mobile_act_weekday, 
  file = here("data", "working", "mobile_activity_weekday.rds")
)