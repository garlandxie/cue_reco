# libraries ----
library(readr)
library(here)
library(assertr)
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

errors <- mapbox %>% 
  assertr::verify(xlat >= 43) %>%
  assertr::verify(xlon <= 79) %>%
  assertr::verify(agg_day_period >= 0) %>%
  assertr::verify(agg_time_period >= 0)
  