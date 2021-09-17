# libraries ----
library(readr)
library(here)
library(skimr)

# import ----
mapbox <- read_csv(
  here("data/raw", "0302231.wdwe-2h.csv")
)

# check packaging ----
head(mapbox, n = 10)
str(mapbox)

# check names ----
names(mapbox)
