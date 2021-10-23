# libraries ----
library(here)
library(sf)
library(ggplot2)
library(skimr)

# import ----

trca_trails <- read_sf(
  here("data", "raw", "Trail_Strategy_trca.shp")
)

# check packaging ----
skimr::skim(trca_trails)

# plots -----

trca_trails |>
  ggplot() +
  geom_sf()