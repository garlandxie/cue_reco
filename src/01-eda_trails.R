# libraries ----
library(here)
library(sf)
library(ggplot2)
library(skimr)

# import ----

trca_trails <- read_sf(
  here("data", "raw", "Trail_Strategy_trca.shp")
)

halt_trails <- read_sf(
  here("data", "raw", "Halton_Trails.shp")
)

# check packaging ----
skimr::skim(trca_trails)
skimr::skim(halt_trails)

# plots -----

ggplot() + 
  geom_sf(data = halt_trails) + 
  geom_sf(data = trca_trails)