library(tidyverse)
library(sf)

# first, load water area
awater <- read_sf("out_dataset/ca_water_areas.shp")

us_counties <- read_sf("~/GIS DataBase/tl_2019_us_county/tl_2019_us_county.shp")

# perform the removal
ca_counties_nowater <- us_counties %>% 
  filter(STATEFP == "06") %>% 
  st_difference(awater)

ca_counties_nowater %>%
  select(GEOID, NAME, INTPTLAT, INTPTLON, geometry) %>% 
  write_sf("out_dataset/ca_counties_nowater.shp")

ca_counties_nowater %>% 
  summarize(state = "California") %>%
  write_sf("out_dataset/ca_state_bounds.shp")
