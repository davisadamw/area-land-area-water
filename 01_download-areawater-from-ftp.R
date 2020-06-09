library(tidyverse)
library(sf)

all_files <- list.files("TigerZipDump", pattern = ".shp$", full.names = TRUE)

all_polys <- all_files %>% 
  map_dfr(read_sf)

# only a very small number of polygons have both ALAND and AWATER
all_polys %>% st_drop_geometry() %>% count(ALAND > 0, AWATER > 0)

# only interested in lakes/reservoirs bigger than 10km2 OR Bay / 
water_polys <- all_polys %>% 
  filter(FULLNAME %in% c("Lk Tahoe", "Salton Sea", "Pacific Ocean", "Carquinez Strait",
                         "San Francisco Bay", "San Pablo Bay", "Monterey Bay", "Suisun Bay",
                         "San Diego Bay") |
           (AWATER > 1e8 & is.na(FULLNAME)))

# output the results as one shapefile for now
water_polys %>% 
  summarize() %>% 
  write_sf("out_dataset/ca_water_areas.shp")
