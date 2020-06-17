library(tidyverse)
library(sf)

# load block groups
ca_bgs_all <- read_sf("~/GIS DataBase/tl_2016_06_bg/tl_2016_06_bg.shp") %>% 
  mutate(county = paste0(STATEFP, COUNTYFP), .after = 2) %>% 
  st_transform(3310)

# load water areas
all_files <- list.files("TigerZipDump", pattern = ".shp$", full.names = TRUE)

read_sf_county <- function(filename) {
  county_GEOID <- str_extract(filename, "06[0-9]{3}(?=_areawater.shp)")
  
  read_sf(filename) %>% 
    mutate(county = county_GEOID, .before = 1)
  
}

all_polys <- all_files %>% 
  map_dfr(read_sf_county) %>% 
  st_transform(3310)

# only interested in lakes/reservoirs bigger than 10km2 OR Bay / Ocean
# also collapse all water areas in each county
water_polys <- all_polys %>% 
  filter(FULLNAME %in% c("Lk Tahoe", "Salton Sea", "Pacific Ocean", "Carquinez Strait",
                         "San Francisco Bay", "San Pablo Bay", "Monterey Bay", "Suisun Bay",
                         "San Diego Bay") |
           (AWATER > 1e8 & is.na(FULLNAME))) %>%
  select(county, geometry) %>% 
  group_by(county) %>% 
  nest() %>% 
  mutate(data = map(data, st_union))

# match block groups to water polygons, but only for County that have any water
bg_aux_data <- ca_bgs_all %>% 
  st_drop_geometry()

bgs_nowater <- ca_bgs_all %>% 
  filter(AWATER == 0 | ! county %in% water_polys$county) %>% 
  select(county, GEOID)

# nest block groups by county, attach relevant water data
bg_water_bycounty <- ca_bgs_all %>% 
  select(county, GEOID) %>% 
  anti_join(st_drop_geometry(bgs_nowater), by = "GEOID") %>% 
  group_by(county) %>% 
  nest() %>% 
  left_join(water_polys, by = "county", suffix = c("_bg", "_water"))

# run water removal ... note: some bgs are completely removed
bg_water_removed <- bg_water_bycounty %>% 
  mutate(nowater = map2(data_bg, data_water,
                        ~ st_difference(.x, .y) %>% st_cast("MULTIPOLYGON"))) %>% 
  select(county, nowater) %>% 
  unnest(nowater) %>% 
  ungroup() %>% 
  st_as_sf(sf_column_name = "geometry")
  
# combine with no water and reattach auxiliary data
bg_updated_geom <- bind_rows(bgs_nowater, bg_water_removed) %>% 
  left_join(bg_aux_data, by = c("county", "GEOID")) %>% 
  arrange(GEOID)

# save as shapefile and rds
bg_updated_geom %>% 
  write_rds("out_dataset/ca_bgs_nowater.rds", compress = "gz") %>% 
  write_sf("out_dataset/ca_bgs_nowater.shp")



