#### RSA presentation analysis #################################################

library(tidyverse)
library(sf)
library(strr)
library(upgo)
library(cancensus)
library(nngeo)

### Census data ################################################################

## CMAs

CMA_names <-
  list_census_regions("CA16") %>%
  filter(municipal_status == "B")

CMA <-
  get_census("CA16",
             regions = list(CMA = CMA_names$region),
             geo_format = "sf") %>% 
  st_transform(3347) %>% 
  inner_join(CMA_names, by = c("GeoUID" = "region")) %>% 
  select(name = name.y, GeoUID, CMA_pop = Population, geometry) %>% 
  st_set_agr(c(name = "identity", GeoUID = "identity", CMA_pop = "constant"))

rm(CMA_names)

## CSDs

CSD_names <-
  list_census_regions("CA16") %>%
  filter(level == "CSD") %>% 
  select(region, name)

CSD <- 
  get_census(
    dataset = "CA16", 
    regions = list(C = "1"),  
    level = "CSD",
    vectors =  c("v_CA16_4836", "v_CA16_4838", "v_CA16_4886", "v_CA16_4888",
                 "v_CA16_2207", "v_CA16_406"),
    geo_format = "sf") %>% 
  inner_join(CSD_names, by = c("GeoUID" = "region")) %>%
  st_transform(3347) %>%   
  select(-(`Shape Area`:Households), -name.x,
         -`Adjusted Population (previous Census)`, -CD_UID, -`Region Name`, 
         -`Area (sq km)`) %>% 
  set_names(c("GeoUID", "population", "PR_UID", "CMA_UID", "geometry",
              "households_1", "renter", "households_2", "core_housing_need",
              "median_income", "density", "name")) %>% 
  mutate(renter_pct = renter / households_1,
         core_housing_need_pct = core_housing_need / households_2) %>% 
  select(name, GeoUID, population:CMA_UID, renter_pct, core_housing_need_pct,
         median_income, density, geometry)

rm(CSD_names)

## Add nearest_CMA and dist_to_CMA fields

distances <- 
  st_nn(st_centroid(CSD), st_centroid(CMA), returnDist = TRUE)

CSD <-
  CSD %>% 
  mutate(
    nearest_CMA = map2_chr(distances[[1]], distances[[2]], ~{
      st_drop_geometry(CMA)[.x,]$name}),
    dist_to_CMA = map_dbl(distances[[2]], ~{.x})
  )

rm(distances)

## Define suburb, central city and regional variables

CSD <- 
  CSD %>% 
  group_by(nearest_CMA) %>% 
  mutate(geography = case_when(
    str_detect(nearest_CMA, name) == TRUE & 
      (str_detect(nearest_CMA, "-") == TRUE | 
         population == max(population) | name == "Victoria") ~ "central city",
    (CMA_UID %in% CMA$GeoUID)                                ~ "suburb",
    lengths(st_nn(st_centroid(geometry), 
                  st_centroid(CMA), maxdist = 100000)) > 0   ~ "inner region",
    lengths(st_nn(st_centroid(geometry), 
                  st_centroid(CMA), maxdist = 250000)) > 0   ~ "outer region",
    TRUE                                                     ~ "no region"
  )) %>% 
  ungroup() %>% 
  select(-geometry, everything(), geometry)


### STR data ###################################################################

## Retrieve property file from server

upgo_connect()

property <- 
  property_all %>% 
  filter(country == "Canada") %>% 
  collect() %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(3347)

## Associate listings with CSDs

property <- 
  CSD %>% 
  select(GeoUID) %>% 
  st_join(property, .)

## Calculate FREH listings and join to property file

FREH_compressed <- 
  daily_all %>% 
  filter(property_ID %in% !! property$property_ID, 
         start_date >= "2018-08-01", listing_type == "Entire home/apt") %>% 
  collect()

rm(con, daily_all, property_all)

FREH <- 
  FREH_compressed %>% 
  filter(status %in% c("R", "A")) %>% 
  strr_expand_daily(cores = 6)

FREH <- 
  FREH %>% 
  group_by(property_ID) %>% 
  summarize(sum_r = sum(status == "R"),
            sum_ar = n()) %>% 
  filter(sum_r >= 90, sum_ar >= 183) %>% 
  mutate(FREH = TRUE) %>% 
  select(property_ID, FREH)

property <- 
  property %>% 
  left_join(FREH) %>% 
  mutate(FREH = if_else(is.na(FREH), FALSE, FREH))

## Summarize property file attributes to CSD

CSD <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(created <= "2019-07-31", scraped >= "2019-07-31") %>% 
  count(GeoUID) %>% 
  rename(active_listings = n) %>% 
  left_join(CSD, .)

CSD <- 
  property %>% 
  st_drop_geometry %>% 
  group_by(GeoUID) %>% 
  summarize(FREH = sum(FREH == TRUE)) %>% 
  left_join(CSD, .)

CSD <- 
  property %>% 
  st_drop_geometry() %>% 
  filter(created <= "2018-07-31", scraped >= "2018-07-31") %>% 
  count(GeoUID) %>% 
  rename(active_listings_2018 = n) %>% 
  left_join(CSD, .)

rm(FREH)


### Add tourism ################################################################

load("data/tourism_codes.Rdata")
## Import Tourism variable

CSD <- 
  tourism_codes %>% 
  group_by(GeoUID) %>% 
  summarise (tourism_naics = sum(Total)) %>% 
  left_join(CSD, .)

rm(tourism_codes)


### Add final variables ########################################################

CSD <- 
  CSD %>% 
  mutate(active_listings_per_cap = active_listings / population,
         FREH_per_cap = FREH / population,
         active_listings_2018_per_cap = active_listings_2018 / population,
         tourism_per_cap = tourism_naics / population)


CSD %>% 
  filter(geography == "central city") %>% 
  group_by()




CMA_attributes <-
  CSD %>% 
  pull(nearest_CMA) %>% 
  map_df(~{
    CSD %>% 
      st_drop_geometry() %>% 
      group_by(nearest_CMA) %>% 
      filter(geography == "central city", nearest_CMA == .x, 
             (population == max(population) | name == "Victoria")) %>% 
      ungroup() %>% 
      select(renter_pct, core_housing_need_pct, median_income, 
             active_listings_2018_per_cap, tourism_per_cap)
  })

CSD <- 
  CSD %>% 
  mutate(CMA_renter = CMA_attributes %>% pull(1),
         CMA_core_housing = CMA_attributes %>% pull(2),
         CMA_income = CMA_attributes %>% pull(3),
         CMA_active = CMA_attributes %>% pull(4),
         CMA_tourism = CMA_attributes %>% pull(5)) %>% 
  st_set_agr("identity")

rm(CMA_attributes)

CSD <- 
  CSD %>% 
  mutate(CMA_total_pop = map_dbl(nearest_CMA, ~{
    CMA %>% 
      st_drop_geometry() %>% 
      filter(name == .x) %>% 
      pull(CMA_pop)
  }))


### Save output ################################################################

save(CMA, file = "data/CMA.Rdata")
save(CSD, file = "data/CSD.Rdata")
save(FREH, file = "data/FREH.Rdata")
save(property, file = "data/property.Rdata")
