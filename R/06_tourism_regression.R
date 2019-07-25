########################## REGRESSION PREP ############################

source("R/01_helper_functions.R")
source("R/02_5_data_import.R")
source("R/05_tourism_and_travel_labour.R")

###############

## Tourism Indicator for every CSD with GEOUID
  
tourism_indicator <- codes %>% 
    group_by(GeoUID) %>% 
    summarise (Count = sum(Total))

## Nearest CMA
vancouver<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "59933"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(32618)%>%
  mutate(name = "Vancouver")

toronto<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "35535"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(32618)%>%
  mutate (name="Toronto")

montreal<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "24462"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(32618)%>%
  mutate(name = "Montreal")

calgary<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "48825"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(32618)%>%
  mutate(name="Calgary")

  ###
  CMAs <- rbind(toronto, vancouver, montreal, calgary)
  CSDs <- inner_join(canada, tourism_indicator)
  CSDs$Quality <- NULL
  CSDs$NHS <- NULL
  
  CSDs_tourism <- CSDs%>%
    mutate(distance = st_distance(st_centroid(CSDs$geometry), st_centroid(CMAs[1,st_nearest_feature(st_centroid(CSDs$geometry), st_centroid(CMAs$geometry))])))
  
