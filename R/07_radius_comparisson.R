############ DATA AND BOUNDARY IMPORT FOR ALL SEASONAL TOWNS ###############

source("R/01_helper_functions.R")
source("R/02_data_import.R")

###############

property_pec <- property%>%
  filter(str_detect( City, "Prince Edward County"))
property_blue_mountains <- property%>%
  filter(str_detect( City, "Blue Mountains"))
property_mont_tremblant <- property%>%
  filter(str_detect( City, "Mont-Tremblant"))
property_banff <- st_intersection(property, st_buffer(banff, 200))

## City census and centroid
mont_tremblant<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "2478102"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(3347)
mont_tremblant_c <-  st_centroid(mont_tremblant)

pec<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "3513020"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(3347)
pec_c <-  st_centroid(pec)

whistler<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "5931020"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(3347)
whistler_c <-  st_centroid(whistler)

tofino<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "5923025"),
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(3347)
tofino_c <-  st_centroid(tofino)

banff<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "4815035"),
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(3347)
banff_c <-  st_centroid(banff)

## Urban agglomerations
toronto<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "35535"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(3347)
toronto_c <-  st_centroid(toronto)

##radius_toronto <- radius_toronto+10000 
radius_toronto <- st_buffer(toronto_c, 250000)
toronto_intersect <- st_intersection(canada_CSD, radius_toronto$geometry)
toronto_intersect <- toronto_intersect%>%
  filter(Type == "CSD", Population >= 1500)

##
montreal<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "24462"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(3347)

montreal_c <-  st_centroid(montreal)
##add a buffer"
#radius_mtl <- radius_mtl+10000 
radius_montreal <- st_buffer(montreal_c, 250000)

montreal_intersect <- st_intersection(canada_CSD, radius_montreal)
montreal_intersect <- montreal_intersect%>%
  filter(Type == "CSD", Population >= 1500, Population <= 11000)

########

calgary<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "48825"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(3347)
calgary_c <-  st_centroid(calgary)

radius_calgary <- st_buffer(calgary_c, 250000)
calgary_intersect <- st_intersection(canada_CSD, radius_calgary)
calgary_intersect <- calgary_intersect%>%
  filter(Type == "CSD", Population >= 1500, Population <= 11000)

######################## ADD PROPERTIES ##################################

## Intersect property and radius around urban agglomerations
toronto_alentours_all <- st_intersection(property, toronto_intersect)
toronto_alentours <- toronto_alentours_all %>%
  filter((is.na(toronto_alentours_all$CMA_UID)==TRUE))%>%
  filter(City!="Prince Edward County", City!="The Blue Mountains", Name!= "Prince Edward County (CY)", Name!="The Blue Mountains (T)")
toronto_alentours_noCMA <- toronto_alentours_all %>%
  filter((is.na(toronto_alentours_all$CMA_UID)==TRUE))

montreal_alentours_all <- st_intersection(property, montreal_intersect)
montreal_alentours <- montreal_alentours_all %>%
  filter((is.na(montreal_alentours_all$CMA_UID)==TRUE))%>%
  filter(City!="Mont-Tremblant", Name!="Mont-Tremblant (V)")
montreal_alentours_noCMA <- montreal_alentours_all %>%
  filter((is.na(montreal_alentours_all$CMA_UID)==TRUE))

calgary_alentours_all <- st_intersection(property, calgary_intersect)
calgary_alentours_noCMA <- calgary_alentours_all %>%
  filter((is.na(calgary_alentours_all$CMA_UID)==TRUE))
calgary_alentours <- calgary_alentours_noCMA%>%
  filter(calgary_alentours_noCMA$Airbnb_PID %in% property_banff$Airbnb_PID ==FALSE)

Annual_Rev <- aggregate(calgary_alentours$Annual_Revenue, by=list(GeoUID=calgary_alentours$GeoUID), FUN=sum)

canada_CSD%>%
  filter(GeoUID=="5901017")

Listings_GeoUID <- calgary_alentours_noCMA%>%
  group_by(GeoUID)%>%
  count()
Listings_GeoUID$geometry <- NULL

tst <- inner_join(Annual_Rev, Listings_GeoUID)%>%
  mutate(revperlist=x/n)

Lperd <- inner_join(canada_CSD, tst)
Lperd <- Lperd%>%
  mutate(List_per_dwelling=n/Dwellings)
View(Lperd)



canada_CSD%>%
  filter(GeoUID =="3531030")

property_banff

tm_shape(radius_toronto)+
  tm_dots(col="white")+
  tm_shape(toronto)+
  tm_borders(col="black")+
  tm_shape(Lperd)+
  tm_fill(col="List_per_dwelling")+
  tm_shape(canada_CSD)+
  tm_borders(col="black")


tst <- st_join(tst, canada_CSD)
tst%>%
  mutate(dperp=n/Population)

#################

Annual_Rev <- aggregate(montreal_alentours_noCMA$Annual_Revenue, by=list(GeoUID=montreal_alentours_noCMA$GeoUID), FUN=sum)

Listings_GeoUID <- montreal_alentours_noCMA%>%
  group_by(GeoUID)%>%
  count()
Listings_GeoUID$geometry <- NULL

tst <- inner_join(Annual_Rev, Listings_GeoUID)%>%
  mutate(revperlist=x/n)

Lperd <- inner_join(canada_CSD, Listings_GeoUID)
Lperd <- Lperd%>%
  mutate(List_per_dwelling=n/Dwellings)
View(Lperd)

