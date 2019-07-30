############ DATA AND BOUNDARY IMPORT FOR ALL SEASONAL TOWNS ###############

source("R/01_helper_functions.R")
source("R/02_data_import.R")

######### DEFINE YEARS #######


#CMA_Codes <- filter(list_census_regions(dataset = "CA16"), level == "CMA")
#CMA_Codes%>%
#  filter(str_detect(name, "Calgary"))

### Select by city name
property_mt <- property%>%
  filter(str_detect(City,"Mont-Tremblant"))

### Select by city boundaries
property_mt2 <- 
  st_intersection(property, city)

## Get shapes and centroids

canada_CSD

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
### Toronto radius
#dist_pec <- as.numeric(st_distance(toronto_c,pec_c))
##add a buffer"
##radius_toronto <- radius_toronto+10000 
radius_toronto <- st_buffer(toronto_c, 400000)

toronto_intersect <- st_intersection(canada_CSD, radius_toronto$geometry)

toronto_intersect <- toronto_intersect%>%
  filter(Type == "CSD", Population >= 1500, Population <= 11000)
plot(radius_toronto$geometry)
plot(toronto, add=TRUE)
plot(pec, add=TRUE)
plot(toronto_intersect$geometry, add=TRUE)


##
montreal<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "24462"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(3347)

montreal_c <-  st_centroid(mtl)
### Find all municipalities within radius of mtl 
dist_mtl <- as.numeric(st_distance(mtl_c,mont_tremblant_c))
##add a buffer"
#radius_mtl <- radius_mtl+10000 
radius_mtl <- st_buffer(mtl_c, dist_mtl)

mt_intersect <- st_intersection(quebec, radius_mtl)
mt_intersect <- mt_intersect%>%
  filter(Type == "CSD", Population >= 6000, Population <= 11000)

plot(radius_mtl$geometry)
plot(mtl, add=TRUE)
plot(mont_tremblant, add=TRUE)
plot(mt_intersect,add=TRUE)


########
vancouver<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "59933"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(3347)

vancouver_c <-  st_centroid(vancouver)
dist_vancouver <- as.numeric(st_distance(vancouver_c,whistler_c))
radius_vancouver <- st_buffer(vancouver_c, dist_vancouver)

vancouver_intersect <- st_intersection(bc, radius_vancouver)
vancouver_intersect <- vancouver_intersect%>%
  filter(Type == "CSD", Population >= 6000, Population <= 11000)


plot(radius_vancouver$geometry)
plot(vancouver, add=TRUE)
plot(whistler, add=TRUE)
plot(vancouver_intersect,add=TRUE)
########
victoria<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "59935"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(3347)

victoria_c <-  st_centroid(victoria)
dist_victoria <- as.numeric(st_distance(victoria_c,tofino_c))
radius_victoria <- st_buffer(victoria_c, dist_victoria)
plot(radius_victoria$geometry)
plot(victoria, add=TRUE)
plot(tofino, add=TRUE)

## Check distances
dist_mtl
dist_pec
dist_vancouver
dist_victoria

######################## ADD PROPERTIES ##################################

## Intersect property and radius around urban agglomerations
pec_intersect
pec_intersect <- pec_intersect%>%
  filter(name!="Prince Edward County")
pec_alentours <- st_intersection(property, pec_intersect)

pec_alentours <- pec_alentours%>%
  filter()

## Plot to verify extents
plot(radius_pec$geometry)
plot(toronto, add=TRUE)
plot(pec, add=TRUE)
plot(pec_alentours, add=TRUE)
plot(pec_intersect,add=TRUE)


tm_shape(radius_mtl)+
  tm_borders(col="black")+
  tm_shape(mtl)+
  tm_fill(col="red")+
  tm_shape(mt_intersect)+
  tm_fill(col="green")+
  tm_shape(mont_tremblant)+
  tm_fill(col="blue")
