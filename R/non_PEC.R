CMA_Codes%>%
  filter(str_detect(name, "Vancouver"))

## Get shapes and centroids


quebec<- 
  get_census(
    dataset = "CA16", 
    regions = list(PR = "24"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(32618)

mont_tremblant<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "2478102"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(32618)

mont_tremblant_c <-  st_centroid(mont_tremblant)

prince_edward<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "3513020"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(32618)

prince_edward_c <-  st_centroid(prince_edward)

blue_mountains<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "3542045"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(32618)

blue_mountains_c <-  st_centroid(blue_mountains)

whistler<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "5931020"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(32618)

whistler_c <-  st_centroid(whistler)

##
mtl<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "24462"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(32618)

mtl_c <-  st_centroid(mtl)


### Find all municipalities within radius of mtl 

radius_mtl <- as.numeric(st_distance(mtl_c,mont_tremblant_c))
##add a buffer"
##radius_mtl <- radius_mtl+2000 
radius_mtl <- st_buffer(mtl_c, radius_mtl)
plot(radius_mtl$geometry)
plot(mtl, add=TRUE)
plot(mont_tremblant, add=TRUE)
plot(quebec, add=TRUE)

mt_intersect <- st_intersection(quebec, radius_mtl)
mt_intersect <- mt_intersect%>%
  filter(Type == "CSD" & Population>"8000")


########
vancouver<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "59933"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(32618)

vancouver_c <-  st_centroid(vancouver)
radius_vancouver <- st_distance(vancouver_c,whistler_c)
radius_vancouver <- st_buffer(vancouver_c, radius_vancouver)
plot(radius_vancouver$geometry)
plot(vancouver, add=TRUE)
plot(whistler, add=TRUE)


## Import property files

property_blue_mountains <-
  read_csv("data/blue_mountains_property.csv", col_types = cols_only(
    `Property_ID` = col_character(),
    `Listing_Title` = col_character(),
    `Property_Type` = col_character(),
    `Listing_Type` = col_character(),
    `Created_F` = col_skip(),
    `Created` = col_date(format = ""),
    `Scraped_F` = col_skip(),
    `Scraped` = col_date(format = ""),
    `Latitude` = col_double(),
    `Longitude` = col_double(),
    `Airbnb_PID` = col_double(),
    `Airbnb_HID` = col_double()))%>%
  set_names(c("Property_ID", "Listing_Title", "Property_Type", "Listing_Type",
              "Created", "Scraped", "Latitude", "Longitude", "Airbnb_PID",
              "Airbnb_HID")) %>% 
  arrange(Property_ID) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32618) %>% 
  filter(Property_Type %in% c(
    "House", "Private room in house", "Apartment", "Cabin",
    "Entire condominium", "Townhouse", "Condominium", "Entire apartment",
    "Private room", "Loft", "Place", "Entire house", "Villa", "Guesthouse",
    "Private room in apartment", "Guest suite", "Shared room in dorm",
    "Chalet", "Dorm", "Entire chalet", "Shared room in loft", "Cottage",
    "Resort", "Serviced apartment", "Other", "Bungalow", "Farm stay",
    "Private room in villa", "Entire loft", "Entire villa",
    "Private room in guesthouse", "Island", "Entire cabin", "Vacation home",
    "Entire bungalow", "Earth house", "Nature lodge", "In-law",
    "Entire guest suite", "Shared room in apartment", "Private room in loft",
    "Tiny house", "Castle", "Earth House", "Private room in condominium",
    "Entire place", "Shared room", "Hut", "Private room in guest suite",
    "Private room in townhouse", "Timeshare", "Entire townhouse",
    "Shared room in house", "Entire guesthouse", "Shared room in condominium",
    "Cave", "Private room in cabin", "Dome house",
    "Private room in vacation home", "Private room in dorm",
    "Entire serviced apartment", "Private room in bungalow",
    "Private room in serviced apartment", "Entire Floor", "Entire earth house",
    "Entire castle", "Shared room in chalet", "Shared room in bungalow",
    "Shared room in townhouse", "Entire cottage", "Private room in castle",
    "Private room in chalet", "Private room in nature lodge", "Entire in-law",
    "Shared room in guesthouse", "Casa particular", "Serviced flat", "Minsu",
    "Entire timeshare", "Shared room in timeshare", "Entire vacation home",
    "Entire nature lodge", "Entire island", "Private room in in-law",
    "Shared room in serviced apartment", "Shared room in cabin", "Entire dorm",
    "Entire cave", "Private room in timeshare", "Shared room in guest suite",
    "Private room in cave", "Entire tiny house",
    "Private room in casa particular (cuba)", "Casa particular (cuba)",
    "Private room in cottage", "Private room in tiny house",
    "Entire casa particular", "")) %>% 
  select(-Property_Type)


## TOFINO  
  
  
  ## filter out homeaway listings
  property_tofino%>%
    filter(str_detect(Property_ID, "ab"))
  
  ## Analysis: number of listings per host
  property_tofino%>%
  filter(str_detect(Property_ID, "ab"))%>%
  count(Airbnb_HID)%>%
  View()
  
  ## total number of listings scraped 2019 onwards
 total <-  property_tofino%>%
    filter(str_detect(Property_ID, "ab"))%>%
    filter(Scraped>=2019-01-01)%>%
     count(Property_ID)

  total <- sum(total$n)
 
  ## housing breakdown - Listing_Type
property_tofino%>%
    st_drop_geometry()%>%
    filter(str_detect(Property_ID, "ab"))%>%
    filter(Scraped>=2019-01-01)%>%
    count(Listing_Type)%>%
    mutate(prop = n/total)
  


##### 
## Import property files

property_blue_mountains <-
  read_csv("data/blue_mountains_property.csv", col_types = cols_only(
    `Property_ID` = col_character(),
    `Listing_Title` = col_character(),
    `Property_Type` = col_character(),
    `Listing_Type` = col_character(),
    `Created_F` = col_skip(),
    `Created` = col_date(format = ""),
    `Scraped_F` = col_skip(),
    `Scraped` = col_date(format = ""),
    `Latitude` = col_double(),
    `Longitude` = col_double(),
    `Airbnb_PID` = col_double(),
    `Airbnb_HID` = col_double()))%>%
  set_names(c("Property_ID", "Listing_Title", "Property_Type", "Listing_Type",
              "Created", "Scraped", "Latitude", "Longitude", "Airbnb_PID",
              "Airbnb_HID")) %>% 
  arrange(Property_ID) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32618) %>% 
  filter(Property_Type %in% c(
    "House", "Private room in house", "Apartment", "Cabin",
    "Entire condominium", "Townhouse", "Condominium", "Entire apartment",
    "Private room", "Loft", "Place", "Entire house", "Villa", "Guesthouse",
    "Private room in apartment", "Guest suite", "Shared room in dorm",
    "Chalet", "Dorm", "Entire chalet", "Shared room in loft", "Cottage",
    "Resort", "Serviced apartment", "Other", "Bungalow", "Farm stay",
    "Private room in villa", "Entire loft", "Entire villa",
    "Private room in guesthouse", "Island", "Entire cabin", "Vacation home",
    "Entire bungalow", "Earth house", "Nature lodge", "In-law",
    "Entire guest suite", "Shared room in apartment", "Private room in loft",
    "Tiny house", "Castle", "Earth House", "Private room in condominium",
    "Entire place", "Shared room", "Hut", "Private room in guest suite",
    "Private room in townhouse", "Timeshare", "Entire townhouse",
    "Shared room in house", "Entire guesthouse", "Shared room in condominium",
    "Cave", "Private room in cabin", "Dome house",
    "Private room in vacation home", "Private room in dorm",
    "Entire serviced apartment", "Private room in bungalow",
    "Private room in serviced apartment", "Entire Floor", "Entire earth house",
    "Entire castle", "Shared room in chalet", "Shared room in bungalow",
    "Shared room in townhouse", "Entire cottage", "Private room in castle",
    "Private room in chalet", "Private room in nature lodge", "Entire in-law",
    "Shared room in guesthouse", "Casa particular", "Serviced flat", "Minsu",
    "Entire timeshare", "Shared room in timeshare", "Entire vacation home",
    "Entire nature lodge", "Entire island", "Private room in in-law",
    "Shared room in serviced apartment", "Shared room in cabin", "Entire dorm",
    "Entire cave", "Private room in timeshare", "Shared room in guest suite",
    "Private room in cave", "Entire tiny house",
    "Private room in casa particular (cuba)", "Casa particular (cuba)",
    "Private room in cottage", "Private room in tiny house",
    "Entire casa particular", "")) %>% 
  select(-Property_Type)

