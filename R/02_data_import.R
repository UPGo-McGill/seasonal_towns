############# DATA IMPORT ######################

source("R/01_helper_functions.R")

###

End_date <- as.Date("2019-04-30")
Start_date <- as.Date("2018-04-30")

citycode = "3513"
cityname = "Prince Edward County"

city <- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = citycode),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(32618)

variables_pop = c("v_CA16_401", "v_CA16_402","v_CA16_403","v_CA16_404","v_CA16_405","v_CA16_406")
variables_pop_names = 
variables_housing = c("v_CA16_4843", "v_CA16_4844", "v_CA16_4845", "v_CA16_4846", "v_CA16_4847", "v_CA16_4848",
                      "v_CA16_4862", "v_CA16_4863", "v_CA16_4864", "v_CA16_4865", "v_CA16_4866", "v_CA16_4867", "v_CA16_4868", "v_CA16_4869",
                      "v_CA16_4836", "v_CA16_4837", "v_CA16_4838", 
                      "v_CA16_4886", "v_CA16_4888",
                      "v_CA16_4892", "v_CA16_4899",
                      "v_CA16_4900", "v_CA16_4901",
                      "v_CA16_4893", "v_CA16_4894",
                      "v_CA16_4895", "v_CA16_4896",
                      "v_CA16_6692", "v_CA16_6698",
                      "v_CA16_6719", "v_CA16_6725")
variables_income = c("v_CA16_2207", "v_CA16_2540")

DA <-
  get_census(
    dataset = 'CA16',regions=list(CD="3513"), level = 'DA', 
    vectors =  c(variables_pop, variables_housing, variables_income),
    geo_format = "sf") %>%
  st_transform(32618) 


names(DA) <- c("area","type","dwellings1","households1", "GEOUID", "population1",
               "CD_UID", "CSD_UID","region","area2", "pop","pop11","pop_change",
               "dwellings","dwellings_usual_residents","pop_dens",
               "total_bedrooms", "bedrooms0","bedrooms1","bedrooms2","bedrooms3","bedrooms4",
               "total_tenure", "owner", "renter", 
               "constructed_total", "constructed_before1961","constructed_196180","constructed_198190","constructed_199100", "constructed_200105", "constructed_200610", "constructed_201116",
               "total_rentpressure","rentpressure_both","rentpressure_owner","rentpressure_renter",
               "medianrent",
               "medianownershipcosts", "avgownershipcosts",
               "mediandwellingvalue","avgdwellingvalue",
               "avgrent",
               "total_mobility1","movers1year", "total_mobility5", "movers5year",
               "medinc", "lowinc","geometry")
DA <- 
  DA %>% 
  mutate(dwellings_usual_residents = dwellings_usual_residents/dwellings,
         owner = owner/total_tenure,
              renter = renter/total_tenure,
              rentpressure_both = rentpressure_both/total_rentpressure,
              movers1year = movers1year/total_mobility1,
              movers5year = movers5year/total_mobility5) %>% 
  select(-type,-dwellings1,-households1,-population1,-region,-area2,-total_tenure,-total_rentpressure,-total_mobility1,-total_mobility5)


DA11 <-
  get_census(
    dataset = 'CA11',regions=list(CD="3513"), level = 'DA', 
    vectors =  c("v_CA11N_2252", #tenure_total,
                 "v_CA11N_2253", #owners,
                 "v_CA11N_2254", #renters,
                 "v_CA11F_3", #usual resident
                 "v_CA11N_2283", #ownerpressure,
                 "v_CA11N_2290", #rentpressure,
                 "v_CA11N_2291", #medianrent, 
                 "v_CA11N_2292", #avgrent,
                 "v_CA11N_2284", #medianowncosts,
                 "v_CA11N_2285", #avghomecosts,
                 "v_CA11N_2286", #mediandwellingvalue,
                 "v_CA11N_2287", #avgdwellingvalue
                 "v_CA11N_2606", #lowinc
                 "v_CA11N_1717", #mobilitytotal,
                 "v_CA11N_1723", #movers1year
                 "v_CA11N_1744", #mobilitytotal5,
                 "v_CA11N_1750"), #movers5year
    geo_format = "sf") %>%
  st_transform(32618) 

names(DA11) <- c("area", "quality_flags", "type", "dwellings", "households", "GEOUID", "NHS_nonreturn", "population", "CDUID", "CSD_UID", "Region","area2","NHS_nonreturn2","total_tenure","owner","renter","rentpressure_owner","rentpressure_renter","medianrent","avgrent","medianownership","avgownership","mediandwellingvalue","avgdwellingvalue","lowinc","total_mobility1","movers1year","total_mobility5","movers5year", "usual_residents", "geometry")

DA11 <-
  DA11 %>%   
  mutate(owner = owner/total_tenure,
              renter = renter/total_tenure,
              movers1year = movers1year/total_mobility1,
              movers5year = movers5year/total_mobility5,
              usual_residents = usual_residents/dwellings) %>% 
  select(-quality_flags,-type,-NHS_nonreturn,-CDUID,-CSD_UID,-Region,-area2,-NHS_nonreturn2, -total_tenure,-total_mobility1,-total_mobility5)

DA06 <-
  get_census(
    dataset = 'CA06',regions=list(CD="3513"), level = 'DA', 
    vectors =  c("v_CA06_101", #tenure_total,
                 "v_CA06_102", #owners,
                 "v_CA06_103", #renters,
                 "v_CA06_2053", #ownerpressuretotal,
                 "v_CA06_2056", #ownerpressure,
                 "v_CA06_2049", #rentpressuretotal,
                 "v_CA06_2051", #rentpressure,
                 "v_CA06_2050", #avggrossrent,
                 "v_CA06_2055", #avghomecosts,
                 "v_CA06_2054", #avgdwellingvalue
                 "v_CA06_1981", #lowincaftertax
                 "v_CA06_451", #mobilitytotal,
                 "v_CA06_453", #movers1year
                 "v_CA06_460", #mobilitytotal5,
                 "v_CA06_462"), #movers5year
    geo_format = "sf") %>%
  st_transform(32618) 

names(DA06) <- c("area","quality_flags","type","dwellings","households","GeoUID","population","CD_UID","CSD_UID","region","area2","total_tenure","owner","renter","rentalpressure_owner_total","rentalpressure_owner","rentalpressure_renter","avghomecosts","avgdwellingvalue","rentalpressure_renter_total","avgrent","lowinc","total_mobility1","movers1year","total_mobility5","movers5year","geometry")

DA06 <-
  DA06 %>%   
  mutate(owner = owner/total_tenure,
         renter = renter/total_tenure,
         rentalpressure_owner =  rentalpressure_owner/rentalpressure_owner_total,
         rentalpressure_renter = rentalpressure_renter/rentalpressure_renter_total,
         movers1year = movers1year/total_mobility1,
         movers5year = movers5year/total_mobility5) %>% 
  select(-quality_flags,-type,-CD_UID,-CSD_UID,-region,-area2,-total_tenure,-rentalpressure_owner_total,-rentalpressure_renter_total,-total_mobility1,-total_mobility5)

DA01 <-
  get_census(
    dataset = 'CA01',regions=list(CD="3513"), level = 'DA', 
    vectors =  c("v_CA01_96", #tenure_total,
                 "v_CA01_99", #owners,
                 "v_CA01_100", #renters,
                 "v_CA01_1670",#ownerpressuretotal,
                 "v_CA01_1672", #ownerpressure,
                 "v_CA01_1666", #rentpressuretotal
                 "v_CA01_1668", #rentpressure,
                 "v_CA01_1667", #avggrossrent,
                 "v_CA01_1671", #avghomecosts,
                 "v_CA01_1674", #avgdwellingvalue
                 "v_CA01_1620", #lowinc
                 "v_CA01_381", #mobilitytotal,
                 "v_CA01_383", #movers1year
                 "v_CA01_390", #mobilitytotal5,
                 "v_CA01_392"), #movers5year
    geo_format = "sf") %>%
  st_transform(32618) 

names(DA01) <- c("area","type","dwellings","households","GeoUID","key","population","CD_UID","CSD_UID","region","area2","total_tenure","owner", 
                 "renter","rentalpressure_owner_total","rentalpressure_owner","rentalpressure_renter_total", "rentalpressure_renter","avgrent", 
                 "avghomecosts","avgdwellingvalue","lowinc","total_mobility1","movers1year","total_mobility5","movers5year","geometry")

DA01 <-
  DA01 %>%   
  mutate(owner = owner/total_tenure,
         renter = renter/total_tenure,
         rentalpressure_owner =  rentalpressure_owner/rentalpressure_owner_total,
         rentalpressure_renter = rentalpressure_renter/rentalpressure_renter_total,
         movers1year = movers1year/total_mobility1,
         movers5year = movers5year/total_mobility5) %>% 
  select(-type,-CD_UID,-CSD_UID,-key,-region,-area2,-total_tenure,-rentalpressure_owner_total,-rentalpressure_renter_total,-total_mobility1,-total_mobility5)


property <-
  read_csv("data/PEC_property.csv", col_types = cols_only(
    `Property_ID` = col_character(),
    `Listing_Title` = col_character(),
    `Property_Type` = col_character(),
    `Listing_Type` = col_character(),
    `Created` = col_date(format = ""),
    `Scraped` = col_date(format = ""),
    Latitude = col_double(),
    Longitude = col_double(),
    `City` = col_skip(),
    `Airbnb_PID` = col_double(),
    `Airbnb_HID` = col_double(),
    `HomeAway_PID` = col_character(),
    `HomeAway_HID` = col_character())) %>% 
  set_names(c("Property_ID", "Listing_Title", "Property_Type", "Listing_Type",
              "Created", "Scraped", "Latitude", "Longitude", "Airbnb_PID",
              "Airbnb_HID", "HomeAway_PID", "HomeAway_HID")) %>% 
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


daily <- 
  read_csv("data/PEC_daily.csv", col_types = cols(
    `Property_ID` = col_character(),
    Date = col_date(format = ""),
    Status = col_factor(levels = c("U", "B", "A", "R")),
    # `Booked Date` = col_skip(),
    `Price` = col_double(),
    #`Price (Native)` = col_skip(),
    #`Currency Native` = col_skip(),
    #`Reservation ID` = col_skip(),
    `Airbnb_PID` = col_double(),
    `HomeAway_PID` = col_character())) %>% 
  set_names(c("Property_ID", "Date", "Status", "Price", "Airbnb_PID", 
              "HomeAway_PID")) %>% 
  filter(!is.na(Status)) %>%
  arrange(Property_ID, Date)


## Trim listings to city geometry and inputted dates
property <-
  property %>% 
  filter(Property_ID %in% daily$Property_ID,
         Scraped >= Start_date,
         Created <= End_date) %>% 
    st_join(st_buffer(city["geometry"], 200),
          join = st_within, left = FALSE)

daily <- 
  daily %>% 
  filter(Property_ID %in% property$Property_ID,
         Date >= Start_date,
         Date <= End_date)


## Join property and daily file

daily <- inner_join(daily, st_drop_geometry(property)) %>% 
  select(Property_ID, Date, Status, Price, Airbnb_PID, HomeAway_PID, Airbnb_HID,
         HomeAway_HID, Listing_Type)


## Find FREH listings and revenue

property <- 
  daily %>%
  group_by(Property_ID) %>% 
  summarize(
    n_reserved = sum(Status == "R"),
    n_available = sum(Status == "A" | Status == "R"),
    revenue = sum((Status == "R") * Price),
    FREH = if_else(
      first(Listing_Type) == "Entire home/apt" & n_reserved >= 90 &
        n_available >= 183, TRUE, FALSE)) %>% 
  inner_join(property, .)


## Find multi-listings

daily <- strr_multilistings(daily, listing_type = Listing_Type,
                            host_ID = Airbnb_HID, date = Date)

property <- 
  daily %>%
  group_by(Property_ID) %>% 
  summarize(ML = as.logical(ceiling(mean(ML)))) %>% 
  inner_join(property, .)

# Identify ghost hotels
GH_list <-
  strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, Start_date,
             End_date, listing_type = Listing_Type) %>% 
  pull(property_IDs) %>%
  unlist() %>%
  unique()

property <-
  property %>% 
  mutate(GH = if_else(Property_ID %in% GH_list, TRUE, FALSE))

rm(GH_list)

