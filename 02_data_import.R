############# DATA IMPORT ######################


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

End_date <- as.Date("2019-04-30")
Start_date <- as.Date("2018-04-30")

citycode = "24735"
cityname = "Prince Edward County"

city <- 
  get_census(
    dataset = "CA16", 
    regions = list(CD = "24735"),  
    level = "CD",
    geo_format = "sf") %>% 
  st_transform(32618)

DA <-
  get_census(
    dataset = 'CA16',regions=list(CD="3513"), level = 'DA', 
    geo_format = "sf") %>%
  st_transform(32618) 


property1 <-
  property %>% 
  filter(Scraped >= Start_date,
         Created <= End_date)
  st_join(st_buffer(city["geometry"], 200),
          join = st_within, left = FALSE)

mapview(property1)
viewmap


