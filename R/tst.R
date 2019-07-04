### PICTON AND WELLINGTON PROPERTIES


## PICTON BOUNDARIES
#cities <- read_sf(dsn = ".", layer = "lpc_000b16a_e")
#picton <- cities%>%
#  filter(str_detect(PCNAME, "Picton"))%>%
# st_set_crs(102009) %>% 
#  st_transform(32618)

picton <- DA%>%
  filter(GEOUID == 35130061|GEOUID ==35130066| GEOUID == 35130064|GEOUID == 5130065|GEOUID == 35130063)
picton_buff <- st_buffer(picton, 200)

## WELLINGTON BOUNDARIES
wellington <- DA%>%
  filter(GEOUID == 35130055|GEOUID == 35130056)
wellington_buff <- st_buffer(wellington, 200)

picton_prop <- st_intersection(property, picton_buff)
wellington_prop <- st_intersection(property, wellington_buff)

plot(city$geometry)
plot(picton_prop, add=TRUE)
plot(wellington_prop, add=TRUE)


##### Sampling properties for Host Locations #######

prop_sample <- property[c(sample(1:774,100)),]
prop_sample <- prop_sample%>%
  group_by(Airbnb_HID)
View(prop_sample)


##### Revenue per DA ####
## import property without assigning geometry

prope <-
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
  #st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #st_transform(32618) %>% 
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

##
tst <- inner_join(daily, prope)
tst <- tst%>%
  filter(Date <= End_date & Date>=year_prior & Status == "R")
## 
tst <- tst %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32618) 
##
tst <- tst%>%
  group_by(Airbnb_PID)%>%
  summarize(rev = sum(Price, na.rm = TRUE) * exchange_rate) 

## check sum is accurate : sum(tst$rev)


tst <- st_intersection(DA, tst)

tst <- tst %>% 
  group_by(GEOUID) %>%
  summarize(revenue = sum(rev)) 


tst <- st_join(DA_raffle,tst)


tst1 <- tst%>%
  mutate(revperl = revenue/raffle_n)

base_map +
  tm_shape(tst1) +
  tm_polygons(col = "revperl", 
              palette = "Purples",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "Average revenue per listing",
              legend.format=list(fun=function(lperd_raffle) paste0(formatC(lperd_raffle, digits=0, format="f"), " %")))+
  tm_legend(position = c("right", "top"),
            bg.color = "white",
            bg.alpha=.2,
            width = .25, title.size = 1)+
  tm_shape(DA)+
  tm_borders(col="grey")
  # breaks = c(0,.15,.3,.45,.6),) 
 # tm_layout(title = "Figure X. Revenue per number of listings")





