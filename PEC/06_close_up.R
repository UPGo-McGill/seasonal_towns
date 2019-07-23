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


### FILTER DAILY
## PICTON
picton_daily <- 
  daily %>% 
  filter(Property_ID %in% picton_prop$Property_ID)

## WELLINGTON
wellington_daily <- 
  daily %>% 
  filter(Property_ID %in% wellington_prop$Property_ID)

#### RUN NUMBERS #######

## Number of Active Listings last year
picton_prop%>%
  filter(Scraped>="2019-01-01")


picton_daily %>% 
  filter(Date <= End_date & Date >= year_prior) %>% 
  group_by(Date) %>% 
  summarize(Listings = n()) %>%
  summarise(mean_Listings = min(Listings))%>%
  as.numeric()

## Number of Active Listings year prior
picton_daily %>% 
  filter(Date <=year_prior & Date >= year_prior_prior) %>% 
  group_by(Date) %>% 
  summarize(Listings = n()) %>%
  summarise(mean_Listings = mean(Listings))

## Number of Active Listings last year
daily %>% 
  filter(Date <= End_date & Date >= year_prior) %>% 
  group_by(Date) %>% 
  summarize(Listings = n()) %>%
  summarise(mean_Listings = mean(Listings))%>%
  as.numeric()

## Number of Active Listings year prior
daily %>% 
  filter(Date <=year_prior & Date >= year_prior_prior) %>% 
  group_by(Date) %>% 
  summarize(Listings = n()) %>%
  summarise(mean_Listings = mean(Listings))


## Total revenue
## Revenue over past 12 months
picton_daily %>% 
  filter(Date <= Date & 
           Date >= year_prior &
           Status == "R" ) %>%
  summarise(sum_revenue = sum(Price, na.rm = TRUE) * exchange_rate)%>%
  as.numeric()

## Revenue year prior
picton_daily %>% 
  filter(Date <= year_prior & 
           Date >= year_prior_prior &
           Status == "R" ) %>%
  summarise(sum_revenue = sum(Price, na.rm = TRUE) * exchange_rate)

## Total revenue
## Revenue over past 12 months
daily %>% 
  filter(Date <= Date & 
           Date >= year_prior &
           Status == "R" ) %>%
  summarise(sum_revenue = sum(Price, na.rm = TRUE) * exchange_rate)%>%
  as.numeric()

## Revenue year prior
daily %>% 
  filter(Date <= year_prior & 
           Date >= year_prior_prior &
           Status == "R" ) %>%
  summarise(sum_revenue = sum(Price, na.rm = TRUE) * exchange_rate)

## Revenue by host
host_revenue<-
  wellington_daily %>%
  filter(Date >= year_prior, Date <= End_date, Status == "R") %>%
  group_by(Airbnb_HID) %>%
  summarize(rev = sum(Price)) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev))) %>% 
  gather(`Top 1%`, `Top 5%`, `Top 10%`, key = "percentile", value = "value") %>% 
  mutate(percentile = factor(percentile, levels = c('Top 1%', 'Top 5%', 'Top 10%')))

host_revenue%>%
  mutate(revenue = value * 3029429)

## List by Host ID
property%>%
  count(Airbnb_HID)

## Revenue by Host ID
host_revenue <-
  picton_daily %>%
  filter(Date >= year_prior, Date <= End_date, Status == "R") %>%
  group_by(Airbnb_HID)%>%
  #  summarize(rev = sum(Price)) %>%
  summarise(rev = sum(Price, na.rm = TRUE) * exchange_rate)%>%
  filter(rev > 0)
sum(host_revenue$rev)

## Breakdown Listing Type
## Entire homes
nrow(daily %>% 
       filter(Date == End_date) %>% 
       group_by(Property_ID) %>% 
       filter(Listing_Type == "Entire home/apt"))*100/
  nrow(daily %>% 
         filter(Date == End_date))

## Private rooms
nrow(daily %>% 
       filter(Date == End_date) %>% 
       group_by(Property_ID) %>% 
       filter(Listing_Type == "Private room"))*100/
  nrow(daily %>% 
         filter(Date == End_date))

## Shared rooms
nrow(daily %>% 
       filter(Date == End_date) %>% 
       group_by(Property_ID) %>% 
       filter(Listing_Type == "Shared room"))*100/
  nrow(daily %>% 
         filter(Date == End_date))

## Housing market loss
## Last year
st_drop_geometry(strr_ghost(picton_prop, Property_ID, Airbnb_HID, Created, Scraped, year_prior,
                            End_date, listing_type = Listing_Type) %>% 
                   filter(date == End_date) %>% 
                   group_by(ghost_ID) %>% 
                   summarize(n = sum(housing_units)) %>% 
                   ungroup() %>% 
                   summarize(GH_housing_loss = sum(n))) +
  nrow(picton_daily %>% 
         filter(Date == End_date) %>% 
         inner_join(picton_prop, .) %>% 
         filter(FREH == TRUE))

## Previous year
st_drop_geometry(strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, year_prior-prior,
                            year_prior, listing_type = Listing_Type) %>% 
                   filter(date == year_prior) %>% 
                   group_by(ghost_ID) %>% 
                   summarize(n = sum(housing_units)) %>% 
                   ungroup() %>% 
                   summarize(GH_housing_loss = sum (n))) +
  nrow(daily %>% 
         filter(Date == "2017-07-01") %>% 
         inner_join(property, .) %>% 
         filter(FREH == TRUE))

## Reserved nights over last year
daily %>%
  filter(Date >= year_prior, Date <= End_date, Status == "R") %>%
  summarize(Listings = n())

