#### DEFINE YEARS #######

# Set up timeframes
year_prior <- as.POSIXlt(End_date)
year_prior$year <- year_prior$year - 1
year_prior_prior <- as.POSIXlt(year_prior)
year_prior_prior$year <- year_prior$year - 1

exchange_rate <- 1.34

###### RUN RAFFLE ########
run_raffle <- strr_raffle(property, DA, GEOUID, dwellings)
intersection <- st_intersection(DA, run_raffle)

## Number of listings in DA with raffle
DA_raffle <- intersection %>% 
  count(winner)%>%
  st_drop_geometry()
DA_raffle <- DA_raffle%>%
  rename(GEOUID = winner, raffle_n = n)

## number of listings in DA without raffle
intersection <- intersection %>% 
  group_by(GEOUID) %>% 
  count()%>%
  st_drop_geometry()%>%
  rename(no_raffle_n = n)

DA_raffle <- inner_join(intersection, DA_raffle)
DA_raffle <- inner_join(DA, DA_raffle)
DA_raffle <- DA_raffle%>%
  mutate (lperd_no_raffle = no_raffle_n/dwellings,
          lperd_raffle = raffle_n/dwellings)

#### RUN NUMBERS #######

## Number of Active Listings last year
#listings2019 <- 
  daily %>% 
  filter(Date <= End_date & Date >= year_prior) %>% 
  group_by(Airbnb_HID)
  group_by(Airbnb_HID)%>%
  summarize(Listings = n()) %>%
  summarise(mean_Listings = mean(Listings))%>%
  as.numeric()
daily%>%
  

## Number of Active Listings year prior
daily %>% 
  filter(Date <=year_prior & Date >= year_prior_prior) %>% 
  group_by(Date) %>% 
  summarize(Listings = n()) %>%
  summarise(mean_Listings = mean(Listings))

## Total revenue
## Revenue over past 12 months
rev2019 <- daily %>% 
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
  daily %>%
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
  mutate(revenue = value * rev2019)

## List by Host ID
property%>%
  count(Airbnb_HID)

## Revenue by Host ID
host_revenue <-
  daily %>%
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
st_drop_geometry(strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, year_prior,
                            End_date, listing_type = Listing_Type) %>% 
                   filter(date == End_date) %>% 
                   group_by(ghost_ID) %>% 
                   summarize(n = sum(housing_units)) %>% 
                   ungroup() %>% 
                   summarize(GH_housing_loss = sum(n))) +
  nrow(daily %>% 
         filter(Date == End_date) %>% 
         inner_join(property, .) %>% 
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


## Number of nights reserved per host in last year
daily %>%
  filter(Date >= year_prior, Date <= End_date, Status == "R") %>%
  group_by(Airbnb_HID)%>%
  summarize(Listings = n())%>%
  summarise(mean_Listings = mean(Listings))

property%>%
  count(Airbnb_HID)%>%
  View()
