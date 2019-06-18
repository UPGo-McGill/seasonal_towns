#### RUN NUMBERS #######
#### AND RAFFLE #######

## Number of Active Listings last year
round(daily %>% 
        filter(Date <= End_date & Date >= year_prior) %>% 
        group_by(Date) %>% 
        summarize(Listings = n()) %>%
        summarise(mean_Listings = mean(Listings)))

## Number of Active Listings year prior
round(daily %>% 
        filter(Date <= year_prior & Date >= year_prior_prior) %>% 
        group_by(Date) %>% 
        summarize(Listings = n()) %>%
        summarise(mean_Listings = mean(Listings)))


## List by Host ID
property%>%
  count(Airbnb_HID)%>%

## Revenue by Host ID
host_revenue <-
  daily %>%
  filter(Date >= year_prior, Date <= End_date, Status == "R") %>%
  group_by(Airbnb_HID)%>%
  summarize(rev = sum(Price)) %>%
  filter(rev > 0)


## RAFFLE

prop_raffle <- strr_raffle(property, DA, GEOUID, dwellings)

counts <- prop_raffle%>%
  count(winner)

counts <- st_join(DA, counts)
counts <- counts %>%
  mutate(listingsperdwell = n/dwellings)

## n = number of listing per DA // listperdwell = num of listings per dwellings in DAs


