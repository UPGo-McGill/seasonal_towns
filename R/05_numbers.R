#### RUN NUMBERS #######
#### AND RAFFLE #######

## Number of Active Listings last year
listings2019 <- daily %>% 
        filter(Date <= End_date & Date >= year_prior) %>% 
        group_by(Date) %>% 
        summarize(Listings = n()) %>%
        summarise(mean_Listings = mean(Listings))%>%
        as.numeric()

## Number of Active Listings year prior
daily %>% 
        filter(Date <= year_prior & Date >= year_prior_prior) %>% 
        group_by(Date) %>% 
        summarize(Listings = n()) %>%
        summarise(mean_Listings = mean(Listings))


## Revenue last year
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

## Revenue per host

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

##

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



#### MAPPING CODE -stashed changes

names <- 
  read_csv("data/names.csv")%>%
  set_names(c("ID", "Geog_Title", "Term", "Category",
              "Code", "Latitude", "Longitude", "Location", "Province", "Relevance")) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32618)

intersection <- st_intersection(DA, property)
intersection <- intersection %>% 
  group_by(GEOUID) %>% 
  count()
intersection
DA <- st_join(DA, intersection)
DA <- DA%>%
  mutate(listperdwell = n/dwellings)



