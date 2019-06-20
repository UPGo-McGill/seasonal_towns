##################### MAPPING ##########################################

source("R/01_helper_functions.R")

## IMPORT STREETS OSM
streets <- 
  getbb("County of Prince Edward") %>% 
  opq() %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

streets <- 
  rbind(streets$osm_polygons %>% st_cast("LINESTRING"),streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32618) %>%
  select(osm_id, name, geometry)

# IMPORT WATER

water <-read_sf("data","lhy_000c16a_e") 
water <- st_union(st_combine(water))
coastal_water <- read_sf("data", "lhy_000h16a_e")
coastal_water <- st_union(st_combine(coastal_water))

# ADD TOWN NAMES
names <- 
  read_csv("data/names.csv")%>%
  set_names(c("ID", "Geog_Title", "Term", "Category",
              "Code", "Latitude", "Longitude", "Location", "Province", "Relevance")) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(32618)

## CREATE BASEMAP 

figure <- list()

base_map <- tm_shape(DA, bbox = bb(st_bbox(DA), xlim=c(-0.02, 1.02),
                                ylim=c(0.01, 1.05), relative = TRUE),
                                unit = "km") +
  tm_fill(col = "#f0f0f0") +
#  tm_shape(streets)+
#  tm_lines(col = "grey60") +
#  tm_shape(coastal_water)+
#  tm_fill(col = "black") + 
#  tm_shape(water)+
#  tm_fill(col = "black") +
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_layout(frame = TRUE, main.title.size = 1.5, legend.title.size = 1.2,
            legend.title.fontfamily = "Futura-CondensedExtraBold",
            legend.position = c("right", "top"),
            fontfamily = "Futura-Medium",
            title.fontfamily = "Futura-CondensedExtraBold")


## CREATE INSET

inset_picton <- viewport(x = 0.87, y = .42, width = 0.3, height = 0.3)

#m_picton <- 
  tm_shape(st_buffer(filter(DA, GEOUID == 35130064|GEOUID == 5130065|GEOUID == 35130063),100)) + tm_borders(col = "#f0f0f0") +
  tm_layout(legend.show = F) +
  tm_shape(DA)+ tm_polygons(col = "lowinc",  palette = "Greens")
  print(m_picton, vp = inset_picton)

######################  FIGURE CREATION ###########################33
#FIGURE 1. POPULATION DENSITY

figure[[1]] <-  
 base_map +
  tm_shape(DA) +
  tm_polygons(col = "pop_dens", 
              palette = "Purples",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,20,50,100,250,500,1000,2000)) +
  tm_layout(title = "Figure 1. Population Density") 

tmap_save(figure[[1]], "output/figure_1.png", width = 2400, height = 1500)

#FIGURE 2. AIRBNBS BY LISTING TYPE AND REVENUE
figure[[2]] <- 
tm_shape(property, ext = 1.2)+
  tm_dots(scale = 0)+
  #  tm_shape(streets)+
  #  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(city) +
  tm_borders(lwd = 1) + 
  tm_shape(DA)+
  tm_borders(col="grey")+
  tm_shape(property)+
  tm_dots(col = "Listing_Type",
          scale = 4/3, 
          palette = get_brewer_pal("-Dark2", n = 3), 
          alpha = 0.6, 
          size = "revenue", 
          title.size = "Revenue", 
          size.lim = c(0, 100000),
          legend.show = FALSE,
          legend.size.show = TRUE) +
  tm_layout(legend.position = c("left", "bottom"),
            frame = FALSE, legend.bg.alpha = 0.6, legend.bg.color = "white") +
  tm_add_legend(type="symbol",
                col= get_brewer_pal("-Dark2", n = 3),
                labels=c("Entire Home", "Private Room", "Shared Room"),
                border.lwd = NA,
                alpha = 1,
                title="Listing Type")+
  tm_compass()

tmap_save(figure[[2]], "output/figure_2.png", width = 2400, height = 1500)

#FIGURE 3. LISTINGS PER MONTH
figure[[3]]<-
  ggplot(daily %>% 
         group_by(Date) %>% 
         summarize(Listings = n())) +
  geom_line(aes(Date, Listings)) +
  theme_minimal()

ggsave("output/figure3.jpg")

#FIGURE 4. LISTINGS PER DA
#FIGURE 5. LISTINGS PER DWELLING
figure[[5]] <-
  
tmap_save(figure[[5]], "output/figure_5.png", width = 2400, height = 1500)
#FIGURE 6. REVENUE OVER TIME
exchange_rate = 1.34
figure[[6]] <- ggplot(daily %>% 
                    filter(Date<="2019-04-30" & Date>="2016-07-01"& Status == "R") %>%
                    group_by(Date)%>%
                    summarise(rev = sum(Price, na.rm = TRUE) * exchange_rate))+
  geom_line(aes(Date, rev)) +
  theme_minimal()

ggsave("output/figure6.jpg")

#FIGURE 7. TOP EARNERS AND SHARE OF REVENUE
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

figure[[7]] <- 
  ggplot(host_revenue)+
  geom_bar(mapping = aes(x = percentile, y = value, fill = percentile), stat = "identity")+
  theme_minimal()+
  scale_fill_manual("Percentile", values = alpha(c("lightblue", "blue", "darkblue"), .6))+
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank())

ggsave("output/figure7.jpg")

#FIGURE 8. DWELLINGS OCCUPIED BY USUAL RESIDENT
figure[[8]]<-
base_map +
  tm_shape(DA) +
  tm_polygons(col = "dwellings_usual_residents", 
              palette = "-Oranges",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "") +
  tm_layout(title = "Figure X. Dwellings Occupied by Usual Residents")

tmap_save(figure[[8]], "output/figure_8.png", width = 2400, height = 1500)

#FIGURE 9. TOP 5 OWNERS AIRBNBS


#FIGURE 9. PROPERTY VALUES
figure[[9]]<-
  base_map +
  tm_shape(DA) +
  tm_polygons(col = "avgdwellingvalue", 
              palette = "Greens",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "") +
  tm_layout(title = "Figure X. Average Dwelling Value 2016")

tmap_save(figure[[9]], "output/figure_9.png", width = 2400, height = 1500)

#FIGURE 10. RENTERS
figure[[10]] <-
base_map +
  tm_shape(DA) +
  tm_polygons(col = "renter", 
              palette = "Oranges",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,0.1,0.2,0.4,0.55,0.7)) +
  tm_layout(title = "Figure 10. Renters")

m_picton <- 
  tm_shape(st_buffer(filter(DA, GEOUID == 35130064|GEOUID == 5130065|GEOUID == 35130063),100)) + tm_borders(col = "#f0f0f0") +
  tm_layout(legend.show = F, main.title = 'Picton', main.title.size = 0.9) +
  tm_shape(DA)+ tm_polygons(col = "renter", 
                            palette = "Oranges",
                            border.col = "#f0f0f0",
                            border.alpha = .2,
                            breaks = c(0,0.1,0.2,0.4,0.55,0.7))
print(m_picton, vp = inset_picton)

tmap_save(figure[[10]], "output/figure_10.png", width = 2400, height = 1500)


#FIGURE 11. MONTHLY HOUSING COSTS, RENTERS VS OWNERS
panel1 <-   
  base_map +
  tm_shape(filter(DA, avgrent > 0)) +
  tm_polygons(col = "avgrent", 
              palette = "Greens",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(500,700,900,1100,1300,1500)) +
  tm_layout(title = "Average Monthly Rent")

panel2 <- 
  base_map +
  tm_shape(DA) +
  tm_polygons(col = "avgownershipcosts", 
              palette = "Greens",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(500,700,900,1100,1300,1500)) +
  tm_layout(title = "Average Monthly Housing Costs, Homeowners")

figure[[11]] <- tmap_arrange(panel1,panel2)
rm(panel1, panel2)

tmap_save(figure[[11]], "output/figure_11.png", width = 2400, height = 3500)

#FIGURE 12. HOUSING STRESS
figure[[12]]<- base_map +
  tm_shape(DA) +
  tm_polygons(col = "rentpressure_both", 
              palette = "Blues",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,.15,.3,.45,.6)) +
  tm_layout(title = "Figure X. Households spending 30%+ of income on housing, tenants and owners")

tmap_save(figure[[12]], "output/figure_12.png", width = 2400, height = 1500)

#FIGURE 13. HOUSING STRESS, RENTERS
figure[[13]]<-
base_map +
  tm_shape(filter(DA, rentpressure_renter > 0)) +
  tm_polygons(col = "rentpressure_renter", 
              palette = "Blues",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,20,40,60,80)) +
  tm_layout(title = "Figure X. Households spending more than 30% of on rent")

tmap_save(figure[[13]], "output/figure_13.png", width = 2400, height = 1500)

tm_shape(st_buffer(filter(DA, GEOUID == 35130064|GEOUID == 5130065|GEOUID == 35130063),2000)) + tm_borders(col = "#f0f0f0") +
  tm_layout(title = "Figure X. Rental Pressure in Picton", legend.bg.color = "white", legend.bg.alpha = 0.5)+
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_shape(DA)+ tm_polygons(col = "rentpressure_renter", 
                            palette = "Blues",
                            border.col = "#f0f0f0",
                            border.alpha = .2,
                            title = "",
                            breaks = c(0,20,40,60,80))
  



####OTHER EXPLORATORY MAPS 

#MOBILITY
panel5 <- 
base_map +
  tm_shape(DA) +
  tm_polygons(col = "movers1year", 
              palette = "Purples",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,0.05,0.1,0.2,0.25)) +
  tm_layout(title = "Figure X. Moved within past 12 Months")

panel6 <-  
base_map +
  tm_shape(DA) +
  tm_polygons(col = "movers5year", 
              palette = "Purples",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,0.15,0.3,0.45,0.8)) +
  tm_layout(title = "Figure X. Moved within past 5 years")

tmap_arrange(panel5,panel6)
rm(panel5,panel6)

#INCOME 
base_map +
  tm_shape(DA) +
  tm_polygons(col = "medinc", 
              palette = "Greens",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "") +
  tm_layout(title = "Figure X. Median Income")

#LOW INCOME
base_map +
  tm_shape(DA) +
  tm_polygons(col = "lowinc", 
              palette = "Greens",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,5,10,20,30,40)) +
  tm_layout(title = "Figure X. Low Inc")

## DWELLING VALUE OVER TIME
base_map +
  tm_shape(DA) +
  tm_polygons(col = "avgdwellingvalue", 
              palette = "Greens",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "") +
  tm_layout(title = "Figure X. Average Dwelling Value 2016")
base_map +
  tm_shape(DA11) +
  tm_polygons(col = "avgdwellingvalue", 
              palette = "Greens",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "") +
  tm_layout(title = "Figure X. Average Dwelling Value 2011")
base_map +
  tm_shape(DA06) +
  tm_polygons(col = "avgdwellingvalue", 
              palette = "Greens",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "") +
  tm_layout(title = "Figure X. Average Dwelling Value 2006")
base_map +
  tm_shape(DA01) +
  tm_polygons(col = "avgdwellingvalue", 
              palette = "Greens",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "") +
  tm_layout(title = "Figure X. Average Dwelling Value 2001")

### LISTING PER DA
# No raffle
base_map +
  tm_shape(DA_no_raffle) +
  tm_polygons(col = "n", 
              palette = "Reds",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "")+
  tm_layout(title = "Figure X. Number of Listings per Dissemination Area")

### Listings per DA - with - raffle
base_map +
  tm_shape(DA_raffle) +
  tm_polygons(col = "n", 
              palette = "Reds",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "")+
  tm_layout(title = "Figure X. Number of Listings per Dissemination Area")

## Listings per dwelling unit, by DA

base_map +
  tm_shape(DA_no_raffle) +
  tm_polygons(col = "no_raffle_lperd", 
              palette = "Reds",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "")+
  #breaks = c(0,.15,.3,.45,.6)) +
  tm_layout(title = "Figure X. Listings per number of dwellings")

## with raffle
base_map +
  tm_shape(DA_raffle) +
  tm_polygons(col = "raffle_lperd", 
              palette = "Reds",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "")+
  #breaks = c(0,.15,.3,.45,.6)) +
  tm_layout(title = "Figure X. Listings per number of dwellings")


base_map +
  tm_shape(filter(property, Airbnb_HID == 108999347 | Airbnb_HID == 49480845 |
                    Airbnb_HID == 160599890 | Airbnb_HID == 233942 | Airbnb_HID==62584153)) +
  tm_dots(col = "Airbnb_HID", 
               palette = rainbow(5),
               size = "revenue",
               alpha = 0.6,
          style ="fixed",
          breaks = c(0,250000,50000000, 62584190,109999347), 
              #border.col = "#f0f0f0",
              #border.alpha = .2,  
              title = "")+
  tm_layout(title = "Figure X. Top-5 highest earning hosts")



