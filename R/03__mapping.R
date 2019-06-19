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

#print(m_picton, vp = inset_picton)

### MAPPING LISTINGS
#Map of Listing Type (Entire Home, Private Room or Shared Room) and Revenue

figure1 <- 
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


### MAPPING CENSUS VARIABLES

#POPULATION DENSITY  
base_map +
  tm_shape(DA) +
  tm_polygons(col = "pop_dens", 
              palette = "Purples",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,20,50,100,250,500,1000,2000)) +
  tm_layout(title = "Figure X. Population Density") 


#RENTERS   
base_map +
  tm_shape(DA) +
  tm_polygons(col = "renter", 
              palette = "Oranges",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,0.1,0.2,0.4,0.55,0.7)) +
  tm_layout(title = "Figure X. Renters")

m_picton <- 
  tm_shape(st_buffer(filter(DA, GEOUID == 35130064|GEOUID == 5130065|GEOUID == 35130063),100)) + tm_borders(col = "#f0f0f0") +
  tm_layout(legend.show = F, main.title = 'Picton', main.title.size = 0.9) +
  tm_shape(DA)+ tm_polygons(col = "renter", 
                            palette = "Oranges",
                            border.col = "#f0f0f0",
                            border.alpha = .2,
                            breaks = c(0,0.1,0.2,0.4,0.55,0.7))

print(m_picton, vp = inset_picton)

#HOUSING COSTS
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

tmap_arrange(panel1,panel2)
rm(panel1, panel2)


#RENT PRESSURE - BY TENURE
panel3 <-   
base_map +
  tm_shape(filter(DA, rentpressure_renter > 0)) +
  tm_polygons(col = "rentpressure_renter", 
              palette = "Blues",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,20,40,60,80)) +
  tm_layout(title = "Figure X. Rental Pressure - 30%+ on rent, 2016")

panel4 <-   
base_map +
  tm_shape(filter(DA, rentpressure_owner > 0)) +
  tm_polygons(col = "rentpressure_owner", 
              palette = "Blues",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,10,20,30,40)) +
  tm_layout(title = "Figure X. Home Ownership Pressure - 30%+ on housing")

tmap_arrange(panel3,panel4)
rm(panel3,panel4)


#RENT PRESSURE - BOTH
base_map +
  tm_shape(DA) +
  tm_polygons(col = "rentpressure_both", 
              palette = "Blues",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "",
              breaks = c(0,.15,.3,.45,.6)) +
  tm_layout(title = "Figure X. 30%+ on housing, tenants and owners")

tm_shape(st_buffer(filter(DA, GEOUID == 35130064|GEOUID == 5130065|GEOUID == 35130063),2000)) + tm_borders(col = "#f0f0f0") +
  tm_layout(title = "Figure X. Rental Pressure in Picton", legend.bg.color = "white", legend.bg.alpha = 0.5)+
  tm_scale_bar(position = c("right", "bottom"), color.dark = "grey50") +
  tm_shape(DA)+ tm_polygons(col = "rentpressure_renter", 
                            palette = "Blues",
                            border.col = "#f0f0f0",
                            border.alpha = .2,
                            title = "",
                            breaks = c(0,20,40,60,80))
  


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


#USUAL RESIDENTS
base_map +
  tm_shape(DA) +
  tm_polygons(col = "dwellings_usual_residents", 
              palette = "-Oranges",
              border.col = "#f0f0f0",
              border.alpha = .2,
              title = "") +
  tm_layout(title = "Figure X. Dwellings Occupied by Usual Residents")




## dwelling value over time
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

