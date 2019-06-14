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

water <-read_sf("data","lhy_000c16a_e") 
water <- st_union(st_combine(water))
coastal_water <- read_sf("data", "lhy_000h16a_e")
coastal_water <- st_union(st_combine(coastal_water))



  
  
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


inset_map <- DA %>% filter (GEOUID == 35130064 | GEOUID == 5130065| GEOUID == 35130063)

m_picton <- tm_shape(inset_map) + 
  tm_polygons(col = "green")

inset_picton <- viewport(x = 0.8, y = .4, width = 0.2, height = 0.2)

base_map
print(m_picton, vp = inset_picton)



#create an inset


### MAPPING LISTINGS
#Map of Listing Type (Entire Home, Private Room or Shared Room) and Revenue

#figure1 <- 
tm_shape(property, ext = 1.2)+
  tm_dots(scale = 0)+
  #  tm_shape(streets)+
  #  tm_lines(col="grey", alpha = 0.5)+
  tm_shape(city) +
  tm_borders(lwd = 1) + 
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

#figure2 <- 
  base_map +
  tm_shape(DA) +
  tm_polygons(col = "rentpressure_both", 
              palette = "Greens",
              border.col = "#f0f0f0",
              title = "") +
  tm_layout(title = "Figure 1. Housing Pressure - Households Spending More than 30% on Housing, Renters and Homeowners")
  
  
  

  
