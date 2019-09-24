#### Maps ######################################################################

library(tidyverse)
library(sf)
library(strr)
library(upgo)
library(cancensus)
library(ggspatial)

load("data/CMA.Rdata")
load("data/CSD.Rdata")
load("data/FREH.Rdata")
load("data/property.Rdata")

### Mapping prep ###############################################################

active_CSD <- 
  CSD %>% 
  filter(population >= 1500, active_listings >= 5) %>% 
  st_intersection(st_buffer(st_centroid(CMA), 250000))


### Active listings map ########################################################

active_listings_map <- 
  map(c("Toronto", "Montréal", "Vancouver", "Calgary", "Edmonton",
        "Ottawa - Gatineau"), ~{
    
    CMA_chosen <- CMA %>% filter(name == .x)
    
    active_CSD %>% 
      filter(GeoUID.1 == {CMA %>% filter(name == .x) %>% pull(GeoUID)}) %>% 
      ggplot() +
      geom_sf(data = CSD,
              colour = alpha("grey50", 0.3),
              lwd = 0.5,
              fill = "transparent") +
      geom_sf(aes(fill = active_listings_per_cap),
              colour = "transparent",
              lwd = 0) +
      geom_sf(data = CMA,
              fill = "transparent",
              lwd = 0.75) +
      geom_sf(data = st_buffer(st_centroid(CMA_chosen), 100000),
              fill = "transparent",
              linetype = "dotted") +
      geom_sf(data = st_buffer(st_centroid(CMA_chosen), 250000),
              fill = "transparent",
              linetype = "dotted") +
      scale_fill_distiller(name = "Listings per cap.",
                           palette = "YlGnBu", 
                           limits = c(0, 0.08), 
                           oob = scales::squish,
                           direction = 1) +
      ggtitle(.x) +
      theme_void() +
      theme(panel.border = element_rect(colour = "black", fill = "transparent",
                                        size = 1),
            legend.position = "none",
            plot.title = element_text(margin = margin(t = 10, b = -20),
                                      face = "bold",
                                      hjust = 0.02)) +
      {if (.x == "Ottawa - Gatineau") theme(
        panel.border = element_rect(colour = "black", fill = "transparent",
                                    size = 1),
        legend.position = c(0.98, 0.02),
        legend.justification = c(1, 0),
        plot.title = element_text(margin = margin(t = 10, b = -20),
                                  face = "bold", hjust = 0.02))} +
      gg_bbox(st_buffer(st_centroid(CMA_chosen), 250000))
    
  }) %>% 
  cowplot::plot_grid(plotlist = ., nrow = 2)
  

ggsave("output/active_listings.png", plot = active_listings_map, width = 18, 
       height = 12)



map_function("Toronto", 
             1000000 * active_listings / units::drop_units(st_area(geometry)), 
             15, 
             "Listings per km^2")

