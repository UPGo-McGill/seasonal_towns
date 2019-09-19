############ DATA AND BOUNDARY IMPORT FOR ALL SEASONAL TOWNS ###############

source("R/01_helper_functions.R")
source("R/02_data_import.R")

###############

property_pec <- property%>%
  filter(str_detect( City, "Prince Edward County"))
property_blue_mountains <- property%>%
  filter(str_detect( City, "Blue Mountains"))
property_mont_tremblant <- property%>%
  filter(str_detect( City, "Mont-Tremblant"))
#property_banff <- st_intersection(property, st_buffer(banff, 200))

## City census and centroid for MTL
mont_tremblant<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "2478102"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(3347)
mont_tremblant_c <-  st_centroid(mont_tremblant)

#################

min_pop <- 1500
buff_dist <- 250000

montreal<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "24462"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(3347)

montreal_c <-  st_centroid(montreal)
##add a buffer"
#radius_mtl <- radius_mtl+10000 
radius_montreal <- st_buffer(montreal_c, buff_dist)

montreal_intersect <- st_intersection(canada_CSD, radius_montreal)
montreal_intersect <- montreal_intersect%>%
  filter(Type == "CSD", Population >= min_pop)

montreal_alentours_all <- st_intersection(property, montreal_intersect)
montreal_alentours_noCMA <- montreal_alentours_all %>%
  filter((is.na(montreal_alentours_all$CMA_UID)==TRUE))
montreal_alentours <- montreal_alentours_all %>%
  filter(City!="Mont-Tremblant", Name!="Mont-Tremblant (V)")

mtl_Listings <- montreal_alentours_all%>%
  group_by(GeoUID)%>%
  count()
mtl_Listings$geometry <- NULL

#mtl_Rev <- aggregate(montreal_alentours_noCMA$Annual_Revenue, by=list(GeoUID=montreal_alentours_noCMA$GeoUID), FUN=sum)

#mtl_Data <- inner_join(mtl_Rev, mtl_Listings)%>%
#  mutate(revperlist=x/n)

mtl_Data <- inner_join(canada_CSD, mtl_Listings)
mtl_Data <- mtl_Data%>%
  mutate(List_per_dwelling=n/Dwellings)
View(mtl_Data)

#### MAPPING MTL/Mont Tremblant

figureB <- 
  tm_shape(radius_montreal)+
  tm_borders(col="grey", lty="dashed")+
  tm_shape(mtl_Data)+
  tm_fill(col="List_per_dwelling",
          palette="Oranges",  
          border.col = "#f0f0f0",
          border.alpha = .2,
          breaks = c(0,0.025,0.05,0.075,0.1,0.15,Inf),
          title="Listings per Dwellings")+
  #   legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")))+
  tm_shape(canada_CSD)+
  tm_borders(col="lightgrey")+
  tm_shape(montreal)+
  tm_borders(col="black")+
  tm_shape(mont_tremblant)+
  tm_borders(col="black")+
  tm_legend(position = c("right", "bottom"),
            bg.color = "white",
            bg.alpha=.2,
            width = .25, title.size = 1)+
  tm_add_legend(type="fill",
                col= "grey",
                labels=c("Montreal CMA"),
                border.lwd = NA,
                alpha = 1)
#                title="Listing Type")
tmap_save(figureB, "output/figureB.png", width = 2400, height = 1500)


##### Other areas




pec<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "3513020"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(3347)
pec_c <-  st_centroid(pec)

whistler<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "5931020"),  
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(3347)
whistler_c <-  st_centroid(whistler)

tofino<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "5923025"),
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(3347)
tofino_c <-  st_centroid(tofino)

banff<- 
  get_census(
    dataset = "CA16", 
    regions = list(CSD = "4815035"),
    level = "CSD",
    geo_format = "sf") %>% 
  st_transform(3347)
banff_c <-  st_centroid(banff)

## Urban agglomerations
toronto<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "35535"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(3347)
toronto_c <-  st_centroid(toronto)

##radius_toronto <- radius_toronto+10000 
radius_toronto <- st_buffer(toronto_c, buff_dist)
toronto_intersect <- st_intersection(canada_CSD, radius_toronto$geometry)
toronto_intersect <- toronto_intersect%>%
  filter(Type == "CSD", Population >= min_pop)


## Urban agglomerations
vancouver<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "59933"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(3347)
vancouver_c <-  st_centroid(vancouver)

##radius_toronto <- radius_toronto+10000 
radius_vancouver <- st_buffer(vancouver_c, buff_dist)
vancouver_intersect <- st_intersection(canada_CSD, radius_vancouver$geometry)
vancouver_intersect <- vancouver_intersect%>%
  filter(Type == "CSD", Population >= min_pop)


##

calgary<- 
  get_census(
    dataset = "CA16", 
    regions = list(CMA = "48825"),  
    level = "CMA",
    geo_format = "sf") %>% 
  st_transform(3347)
calgary_c <-  st_centroid(calgary)

radius_calgary <- st_buffer(calgary_c, buff_dist)
calgary_intersect <- st_intersection(canada_CSD, radius_calgary)
calgary_intersect <- calgary_intersect%>%
  filter(Type == "CSD", Population >= min_pop)

######################## ADD PROPERTIES ##################################
#save(calgary_intersect, file="Calgary_CSD.RData")
#save(montreal_intersect, file="Montreal_CSD.RData")
#save(vancouver_intersect, file="Vancouver_CSD.RData")
#save(toronto_intersect, file="Toronto_CSD.RData")


## Intersect property and radius around urban agglomerations

toronto_alentours_all <- st_intersection(property, toronto_intersect)
toronto_alentours_noCMA <- toronto_al %>%
  filter((is.na(toronto_al$CMA_UID)==TRUE))
toronto_alentours <- toronto_alentours_noCMA %>%
  filter(City!="Prince Edward County", City!="The Blue Mountains", Name!= "Prince Edward County (CY)", Name!="The Blue Mountains (T)")



calgary_alentours_all <- st_intersection(property, calgary_intersect)
calgary_alentours_noCMA <- calgary_alentours_all %>%
  filter((is.na(calgary_alentours_all$CMA_UID)==TRUE))
calgary_alentours <- calgary_alentours_noCMA%>%
  filter(calgary_alentours_noCMA$Airbnb_PID %in% property_banff$Airbnb_PID ==FALSE)

vancouver_alentours_all <- st_intersection(property, vancouver_intersect)
vancouver_alentours_noCMA <- vancouver_alentours_all %>%
  filter((is.na(vancouver_alentours_all$CMA_UID)==TRUE))
vancouver_alentours <- vancouver_alentours_all%>%
  filter(City!="Whistler", Name!="Whistler (DM)")

tm_shape(radius_vancouver)+
  tm_borders(col="grey")+
  tm_shape(vancouver_al)+
  tm_dots(col = "red")

#############

van_Listings <- vancouver_alentours_all%>%
  group_by(GeoUID)%>%
  count()
van_Listings$geometry <- NULL

#van_Rev <- aggregate(vancouver_alentours$Annual_Revenue, by=list(GeoUID=vancouver_alentours$GeoUID), FUN=sum)

#van_Data <- inner_join(van_Rev, van_Listings)%>%
#  mutate(revperlist=x/n)

van_Data <- inner_join(canada_CSD, van_Listings)
van_Data <- van_Data%>%
  mutate(List_per_dwelling=n/Dwellings)
View(van_Data)

cal_Listings <- calgary_alentours_all%>%
  group_by(GeoUID)%>%
  count()
cal_Listings$geometry <- NULL

#cal_Rev <- aggregate(calgary_alentours$Annual_Revenue, by=list(GeoUID=calgary_alentours$GeoUID), FUN=sum)

#cal_Data <- inner_join(cal_Rev, cal_Listings)%>%
#  mutate(revperlist=x/n)

cal_Data <- inner_join(canada_CSD, cal_Listings)
cal_Data <- cal_Data%>%
  mutate(List_per_dwelling=n/Dwellings)
View(cal_Data)


#######

tor_Listings <- toronto_alentours_all%>%
  group_by(GeoUID)%>%
  count()
tor_Listings$geometry <- NULL

tor_Data <- inner_join(canada_CSD, tor_Listings)
tor_Data <- tor_Data%>%
  mutate(List_per_dwelling=n/Dwellings)
View(tor_Data)

figureA <- 
  tm_shape(radius_toronto)+
  tm_borders(col="grey", lty="dashed")+
  tm_shape(tor_Data)+
  tm_fill(col="List_per_dwelling",
          palette="Oranges",  
          border.col = "#f0f0f0",
          border.alpha = .2,
           breaks = c(0,0.025,0.05,0.075,0.1,0.15, 0.2),
          title="Listings per Dwellings")+
  #   legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")))+
  tm_shape(canada_CSD)+
  tm_borders(col="lightgrey")+
  tm_shape(toronto)+
  tm_borders(col="black")+
  tm_legend(position = c("right", "bottom"),
            bg.color = "white",
            bg.alpha=.2,
            width = .25, title.size = 1)+
  tm_add_legend(type="fill",
                col= "grey",
                labels=c("Toronto CMA"),
                border.lwd = NA,
                alpha = 1)
#                title="Listing Type")
tmap_save(figureA, "output/figureA.png", width = 2400, height = 1500)


figureC <- 
  tm_shape(radius_calgary)+
  tm_borders(col="grey", lty="dashed")+
  tm_shape(cal_Data)+
  tm_fill(col="List_per_dwelling",
          palette="Oranges",  
          border.col = "#f0f0f0",
          border.alpha = .2,
          breaks = c(0,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1.5,2, Inf),
          title="Listings per Dwellings")+
  #   legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")))+
  tm_shape(canada_CSD)+
  tm_borders(col="lightgrey")+
    tm_shape(calgary)+
    tm_borders(col="black")+
  tm_legend(position = c("right", "bottom"),
            bg.color = "white",
            bg.alpha=.2,
            width = .25, title.size = 1)+
  tm_add_legend(type="fill",
                col= "grey",
                labels=c("Calgary CMA"),
                border.lwd = NA,
                alpha = 1)
#                title="Listing Type")
tmap_save(figureC, "output/figureC.png", width = 2400, height = 1500)

figureD <- 
  tm_shape(radius_vancouver)+
  tm_borders(col="grey", lty="dashed")+
  tm_shape(vancouver)+
  tm_fill(col="grey")+
  tm_shape(van_Data)+
  tm_fill(col="List_per_dwelling",
          palette="Oranges",  
          border.col = "#f0f0f0",
          border.alpha = .2,
          breaks = c(0,0.05,0.1, 0.15,0.2,0.30,0.45,Inf),
          title="Listings per Dwellings")+
  #   legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")))+
  tm_shape(canada_CSD)+
  tm_borders(col="lightgrey")+
  tm_shape(vancouver)+
  tm_borders(col="black")+
  #tm_shape(filter(van_Data, Name == "Whistler (DM)"))+
  #tm_borders(col="red")+
  tm_legend(position = c("left", "bottom"),
            bg.color = "white",
            bg.alpha=.2,
            width = .25, title.size = 1)+
  tm_add_legend(type="fill",
                col= "grey",
                labels=c("Calgary CMA"),
                border.lwd = NA,
                alpha = 1)
#                title="Listing Type")
tmap_save(figureD, "output/figureD.png", width = 2400, height = 1500)
