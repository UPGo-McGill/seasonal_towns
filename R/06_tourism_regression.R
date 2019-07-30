########################## REGRESSION PREP ############################

source("R/01_helper_functions.R")
source("R/02_5_data_import.R")
source("R/05_tourism_and_travel_labour.R")

###############

## Tourism Indicator for every CSD with GEOUID
  

#tt <- codes%>%
#  filter(code!=721198, code!=561590)%>%
#  group_by(GeoUID) %>% 
#  summarise (tourism_naics = sum(Total))
#sum(tt$tourism_naics)

tourism_indicator <- codes %>% 
    group_by(GeoUID) %>% 
    summarise (tourism_naics = sum(Total))

  ###
canada_CMA_c <- canada_CMA %>%
  mutate(geometry= st_centroid(geometry))
canada_CSD_c <- canada_CSD%>%
  mutate(geometry= st_centroid(geometry))

canada_CSD_c <- inner_join(canada_CSD_c, tourism_indicator)

#tt <- inner_join(canada_CSD_c, tt)


CSD_tourism <- canada_CSD_c%>%
  mutate(closest_GeoUID = canada_CMA_c$GeoUID[st_nearest_feature(canada_CSD_c, canada_CMA_c)])%>%
  mutate(closest_name = canada_CMA_c$name[st_nearest_feature(canada_CSD_c, canada_CMA_c)])%>%
  mutate(distance = st_nearest_feature(canada_CSD_c, canada_CMA_c))

for(i in 1:nrow(CSD_tourism)){
  CSD_tourism$distance[i] = st_distance(CSD_tourism[i,], canada_CMA_c[CSD_tourism$distance[i],])
}
##

CSD_listings <- read_csv("data/CSD_counts.csv")

CSD_tourism <- CSD_tourism%>%
  mutate(GeoUID = as.numeric(GeoUID))

CSD_tourism <- inner_join(CSD_tourism, CSD_listings) 

bc_tourism <- CST_tourism%>%
  filter(PR_UID == "59")

multi.fit = lm(Listings~tourism_naics+distance, data=CSD_tourism)
summary(multi.fit)


simple.fit = lm(Listings~tourism_naics, data=CSD_tourism)
summary(simple.fit)

ggplot(CSD_tourism)+
  geom_histogram(aes(tourism_naics))

CSD_tourism%>%
  count(tourism_naics)%>%
  view()

#tt <- tt%>%
#  mutate(closest_GeoUID = canada_CMA_c$GeoUID[st_nearest_feature(tt, canada_CMA_c)])%>%
#  mutate(closest_name = canada_CMA_c$name[st_nearest_feature(tt, canada_CMA_c)])%>%
#  mutate(distance = st_nearest_feature(tt, canada_CMA_c))

#for(i in 1:nrow(tt)){
#  tt$distance[i] = st_distance(tt[i,], canada_CMA_c[tt$distance[i],])
#}
##

#tt <- tt%>%
#  mutate(GeoUID = as.numeric(GeoUID))

#tt <- inner_join(tt, CSD_listings) 