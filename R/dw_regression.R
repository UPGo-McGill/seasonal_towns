#### Updated regression models #################################################

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


### All regions ################################################################

# Predict active listings

CSD %>% 
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + geography + CMA_renter +
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active, data = .) %>% 
  summary()

# Predict FREH

CSD %>% 
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + core_housing_need_pct + 
       median_income + geography + CMA_renter + CMA_core_housing + CMA_income + 
       CMA_tourism + CMA_active, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>% 
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + geography + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active, data = .) %>% 
  summary()


### Central cities #############################################################

# Predict active listings

CSD %>%
  filter(geography == "central city") %>% 
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income, data = .) %>% 
  summary()

# Predict FREH

CSD %>%
  filter(geography == "central city") %>% 
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + core_housing_need_pct + 
       median_income, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>%
  filter(geography == "central city") %>% 
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income, data = .) %>% 
  summary()


### Suburbs ####################################################################

# Predict active listings

CSD %>%
  filter(geography == "suburb") %>% 
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + CMA_renter + CMA_core_housing + 
       CMA_income + CMA_tourism + CMA_active, data = .) %>% 
  summary()

# Predict FREH

CSD %>%
  filter(geography == "suburb") %>% 
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + core_housing_need_pct + 
       median_income + CMA_renter + CMA_core_housing + CMA_income + 
       CMA_tourism + CMA_active, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>%
  filter(geography == "suburb") %>% 
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + CMA_renter + CMA_core_housing + 
       CMA_income + CMA_tourism + CMA_active, data = .) %>% 
  summary()


### Inner region ###############################################################

# Predict active listings

CSD %>%
  filter(geography == "inner region") %>% 
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + CMA_renter + CMA_core_housing + 
       CMA_income + CMA_tourism + CMA_active, data = .) %>% 
  summary()

# Predict FREH

CSD %>%
  filter(geography == "inner region") %>% 
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + core_housing_need_pct + 
       median_income + CMA_renter + CMA_core_housing + CMA_income + 
       CMA_tourism + CMA_active, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>%
  filter(geography == "inner region") %>% 
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + CMA_renter + CMA_core_housing + 
       CMA_income + CMA_tourism + CMA_active, data = .) %>% 
  summary()


### Outer region ###############################################################

# Predict active listings

CSD %>%
  filter(geography == "outer region") %>% 
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + CMA_renter + CMA_core_housing + 
       CMA_income + CMA_tourism + CMA_active, data = .) %>% 
  summary()

# Predict FREH

CSD %>%
  filter(geography == "outer region") %>% 
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + core_housing_need_pct + 
       median_income + CMA_renter + CMA_core_housing + CMA_income + 
       CMA_tourism + CMA_active, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>%
  filter(geography == "outer region") %>% 
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + CMA_renter + CMA_core_housing + 
       CMA_income + CMA_tourism + CMA_active, data = .) %>% 
  summary()


### No region ##################################################################

# Predict active listings

CSD %>%
  filter(geography == "no region") %>% 
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + CMA_renter + CMA_core_housing + 
       CMA_income + CMA_tourism + CMA_active, data = .) %>% 
  summary()

# Predict FREH

CSD %>%
  filter(geography == "no region") %>% 
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + core_housing_need_pct + 
       median_income + CMA_renter + CMA_core_housing + CMA_income + 
       CMA_tourism + CMA_active, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>%
  filter(geography == "no region") %>% 
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + CMA_renter + CMA_core_housing + 
       CMA_income + CMA_tourism + CMA_active, data = .) %>% 
  summary()
