#### Updated regression models #################################################

library(tidyverse)
library(sf)
library(strr)
library(upgo)
library(cancensus)

load("data/CMA.Rdata")
load("data/CSD.Rdata")
load("data/FREH.Rdata")
load("data/property.Rdata")


### All regions ################################################################

# Predict active listings

CSD %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + geography + 
       CMA_renter + CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH

CSD %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + geography + 
       CMA_renter + CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + geography + 
       CMA_renter + CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()


### Central cities #############################################################

# Predict active listings

CSD %>%
  filter(geography == "central city") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_total_pop, 
     data = .) %>% 
  summary()

# Predict FREH

CSD %>%
  filter(geography == "central city") %>% 
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + core_housing_need_pct + 
       median_income + density + CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>%
  filter(geography == "central city") %>% 
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_total_pop, 
     data = .) %>% 
  summary()


### Suburbs ####################################################################

# Predict active listings

CSD %>%
  filter(geography == "suburb") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH

CSD %>%
  filter(geography == "suburb") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>%
  filter(geography == "suburb") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()


### Inner region ###############################################################

# Predict active listings

CSD %>%
  filter(geography == "inner region") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH

CSD %>%
  filter(geography == "inner region") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>%
  filter(geography == "inner region") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()



### Outer region ###############################################################

# Predict active listings

CSD %>%
  filter(geography == "outer region") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH

CSD %>%
  filter(geography == "outer region") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>%
  filter(geography == "outer region") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()



### No region ##################################################################

# Predict active listings

CSD %>%
  filter(geography == "no region") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(active_listings_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH

CSD %>%
  filter(geography == "no region") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(FREH_per_cap ~ tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

# Predict FREH with active listings as IV

CSD %>%
  filter(geography == "no region") %>% 
  filter(population >= 1500, active_listings >= 5) %>%
  lm(FREH_per_cap ~ active_listings_per_cap + tourism_per_cap + renter_pct + 
       core_housing_need_pct + median_income + density + CMA_renter + 
       CMA_core_housing + CMA_income + CMA_tourism + CMA_active +
       CMA_total_pop, data = .) %>% 
  summary()

