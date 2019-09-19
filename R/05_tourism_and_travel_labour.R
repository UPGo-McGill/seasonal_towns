#################### IMPORT ALL TOURIST INDUSTRY CATEGORIES #####################

source("R/01_helper_functions.R")

###

ca_561520 <-
  read_csv("data/561520.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 561520)
  
ca_561510 <-
  read_csv("data/561510.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 561510)


ca_561590 <-
  read_csv("data/561590.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 561590)


ca_721198 <-
  read_csv("data/721198.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721198)


ca_721192 <-
  read_csv("data/721192.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721192)


ca_721191 <-
  read_csv("data/721191.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721191)



ca_721120<-
  read_csv("data/721120.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721120)


ca_721114 <-
  read_csv("data/721114.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721114)


ca_721113 <-
  read_csv("data/721113.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721113)


ca_721111 <-
  read_csv("data/721111.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721111)


ca_713990 <-
  read_csv("data/713990.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 713990)


ca_487110 <-
  read_csv("data/487110.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 487110)


ca_487210 <-
  read_csv("data/487210.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 487210)


ca_487990 <-
  read_csv("data/487990.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 487990)


ca_713920 <-
  read_csv("data/713920.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 713920)


ca_713930 <-
  read_csv("data/713930.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 713930)


ca_713940 <-
  read_csv("data/713940.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 713940)


ca_722210 <-
  read_csv("data/722210.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 722210)

ca_722110 <-
  read_csv("data/722110.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 722110)

ca_721310 <-
  read_csv("data/721310.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721310)

ca_721213 <-
  read_csv("data/721213.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721213)

ca_721212 <-
  read_csv("data/721212.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721212)


ca_721211 <-
  read_csv("data/721211.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 721211)

ca_712120 <-
  read_csv("data/712120.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 712120)

ca_712190 <-
  read_csv("data/712190.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 712190)

ca_483115  <-
  read_csv("data/483115.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 483115)

ca_483116 <-
  read_csv("data/483116.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 483116)

ca_483214 <-
  read_csv("data/483214.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 483214)

ca_483213 <-
  read_csv("data/483213.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 483213)

ca_713110 <-
  read_csv("data/713110.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 713110)

ca_713210 <-
  read_csv("data/713210.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 713210)

ca_713910 <-
  read_csv("data/713910.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 713910)

ca_485990 <-
  read_csv("data/485990.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1_4", "10_19", "100_199", "20_49", "200_499",
              "5_9", "50_99", "500plus"))%>%
  mutate(code= 485990)


tourism_df <- rbind(ca_561510,ca_561520, ca_561590,
            #hotels, resorts, casino hotels, bnb
            ca_721111, ca_721113,  ca_721120, ca_721191, ca_721192, ca_721198,
            #motel:  ca_721114, 
            #scenic transportation: 
            ca_487110, ca_487210, ca_487990, 
            #skiing, marinas, golf, fitness and recreation
            ca_713920, ca_713930, ca_485990, ca_713990, #ca_713940,
            # RV and camping
            ca_721211, ca_721212, ca_721213, 
            #boarding: ca_721310, 
            #food and dining
            ca_722110, ca_722210,
            #historic and parks
            ca_712120, ca_712190,
            #deep sea:  ca_483115, ca_483116, 
            #inland water:  
            ca_483214, ca_483213)
            #amusement, theme parks etc
            # ca_713110, ca_713210, ca_713910)


tourism_df <- tourism_df%>%
  filter(str_detect(PROV_CSD, "-"))%>%
  separate(PROV_CSD, c("GeoUID", "name"), " - ")

tourism_codes <- tourism_df

tourism_df$name <- NULL
tourism_df$code <- NULL

tourism_result <- tourism_df %>% 
  group_by(GeoUID) %>% 
  summarise_each(list(sum))

tourism_result <- inner_join(tourism_result,canada_CSD)
tourism_result <- tourism_result%>%
  mutate(tourism_prop= Total/ Population)

tourism_result%>%
  filter(Population>=1200)%>%
  filter(is.na(CMA_UID))%>%
  View()

save(tourism_result, file="data/tourism_result.RData")
save(tourism_codes, file="data/tourism_codes.RData")




### ALTERNATIVE: Using size of businesses = number of employees
## do not run
codes_prop <- tourism_df %>%
  mutate(`1_4`= `1_4`*2.5)%>%
  mutate(`10_19`= `10_19`*14.5)%>%
  mutate(`100_199`= `100_199`*149.5)%>%
  mutate(`20_49`= `20_49`*34.5)%>%
  mutate(`200_499`= `200_499`*349.5)%>%
  mutate(`5_9`= `5_9`*7)%>%
  mutate(`50_99`= `50_99`*149)%>%
  mutate(`500plus`= `500plus`*500)

result_prop <- codes_prop %>% 
  group_by(GeoUID) %>% 
  summarise_each(list(sum))

result_prop <- inner_join(result_prop,canada)
result_prop <- result_prop%>%
  mutate(tourism_prop= Total/ Population)

result_prop%>%
  filter(Population>=1500)%>%
  filter(is.na(CMA_UID))%>%
  View()
