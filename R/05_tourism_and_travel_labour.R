

ca_561520 <-
  read_csv("data/561520.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))
  
ca_561510 <-
  read_csv("data/561510.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))

ca_561590 <-
  read_csv("data/561590.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))

ca_721198 <-
  read_csv("data/721198.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))

ca_721192 <-
  read_csv("data/721192.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))

ca_721191 <-
  read_csv("data/721191.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))

ca_721120<-
  read_csv("data/721120.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))

ca_721114 <-
  read_csv("data/721114.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))

ca_721113 <-
  read_csv("data/721113.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))

ca_721111 <-
  read_csv("data/721111.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))

ca_713990 <-
  read_csv("data/713990.csv")%>%
  set_names(c("PROV_CSD", "Total", "Subtotal", "Indeterminate",
              "1-4", "10-19", "100-199", "20-49", "200-499",
              "5-9", "50-99", "500+"))


df <- rbind(ca_561510,ca_561520, ca_561590, ca_713990, ca_721111, ca_721113, 
            ca_721114, ca_721120, ca_721191, ca_721192, ca_721198)

df <- df%>%
  filter(str_detect(PROV_CSD, "-"))%>%
  separate(PROV_CSD, c("GeoUID", "name"), " - ")

result <- df %>% 
  group_by(GeoUID) %>% 
  summarise(Count = sum(Total))

tst <- inner_join(result,canada)
tst <- tst%>%
  mutate(tourism_prop= Count/ Population)

tst%>%
  filter(Population>=1500)%>%
  View()

library(stringr)



inner_join(result,canada)

  


colnames(workforce)[colnames(workforce)=="GEO_CODE (POR)"] <- "GEO_CODE"


workforce%>%
  filter(GEO_CODE == "5931020")%>%
  View()


