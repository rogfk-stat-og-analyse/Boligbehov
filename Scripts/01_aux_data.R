# Trekk kategorier og informasjoner fra SSB
## - Sentralitets klasse til kommunene
## - Kommune koder
## - Fylkes regioner



# Setup ------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","klassR","openxlsx","here"))


# kommune koder ----------------------------------------------------------
klassR::search_klass(query = "kommuneinndeling",codelists = T)

kommunene_i_fylke_2024<- klassR::get_klass(classification = 131,correspondID = 1309)

colnames(kommunene_i_fylke_2024)<-c("fylke_kode","fylke_navn","kommune_kode","kommune_navn")

write.xlsx(x = kommunene_i_fylke_2024,file = here("Data","Aux_data","fylke_kommune_koder_2024.xlsx"))

# sentralitet ------------------------------------------------------------

kommune_sentralitet_2024<- klassR::get_klass(classification = 710,output_level = 2) %>% 
  select(-level)

colnames(kommune_sentralitet_2024)<- c("kommune_kode","sentralitetsklasse","kommune_navn")

write.xlsx(kommune_sentralitet_2024,file = here("Data","Aux_data","kommune_sentralitet_1994_2024.xlsx"))

# fylke regioner ---------------------------------------------------------


fylke_regioner<- klassR::get_klass(classification = 108,correspondID = 1602)

colnames(fylke_regioner)<- c("oekonomiskregion_kode","oekonomiskregion_navn","kommune_kode","kommune_navn")


# mother of all regional omkoding ----------------------------------------

geografiske_referanse_koder<- left_join(fylke_regioner,kommunene_i_fylke_2024,by = c("kommune_kode","kommune_navn")) %>% 
  left_join(.,select(kommune_sentralitet_2024,kommune_kode,sentralitetsklasse), by = "kommune_kode")

write.xlsx(geografiske_referanse_koder,here("Data","Aux_data","geografiske_referansekoder_2024.xlsx"))
