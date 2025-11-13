# Get befolkningsframskrivinger:
# use api V1 for now...
## Prøve framskrivinger: 
#    -2024(14288),
#    -2022(13600),
#    -2020(12882),
#    -2018(11668),
#    -2016(11168)
#    -2012(09482)
# #any data before 2020 must be evaluated seperately

# setup ------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","openxlsx","here","PxWebApiData"))


# data -------------------------------------------------------------------

fylke_kommuner<- read.xlsx(xlsxFile = here("Data","Aux_data","geografiske_referansekoder_2024.xlsx"),sheet = 1) %>% 
  filter(fylke_kode == 1100)  %>% 
  select(kommune_navn,kommune_kode)

kommune_koder<- fylke_kommuner %>% 
  pull(kommune_kode)

bf_tabellene<- c("2024"=14288,"2022"=13600,"2020" = 12882)

bf_Df<- map(.x = bf_tabellene, ~{
  api_response<- ApiData1(.x,Region = kommune_koder, Tid = T, Alder = T, Kjonn = F)

  api_response<- left_join(api_response,fylke_kommuner,by = c("region" = "kommune_navn")) 

} ) %>% 
  purrr::list_rbind(.,names_to = "framskriving_dato")

write.xlsx(bf_Df,here("Data","B_framskrivinger","rogaland_befolkning_framskrivinger_etter2020.xlsx"))


# pre-2020 kommuner & framskrivinger -------------------------------------
fk_p2020<- read.xlsx(xlsxFile = here("Data","Aux_data","fylke_kommune_koder_2020.xlsx")) %>% 
  filter(fylkes_kode == 1100) %>% 
  select(kommune_navn,kommune_kode)


fk_p2020_kk<- fk_p2020 %>% pull(kommune_kode)

bf_tabellene_p2020<-c("2018"=11668,"2016"=11168,"2012"=09482)

bf_DF_p2020<- map(.x = bf_tabellene_p2020, ~{
  api_response<- ApiData1(.x,Region = fk_p2020_kk, Tid = T, Alder = T, Kjonn = F)

  api_response<- left_join(api_response,fk_p2020,by = c("region" = "kommune_navn")) 

} ) %>% 
  purrr::list_rbind(.,names_to = "framskriving_dato")

write.xlsx(bf_DF_p2020,here("Data","B_framskrivinger","rogaland_befolkning_framskrivinger_pre2020.xlsx"))


# sammenbinding ----------------------------------------------------------

befolknings_Framskrivinger<- bind_rows(bf_Df,bf_DF_p2020)

write.xlsx(befolknings_Framskrivinger,here("Data","B_framskrivinger","rogaland_bf_2012_2024.xlsx"))

write.table(befolknings_Framskrivinger,file = here("Data","B_framskrivinger","rogaland_bf_2012_2024.txt"),sep = ";",row.names = F, fileEncoding = "UTF-8")
