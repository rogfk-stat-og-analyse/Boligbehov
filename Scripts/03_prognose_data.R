#Boligbehov prognose analyse


# setup ------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","openxlsx"))


# data -------------------------------------------------------------------

prognose_dir<- list.files(path = here("Data","Panda_prognoser"),pattern = "*.xlsx",full.names = T)


prognose_Data<- map(.x = prognose_dir,~{
  data<- read.xlsx(xlsxFile = .x,sheet = 1)
}) %>% list_rbind()

geografiske_referanse_koder<- read.xlsx(here("Data","Aux_data","geografiske_referansekoder_2024.xlsx"),sheet = 1) %>% 
  select(-fylke_kode,-fylke_navn,-kommune_navn)

prognose_Data<- left_join(prognose_Data,geografiske_referanse_koder, by = c("municipality_code" = "kommune_kode"))


# data correction --------------------------------------------------------

analyse_data<- prognose_Data %>% 
  mutate(year = paste0(as.character(year),"-01-01")) %>% 
  mutate(year = ymd(as_date(year))) %>% 
  mutate(age = str_remove(age_interval,pattern = "INTERVAL_"),
         age = gsub("_","-",age),
        age = sub("-INF","+",age)) %>%
  rename(household_age = age) %>% 
  select(-age_interval) %>% 
  mutate(household_size = str_remove(household_size, "SIZE_"),
         household_size = sub("_INF","+",household_size),
        household_size = paste0(household_size," Personer")) %>% 
  rename(bolig_storrelse = utility_floor_space_aggregated) %>% 
  mutate(bolig_storrelse = case_when(bolig_storrelse == "LARGE" ~ "160+ kvdm",
                                     bolig_storrelse == "MEDIUM"~ "80-159 kvdm",
                                     bolig_storrelse == "SMALL" ~ "0-79 kvdm"))

write.table(analyse_data,file = here("Data","Analyse_data","prognose_analyse_data.txt"),sep = ",",row.names = F,fileEncoding = "UTF-8")
write.table(analyse_data,file = here("Data","Analyse_data","prognose_analyse_data.csv"),sep = ",",row.names = F,fileEncoding = "UTF-8")
write.xlsx(analyse_data,file = here("Data","Analyse_data","prognose_analyse_data.xlsx"))




