
# Setup ------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","openxlsx","summarytools"))


# data -------------------------------------------------------------------

## population

##TODO:
### 1) age groups need to be aggregated to the correct categories! +
### 2) add centrality +
### 2) split into nested data
### 3) population adjustment coefficient: BF_a,k*J_a,k
### 4) distribute the population to household: HD_a,s = BF_a*P(h|a,s)
### 5) aggregate the distribution to houshold: HH_a,s,k = HD_a_s * P(c|h,a,s)
### 6) assign dwelling type: DW_k = HH_a,s,k * P(d|c,a,s)
### 7) Regional adjustment weights (?)
### a: age group, s: centrality, c: contact person, k: municipality,h: household size


bf_faktisk<- read.xlsx(here("Data","panda_vekter","befolkning_justerings_faktor_2024.xlsx"),sheet = 1)  %>% 
  select(-Total) %>% 
  pivot_longer(cols =  `1101-Eigersund`:`1160-Vindafjord`,names_to = "kommune",values_to = "befolkning_faktisk")

bf_formell<- read.xlsx(here("Data","panda_vekter","befolkning_justerings_faktor_2024.xlsx"),sheet = 2)  %>% 
  pivot_longer(cols =  `1101-Eigersund`:`1160-Vindafjord`,names_to = "kommune",values_to = "befolkning_formell")

bf_jf<- left_join(bf_faktisk,bf_formell,by = c("alder_gruppe","kommune")) %>% 
  mutate(justeringsfaktor = befolkning_faktisk/befolkning_formell) %>% 
  mutate(kommune_kode = str_extract(string = kommune,pattern = "[[:digit:]]+"),
         kommune_navn = str_split_i(kommune, pattern = "-", i = 2)) %>% 
  select(kommune_navn,kommune_kode,alder_gruppe,justeringsfaktor)

remove(bf_faktisk,bf_formell)

km_sentralitet<- read.xlsx(xlsxFile = here("Data","Aux_data","kommune_sentralitet_1994_2024.xlsx"),
sheet = 1) %>% 
  rename(sentralitet = sentralitetsklasse)




bf<- read.xlsx(xlsxFile = here("Data","B_framskrivinger","rogaland_befolkning_framskrivinger_etter2020.xlsx")) %>% 
  #filter(!grepl("(MMMM)",statistikkvariabel, fixed = T))   %>% 
  mutate(alder_numerisk = as.numeric(str_extract(string = alder,pattern = "[[:digit:]]+"))) %>% 
  mutate(alder_gruppe = case_when(alder_numerisk>=0 & alder_numerisk<15 ~"0-14 år",
                                  alder_numerisk>=15 & alder_numerisk<20 ~"15-19 år",
                                  alder_numerisk>=20 & alder_numerisk<25 ~"20-24 år",
                                  alder_numerisk>=25 & alder_numerisk<30 ~"25-29 år",
                                  alder_numerisk>=30 & alder_numerisk<40 ~"30-39 år",
                                  alder_numerisk>=40 & alder_numerisk<50 ~"40-49 år",
                                  alder_numerisk>=50 & alder_numerisk<60 ~"50-59 år",
                                  alder_numerisk>=60 & alder_numerisk<70 ~"60-69 år",
                                  alder_numerisk>=70 & alder_numerisk<80 ~"70-79 år",
                                  alder_numerisk >= 80 ~ "80+ år",.default = as.character(alder_numerisk))) %>% 
  group_by(framskriving_dato,region,kommune_kode,statistikkvariabel,år,alder_gruppe) %>% 
  summarise(value = sum(value),.groups = "drop") %>% 
  left_join(.,km_sentralitet,by = c("region" = "kommune_navn","kommune_kode"),relationship = "many-to-many") %>% 
  rename(kommune_navn = region)


#dfSummary(bf)




## probability distributions
#TODO:
### 1) replicate with national distributions
### Remember to pivot everything to long
### dataformats are all fucked up for some reason and 
### must be corrected per dataset... Not the most efficient way of doing it
### can be optimized in the future
###P(h|a,s)

P_h_a_s <- read.xlsx(here("Data","panda_vekter","p_dist_nasjonal.xlsx"),
                      sheet = "hhfreq_nasjonal",
                      na.strings = "-") %>% 
  filter(alder_gruppe != "Total")  %>% 
  pivot_longer(cols = hh_1:hh_6,names_to = "hhstr",values_to = "value") %>% 
  mutate(value = replace_na(value,0)) %>% 
  mutate(sentralitet = if_else(sentralitet/10 <1,paste0("0",as.character(sentralitet)),as.character(sentralitet))) %>% 
  mutate(value = value/100)#rescale the prob distributions to [0-1]
##P(c|h,a,s)

P_c_h_a_s<- read.xlsx(here("Data","panda_vekter","p_dist_nasjonal.xlsx"),
                      sheet = "andel_kontakt_nasjonal",
                    na.strings = "-") %>% 
  filter(alder_gruppe!="Total") %>% 
  pivot_longer(cols = hh_1:hh_6,names_to = "hhstr", values_to = "value") %>% 
  mutate(sentralitet = if_else(sentralitet/10 <1,paste0("0",as.character(sentralitet)),as.character(sentralitet)))



##P(d|c,a,s)
P_d_c_a_s<- read.xlsx(here("Data","panda_vekter","p_dist_nasjonal.xlsx"),sheet = "full_pdist_nasjonal")%>% 
  mutate(across(mellom.størrelse.bolig:udefinert,~(.x/100))) %>% 
  mutate(sentralitet = if_else(sentralitet/10 <1,paste0("0",as.character(sentralitet)),as.character(sentralitet))) %>% 
  mutate(hhstr = paste0("hh_",as.character(hhstr)))
    


# projections ------------------------------------------------------------

projection_bf<- bf %>% 
  filter(framskriving_dato == 2024) %>% 
  group_by(statistikkvariabel) %>% 
  nest()

bolig_prognoser<- tibble()

for(i in 1:nrow(projection_bf)){

  alternativ<- projection_bf$statistikkvariabel[i]

  cat(paste0("projections for: ",alternativ),sep = "\n")

  befolkning<- projection_bf$data[[i]]

  befolkning_justert<- left_join(befolkning,bf_jf,by = c("alder_gruppe","kommune_navn","kommune_kode"),
                                 relationship = "many-to-many") %>% 
                       mutate(justert_value = round(value*justeringsfaktor))

  husholdning_sammensetning<- left_join(befolkning_justert,P_h_a_s, by = c("sentralitet","alder_gruppe"),relationship = "many-to-many") %>%
    mutate(hh_sammensetning = round(value.x*value.y)) %>% 
    select(-value.x,-value.y,-justeringsfaktor,-justert_value)
  
  antall_husholdning<- left_join(husholdning_sammensetning, P_c_h_a_s, by = c("sentralitet","alder_gruppe","hhstr")) %>% 
    mutate(antall_husholdning = round(hh_sammensetning*value)) %>% 
    select(-hh_sammensetning,-value)

  bolig_preferanse<- left_join(antall_husholdning,P_d_c_a_s, by = c("sentralitet","alder_gruppe","hhstr")) %>% 
    mutate(små_bolig_p = round(antall_husholdning * små.bolig),
           mellom_bolig_p = round(antall_husholdning * mellom.størrelse.bolig),
           stor_bolig_p = round(antall_husholdning * stor.bolig),
           udefinert_p = round(antall_husholdning * udefinert)) %>% 
    mutate(framskriving_alternativ = alternativ) %>% 
    select(framskriving_alternativ,kommune_navn,kommune_kode, år, sentralitet,alder_gruppe,hhstr,små_bolig_p,mellom_bolig_p,stor_bolig_p,udefinert_p) %>% 
    drop_na() %>% 
    mutate(hhstr = gsub("hh_","",hhstr),
           hhstr = if_else(hhstr == "6",paste0(hhstr,"+"),hhstr)) %>% 
    rename(husholdning_størrelse = hhstr)

  bolig_prognoser<- bind_rows(bolig_prognoser,bolig_preferanse)




}

##inspect

prognose_test_graph<-bolig_prognoser %>% 
  filter(grepl("MMMM|HHMH|LLML",framskriving_alternativ,fixed = F) & kommune_navn == "Stavanger" & husholdning_størrelse == "3" & alder_gruppe == "30-39 år") %>% 
  select(år,framskriving_alternativ,contains("_p")) %>% 
  select(-udefinert_p) %>% 
  pivot_longer(cols = små_bolig_p:stor_bolig_p, names_to = "bolig_størrelse", values_to = "antall_boliger") %>% 
  mutate(bolig_størrelse = gsub("_p","",bolig_størrelse),
         bolig_størrelse = if_else(bolig_størrelse == "små_bolig","liten_bolig",bolig_størrelse))

prognose_test_graph %>% 
  mutate(år = lubridate::as_date(paste0(år,"-01-01"))) %>% 
  ggplot(aes(x = år, y = antall_boliger, group = framskriving_alternativ)) +
  geom_line(aes(color  = framskriving_alternativ))+
  theme_bw(base_size = 16)+
  theme(axis.text.x = element_text(angle = 45,vjust = .5))+
  labs(title = "Stavanger - 3 personer - 30-39 år")+
  facet_wrap(~bolig_størrelse)



write.table(bolig_prognoser, file = here("Data","Panda_prognoser","befolkning_scenario_prognoser_redigert.txt"),sep = ";",row.names = F,col.names = T,fileEncoding = "UTF-8")

write.xlsx(x = bolig_prognoser,file = here("Data","Panda_prognoser","befolkning_scenario_prognoser_redigert.xlsx"))


##DO NOT RUN - PANDA MODEL YIELDS HIGHER VALUES THAN SSB##
# merge projections ------------------------------------------------------

# firem <- read.xlsx(here("Data","Analyse_data","prognose_analyse_data.xlsx"),sheet = 1) %>% 
#   rename(kommune_kode = municipality_code,
#          kommune_navn = municipality_name,
#          år = year,
#          husholdning_størrelse = household_size,
#          alder_gruppe = household_age,
#         sentralitet = sentralitetsklasse) %>% 
#   mutate(bolig_storrelse = case_when(bolig_storrelse == "0-79 kvdm"~"små_bolig_p",
#                                      bolig_storrelse == "80-159 kvdm"~"mellom_bolig_p",
#                                      bolig_storrelse == "160+ kvdm" ~ "stor_bolig_p",.default = "udefiner_p")) %>% 
#   pivot_wider(names_from = bolig_storrelse,values_from = housing_demand) %>% 
#   mutate(framskriving_alternativ = "Hovedalternativet (MMMM)") %>% 
#   mutate(år = gsub("-01-01","",år)) %>% 
#   mutate(alder_gruppe = paste0(alder_gruppe," år")) %>% 
#   filter(alder_gruppe != "0-14 år")

# distrikter<- firem %>% 
#   select(kommune_kode,kommune_navn,oekonomiskregion_kode,oekonomiskregion_navn) %>% 
#   distinct()
  
# andre_alternativer<- read.xlsx(here("Data","Panda_prognoser","befolkning_scenario_prognoser.xlsx")) %>% 
#   left_join(.,distrikter, by = c("kommune_kode","kommune_navn")) %>% 
#   select(-udefinert_p) %>% 
#   filter(år != "2024") %>% 
#   mutate(husholdning_størrelse = paste0(husholdning_størrelse," Personer"))


# full_prognoser<- rbind(firem,andre_alternativer)

# summarytools::dfSummary(full_prognoser)

# #.txt
# write.table(full_prognoser,
#    file = here("Data","Analyse_data","boligbehov_scenario_prognoser.txt"),
#    sep = ";",
#    row.names = F,
#    col.names = T,
#    fileEncoding = "UTF-8")

# #.csv

# write.table(full_prognoser,
#   file = here("Data","Analyse_data","boligbehov_scenario_prognoser.csv"),
#   sep = ",",
#   row.names = F,
#   col.names = T,
#   fileEncoding = "UTF-8")

# #.xlsx
# write.xlsx(full_prognoser,
#   file = here("Data","Analyse_data","boligbehov_scenario_prognoser.xlslx"))