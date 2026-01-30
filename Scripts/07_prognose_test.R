
# framskriving test ------------------------------------------------------
library(pacman)

p_load(char = c("tidyverse","here","openxlsx","patchwork"))


data<- read.xlsx(here("Data","Analyse_data","boligbehov_scenario_prognoser.xlsx"),sheet = 1) %>% 
  pivot_longer(stor_bolig_p:små_bolig_p,names_to = "bolig_type",values_to="antall_bolig")

kommuner<- data %>% pull(kommune_navn) %>% unique()

husholdning_Storrelse<- data %>% pull(husholdning_størrelse) %>% unique()

alder_Gruppe<- data %>% pull(alder_gruppe) %>% unique()
#test#

test<- data %>% filter(kommune_navn == kommuner[1] & alder_gruppe == alder_Gruppe[1] & husholdning_størrelse == husholdning_Storrelse[1])

test_plot<- ggplot(test,
  aes(x = år, y = antall_bolig,group = interaction(framskriving_alternativ,bolig_type)))+
  geom_line(aes(color = framskriving_alternativ,linetype = bolig_type))+
  theme_minimal(base_size = 16)


km_sentralitet<- read.xlsx(xlsxFile = here("Data","Aux_data","kommune_sentralitet_1994_2024.xlsx"),
sheet = 1)

vindafjord<- km_sentralitet %>% 
  filter(kommune_navn == "Vindafjord (2006-)") %>% 
  mutate(kommune_navn = gsub(" \\(2006-\\)","",kommune_navn))

km_sentralitet_redigert<- km_sentralitet %>% 
  filter(!grepl("\\(|\\)",kommune_navn,perl = T)) %>% 
  rbind(.,vindafjord) %>% 
  rename(sentralitet = sentralitetsklasse)



bf<- read.xlsx(xlsxFile = here("Data","B_framskrivinger","rogaland_befolkning_framskrivinger_etter2020.xlsx")) %>% 
#  filter(!grepl("(MMMM)",statistikkvariabel, fixed = T))   %>% 
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
  left_join(.,km_sentralitet_redigert,by = c("region" = "kommune_navn","kommune_kode"),relationship = "many-to-many") %>% 
  rename(kommune_navn = region)

#
bf_graph<- bf %>% 
  filter(kommune_navn == kommuner[1] & alder_gruppe == alder_Gruppe[1], framskriving_dato == 2024) %>% 
  ggplot(aes(x = år, y = value,group = interaction(alder_gruppe,statistikkvariabel)))+
  geom_line(aes(color = statistikkvariabel, linetype = alder_gruppe))+
  theme_minimal(base_size = 16)

test_plot + bf_graph


## model hesaplama kodunda bir hata var gibi

test_bf<-bf %>% 
  filter(kommune_navn == kommuner[1] & alder_gruppe == alder_Gruppe[1], framskriving_dato == 2024)

P_h_a_s <- read.xlsx(here("Data","panda_vekter","p_dist_nasjonal.xlsx"),
                      sheet = "hhfreq_nasjonal",
                      na.strings = "-") %>% 
  filter(alder_gruppe != "Total")  %>% 
  pivot_longer(cols = hh_1:hh_6,names_to = "hhstr",values_to = "value") %>% 
  mutate(value = replace_na(value,0)) %>% 
  mutate(sentralitet = if_else(sentralitet/10 <1,paste0("0",as.character(sentralitet)),as.character(sentralitet))) %>% 
  mutate(value = value/100)
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

#test med egersund

befolkning_justert<- left_join(test_bf,bf_jf,by = c("alder_gruppe","kommune_navn","kommune_kode"),
                                 relationship = "many-to-many") %>% 
                       mutate(justert_value = round(value*justeringsfaktor))

#year duplication due to household size categories
  husholdning_sammensetning<- left_join(befolkning_justert,P_h_a_s, by = c("sentralitet","alder_gruppe"),relationship = "many-to-many") %>%
    mutate(hh_sammensetning = round(value.x*value.y))# %>% 
    #select(-value.x,-value.y,-justeringsfaktor,-justert_value)
  
  antall_husholdning<- left_join(husholdning_sammensetning, P_c_h_a_s, by = c("sentralitet","alder_gruppe","hhstr")) %>% 
    mutate(antall_husholdning = round(hh_sammensetning*value)) #%>% 
    #select(-hh_sammensetning,-value)

  bolig_preferanse<- left_join(antall_husholdning,P_d_c_a_s, by = c("sentralitet","alder_gruppe","hhstr")) %>% 
    mutate(små_bolig_p = round(antall_husholdning * små.bolig),
           mellom_bolig_p = round(antall_husholdning * mellom.størrelse.bolig),
           stor_bolig_p = round(antall_husholdning * stor.bolig),
           udefinert_p = round(antall_husholdning * udefinert)) %>% 
    rename(framskriving_alternativ = statistikkvariabel) #%>% 
    #select(framskriving_alternativ,kommune_navn,kommune_kode, år, sentralitet,alder_gruppe,hhstr,små_bolig_p,mellom_bolig_p,stor_bolig_p,udefinert_p) %>% 
    #drop_na() %>% 
    mutate(hhstr = gsub("hh_","",hhstr),
           hhstr = if_else(hhstr == "6",paste0(hhstr,"+"),hhstr)) %>% 
    rename(husholdning_størrelse = hhstr)


write.xlsx(bolig_preferanse,here("Data","Analyse_data","prognose_test_eigersund.xlsx"))
