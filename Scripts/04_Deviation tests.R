#Deviation tests

# setup ------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","openxlsx","palettes","scales"))

sansynlighetsvekter_dir<- here("Data","panda_vekter","panda_sansynlighetsvekter.xlsx")

sheet_names<- openxlsx::getSheetNames(file = sansynlighetsvekter_dir)

# Kontak person avvik ----------------------------------------------------

andel_kontakt_nasjonal<- read.xlsx(xlsxFile = sansynlighetsvekter_dir,sheet = sheet_names[1]) %>% 
  select(-Total) %>% 
  pivot_longer(cols = `1`:`6`,names_to= "husholdnings_storrelse",values_to = "andel_nasjonal") 

andel_kontakt_rogaland<- read.xlsx(xlsxFile = sansynlighetsvekter_dir,sheet = sheet_names[2])%>% 
  select(-Total) %>% 
  pivot_longer(cols = `1`:`6`,names_to= "husholdnings_storrelse",values_to = "andel_rogaland") 

andel_kontakt<- left_join(andel_kontakt_nasjonal,andel_kontakt_rogaland,by = c("alder_gruppe","sentralitet","husholdnings_storrelse")) %>% 
  filter(sentralitet != 1 & alder_gruppe != "Total")

andel_kontakt_avvik<- andel_kontakt %>% 
  group_by(sentralitet,alder_gruppe) %>% 
  #convert to conditional probability P_h|age,centrality
  mutate(
    p_nat = andel_nasjonal/sum(andel_nasjonal),
    p_rog = andel_rogaland/sum(andel_rogaland)
      ) %>%
  mutate(p_nat = replace_na(p_nat,0),
        p_rog = replace_na(p_rog,0)
      ) %>% 
  #calculate total variation distance, max variation, euclidean distance(L2)
  summarise(
    TV = .5*sum(abs(p_nat-p_rog)),
    Max_Dev = max(abs(p_nat-p_rog)),
    L2 = sqrt(sum((p_nat-p_rog)^2)),
    .groups = "drop"
) %>% 
  mutate(
    TV_pp = TV*100,
    MaxDev_pp = Max_Dev*100  
  ) %>% 
  mutate(tile_labels = paste0(round(TV_pp),"%"))

ggplot(andel_kontakt_avvik, aes(x = alder_gruppe, y = sentralitet))+
  geom_tile(aes(fill = TV_pp),show.legend = F)+
  geom_text(aes(label = tile_labels),color = "white",fontface = "bold",size = 6)+
  theme_minimal(base_size = 12)+
    theme(axis.text.x = element_text(angle = 45,
      vjust = .5,
      hjust = 0.5,
      size = 14),
  axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))+
  scale_fill_gradient2(low = muted("green"),mid = "#f8bf05ff", high = muted("red"),midpoint = 50)+
  labs(x = "Husholdnings alder", y = "Kommune sentralitet")

hh_andel_kontakt_avvik<- andel_kontakt %>% 
    group_by(sentralitet,husholdnings_storrelse) %>% 
    #convert to conditional probability P_age|størrelse,centrality
    mutate(
      p_nat = andel_nasjonal/sum(andel_nasjonal),
      p_rog = andel_rogaland/sum(andel_rogaland)
        ) %>%
    mutate(p_nat = replace_na(p_nat,0),
          p_rog = replace_na(p_rog,0)
        ) %>% 
    #calculate total variation distance, max variation, euclidean distance(L2)
    summarise(
      TV = .5*sum(abs(p_nat-p_rog)),
      Max_Dev = max(abs(p_nat-p_rog)),
      L2 = sqrt(sum((p_nat-p_rog)^2)),
      .groups = "drop"
  ) %>% 
    mutate(
      TV_pp = TV*100,
      MaxDev_pp = Max_Dev*100  
    ) %>% 
    mutate(tile_labels = paste0(round(TV_pp),"%"),
           husholdnings_storrelse = case_when(husholdnings_storrelse == 6 ~"6+",.default = husholdnings_storrelse))

  ggplot(hh_andel_kontakt_avvik, aes(x = husholdnings_storrelse, y = sentralitet))+
    geom_tile(aes(fill = TV_pp),show.legend = F)+
    geom_text(aes(label = tile_labels),color = "white",fontface = "bold", size = 6)+
    theme_minimal(base_size = 12)+
    theme(axis.text.x = element_text(angle = 45,
          vjust = .5,
          hjust = 0.5,
          size = 14),
      axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))+
    scale_fill_gradient2(low = muted("green"),mid = "#f8bf05ff", high = muted("red"),midpoint = 50)+
    labs(x = "Husholdnings størrelse", y = "Kommune sentralitet")
 

# husholdning sammensetning ----------------------------------------------
## total should be used for pertuberation measure
### Total variation representes (Y_n+x_n)-(Y_r+x_r) where x_n != x_r

hhfreq_nasj<- read.xlsx(xlsxFile = sansynlighetsvekter_dir,sheet = sheet_names[3],na.strings = "-") %>% 
  select(-Total) %>% 
  pivot_longer(cols = hh_1:hh_6,names_to= "husholdnings_storrelse",values_to = "andel_nasjonal") %>% 
  mutate(andel_nasjonal = replace_na(andel_nasjonal,0),
         andel_nasjonal = andel_nasjonal/100) %>% 
  filter(sentralitet != 1)


hhfreq_rog<- read.xlsx(xlsxFile = sansynlighetsvekter_dir,sheet = sheet_names[4],na.strings = "-") %>% 
  select(-Total) %>% 
  pivot_longer(cols = hh_1:hh_6,names_to= "husholdnings_storrelse",values_to = "andel_rog")  %>% 
  mutate(andel_rog = replace_na(andel_rog, 0),
         andel_rog = andel_rog/100)

hhfreq_avvik<- left_join(hhfreq_nasj, hhfreq_rog, by = c("sentralitet"="Sentralitet","alder_gruppe","husholdnings_storrelse"))

#### household age group difference ####
hhfreq_avvik_rep<- hhfreq_avvik %>% 
  group_by(sentralitet,alder_gruppe) %>% 
  #convert to conditional probability P_h|age,centrality
  mutate(
    p_nat = andel_nasjonal/sum(andel_nasjonal),
    p_rog = andel_rog/sum(andel_rog)
      ) %>%
        mutate(p_nat = replace_na(p_nat,0),
      p_rog = replace_na(p_rog,0)
    ) %>% 
#calculate total variation distance, max variation, euclidean distance(L2)
summarise(
  TV = .5*sum(abs(p_nat-p_rog)),
  Max_Dev = max(abs(p_nat-p_rog)),
  L2 = sqrt(sum((p_nat-p_rog)^2)),
  .groups = "drop"
) %>% 
mutate(
  TV_pp = TV*100,
  MaxDev_pp = Max_Dev*100  
) %>% 
mutate(tile_labels = paste0(round(TV_pp),"%"))

ggplot(hhfreq_avvik_rep, aes(x = alder_gruppe, y = sentralitet))+
  geom_tile(aes(fill = TV_pp),show.legend = F)+
  geom_text(aes(label = tile_labels),color = "white",fontface = "bold",size = 6)+
  theme_minimal(base_size = 12)+
    theme(axis.text.x = element_text(angle = 45,
      vjust = .5,
      hjust = 0.5,
      size = 14),
  axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))+
  scale_fill_gradient2(low = muted("green"),mid = "#f8bf05ff", high = muted("red"),midpoint = 7)+
  labs(x = "Husholdnings størrelse", y = "Kommune sentralitet")

######husholdning storrelse avvik#####
hhfreq_avvik_str<- hhfreq_avvik %>% 
    group_by(husholdnings_storrelse,sentralitet) %>% 
    #convert to conditional probability P_h|age,centrality
    mutate(
      p_nat = andel_nasjonal/sum(andel_nasjonal),
      p_rog = andel_rog/sum(andel_rog)
        ) %>%
          mutate(p_nat = replace_na(p_nat,0),
        p_rog = replace_na(p_rog,0)
      ) %>% 
  #calculate total variation distance, max variation, euclidean distance(L2)
  summarise(
    TV = .5*sum(abs(p_nat-p_rog)),
    Max_Dev = max(abs(p_nat-p_rog)),
    L2 = sqrt(sum((p_nat-p_rog)^2)),
    .groups = "drop"
  ) %>% 
  mutate(
    TV_pp = TV*100,
    MaxDev_pp = Max_Dev*100  
  ) %>% 
  mutate(tile_labels = paste0(round(TV_pp),"%")) %>% 
  mutate(husholdnings_storrelse = gsub("hh_","",husholdnings_storrelse),
         husholdnings_storrelse = if_else(husholdnings_storrelse == "6",paste0(husholdnings_storrelse,"+"),husholdnings_storrelse))
  
  ggplot(hhfreq_avvik_str, aes(x = husholdnings_storrelse, y = sentralitet))+
    geom_tile(aes(fill = TV_pp),show.legend = F)+
    geom_text(aes(label = tile_labels),color = "white",fontface = "bold",size = 6)+
    theme_minimal(base_size = 12)+
      theme(axis.text.x = element_text(angle = 45,
        vjust = .5,
        hjust = 0.5,
        size = 14),
    axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16))+
      scale_fill_gradient2(low = muted("green"),mid = "#f8bf05ff", high = muted("red"),midpoint = 7)+
    labs(x = "Husholdnings størrelse", y = "Kommune sentralitet")


## Perturberasjon fra microdata

ag_noise_rog<- read.xlsx(xlsxFile = sansynlighetsvekter_dir,sheet = sheet_names[9]) %>% 
  select(1,2,9,10) %>% 
  mutate(relativ_avvik = round(abs(Total-Test_total)/Total,2)*100) %>% 
  mutate(ra_label = paste(relativ_avvik,"%"))

ggplot(ag_noise_rog,aes(x = alder_gruppe, y = sentralitetsklasse))+
  geom_tile(aes(fill = relativ_avvik),show.legend = F)+
  geom_text(aes(label = ra_label),color = "white",fontface = "bold",size = 6)+
  theme_minimal(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45,
      vjust = .5,
      hjust = 0.5,
      size = 14),
  axis.text.y = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16))+
  scale_fill_gradient2(low = muted("green"),mid = "#f8bf05ff", high = muted("red"),midpoint = 50)+
  labs(x = "Alder gruppe", y = "Kommune sentralitet")

ag_noise_nasjonal<- read.xlsx(xlsxFile = sansynlighetsvekter_dir,sheet = sheet_names[11]) %>% 
  select(1,2,9,10) %>% 
  mutate(relativ_avvik = round(abs(Total-Test_total)/Total,2)*100) %>% 
  mutate(ra_label = paste(relativ_avvik,"%"))


ggplot(ag_noise_nasjonal,aes(x = alder_gruppe, y = sentralitetsklasse))+
  geom_tile(aes(fill = relativ_avvik))+
  geom_text(aes(label = ra_label),color = "white",fontface = "bold")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.5))+
  scale_fill_gradient2(low = muted("green"),mid = "#f8bf05ff", high = muted("red"),midpoint = 50)+
  labs(x = "Alder gruppe", y = "Kommune sentralitet")

bolig_noise_rog<- read.xlsx(xlsxFile = sansynlighetsvekter_dir,sheet = sheet_names[10]) %>% 
  select(1,2,7,8) %>% 
  mutate(relativ_avvik = round(abs(Total-Test_total)/Total,2)*100) %>% 
  mutate(ra_label = paste(relativ_avvik,"%"))

ggplot(bolig_noise_rog,aes(x = alder_gruppe, y = sentralitetsklasse))+
  geom_tile(aes(fill = relativ_avvik))+
  geom_text(aes(label = ra_label),color = "white",fontface = "bold")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.5))+
  scale_fill_gradient2(low = muted("green"),mid = "#f8bf05ff", high = muted("red"),midpoint = 50)+
  labs(x = "Alder gruppe", y = "Kommune sentralitet")




bolig_noise_nasjonal<- read.xlsx(xlsxFile = sansynlighetsvekter_dir,sheet = sheet_names[12]) %>% 
  select(1,2,7,8) %>% 
  mutate(relativ_avvik = round(abs(Total-Test_total)/Total,2)*100) %>% 
  mutate(ra_label = paste(relativ_avvik,"%"))

ggplot(bolig_noise_nasjonal,aes(x = alder_gruppe, y = sentralitetsklasse))+
  geom_tile(aes(fill = relativ_avvik))+
  geom_text(aes(label = ra_label),color = "white",fontface = "bold")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.5))+
  scale_fill_gradient2(low = muted("green"),mid = "#f8bf05ff", high = muted("red"),midpoint = 50)+
  labs(x = "Alder gruppe", y = "Kommune sentralitet")


# bolig eierskap avvik ---------------------------------------------------

bolig_eierskap_3_1<- read.xlsx(xlsxFile = here("Data","panda_vekter","figure3_2-3_1.xlsx"),sheet = 2) %>% 
  pivot_longer(cols = sentralitet_1:sentralitet_6,names_to = "sentralitet",values_to = "kvd_meter") %>% 
  mutate(kvd_meter = round(kvd_meter)) %>% 
  mutate(hhstr = as.character(hhstr)) %>% 
  mutate(sentralitet = gsub("_"," ",sentralitet))

ggplot(bolig_eierskap_3_1,aes(x = hhstr,y = kvd_meter,group = interaction(sentralitet,region)))+
  geom_line(aes(color = sentralitet,linetype = region),linewidth = 1.3)+
  theme_minimal(base_size = 12)+
  theme(axis.text.x = element_text(angle = 45,
      vjust = .5,
      hjust = 0.5,
      size = 14),
  axis.text.y = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  legend.title = element_text("bold",size = 14),
  legend.text  = element_text(size = 14))+
  labs(x = "Husholdning størrelse", y = "Bruksareal (kvd meter)")+
  guides(color = guide_legend(title = "Sentralitet"),linetype = guide_legend(title = "Region"))


bolig_eierskap_3_2<- read.xlsx(xlsxFile = here("Data","panda_vekter","figure3_2-3_1.xlsx"),sheet = 3) %>% 
  pivot_longer(cols = sentralitet_1:sentralitet_6,names_to = "sentralitet",values_to = "kvd_meter") %>% 
  mutate(kvd_meter = round(kvd_meter)) %>% 
  mutate(sentralitet = gsub("_"," ",sentralitet))

ggplot(bolig_eierskap_3_2,aes(x = alder_gruppe,y = kvd_meter,group = interaction(sentralitet,region)))+
  geom_line(aes(color = sentralitet,linetype = region),linewidth = 1.3)+
  theme_minimal()+
    theme(axis.text.x = element_text(angle = 45,
      vjust = .5,
      hjust = 0.5,
      size = 14),
  axis.text.y = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  legend.title = element_text("bold",size = 14),
  legend.text  = element_text(size = 14))+
  labs(x = "Husholdning alder", y = "Bruksareal (kvd meter)")+
  guides(color = guide_legend(title = "Sentralitet"),linetype = guide_legend(title = "Region"))