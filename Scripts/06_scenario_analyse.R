
# Setup ------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","openxlsx","summarytools"))


# data -------------------------------------------------------------------

## population

##TODO:
### 1) age groups need to be aggregated to the correct categories!
### 2) add centrality
### 2) split into nested data
### 3) population adjustment coefficient: BF_a,k*J_a,k
### 4) distribute the population to household: HD_a,s = BF_a*P(h|a,s)
### 5) aggregate the distribution to houshold: HH_a,s,k = HD_a_s * P(c|h,a,s)
### 6) assign dwelling type: DW_k = HH_a,s,k * P(d|c,a,s)
### a: age group, s: centrality, c: contact person, k: municipality
bf_<- read.xlsx(xlsxFile = here("Data","B_framskrivinger","rogaland_befolkning_framskrivinger_etter2020.xlsx")) %>% 
  filter(!grepl("(MMMM)",statistikkvariabel, fixed = T))  
dfSummary(bf)

bf_faktisk<- read.xlsx(here("Data","panda_vekter","befolkning_justerings_faktor_2024.xlsx"),sheet = 1)  %>% 
  select(-Total) %>% 
  pivot_longer(cols =  `1101-Eigersund`:`1160-Vindafjord`,names_to = "kommune",values_to = "befolkning_faktisk")

bf_formell<- read.xlsx(here("Data","panda_vekter","befolkning_justerings_faktor_2024.xlsx"),sheet = 2)  %>% 
  pivot_longer(cols =  `1101-Eigersund`:`1160-Vindafjord`,names_to = "kommune",values_to = "befolkning_formell")

bf_jf<- left_join(bf_faktisk,bf_formell,by = c("alder_gruppe","kommune")) %>% 
  mutate(justerings_faktor = befolkning_faktisk/befolkning_formell)



## probability distributions

