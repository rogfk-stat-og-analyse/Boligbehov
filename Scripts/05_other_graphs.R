#other graphs

# setup ------------------------------------------------------------------


library(pacman)

p_load(char = c("tidyverse","here","openxlsx","ggrepel"))

# main alternativ graphs -------------------------------------------------


data<- read.xlsx(xlsxFile = here("Data","Analyse_data","prognose_analyse_data.xlsx")) |> 
    mutate(year = as.character(year)) |> 
    mutate(year = gsub("-01-01","",year))


bfs<- read.xlsx(here("Data","B_framskrivinger","rogaland_befolkning_framskrivinger_etter2020.xlsx")) %>% filter(grepl(pattern = "(MMMM)",x = statistikkvariabel) & framskriving_dato == "2024")


rogaland_boligprognoser<- data %>%
    group_by(year,bolig_storrelse) %>% 
    summarise(antall_bolig = sum(housing_demand),.groups = "drop") 

rogaland_befolknings_prognoser<- bfs %>%
    group_by(år) %>% 
    summarise(befolkning = sum(value))

graph_data<- left_join(rogaland_boligprognoser,rogaland_befolknings_prognoser,by = c("year"="år"))

graph_data |> 
    mutate(befolkning = round(befolkning/1000),
           antall_bolig = round(antall_bolig/1000)) |> 
    ggplot() +
    geom_line(aes(x = year,y = befolkning,group = 1),linewidth = 1.2,linetype =2)+
    geom_col(aes(x= year, y = antall_bolig, fill = bolig_storrelse),position = "dodge")+
    theme_bw(base_size = 12)+
    theme(axis.text.x = element_text(angle = 45,
          vjust = 1,
          hjust = 1,
          size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "bottom",
      legend.text  = element_text(size = 12))+
    scale_y_continuous(breaks = seq(0,max(graph_data$befolkning)/1000,by= 50))+
    labs(x = "År",y = "Antall (per 1000)")+
    annotate("text", x = "2029", y = (450),label = "Framskrevet befolkning - MMMM" ,fontface = "bold")+
    guides(fill = guide_legend(title = "Bolig størrelse"))


  
  graph_data<- data |> 
      group_by(year,household_size,bolig_storrelse) |> 
      summarise(antall_bolig = sum(housing_demand)/1000) |> 
      mutate(husholdning_bolig = paste0(household_size," - ",bolig_storrelse)) |> 
      #mutate(antall_bolig = round(antall_bolig/1000)) |> 
      mutate(text_label = if_else(antall_bolig >=30 & year == 2046,husholdning_bolig,NA)) |> 
      ungroup() 
  
  data_labels<- graph_data %>% filter(!is.na(text_label))
  
  
  graph_data |> 
    filter(antall_bolig >= 20) %>% 
      ggplot(aes(x = year, y = antall_bolig,group = husholdning_bolig, color = husholdning_bolig))+
      geom_line(linewidth = 1.5,show.legend = F)+
      geom_text(
      data = data_labels,
      aes(label = husholdning_bolig),
      hjust = 0,            # left‐justify so text moves right
      nudge_x = -4,
      nudge_y = 1.5,        # move labels slightly to the right
      size = 6,
      show.legend = FALSE,
      color = "black"
    ) +
      theme_bw(base_size = 12)+
        theme(axis.text.x = element_text(angle = 45,
              vjust = 1,
              hjust = 1,
              size = 13),
          axis.text.y = element_text(size = 13),
          legend.position = "bottom",
          legend.text  = element_text(size = 13))+
      coord_cartesian(clip = "off")+
      labs(x = "År", y = "Antall boliger(per 1000)")


    graph_data<- data |> 
      group_by(year,sentralitetsklasse,bolig_storrelse) |> 
      summarise(antall_bolig = sum(housing_demand)) |> 
      mutate(sentralitetsklasse = paste0("sentralitetsklasse: ", sentralitetsklasse)) |> 
      ungroup() |> 
      group_by(sentralitetsklasse,bolig_storrelse) |> 
      arrange(as.numeric(year)) |> 
      mutate(indeks = (antall_bolig/first(antall_bolig))*100)
  
  
  
  graph_data |>  
    mutate(year = paste0(year,"01-01"),
           year = lubridate::year(as_date(year))) %>% 
      ggplot(aes(x = year, y = indeks,group = bolig_storrelse))+
      geom_line(aes(color = bolig_storrelse),linewidth = 1.2)+
      labs(x = "År", y = "Antall bolig", subtitle = "indeks(2025 =100)")+
        theme_bw(base_size = 12)+
          theme(axis.text.x = element_text(angle = 90,
                vjust = .5,
                hjust = 0.5,
                size = 13),
            axis.text.y = element_text(size = 13),
            legend.position = "bottom",
            legend.text  = element_text(size = 13),
          legend.title = element_blank(),
          strip.text = element_text(size = 12),
          plot.subtitle = element_text(size = 13))+
        facet_wrap(~sentralitetsklasse)



# other projections ------------------------------------------------------

scenario_data<-read.xlsx(xlsxFile = here("Data","Analyse_data","boligbehov_scenario_prognoser.xlsx"),sheet = 1)

scenario_region<- scenario_data %>% 
  group_by(framskriving_alternativ,oekonomiskregion_navn,år) %>% 
  summarise(stor_bolig = sum(stor_bolig_p),
            mellom_bolig = sum(mellom_bolig_p),
            små_bolig = sum(små_bolig_p),.groups = "drop") %>%
  pivot_longer(cols = stor_bolig:små_bolig, names_to = "bolig_storrelse",values_to = "antall_bolig") %>% 
  mutate(bolig_storrelse = case_when(bolig_storrelse == "stor_bolig" ~ "stor bolig (160+ kvd)",
                                     bolig_storrelse == "mellom_bolig" ~ "mellom størrelse bolig (80-159 kvd)",
                                     bolig_storrelse == "små_bolig" ~ "liten bolig (0-79 kvd)")) %>% 
  mutate(år = paste0(år,"-01-01"),
         år = lubridate::as_date(år),
         år = year(år))
  

scenario_region %>% 
  ggplot(aes(x = år,
             y = antall_bolig,
             groups = interaction(framskriving_alternativ,bolig_storrelse)))+
  geom_line(aes(linetype = bolig_storrelse,  color= framskriving_alternativ))+
  facet_wrap(~oekonomiskregion_navn)