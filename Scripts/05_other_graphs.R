#other graphs

library(pacman)

p_load(char = c("tidyverse","here","openxlsx","ggrepel"))

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
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),legend.position = "bottom")+
    scale_y_continuous(breaks = seq(0,max(graph_data$befolkning)/1000,by= 50))+
    labs(x = "År",y = "Antall (per 1000)")+
    annotate("text", x = "2029", y = (450),label = "Framskrevet befolkning" ,fontface = "bold")+
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
      nudge_x = -2,
      nudge_y = 1,        # move labels slightly to the right
      size = 3,
      show.legend = FALSE,
      color = "black"
    ) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1 ,hjust = 1))+
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
      ggplot(aes(x = year, y = indeks,group = bolig_storrelse))+
      geom_line(aes(color = bolig_storrelse),linewidth = 1.2)+
      labs(x = "År", y = "Antall bolig", subtitle = "indeks(2025 =100)")+
      theme_bw(base_size = 12)+
      theme(legend.position = "bottom",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
      facet_wrap(~sentralitetsklasse)
  