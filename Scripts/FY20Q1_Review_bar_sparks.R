##  PROJECT: Q2 review target analysis
##  AUTHOR:  tessam | USAID
##  PURPOSE: munge historic targets to fy21 targets, viz
##  LICENCE: MIT
##  DATE:    2020-04-03
##  UPDATE:

#Dependancies----------------------------------------------------------

library(tidyverse)
library(vroom)
library(scales)
library(extrafont)
library(ggrepel)


source("Scripts/si_style.R")


# GLOBALS -----------------------------------------------------------------

  data_in <- "Data"
  data_out <- "Dataout"
  viz_out <- "Images"
  
  # Plot to create tiny progress graphs in bar form
  # Requires a long data frame named df_long
  bar_spark <- function(ind) {
    viz <- df_long %>% 
      filter(indicator == {{ind}}, Agency == "USAID") %>% 
      ggplot() + 
      geom_col(aes(Agency, target), fill = "#C0C0C0") +
      geom_col(aes(Agency, share), fill = "#e04745") +
      coord_flip() + 
      theme_void() +
      theme(legend.position = "none",
        plot.background = element_rect(fill = "#f3f3f3",
          color = "#f3f3f3"),
      )
    
    ggsave(file.path(viz_out, paste0("Q1_sparks", {{ind}}, "_tgt", ".png")),
      plot = viz, dpi = 330, width = 1.25, height = 0.1)
    
    print(viz)
  } 
  
    
    

# LOAD AND MUNG -----------------------------------------------------------

  # Ad hoc munge and viz request from team lead
  df <- read_csv(file.path(data_out, "FY20Q1_keyindic_shares.csv"))
  
  df_long <- 
    df %>% pivot_longer(cols = USAID:PEPFAR,
      names_to = "Agency",
      values_to = "Value")

  # Preview plot
  bar_spark(ind ="TX_CURR")
  
  #output for each ind
  unique(df_long$indicator) %>% 
   walk(bar_spark)

  rm(df_long)  
  
  
  # TODO: Fix ggsave names call so you are not overwriting progress vs. targets
  # Munge request for targets
  df_21 <- vroom(file.path(data_in, "df_mer_21.csv")) 

  df_long <- df_21 %>% 
    filter(fiscal_year == 2021) %>% 
    group_by(indicator, agency_other) %>% 
    summarise(targets = sum(targets)) %>% 
    spread(agency_other, targets) %>%
    mutate(total = sum(CDC, Other, USAID)) %>% 
    pivot_longer(cols = CDC:USAID, names_to = "Agency", values_to = "target_tot") %>% 
    mutate(share = target_tot / total,
      target = 1)
  
  #output for each ind
  unique(df_long$indicator)  %>% 
    walk(bar_spark)
  

    