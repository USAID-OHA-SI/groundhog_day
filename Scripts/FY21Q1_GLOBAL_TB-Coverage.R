# PROJECT:  FY21Q1 Review
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  inspect TB coverage rates for FY21Q1
# LICENSE:  MIT
# DATE:     2021-03-05
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)


# GLOBAL VARIABLES --------------------------------------------------------
  
  threshold <- .9

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
  

# MUNGE -------------------------------------------------------------------

  df_art <- df %>% 
    filter(fundingagency == "USAID",
           indicator %in% c("TB_STAT", "TB_ART"),
           standardizeddisaggregate %in% c("Total Denominator","Total Numerator"),
           fiscal_year == 2021) %>% 
    group_by(operatingunit, indicator, numeratordenom) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = numeratordenom,
                values_from = cumulative) %>% 
    mutate(N = ifelse(is.na(N), 0, N),
           coverage = N/D,
           flag = coverage < threshold)
  

# PLOT --------------------------------------------------------------------

  
  v_stat <- df_art %>% 
    mutate(indicator = ifelse(indicator == "TB_STAT", "TB_STAT_D", indicator)) %>% 
    filter(indicator == "TB_STAT_D",
           operatingunit != "Cote d'Ivoire") %>% 
    ggplot(aes(operatingunit, coverage, width = D, fill = flag)) +
    geom_col(color = "white", size = 1, alpha = .9) +
    geom_hline(yintercept = 0, color = trolley_grey) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = trolley_grey) +
    facet_grid(~fct_reorder(operatingunit, coverage, .desc = TRUE), scales = "free_x", space = "free_x") +
    scale_fill_manual(values = c("#5BB5D5", burnt_sienna_light)) +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(panel.spacing.x = unit(0, "npc"),
          axis.text = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_text(hjust = .5),
          legend.position = "none")
  
  v_art <- df_art %>% 
    mutate(indicator = ifelse(indicator == "TB_ART", "TB_STAT_POS", indicator)) %>% 
    filter(indicator == "TB_STAT_POS",
           operatingunit != "Cote d'Ivoire") %>% 
    ggplot(aes(operatingunit, coverage, width = D, fill = flag)) +
    geom_col(color = "white", size = 1, alpha = .9) +
    geom_hline(yintercept = 0, color = trolley_grey) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = trolley_grey) +
    facet_grid(~fct_reorder(operatingunit, coverage, .desc = TRUE), scales = "free_x", space = "free_x") +
    scale_fill_manual(values = c("#5BB5D5", burnt_sienna_light)) +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(panel.spacing.x = unit(0, "npc"),
          axis.text = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_text(hjust = .5),
          legend.position = "none")
  
  #combine
    v_stat / v_art
  

# EXPORT ------------------------------------------------------------------

    si_save("Images/FY21Q1_TB.png")