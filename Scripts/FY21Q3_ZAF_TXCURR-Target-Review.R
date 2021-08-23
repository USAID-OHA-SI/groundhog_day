# PROJECT:  groundhogday
# AUTHOR:   A.Chafetz, K.Srikanth | USAID
# PURPOSE:  Taget/Buget Share
# LICENSE:  MIT
# DATE:     2021-08-23
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  source <- source_info()

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_rds()   
  
  df_archive <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_rds()   
  
  df_genie <- read_msd("Data/Genie-OUByIMs-Global-Daily-2021-08-23.zip")


# MUNGE -------------------------------------------------------------------

  #bind all data together
  df_all <- bind_rows(df_archive, df, df_genie)    
  
  #
  df_all <- df_all %>% 
    bind_rows(df_all %>% mutate(operatingunit = "Global")) %>% 
    filter(operatingunit %in% c("South Africa", "Global"),
           fundingagency %in% c("USAID", "HHS/CDC"),
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year >= 2016,
           mech_code != "16772") %>%
    count(fiscal_year, operatingunit, fundingagency, indicator,
          wt = targets, name = "targets") %>% 
    clean_agency()
  

  df_all <- df_all %>% 
    group_by(fiscal_year, operatingunit) %>% 
    mutate(share = targets/sum(targets)) %>% 
    ungroup()
  
  df_all <- df_all %>% 
    mutate(fill_color = case_when(fundingagency == "USAID" ~ usaid_blue,
                                  fundingagency == "CDC" ~ usaid_lightblue))
  
  df_all <- df_all %>% 
    mutate(fundingagency = fct_rev(fundingagency),
           fill_color = factor(fill_color, c(usaid_lightblue, usaid_blue)))
  
  df_all <- df_all %>% 
    mutate(lab = case_when(fiscal_year == max(fiscal_year) ~ glue("{number(targets, scale = 1e-6, accuracy = .1, suffix = 'M')} {fundingagency}")))
  
  df_all <- df_all %>% 
    mutate(lab_share = case_when(fiscal_year %in% c(min(fiscal_year), max(fiscal_year)) & fundingagency == "USAID" ~ percent(share, 1)))
  
  df_all %>% 
    ggplot(aes(fiscal_year, targets, color = fill_color)) +
    geom_line(size = 1.2) +
    geom_point(data = df_all %>% filter(fiscal_year %in% c(min(fiscal_year), max(fiscal_year))),
               size = 4) +
    geom_text(data = df_all %>% filter(fiscal_year == min(fiscal_year)),
               aes(label = number(targets, scale = 1e-6, accuracy = .1, suffix = "M")),
              family = "Source Sans Pro", size = 9/.pt, hjust = 1.4) +
    geom_text(aes(label = lab), na.rm = TRUE, family = "Source Sans Pro", size = 9/.pt, hjust = -.2) +
    coord_cartesian(clip = "off") +
    expand_limits(y = 0, x = c(2015.75, 2022.5)) +
    scale_y_continuous(label = label_number_si()) + 
    scale_x_continuous(breaks = c(2016:2022)) +
    scale_color_identity() +
    facet_wrap(~operatingunit, scales = "free_y") +
    labs(x = NULL, y = NULL,
         caption = "Note: TX_CURR Targets for USAID and CDC only
         Targets for COP21 are still provisional
         Source: FY21Q3i MSD + COP21 Targets from Genie [2021-08-23]
         US Agency for International Development") +
    si_style() +
    theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))
  
  si_save("Graphics/ZAF_TX_CURR_target_trends.svg")
  
  df_all %>% 
    ggplot(aes(fiscal_year, share, fill = fill_color)) +
    geom_area(alpha = .75) +
    geom_text(aes(label = lab_share), na.rm = TRUE,
              family = "Source Sans Pro", size = 9/.pt, color = "white") +
    geom_hline(yintercept = c(.25, .5, .75, 1), color = "white", linetype = "dotted") +
    scale_y_continuous(label = percent) + 
    scale_x_continuous(n.breaks = 7) +
    scale_fill_identity() +
    facet_wrap(~operatingunit, scales = "free_y") +
    expand_limits(y = 0) +
    labs(x = NULL, y = NULL,
         caption = "Note: TX_CURR Targets for USAID and CDC only | COP21 targets are still provisional
         Source: FY21Q3i MSD + COP21 Targets from Genie [2021-08-23]
         US Agency for International Development") +
    si_style_nolines() +
    theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))
    
  si_save("Graphics/ZAF_TX_CURR_targetshare_trends.svg")
  