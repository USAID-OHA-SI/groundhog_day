# PROJECT:  groundhog_day
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2021-03-04
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
  library(ICPIutilities)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  saturation <- .95

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_rds()   
    
  df_nat <- si_path() %>% 
    return_latest("NAT") %>% 
    read_rds()  

# MUNGE -------------------------------------------------------------------

  #identify USAID districts
  usaid_tx_psnus <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID",
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == 2021) %>%
    distinct(psnuuid) %>% 
    pull()
  
  #extract PLHIV figures
  df_plhiv <- df_nat %>% 
    filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex"),
           fiscal_year %in% c(2020, 2021)) %>%
    rename(countryname = countrynamename) %>% 
    group_by(fiscal_year, indicator, operatingunit, countryname, snu1, psnu, psnuuid) %>% 
    summarise(value = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = c(indicator, fiscal_year)) %>% 
    rename_all(tolower)
  
  #extract FY21 TX_CURR
  df_tx_pepfar <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == 2021) %>%
    group_by(operatingunit, countryname, snu1, psnu, psnuuid) %>% 
    summarise(across(c(cumulative), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(tx_curr_2021q1 = cumulative)
  
  #join NAT + MSD data together
  df_combo <- full_join(df_plhiv, df_tx_pepfar)
  
  #filter down to just USAID districts
  df_combo_usaid <- df_combo %>% 
    filter(psnuuid %in% usaid_tx_psnus)
  
  #create coverage
  df_combo_usaid <- df_combo_usaid %>% 
    mutate(art_cov = tx_curr_subnat_2021 / plhiv_2021)
  