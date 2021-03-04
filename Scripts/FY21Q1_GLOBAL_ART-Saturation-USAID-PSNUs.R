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
  library(svglite)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  saturation <- .95*.95

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
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    rename(tx_curr_2021_q1 = cumulative,
           tx_curr_2021_targets = targets)
  
  #join NAT + MSD data together
  df_combo <- full_join(df_plhiv, df_tx_pepfar)
  
  #filter down to just USAID districts
  df_combo_usaid <- df_combo %>% 
    filter(psnuuid %in% usaid_tx_psnus)
  
  #create coverage
  df_combo_usaid <- df_combo_usaid %>% 
    mutate(art_cov = tx_curr_subnat_2021 / plhiv_2021,
           art_cov_capped = ifelse(art_cov > 1.1, 1.11, art_cov)) %>% 
    group_by(operatingunit) %>% 
    mutate(target_share = tx_curr_2021_targets/sum(tx_curr_2021_targets, na.rm = TRUE)) %>% 
    ungroup()
  
  #overall saturation
  df_combo_usaid <- df_combo_usaid %>%
    filter(operatingunit != "DRC") %>% 
    group_by(operatingunit) %>% 
    mutate(art_cov_ou = sum(tx_curr_subnat_2021, na.rm = TRUE)/sum(plhiv_2021, na.rm = TRUE),
           art_cov_ou = ifelse(is.nan(art_cov_ou), NA, art_cov_ou),
           art_cov_ou_m = case_when(plhiv_2021 == max(plhiv_2021, na.rm = TRUE) ~ art_cov_ou)) %>%
    ungroup() 
  
  #clean up names
  df_viz <- df_combo_usaid %>% 
    mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                     operatingunit =="Dominican Republic" ~ "DR",
                                     operatingunit == "Western Hemisphere Region" ~ "WHR",
                                     TRUE ~ operatingunit))
  
  
  #flag
  df_viz <- df_viz %>% 
    mutate(flag = case_when(art_cov < saturation ~ moody_blue,
                            TRUE ~ trolley_grey),
           flag_plus = case_when(art_cov < saturation & target_share < .15 ~ moody_blue,
                                 art_cov < saturation & target_share > .15 ~ scooter,
                                 TRUE ~ trolley_grey),
           flag_label = case_when(art_cov < saturation & target_share > .15 ~ psnu))
  

# PLOT --------------------------------------------------------------------

  df_viz %>% 
    ggplot(aes(art_cov_capped, fct_reorder(operatingunit, art_cov_ou_m, na.rm = TRUE),
               fill = flag_plus)) +
    geom_vline(aes(xintercept = saturation), linetype = "dotted", color = trolley_grey) +
    geom_vline(aes(xintercept = 1.11), color = trolley_grey_light) +
    geom_errorbar(aes(xmin = art_cov_ou_m, xmax = art_cov_ou_m), size = 1.5, color = trolley_grey) +
    geom_jitter(aes(size = target_share), height = .3,  shape = 21, alpha = .6, color = "white", na.rm = TRUE) +
    geom_text(aes(label = flag_label), na.rm = TRUE,
              family = "Source Sans Pro", color = "#505050", size = 3) +
    scale_x_continuous(label = percent_format(1), breaks = seq(0, 1.1, by = .2)) +
    scale_fill_identity() +
    labs(x = NULL, y = NULL,
         subtitle = "Estimated ART coverage in USAID supported treament PSNUs",
         caption = "Estimated ART Coverage = FY21 TX_CURR_SUBNAT / FY21 PLHIV,
         DRC removed with no PLHIV estimates for FY21
         Source: FY21Q1i NAT_SUBNAT + MSD") +
    si_style() +
    theme(legend.position = "none")


# EXPORT ------------------------------------------------------------------

  si_save("Graphics/ART_Coverage.svg")  
    