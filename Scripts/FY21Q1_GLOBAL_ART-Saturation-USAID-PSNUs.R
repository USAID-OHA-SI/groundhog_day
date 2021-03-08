# PROJECT:  groundhog_day
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2021-03-04
# UPDATED:  2021-03-08

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(ggrepel)
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

  #identify USAID districts (remove KP districts)
  usaid_tx_psnus <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate %in% c("Total Numerator", "KeyPop/HIVStatus"),
           fundingagency == "USAID",
           str_detect(psnu, "_Military", negate = TRUE),
           fiscal_year == 2021) %>% 
    count(operatingunit, psnuuid, standardizeddisaggregate, wt = cumulative) %>% 
    spread(standardizeddisaggregate, n, fill = 0) %>% 
    mutate(kp_share = `KeyPop/HIVStatus` / `Total Numerator`) %>% 
    filter(kp_share < .9) %>%
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
           art_cov_capped = ifelse(art_cov > 1.2, 1.21, art_cov)) %>% 
    group_by(operatingunit) %>% 
    mutate(target_share = tx_curr_2021_targets/sum(tx_curr_2021_targets, na.rm = TRUE)) %>% 
    ungroup()
  
  #overall saturation
  df_combo_usaid <- df_combo_usaid %>%
    filter(operatingunit != "Democratic Republic of the Congo") %>% 
    group_by(operatingunit) %>% 
    mutate(art_cov_ou = sum(tx_curr_subnat_2021, na.rm = TRUE)/sum(plhiv_2021, na.rm = TRUE),
           art_cov_ou = ifelse(is.nan(art_cov_ou), NA, art_cov_ou),
           art_cov_ou_m = case_when(plhiv_2021 == max(plhiv_2021, na.rm = TRUE) ~ art_cov_ou)) %>%
    ungroup() %>% 
    mutate(plhiv_share = plhiv_2021/sum(plhiv_2021, na.rm = TRUE))
  
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
           flag_label = case_when(art_cov < saturation & target_share > .2 ~ psnu),
           flag_plhiv_plus = case_when(art_cov < saturation & plhiv_share < .01 ~ moody_blue,
                                 art_cov < saturation & plhiv_share >= .01 ~ scooter,
                                 TRUE ~ trolley_grey),
           flag_plhiv_label = case_when(art_cov < saturation & plhiv_share >= .01 ~ psnu),
           flag_plhiv_label = flag_plhiv_label %>% 
             str_remove("^[:lower:]{2} ") %>% 
             str_remove_all(" (County|District|Metropolitan|Municipality)") %>% 
             str_remove("City of ") %>% 
             na_if("Data reported above PNSU Level")
           )
  
  #drop regional missions
  df_viz <- df_viz %>% 
    filter(!operatingunit %in% c("West Africa Region", "Asia Region", "WHR"))
  
  #create high vs low burden
  df_viz <- df_viz %>% 
    group_by(operatingunit) %>% 
    mutate(burden = ifelse(sum(plhiv_2021, na.rm = TRUE) > 500000, "High PLHIV Burden", "Low")) %>% 
    ungroup() 
  
  #ou range
  df_viz <- df_viz %>% 
    group_by(operatingunit) %>% 
    mutate(art_cov_ou_min = min(art_cov_capped, na.rm = TRUE),
           art_cov_ou_min = case_when(plhiv_2021 == max(plhiv_2021, na.rm = TRUE) ~ art_cov_ou_min),
           art_cov_ou_max = max(art_cov_capped, na.rm = TRUE),
           art_cov_ou_max = case_when(plhiv_2021 == max(plhiv_2021, na.rm = TRUE) ~ art_cov_ou_max)) %>% 
    ungroup()
  
    
# PLOT --------------------------------------------------------------------

  # df_viz %>% 
  #   ggplot(aes(art_cov_capped, fct_reorder(operatingunit, art_cov_ou_m, na.rm = TRUE, .desc = TRUE),
  #              fill = flag_plus)) +
  #   geom_vline(aes(xintercept = saturation), linetype = "dotted", color = trolley_grey) +
  #   # geom_vline(aes(xintercept = 1.21), color = trolley_grey_light) +
  #   geom_errorbar(aes(xmin = art_cov_ou_m, xmax = art_cov_ou_m), size = 1.5, color = trolley_grey) +
  #   geom_jitter(aes(size = target_share), height = .3,  shape = 21, alpha = .6, color = "white", na.rm = TRUE) +
  #   geom_text(aes(label = flag_label), na.rm = TRUE,
  #             family = "Source Sans Pro", color = "#505050", size = 3) +
  #   scale_x_continuous(label = percent_format(1), breaks = seq(0, 1.2, by = .25)) +
  #   scale_fill_identity() +
  #   labs(x = NULL, y = NULL,
  #        subtitle = "Estimated ART coverage in USAID supported treament PSNUs",
  #        caption = "Estimated ART Coverage = FY21 TX_CURR_SUBNAT / FY21 PLHIV,
  #        DRC removed with no PLHIV estimates for FY21
  #        Source: FY21Q1i NAT_SUBNAT + MSD") +
  #   si_style() +
  #   theme(legend.position = "none")
  
  
  # df_viz %>% 
  #   ggplot(aes(art_cov_capped, fct_reorder(operatingunit, art_cov_ou_m, na.rm = TRUE, .desc = TRUE),
  #              fill = flag_plus), na.rm = TRUE) +
  #   geom_linerange(aes(xmin = art_cov_ou_min, xmax = art_cov_ou_max), na.rm = TRUE, size = .5, color = "gray80") +
  #   geom_vline(aes(xintercept = saturation), linetype = "dotted", color = trolley_grey) +
  #   geom_vline(aes(xintercept = 1.21), color = trolley_grey_light) +
  #   geom_errorbar(aes(xmin = art_cov_ou_m, xmax = art_cov_ou_m), size = 1.5, color = trolley_grey) +
  #   geom_jitter(aes(size = target_share), height = .3,  shape = 21, alpha = .6, color = "white", na.rm = TRUE) +
  #   geom_text(aes(label = flag_label), na.rm = TRUE,
  #             family = "Source Sans Pro", color = "#505050", size = 3) +
  #   scale_x_continuous(label = percent_format(1), breaks = seq(0, 1.2, by = .25)) +
  #   scale_fill_identity() +
  #   labs(x = NULL, y = NULL,
  #        subtitle = "Estimated ART coverage in USAID supported treament PSNUs",
  #        caption = "Estimated ART Coverage = FY21 TX_CURR_SUBNAT / FY21 PLHIV,
  #        DRC removed with no PLHIV estimates for FY21
  #        Source: FY21Q1i NAT_SUBNAT + MSD") +
  #   si_style_nolines() +
  #   theme(legend.position = "none")
  
  df_viz %>% 
    ggplot(aes(art_cov_capped, fct_reorder(operatingunit, art_cov_ou_m, na.rm = TRUE, .desc = TRUE),
               fill = flag_plhiv_plus), na.rm = TRUE) +
    geom_linerange(aes(xmin = art_cov_ou_min, xmax = art_cov_ou_max), na.rm = TRUE, size = .5, color = "gray80") +
    geom_vline(aes(xintercept = saturation), linetype = "dotted", color = trolley_grey) +
    geom_vline(aes(xintercept = 1.21), color = trolley_grey_light) +
    geom_errorbar(aes(xmin = art_cov_ou_m, xmax = art_cov_ou_m), size = 1.5, color = trolley_grey) +
    geom_jitter(aes(size = plhiv_2021), height = .3,  shape = 21, alpha = .6, color = "white", na.rm = TRUE) +
    geom_text_repel(aes(label = flag_plhiv_label), na.rm = TRUE,
              family = "Source Sans Pro", color = "#505050", size = 3) +
    facet_grid(burden ~ ., scales = "free_y", space = "free") +
    scale_x_continuous(label = percent_format(1), breaks = seq(0, 1.2, by = .25)) +
    scale_fill_identity() +
    labs(x = NULL, y = NULL,
         subtitle = "Estimated ART coverage in USAID supported treament PSNUs",
         caption = "Estimated ART Coverage = FY21 TX_CURR_SUBNAT / FY21 PLHIV
         High PLHIV Burden OUs = Total PLHIV (2021) in USAID districts is greater than 500,000
         DRC removed with no PLHIV estimates for FY21;Regional programs also removed
         KP only districts (+90% of the TX_CURR results were in the KP disagg) were excluded
         Source: FY21Q1i NAT_SUBNAT + MSD") +
    si_style_nolines() +
    theme(legend.position = "none",
          panel.spacing.y = unit(.5, "lines"))
  
  
# EXPORT ------------------------------------------------------------------

  si_save("Graphics/ART_Coverage_v3.svg")  

  
  df_viz %>% 
    count(art_cov > saturation)
  
  df_viz %>% 
    count(art_cov > 1.2)
  
  df_viz %>% 
    filter(operatingunit == "South Africa",
           (art_cov < saturation & plhiv_share >= .01)) %>% 
    select(psnu, flag_plhiv_label, plhiv_2021) %>% 
    arrange(desc(plhiv_2021))
  
  df_viz %>% 
    filter(operatingunit == "South Africa",
           !is.na(flag_plhiv_label)) %>% 
    select(psnu, flag_plhiv_label, plhiv_2021) %>% 
    arrange(desc(plhiv_2021)) %>% 
    pull(flag_plhiv_label) %>% 
    paste0(collapse = ", ")
  
  
  df_viz %>% 
    filter(operatingunit == "Nigeria") %>%
    select(psnu, plhiv_2021, tx_curr_subnat_2021, tx_curr_2021_q1, art_cov) %>% 
    arrange(desc(art_cov))
    glimpse()
  