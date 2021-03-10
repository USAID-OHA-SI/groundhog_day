# PROJECT:  FY21Q1 Review
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Saturation by age/sex
# LICENSE:  MIT
# DATE:     2021-03-10
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
  
  # #review completeness
  #   available <- df_nat %>% 
  #     filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
  #            standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex"),
  #            psnuuid %in% usaid_tx_psnus,
  #            operatingunit != "Nigeria",
  #            !ageasentered %in% c("<15", "15+"),
  #            fiscal_year == 2021) %>% 
  #     group_by(operatingunit, psnuuid, indicator) %>% 
  #     summarise(value = sum(targets, na.rm = TRUE)) %>% 
  #     ungroup() %>% 
  #     spread(indicator, value) %>% 
  #     rename_with(tolower)
  #   
  # 
  #   df_usaid_tx <- df %>% 
  #     filter(indicator == "TX_CURR",
  #            standardizeddisaggregate %in% c("Total Numerator", "KeyPop/HIVStatus"),
  #            fundingagency == "USAID",
  #            str_detect(psnu, "_Military", negate = TRUE),
  #            fiscal_year == 2021) %>% 
  #     count(operatingunit, psnuuid, standardizeddisaggregate, wt = cumulative) %>% 
  #     spread(standardizeddisaggregate, n, fill = 0) %>% 
  #     mutate(kp_share = `KeyPop/HIVStatus` / `Total Numerator`) %>% 
  #     filter(kp_share < .9) %>% 
  #     select(operatingunit, psnuuid, tx_curr = `Total Numerator`)
  #   
  #   df_usaid_tx %>% 
  #     tidylog::full_join(available) %>%
  #     filter(str_detect(operatingunit, "Reg", negate = TRUE),
  #            !operatingunit %in% c("Nigeria", "Democratic Republic of the Congo")) %>% 
  #     View()
  
  #extract PLHIV figures
    df_plhiv <- df_nat %>% 
      filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex"),
             psnuuid %in% usaid_tx_psnus,
             str_detect(operatingunit, "Reg", negate = TRUE),
             !operatingunit %in% c("Nigeria", "Democratic Republic of the Congo"),
             !ageasentered %in% c("<15", "15+"),
             fiscal_year %in% c(2020, 2021)) %>%
      rename(countryname = countrynamename) %>% 
      group_by(fiscal_year, indicator, ageasentered, sex) %>% 
      summarise(value = sum(targets, na.rm = TRUE)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = c(indicator, fiscal_year)) %>% 
      rename_all(tolower) %>% 
      mutate(art_cov = tx_curr_subnat_2021 / plhiv_2021,
             art_cov_capped = ifelse(art_cov > 1.2, 1.21, art_cov),
             ageasentered = fct_inorder(ageasentered),
             lab_f = case_when(sex == "Female" & ageasentered %in% c("20-24", "25-29", "30-34",
                                             "35-39", "40-44", "45-49") ~ art_cov),
             lab_m = case_when(sex == "Male" & ageasentered %in% c("20-24", "25-29", "30-34",
                                                                           "35-39", "40-44", "45-49") ~ art_cov))

    df_plhiv %>% 
      ggplot(aes(art_cov, ageasentered)) +
      geom_vline(aes(xintercept = saturation), linetype = "dotted", color = trolley_grey) +
      geom_path(color = "gray50") +
      geom_point(aes(size = plhiv_2021)) +
      geom_point(aes(size = plhiv_2021, fill = sex), shape = 21, color = "white") +
      geom_text(aes(label = percent(lab_f, 1)), family = "Source Sans Pro", 
                color = trolley_grey, hjust = -.8, na.rm = TRUE) +
      geom_text(aes(label = percent(lab_m, 1)), family = "Source Sans Pro", 
                color = trolley_grey, hjust = 1.6, na.rm = TRUE) +
      scale_fill_manual(values = c("Male" = genoa, "Female" = moody_blue)) +
      scale_x_continuous(label = percent) +
      scale_size_continuous(range = c(3, 10)) +
      expand_limits(x = 1) +
      labs(x = NULL, y = NULL,
           subtitle = "Proxy ART Coverage in USAID Supported Treatment PSNUs",
           caption = "Estimated ART Coverage = FY21 TX_CURR_SUBNAT / FY21 PLHIV
           DRC removed with no PLHIV estimates for FY21; Regional programs also removed
           Nigeria removed due to discrepencies between TX_CURR_SUBNAT and TX_CURR
           Source: FY21Q1i NAT_SUBNAT SD"
           ) +
      si_style_xgrid()
    

# EXPORT ------------------------------------------------------------------

    si_save("Graphics/ART_Coverage-age-sex.svg")      
    
    
    
    