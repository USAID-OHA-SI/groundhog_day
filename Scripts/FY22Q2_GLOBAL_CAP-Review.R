# PROJECT:  
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2022-05-04
# UPDATED:  2022-05-16

# DATIM REPORT PARAMETERS -------------------------------------------------

# OU By IM
# DATIM data as of: 05/13/2022 21:50:51 UTC
# Genie report updated: 05/14/2022 05:08:02 UTC

# Operating Unit: Global,
# Daily/Frozen: Daily
# Indicator: HTS_TST_POS,OVC_SERV,PrEP_NEW,TX_CURR,TX_NEW,TX_PVLS,VMMC_CIRC,
# Standardized Disaggregate: Total Denominator,Total Numerator,
# Fiscal Year: 2023,2022,2021,

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
  library(lubridate)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  load_secrets("datim")
  
  ind_sel <- c("PrEP_NEW", "VMMC_CIRC", "OVC_SERV",  "HTS_TST_POS", "TX_NEW", 
               "TX_CURR", "TX_PVLS_D")

  # file_path <-  si_path() %>% return_latest("OU_IM")
  file_path <- "../Downloads/Genie-OUByIMs-Global-Daily-2022-05-16.zip"
  
# IMPORT ------------------------------------------------------------------
  
  df <- read_msd(file_path) 

  curr_fy <- identifypd(df, "year")
  curr_qtr <- identifypd(df, "quarter")
  curr_pd <- identifypd(df)
  
  msd_source <- source_info(file_path)
  
# MUNGE -------------------------------------------------------------------

  df_cap <- df %>% 
    clean_indicator() %>% 
    filter(funding_agency == "USAID",
           indicator %in% ind_sel,
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
           fiscal_year == curr_fy) %>%
    mutate(operatingunit = case_when(operatingunit == "Western Hemisphere Region" ~ "WHR",
                                     operatingunit == "Dominican Republic" ~ "DR",
                                     operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                     TRUE ~ operatingunit)) %>% 
    mutate(countryname = ifelse(country == operatingunit, country, glue("{operatingunit}/{country}")))

  df_cap <- df_cap %>% 
    group_by(fiscal_year, operatingunit, funding_agency, prime_partner_name, mech_code, indicator) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
              .groups = "drop") %>%
    reshape_msd("quarters", qtrs_keep_cumulative = TRUE) %>% 
    adorn_achievement() %>% 
    mutate(quarter = period %>% str_sub(-1) %>% as.integer, .after = fiscal_year,
           across(c(results:achv_color), ~ ifelse(indicator == "OVC_SERV" & quarter %in% c(1,3), NA_real_, .))) 

  
  df_cap <- df_cap %>% 
    mutate(flag = case_when(indicator == "OVC_SERV" & achievement_qtrly < .8 ~ TRUE,
                            indicator %in% c("TX_CURR", "TX_PVLS_D") & quarter == 1 & achievement_qtrly < .80 ~ TRUE,
                            indicator %in% c("TX_CURR", "TX_PVLS_D") & quarter == 2 & achievement_qtrly < .85 ~ TRUE,
                            indicator %in% c("TX_CURR", "TX_PVLS_D") & quarter == 3 & achievement_qtrly < .90 ~ TRUE,
                            indicator %in% c("TX_CURR", "TX_PVLS_D") & quarter == 4 & achievement_qtrly < .95 ~ TRUE,
                            achievement_qtrly < ((.25 * quarter) - .1) ~ TRUE,
                            TRUE ~ FALSE
                            )) %>% 
    group_by(fiscal_year, operatingunit, mech_code, indicator) %>% 
    mutate(flag_lag1 = lag(flag, order_by = period)) %>% 
    ungroup() %>% 
    mutate(cap = case_when(indicator == "OVC_SERV" & flag == TRUE ~ TRUE,
                           flag == TRUE & flag_lag1 == TRUE & quarter > 1 ~ TRUE,
                           TRUE ~ FALSE),
           status = case_when(cap == TRUE ~ "CAP",
                              flag == TRUE ~ "Flag",
                              TRUE ~ "Clear"))
  
  

  
  df_cap_sum <- df_cap %>% 
    filter(period == curr_pd) %>% 
    group_by(operatingunit, mech_code) %>% 
    mutate(flag_mech = max(flag, na.rm = TRUE),
           cap_mech = max(cap, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(operatingunit, mech_code) %>% 
    summarise(mech_total = 1,
              cap_mech = max(cap, na.rm = TRUE),
              cap_total = sum(cap, na.rm = TRUE),
              flag_mech = max(flag, na.rm = TRUE),
              flag_total = sum(flag, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(operatingunit) %>% 
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
    arrange(desc(cap_total)) 
  
  share_elig <- df_cap_sum %>% 
    summarise(across(c(mech_total, cap_mech), sum, na.rm = TRUE)) %>% 
    mutate(share = cap_mech/mech_total) %>% 
    pull()
  
  df_cap_sum %>% 
    ggplot(aes(mech_total, fct_reorder(operatingunit, cap_mech, .desc = FALSE))) +
    geom_col(fill = trolley_grey_light) +
    geom_col(aes(flag_mech), fill = scooter_light) +
    geom_col(aes(cap_mech), fill = scooter) +
    geom_vline(xintercept = seq(1, max(df_cap_sum$mech_total)),
               color = "white") +
    scale_x_continuous(expand = c(.005, .005), position = "top",
                       breaks = seq(5, max(df_cap_sum$mech_total), 5)) +
    labs(x = NULL, y = NULL,
         title = glue("OF ALL USAID MECHANISMS, {percent(share_elig, 1)} ARE ELIGIBLE FOR AT LEAST ONE PERFORMANCE CAP IN {curr_pd}") %>% str_wrap(40),
         caption = glue("Source: {msd_source}")) +
    si_style_nolines()
  

  si_save(glue("../Downloads/{curr_pd}_USAID_CAP-elig.svg"),
          width = 5)

# TEST --------------------------------------------------------------------


  df_cap %>% 
    filter(operatingunit == "Zambia") %>% 
    mutate(indicator = factor(indicator, ind_sel)) %>% 
    ggplot(aes(period, achievement_qtrly, fill = flag)) +
    # geom_col(aes(y = targets), fill = "red") +
    geom_col(alpha = .8) +
    scale_y_continuous(label = percent,
                       limits = c(0, 1.1),
                       oob = oob_squish) +
    scale_fill_manual(values = c("TRUE" = old_rose, "FALSE" = trolley_grey_light)) +
    facet_grid(mech_code ~ indicator, switch = "y") +
    si_style() +
    theme(strip.placement = "outside")
  