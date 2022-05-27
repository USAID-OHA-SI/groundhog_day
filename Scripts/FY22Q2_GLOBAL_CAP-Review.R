# PROJECT:  
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  
# LICENSE:  MIT
# DATE:     2022-05-04
# UPDATED:  2022-05-27

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
               "TX_CURR", "TX_CURR_Lag2", "TX_PVLS_D", "VL Coverage")

  file_path <-  si_path() %>% return_latest("OU_IM")
  # file_path <- file.path(si_path("path_downloads"), 
  #                        "Genie-OUByIMs-Global-Daily-2022-05-16.zip")
  
  msd_source <- source_info(file_path)
  
  curr_fy <- source_info(file_path, return = "fiscal_year")
  curr_qtr <- source_info(file_path, return = "quarter")
  curr_pd <- source_info(file_path, return = "period")
  
# IMPORT ------------------------------------------------------------------
  
  df <- read_msd(file_path) 

# MUNGE -------------------------------------------------------------------

  df_cap <- df %>% 
    pluck_totals() %>% 
    clean_indicator() %>% 
    filter(funding_agency == "USAID",
           indicator %in% ind_sel) %>%
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
    calc_achievement() %>%
    mutate(quarter = period %>% str_sub(-1) %>% as.integer, .after = fiscal_year,
           across(c(results:achievement_qtrly), ~ ifelse(indicator == "OVC_SERV" & quarter %in% c(1,3), NA_real_, .))) 

  df_cap_vlc <- df_cap %>%
    select(-c(targets, results_cumulative, achievement_qtrly)) %>% 
    filter(indicator %in% c("TX_CURR_Lag2", "TX_PVLS_D")) %>% 
    pivot_wider(names_from = "indicator",
                values_from = "results") %>%
    mutate(indicator = "VL Coverage",
           achievement_qtrly = TX_PVLS_D / TX_CURR_Lag2) %>% 
    select(-starts_with("TX"))
  
  df_cap <- df_cap %>% 
    filter(indicator %ni% c("TX_CURR_Lag2", "TX_PVLS_D")) %>% 
    bind_rows(df_cap_vlc) %>% 
    mutate(flag = case_when(indicator == "OVC_SERV" & achievement_qtrly < .8 ~ TRUE,
                            indicator == "VL Coverage" & achievement_qtrly < .85 ~ TRUE,
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
  
  df_cap <- df_cap %>% 
    filter(fiscal_year == curr_fy)
  

# MUNGE OU LEVEL TOPS -----------------------------------------------------

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
    arrange(desc(cap_mech)) 
  
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


# MUNGE OU X INDICATOR LEVELS ---------------------------------------------

  df_cap_ind <- df_cap %>% 
    filter(period == curr_pd) %>% 
    group_by(operatingunit, mech_code, indicator) %>% 
    mutate(flag_mech = max(flag, na.rm = TRUE),
           cap_mech = max(cap, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(operatingunit, mech_code, indicator) %>% 
    summarise(mech_total = 1,
              cap_mech = max(cap, na.rm = TRUE),
              cap_total = sum(cap, na.rm = TRUE),
              flag_mech = max(flag, na.rm = TRUE),
              flag_total = sum(flag, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(operatingunit, indicator) %>% 
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
    arrange(desc(cap_total)) %>% 
    mutate(indicator = factor(indicator, ind_sel),
           operatingunit = factor(operatingunit, df_cap_sum$operatingunit))
  
  df_cap_ind <- df_cap_ind %>% 
    group_by(indicator) %>% 
    mutate(ind_disp = glue("{indicator}\n {percent(sum(cap_mech)/sum(mech_total), 1)} ({sum(cap_mech)}/{sum(mech_total)})")) %>% 
    ungroup() %>%
    arrange(indicator) %>% 
    mutate(ind_disp = fct_inorder(ind_disp))
  
  cap_elig_ind <- df_cap_ind %>% 
    group_by(indicator) %>% 
    summarise(across(c(cap_mech, mech_total), sum, na.rm = TRUE), .groups = "drop") %>% 
    filter(cap_mech == max(cap_mech))
              
  df_cap_ind %>% 
    ggplot(aes(mech_total, fct_rev(operatingunit))) +
    # ggplot(aes(mech_total, fct_reorder(operatingunit, cap_mech, sum, .desc = FALSE))) +
    geom_col(fill = trolley_grey_light) +
    geom_col(aes(flag_mech), fill = scooter_light) +
    geom_col(aes(cap_mech), fill = scooter) +
    geom_vline(xintercept = seq(1, max(df_cap_ind$mech_total)),
               color = "white") +
    facet_grid(~ind_disp) +
    scale_x_continuous(expand = c(.005, .005), position = "top",
                       breaks = seq(5, max(df_cap_ind$mech_total), 5)) +
    labs(x = NULL, y = NULL,
         title = glue("MOST OF USAID'S CAP ELIGIBLE MECHANISM ARISE FROM {toupper(cap_elig_ind$indicator)} WITH {cap_elig_ind$cap_mech} ELIGIBLE"),
         caption = glue("Source: {msd_source}")) +
    si_style_nolines() +
    theme(strip.placement = "outside",
          axis.text.x = element_blank(),
          panel.spacing = unit(.5, "lines"))
    
# CHECK -------------------------------------------------------------------

  df_cap %>% 
    filter(operatingunit == "Zambia") %>% 
    mutate(indicator = factor(indicator, ind_sel)) %>% 
    ggplot(aes(period, achievement_qtrly, fill = flag)) +
    geom_col(alpha = .8) +
    scale_y_continuous(label = percent,
                       limits = c(0, 1.1),
                       oob = oob_squish) +
    scale_fill_manual(values = c("TRUE" = old_rose, "FALSE" = trolley_grey_light)) +
    facet_grid(mech_code ~ indicator, switch = "y") +
    si_style() +
    theme(strip.placement = "outside")
  