# PROJECT:  groundhog_day 
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Review CAP eligible mechanism
# REF ID:   815b7e37 
# LICENSE:  MIT
# DATE:     2022-05-04
# UPDATED:  2022-08-29

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
  
  ref_id <- "815b7e37"
  
  load_secrets("datim")
  
  ind_sel <- c("PrEP_NEW", "VMMC_CIRC", "OVC_SERV",  "HTS_TST_POS", "TX_NEW", 
               "TX_CURR", "TX_CURR_Lag2", "TX_PVLS_D", "VL Coverage")

  file_path <-  si_path() %>% return_latest("OU_IM")
  # file_path <- file.path(si_path("path_downloads"), 
  #                        "Genie-OUByIMs-Global-Daily-2022-05-16.zip")
  file_path_fsd <-  si_path() %>% return_latest("Financial")
  
  msd_source <- source_info(file_path)
  
  curr_fy <- source_info(file_path, return = "fiscal_year")
  curr_qtr <- source_info(file_path, return = "quarter")
  curr_pd <- source_info(file_path, return = "period")
  
# IMPORT ------------------------------------------------------------------
  
  df <- read_msd(file_path) 

  df_fsd <- read_msd(file_path_fsd) 
  
# MUNGE -------------------------------------------------------------------

  df_cap <- df %>% 
    pluck_totals() %>% 
    clean_indicator() %>% 
    filter(indicator %in% ind_sel,
           funding_agency != "Dedup",
           # funding_agency == "USAID",
           operatingunit != "Ukraine"
           ) %>%
    clean_agency() %>% 
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
    filter(TX_CURR_Lag2 != 0,
           !is.na(TX_PVLS_D)) %>% 
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
    group_by(funding_agency, operatingunit, mech_code) %>% 
    mutate(flag_mech = max(flag, na.rm = TRUE),
           cap_mech = max(cap, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(funding_agency, operatingunit, mech_code) %>% 
    summarise(mech_total = 1,
              cap_mech = max(cap, na.rm = TRUE),
              cap_total = sum(cap, na.rm = TRUE),
              flag_mech = max(flag, na.rm = TRUE),
              flag_total = sum(flag, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(funding_agency, operatingunit) %>% 
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
    arrange(desc(cap_mech)) 
  
  share_elig <- df_cap_sum %>% 
    group_by(funding_agency) %>% 
    summarise(across(c(mech_total, cap_mech), sum, na.rm = TRUE)) %>% 
    mutate(share = cap_mech/mech_total)
  
  ou_order <- df_cap_sum %>% 
    filter(funding_agency == "USAID") %>% 
    distinct(operatingunit, cap_mech, mech_total) %>% 
    arrange(desc(cap_mech), desc(mech_total)) %>% 
    pull(operatingunit)
    
  df_cap_sum <- df_cap_sum %>% 
    mutate(operatingunit = factor(operatingunit, ou_order))
    
  agency <- "USAID"
  
  df_cap_sum %>% 
    filter(funding_agency == agency) %>% 
    ggplot(aes(mech_total, fct_rev(operatingunit))) +
    # ggplot(aes(mech_total, fct_reorder(operatingunit, cap_mech, .desc = FALSE))) +
    geom_col(fill = trolley_grey_light) +
    geom_col(aes(flag_mech), fill = scooter_light) +
    geom_col(aes(cap_mech), fill = scooter) +
    geom_vline(xintercept = seq(1, max(df_cap_sum$mech_total)),
               color = "white") +
    scale_x_continuous(expand = c(.005, .005), position = "top",
                       breaks = seq(5, max(df_cap_sum$mech_total), 5)) +
    labs(x = NULL, y = NULL,
         title = glue("OF ALL {agency} MECHANISMS, {filter(share_elig, funding_agency == agency) %>% pull() %>% percent(1)} ARE ELIGIBLE\n FOR AT LEAST ONE PERFORMANCE CAP IN {curr_pd}"),
         caption = glue("Source: {msd_source}")) +
    si_style_nolines()
  

  si_save(glue("Graphics/{curr_pd}_USAID_CAP-elig.svg"),
          width = 5)

  si_save(glue("Images/Downloads/{curr_pd}_USAID_CAP-elig.png"),
          width = 5)

  #USAID & CDC
  df_cap_sum %>% 
    filter(funding_agency %in% c("USAID", "CDC")) %>% 
    ggplot(aes(mech_total, fct_rev(operatingunit))) +
    # ggplot(aes(mech_total, fct_reorder(operatingunit, cap_mech, .desc = FALSE))) +
    geom_col(fill = trolley_grey_light) +
    geom_col(aes(flag_mech), fill = scooter_light) +
    geom_col(aes(cap_mech), fill = scooter) +
    geom_vline(xintercept = seq(1, max(df_cap_sum$mech_total)),
               color = "white") +
    facet_wrap(~fct_rev(funding_agency)) +
    scale_x_continuous(expand = c(.005, .005), position = "top",
                       breaks = seq(5, max(df_cap_sum$mech_total), 5)) +
    labs(x = NULL, y = NULL,
         title = glue("OF ALL {agency} MECHANISMS, {filter(share_elig, funding_agency == agency) %>% pull() %>% percent(1)} ARE ELIGIBLE\n FOR AT LEAST ONE PERFORMANCE CAP IN {curr_pd}"),
         caption = glue("Source: {msd_source}")) +
    si_style_nolines() +
    theme(strip.text = element_blank())
  
  si_save(glue("Graphics/{curr_pd}_USAID_CDC_CAP-elig.svg"))
  
  
  
# MUNGE OU X INDICATOR LEVELS ---------------------------------------------

  
  df_cap_ind <- df_cap %>% 
    filter(period == curr_pd) %>% 
    group_by(funding_agency, operatingunit, mech_code, indicator) %>% 
    mutate(flag_mech = max(flag, na.rm = TRUE),
           cap_mech = max(cap, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(funding_agency, operatingunit, mech_code, indicator) %>% 
    summarise(mech_total = 1,
              cap_mech = max(cap, na.rm = TRUE),
              cap_total = sum(cap, na.rm = TRUE),
              flag_mech = max(flag, na.rm = TRUE),
              flag_total = sum(flag, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(funding_agency, operatingunit, indicator) %>% 
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
    arrange(desc(cap_total)) %>% 
    mutate(indicator = factor(indicator, ind_sel),
           operatingunit = factor(operatingunit, ou_order))
  
  df_cap_ind <- df_cap_ind %>% 
    group_by(funding_agency, indicator) %>% 
    mutate(ind_disp = glue("{indicator}\n {percent(sum(cap_mech)/sum(mech_total), 1)} ({sum(cap_mech)}/{sum(mech_total)})")) %>% 
    ungroup() %>%
    arrange(indicator) %>% 
    mutate(ind_disp = fct_inorder(ind_disp))
  
  cap_elig_ind <- df_cap_ind %>% 
    group_by(funding_agency, indicator) %>% 
    summarise(across(c(cap_mech, mech_total), sum, na.rm = TRUE)) %>% 
    filter(cap_mech == max(cap_mech)) %>% 
    ungroup()
      
  agency <- "USAID"  
  
  df_cap_ind %>% 
    filter(funding_agency == agency) %>% 
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
         title = glue("MOST OF {agency}'S CAP ELIGIBLE MECHANISM ARISE FROM {toupper(filter(cap_elig_ind, funding_agency == agency) %>% pull(indicator))} WITH {filter(cap_elig_ind, funding_agency == agency) %>% pull(cap_mech)} ELIGIBLE"),
         caption = glue("Source: {msd_source}")) +
    si_style_nolines() +
    theme(strip.placement = "outside",
          axis.text.x = element_blank(),
          panel.spacing = unit(0, "lines"))
    
  si_save(glue("Images/{curr_pd}_USAID_CAP-elig_ind.png"))
  
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
  
  

# LARGEST PARTNERS --------------------------------------------------------

  df_partners <- df_fsd %>%
    rename_official() %>% 
    filter(fiscal_year == curr_fy,
           funding_agency == "USAID",
           prime_partner_name %ni% c("TBD", "TBD (000000000)")) %>% 
    remove_mo() %>% 
    remove_sch() 
  
  # unique(df_partners$prime_partner_name) %>% sort() %>% clipr::write_clip()
  
  df_partners_clean <- df_partners %>% 
    mutate(prime_partner_name = recode(prime_partner_name,
                                       "Abt Associates Inc." = "Abt", 
                                       "Abt Associates, Inc." = "Abt", 
                                       "ANOVA HEALTH INSTITUTE" = "ANOVA",
                                       "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA" = "Baylor", 
                                       "BAYLOR COLLEGE OF MEDICINE CH ILDRENS FOUNDATION MALAWI" = "Baylor", 
                                       "Baylor College of Medicine Children's Foundation - Lesotho" = "Baylor", 
                                       "BAYLOR COLLEGE OF MEDICINE CHILDRENS FOUNDATION-UGANDA" = "Baylor", 
                                       "BROADREACH HEALTHCARE (PTY) LTD" = "BROADREACH",
                                       "DELOITTE & TOUCHE" = "Deloitte", 
                                       "DELOITTE CONSULTING LIMITED" = "Deloitte", 
                                       "Elizabeth Glaser Pediatric Aids Foundation" = "EGPAF",
                                       "Family Health International" = "FHI", 
                                       "FHI Development 360 LLC" = "FHI", 
                                       "INTRAHEALTH INTERNATIONAL, INC." = "Intrahealth", 
                                       "INTRAHEALTH NAMIBIA" = "Intrahealth", 
                                       "JHPIEGO CORPORATION" ="JHPIEGO",
                                       "JOHN SNOW HEALTH ZAMBIA LIMITED" = "JSI", 
                                       "John Snow, Incorporated" = "JSI", 
                                       "Johns Hopkins University, The" = "JSI", 
                                       "JSI Research And Training Institute, INC." = "JSI", 
                                       "Pact, Inc." = "Pact",
                                       "Project Hope-The People-To-People Health Foundation, Inc." = "Project Hope", 
                                       "PROJECT HOPE NAMIBIA" = "Project Hope", 
                                       "Population Services International" = "PSI",
                                       "RIGHT TO CARE" = "Right to Care", 
                                       "RIGHT TO CARE ZAMBIA" = "Right to Care", 
                                       "UNAIDS JOINT UNITED NATIONS PROGRAMME ON HIV/AIDS" = "UN", 
                                       "Unicef" = "UN", 
                                       "United nations office on drugs and crime" = "UN", 
                                       "World Vision Inc." = "World Vision", 
                                       "WORLD VISION KENYA" = "World Vision", 
                                       "World Vision Swaziland" = "World Vision", 
    ))
  
  
  df_partners_lrg <- df_partners_clean %>% 
    count(prime_partner_name, wt = cop_budget_total, name = "cop_ptnr_budget_total", sort = TRUE) %>% 
    slice_max(order_by = cop_ptnr_budget_total, n = 15) %>% 
    mutate(partner = ifelse(cop_ptnr_budget_total == max(cop_ptnr_budget_total),
                            glue("{toupper(prime_partner_name)}\n({str_replace((curr_fy - 1), '20', 'COP')} Budget = {scales::comma(cop_ptnr_budget_total, accuracy = 1, scale = 1e-6, prefix = '$', suffix = 'M')})"),
                            glue("{toupper(prime_partner_name)}\n({scales::comma(cop_ptnr_budget_total, accuracy = 1, scale = 1e-6, prefix = '$', suffix = 'M')})")
                            )) 
  
  df_lrg_ptnr_lst <- df_partners_clean %>% 
    tidylog::inner_join(df_partners_lrg) %>% 
    distinct(partner, mech_code)  

# MUNGE PARTNER -----------------------------------------------------------

  df_cap_pntr_sum <- df_cap %>% 
    filter(period == curr_pd) %>%
    tidylog::inner_join(df_lrg_ptnr_lst) %>% 
    group_by(funding_agency, operatingunit, partner) %>% 
    mutate(flag_mech = max(flag, na.rm = TRUE),
           cap_mech = max(cap, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(funding_agency, operatingunit, partner) %>% 
    summarise(mech_total = 1,
              cap_mech = max(cap, na.rm = TRUE),
              cap_total = sum(cap, na.rm = TRUE),
              flag_mech = max(flag, na.rm = TRUE),
              flag_total = sum(flag, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by(funding_agency, partner) %>% 
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>% 
    arrange(desc(cap_mech)) %>% 
    mutate(partner = factor(partner, df_partners_lrg$partner))
  
  cap_elig_ptnr <- df_cap_pntr_sum %>% 
    group_by(funding_agency) %>% 
    summarise(across(c(cap_mech, mech_total), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(share = cap_mech/mech_total)
  
  agency <- "USAID"
  
  df_cap_pntr_sum %>% 
    filter(funding_agency == agency) %>% 
    ggplot(aes(mech_total, fct_rev(partner))) +
    geom_col(fill = trolley_grey_light) +
    geom_col(aes(flag_mech), fill = scooter_light) +
    geom_col(aes(cap_mech), fill = scooter) +
    geom_vline(xintercept = seq(1, max(df_cap_sum$mech_total)),
               color = "white") +
    scale_x_continuous(expand = c(.005, .005), position = "top",
                       breaks = seq(5, max(df_cap_pntr_sum$mech_total), 5)) +
    labs(x = NULL, y = NULL,
         title = glue("OF {agency}'S LARGEST PARTNERS, {filter(cap_elig_ptnr, funding_agency == agency) %>% pull() %>% percent(1)} OF THE {filter(cap_elig_ptnr, funding_agency == agency) %>% pull(mech_total)} MECHANISMS ARE ELIGIBLE\n FOR AT LEAST ONE PERFORMANCE CAP IN {curr_pd}"),
         caption = glue("Source: {msd_source}")) +
    si_style_nolines()
  
  
  si_save(glue("Graphics/{curr_pd}_USAID_Large-Parner_CAP-elig.svg"),
          width = 7)
  