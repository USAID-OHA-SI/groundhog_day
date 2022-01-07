# PROJECT:  groundhogday
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  valuing the size of a site shift
# LICENSE:  MIT
# DATE:     2022-01-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(vroom)
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
  
  nn_path <- "../right_size/Dataout/TX_CURR_NN_Calcs.csv"
  
  authors <- c("Aaron Chafetz")
  
  source <- "Source: FY21Q4i DATIM Pull [2021-11-12] + site adjustments, excluded mech 16772"

# IMPORT ------------------------------------------------------------------

  df_msd <- si_path() %>% 
    return_latest("PSNU_IM") %>% 
    read_rds() %>% 
    filter(operatingunit == "South Africa",
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year %in% c(2020, 2021))
  
    
  df <- vroom(nn_path)


  glimpse(df)  
  
  df_zaf <- df %>% 
    filter(str_detect(period, "18", negate = TRUE),
           operatingunit == "South Africa",
           mech_code != "16772") %>% 
    clean_agency()

  df_zaf_agg <- df_zaf %>% 
    group_by(period) %>% 
    summarise(across(c(tx_curr, tx_net_new, tx_net_new_adj_plus), sum, na.rm = TRUE),
              .groups = "drop")
  
  df_zaf_agg %>% 
    ggplot(aes(period, tx_curr)) +
    geom_hline(yintercept = 0) +
    geom_col(fill = grey50k) +
    geom_errorbar(aes(ymin = tx_net_new, ymax = tx_net_new), size = 1.5, color = burnt_sienna) +
    scale_y_continuous(labels = label_number_si()) +
    labs(x = NULL, y = NULL,
         title = "Relatively constant treatment growth in South Africa since mid-FY20" %>% toupper,
         subtitle = glue("South Africa | <span style='color:{grey50k}'>**TX_CURR**</span> and <span style='color:{burnt_sienna}'>**TX_NET_NEW**</span>"),
         caption =  glue("{source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          plot.subtitle = element_markdown())
  
  si_save("Images/FY21Q4_ZAF_txcurr-nn-trend.png")
  
  
  
  df_zaf_agency <- df_zaf %>%
    filter(mech_code %ni% c("00000", "00001")) %>% 
    group_by(period, fundingagency) %>% 
    summarise(across(c(tx_curr, tx_net_new, tx_net_new_adj_plus), sum, na.rm = TRUE),
              .groups = "drop")
  
  df_zaf_agency %>% 
    ggplot(aes(period, tx_curr, fill = fundingagency)) +
    geom_hline(yintercept = 0) +
    geom_col(alpha = .7) +
    geom_col(data = . %>% filter(period == "FY21Q1")) +
    geom_errorbar(aes(ymin = tx_net_new, ymax = tx_net_new), size = 1.5, color = burnt_sienna) +
    facet_grid(fct_rev(fundingagency)~., switch = "y") +
    scale_y_continuous(labels = number_format(.1, scale = 1e-6, suffix = "M")) +
    scale_fill_manual(values = c("CDC" = scooter, "USAID" = denim)) +
    labs(x = NULL, y = NULL,
         title = "large agency transition at the start of FY21 resulted in an false signal in NET_NEW" %>% toupper,
         subtitle = glue("South Africa | <span style='color:{denim}'>**USAID**</span>/<span style='color:{scooter}'>**CDC**</span> TX_CURR and <span style='color:{burnt_sienna}'>**TX_NET_NEW**</span>"),
         caption =  glue("{source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          plot.subtitle = element_markdown(),
          strip.placement = "outside",
          strip.text = element_text(face = "bold", hjust = .5))
  
  si_save("Images/FY21Q4_ZAF-agency_txcurr-nn-trend.png")
  
  
  
  
  
  df_zaf_agency_psnu <- df_zaf %>%
    filter(mech_code %ni% c("00000", "00001")) %>% 
    group_by(period, fundingagency, psnu) %>% 
    summarise(across(c(tx_curr, tx_net_new, tx_net_new_adj_plus), sum, na.rm = TRUE),
              .groups = "drop")
  
  curr_psnus <- df_zaf_agency_psnu %>% 
    filter(period == max(period)) %>% 
    count(psnu, wt = tx_curr, sort = TRUE) %>%
    mutate(psnu_clean = psnu %>% 
             str_extract("(?<=[:lower:]{2} ).*") %>% 
             str_remove_all(" (District|Metropolitan|Municipality|Municipalit)"),
           psnu_adj = fct_lump(psnu_clean, n = 27, w  = n),
           psnu_adj = fct_reorder(psnu_adj, n, .desc = TRUE))
  
  df_zaf_agency_psnu <- df_zaf_agency_psnu %>% 
    inner_join(curr_psnus)
    
  pds <- df_zaf_agency_psnu %>% 
    distinct(period) %>% 
    mutate(period = ifelse(str_detect(period, "Q1"), str_sub(period, end = 4), "")) %>% 
    pull()
    
    
  df_zaf_agency_psnu %>% 
    ggplot(aes(period, tx_curr, fill = fundingagency)) +
    geom_hline(yintercept = 0) +
    geom_col(alpha = .7) +
    facet_wrap(~psnu_adj) +
    scale_y_continuous(labels = number_format(1, scale = 1e-3, suffix = "K")) +
    scale_fill_manual(values = c("CDC" = scooter, "USAID" = denim)) +
    scale_x_discrete(labels= pds) + 
    labs(x = NULL, y = NULL,
         title = "The resulting agency level 'negative' net_new is casued by the transition in ethekwini, the largest treatment municipality" %>% toupper,
         subtitle = glue("South Africa | <span style='color:{denim}'>**USAID**</span>/<span style='color:{scooter}'>**CDC**</span> TX_CURR"),
         caption =  glue("{source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          plot.subtitle = element_markdown(),
          panel.spacing.x = unit(.2, "lines"),
          panel.spacing.y = unit(.1, "lines"))
  
  
  si_save("Images/FY21Q4_ZAF-agency-psnu_txcurr-trend.png")
  
  
  
  
  
  df_zaf_psnu_nn_q4 <- df_zaf %>% 
    filter(mech_code %ni% c("00000", "00001"),
           period == "FY21Q1") %>% 
    group_by(period, fundingagency, psnu) %>% 
    summarise(across(c(tx_curr, tx_net_new, tx_net_new_adj_plus), sum, na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(share = abs(tx_net_new)/tx_curr,
           psnu_clean = psnu %>% 
             str_extract("(?<=[:lower:]{2} ).*") %>% 
             str_remove_all(" (District|Metropolitan|Municipality|Municipalit)"),
           psnu_clean = reorder_within(psnu_clean, tx_net_new, fundingagency, sum))
  
  
  df_zaf %>% 
    filter(period == "FY19Q4",
           fundingagency == "USAID"
           ) %>% 
    count(period, psnu, wt = tx_curr, sort = TRUE) %>% 
    mutate(share = n/sum(n))
    
    
  
  
  
  df_zaf_psnu_nn_q4 %>% 
    ggplot(aes(psnu_clean, tx_net_new, fill = fundingagency)) +
    geom_col() +
    facet_grid(fct_rev(fundingagency)~ ., scales = "free_x") +
    scale_x_reordered() +
    scale_fill_manual(values = c("CDC" = scooter, "USAID" = denim)) +
    si_style() +
    theme(legend.position = "none")
    
  
  
  
  df_zaf_agency_exclude <- df_zaf %>%
    filter(mech_code %ni% c("00000", "00001"),
           !(psnu == "kz eThekwini Metropolitan Municipality" & period == "FY21Q1")) %>% 
    group_by(period, fundingagency) %>% 
    summarise(across(c(tx_curr, tx_net_new, tx_net_new_adj_plus), sum, na.rm = TRUE),
              .groups = "drop")
  
  df_zaf_agency_exclude %>% 
    ggplot(aes(period, tx_net_new, fill = fundingagency)) +
    geom_hline(yintercept = 0) +
    geom_col(alpha = .7, fill = burnt_sienna) +
    # geom_col(data = . %>% filter(period == "FY21Q1")) +
    geom_errorbar(aes(ymin = tx_net_new, ymax = tx_net_new), size = 1.5, color = burnt_sienna) +
    facet_grid(fct_rev(fundingagency)~., switch = "y") +
    scale_y_continuous(labels = number_format(1, scale = 1e-3, suffix = "K")) +
    scale_fill_manual(values = c("CDC" = scooter, "USAID" = denim)) +
    labs(x = NULL, y = NULL,
         title = "Excluding eThekwini gains/losses in FY21Q1, both agencies had positive NET_NEW each quarter of FY21" %>% toupper,
         subtitle = glue("South Africa excluding eThekwini FY21Q1 | <span style='color:{burnt_sienna}'>**TX_NET_NEW**</span>"),
         caption =  glue("{source}
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          plot.subtitle = element_markdown(),
          strip.placement = "outside",
          strip.text = element_text(face = "bold", hjust = .5))
  

  si_save("Images/FY21Q4_ZAF-no-eThekwini-agency-nn-trend.png")

  
  df_zaf %>%
    filter(mech_code %ni% c("00000", "00001"),
           !(psnu == "kz eThekwini Metropolitan Municipality" & period == "FY21Q1")) %>% 
    group_by(period, fundingagency) %>% 
    summarise(across(c(tx_curr, tx_net_new, tx_net_new_adj_plus), sum, na.rm = TRUE),
              .groups = "drop")
  
  
  
  # df_zaf %>%
  #   filter(fundingagency == "USAID",
  #          psnu == "kz eThekwini Metropolitan Municipality",
  #          period %in% c("FY20Q4", "FY21Q1")) %>% 
  #   select(facility, mech_code, tx_curr, period) %>% 
  #   pivot_wider(names_from = period,
  #               values_from = tx_curr)
  
  
  
  
  df_msd_nn <- df_msd %>% 
    filter(fundingagency != "Dedup",
           mech_code != "16772") %>% 
    clean_agency() %>% 
    mutate(fundingagency = ifelse(psnu == "kz eThekwini Metropolitan Municipality" & fiscal_year == 2020, "CDC", fundingagency)) %>% 
    group_by(fiscal_year, psnu, fundingagency,) %>% 
    summarize(across(c(targets, cumulative), sum, na.rm = TRUE),
              .groups = "drop") %>% 
    arrange(psnu, fundingagency, fiscal_year) %>% 
    group_by(psnu, fundingagency) %>% 
    mutate(nn_cumulative = cumulative - lag(cumulative, order_by = fiscal_year),
           nn_targets = targets - lag(cumulative, order_by = fiscal_year)) %>% 
    filter(fiscal_year == 2021) %>% 
    group_by(fiscal_year, fundingagency) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE),
              .groups = "drop")
  
  
  df_msd_nn %>% 
    ggplot(aes(nn_targets, fundingagency, color = fundingagency, fill = fundingagency)) +
    geom_col(fill = NA, width = .3) +
    geom_col(aes(nn_cumulative), alpha = .6, width = .3) +
    geom_point(aes(nn_cumulative), shape = 21, color = "white", 
               stroke = 2, size = 20) +
    geom_text(aes(x = .5*nn_cumulative, label = label_number_si()(nn_cumulative)),
              family = "Source Sans Pro SemiBold") +
    geom_text(aes(label = label_number_si()(nn_targets)),
              family = "Source Sans Pro SemiBold", hjust = -.2) +
    expand_limits(x = 0) +
    scale_fill_manual(values = c("CDC" = scooter, "USAID" = denim),
                      aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL,
         title = "Accounting for the FY21 eThekwini transition, both agencies fell well short of the NET_NEW need to reach the FY21 treatment targets" %>% toupper %>% str_wrap(),
         subtitle = glue("South Africa | TX_NET_NEW against TX_NET_NEW Targets - adjusted for eThekwini"),
         caption =  glue("Source: {source_info()}, excluded mech 16772
                         FY21 TX_NET_NEW Targets = FY21 TX_CURR Targets - FY20 TX_CURR Cumulative
                     SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    coord_cartesian(clip = "off") +
    si_style_nolines() +
    theme(legend.position = "none",
          axis.text.x = element_blank())  
  
  si_save("Images/FY21Q4_ZAF-adj-nn-target.png")
  