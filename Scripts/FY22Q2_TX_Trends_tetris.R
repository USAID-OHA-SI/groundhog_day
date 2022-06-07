# PROJECT:  FY21Q3 Review
# AUTHOR:   A.Chafetz | T. Essam USAID
# PURPOSE:  treatment scale up since PEPFAR start
# LICENSE:  MIT
# DATE:     2021-05-14
# UPDATED:  2021-12-07

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
  library(janitor)
  library(lubridate)
  library(ggnewscale)
  library(waffle)


# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  merdata <- si_path("path_msd")
  load_secrets()

  # Default should return current year
  make_year <- function(x = 0){
    paste0(20, curr_fy + x) %>% as.numeric()
  }
  

# IMPORT ------------------------------------------------------------------
  
  #Current MSD
  df <- si_path() %>% 
    return_latest("OU_IM_FY20-22") %>% 
    read_msd() %>% 
    resolve_knownissues()
    
  
  #Archived MSD
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15-19") %>% 
    read_msd() %>% 
    resolve_knownissues()
  
  curr_pd <- identifypd(df)
  curr_fy <- substr(curr_pd, 3, 4) %>% as.numeric() 
  
  curr_yr <- make_year()
  min_yr <- make_year(-6)
  max_yr <- make_year(1) 

# MUNGE -------------------------------------------------------------------

  #source info
  source <- source_info()
  
  df_tx <- df %>% 
    bind_rows(df_arch) %>% 
    filter(funding_agency == "USAID",
           indicator %in% c("TX_CURR", "TX_NEW"),
           between(fiscal_year, min_yr + 1, max_yr - 1),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, funding_agency, indicator) %>% 
    summarise(across(matches("cumulative|target"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(achv = cumulative / targets,
           ind_color = ifelse(indicator == "TX_CURR", genoa, moody_blue),
           gap = ifelse(fiscal_year == curr_yr, targets - cumulative, NA_real_))
  
  
  df_tx <- df_tx %>% 
    rename(period = fiscal_year, value = cumulative) %>% 
    mutate(period = str_replace(period, "20", "FY")) %>% 
    arrange(indicator, period) %>% 
    mutate(source = "MSD")

  df_tx <- df_tx %>% 
    mutate(ind_label = case_when(indicator == "TX_CURR" ~ "Currently receiving antiretroviral therapy",
                                 TRUE ~ "Newly enrolled on antiretroviral therapy"))
  
  #OU GAP Share
  df_tx_ou <- df %>% 
    bind_rows(df_arch) %>% 
    filter(funding_agency== "USAID",
           indicator %in% c("TX_CURR"),
           fiscal_year == curr_yr,
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(fiscal_year, funding_agency, indicator, operatingunit) %>% 
    summarise(across(matches("cumulative|target"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(achv = cumulative / targets, 
           target_gap = cumulative - targets) 
  
  df_tx_no_sa <- 
    df_tx_ou %>% 
    filter(operatingunit != "South Africa") %>% 
    arrange(target_gap) %>% 
    group_by(funding_agency) %>% 
    mutate(agency_targets = sum(targets),
           agency_results = sum(cumulative),
           agency_gap = agency_targets - agency_results,
    ) %>% 
    ungroup() %>% 
    # This adjusts for over-achievement counting in the Q4 target share
    mutate(over_achv = cumulative > targets) %>% 
    group_by(over_achv) %>% 
    mutate(agency_gap_adj = sum(target_gap)) %>% 
    ungroup() %>% 
    mutate(ou_gap_sh = case_when(
      target_gap < 0 ~ (target_gap/agency_gap_adj),
      TRUE ~ NA_real_),
      gap_running_sh = cumsum(ou_gap_sh),
      gap_running_target = cumsum(target_gap)
    )
  
  # Extract top four for waffle chart
  top_6 <- df_tx_no_sa %>% 
    select(operatingunit, ou_gap_sh) %>% 
    slice_max(ou_gap_sh, n = 6) %>% 
    pull(operatingunit)


# VIZZZZZZ ----------------------------------------------------------------

nudge_space <- 0.125
  
  df_tx %>% 
    ggplot(aes(x = period, group = indicator)) +
    geom_col(aes(y = targets), fill = grey10k, width = 0.5, position = position_nudge(x = -nudge_space)) +
    geom_col(aes(y = value, fill = ind_color), width = 0.5, position = position_nudge(x = nudge_space)) +
    scale_fill_identity() +
    new_scale_fill() +
    geom_label(data = . %>% filter(indicator == "TX_NEW"), 
                                   aes(y = 0, label = percent(achv, 1), fill = achv, 
                   color = ifelse(achv > 0.3, "white", grey90k)), 
               vjust = 1.3, 
               size = 10/.pt, 
               label.size = NA, family = "Source Sans Pro") +
    scale_fill_si(palette = "moody_blues", lim = c(0.6, 1), alpha = 0.85, labels = percent,
                  oob = squish) +
    new_scale_fill() +
    geom_label(data = . %>% filter(indicator != "TX_NEW"), 
               aes(y = 0, label = percent(achv, 1), fill = achv, 
                   color = ifelse(achv > 0.3, "white", grey90k)), 
               vjust = 1.3, 
               size = 10/.pt, 
               label.size = NA, family = "Source Sans Pro") +
    scale_fill_si(palette = "genoas", lim = c(0.7, 1), alpha = 0.85, labels = percent,
                  oob = squish) +
    geom_text(aes(y = targets, label = comma(gap)), 
              family = "Source Sans Pro", color = grey90k) +
    facet_wrap(fct_rev(indicator) ~ ind_label, nrow = 1, scales = "free_y") +
    scale_color_identity() +
    si_style_ygrid() +
    scale_y_continuous(labels = label_number_si(), position = "left") +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL, fill = NULL,
         title = "USAID",
         caption = glue("Source: {source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))
  si_save(glue("Graphics/{curr_pd}_TX_trends_ou_gaps.svg"), height = 4, width = 10, scale = 1.3)
    
  
  # Waffle Chart
  df_prep_waffle <- 
    df_tx_no_sa %>% 
    select(operatingunit, ou_gap_sh, target_gap) %>% 
    mutate(value = round(ou_gap_sh * 100, 1)) %>% 
    filter(!is.na(value)) %>% 
    mutate(ou = case_when(
      operatingunit %in% top_6 ~ operatingunit, 
      TRUE ~ "Other"
    )) 
  
  
  df_prep_waffle %>% 
    ggplot(aes(fill = ou, values = value)) +
    waffle::geom_waffle(color = "white", size = .25, n_rows = 10, flip = T, make_proportional = T)  +
    scale_fill_si(palette = "carto_sunset", discrete = T, reverse = F, alpha = 0.75) +
    si_style_map() 
  
  
  si_save(glue("Graphics/{curr_pd}_TX_CURR_ou_waffle.svg"), height = 4, width = 4, scale = 1.25)
  
  
  

  