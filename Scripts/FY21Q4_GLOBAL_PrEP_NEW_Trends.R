# PROJECT:  FY21Q4 Q. Review
# AUTHOR:   T. Essam | USAID
# PURPOSE:  Expansion of PrEP_NEW and q3 gaps
# LICENSE:  MIT
# DATE:     2021-12-08
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
  library(ggtext)
  library(waffle)


# GLOBAL VARIABLES --------------------------------------------------------

  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  merdata <- si_path("path_msd")
  source <- source_info()
  
  # helper function to not return achv % when result is 0
  no_zero_ach <- function(x, y, z) {
    ifelse(x >0 &!is.na(z), (x/y), NA_real_)
  }
  
  gen_targets <- function(df){
    df %>% 
      summarise(across(matches("qtr|tar"), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      reshape_msd() %>% 
      mutate(target = ifelse(period_type == "targets", value, NA_integer_),
             fy = substr(period, 1, 4)
      ) %>% 
      group_by(fy) %>% 
      fill(., target, .direction = "down") %>% 
      ungroup()%>% 
      filter(period %ni% c(paste0("FY", min_yr:max_yr))) 
  }
  
  # Default should return current year
  make_year <- function(x = 0){
    paste0(20, curr_fy + x) %>% as.numeric()
  }


# IMPORT ------------------------------------------------------------------

  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()

  curr_pd <- identifypd(df)
  curr_fy <- substr(curr_pd, 3, 4) %>% as.numeric() 
  
  curr_yr <- make_year()
  min_yr <- make_year(-4)
  max_yr <- make_year(1) 
  
  curr_pd_num <- curr_pd %>% 
    gsub("FY", "", .) %>% 
    gsub("Q", ".", .) %>% 
    as.numeric()
  


# MUNGE -------------------------------------------------------------------

  #bind archived + current MSD and filter for PrEP
  df_prep <- df %>%
    bind_rows(df_arch) %>% 
    filter(fundingagency == "USAID",
           indicator == "PrEP_NEW",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year >= 2017)
  

  
  #curr fy prep (for viz title)
  prep_cum <- df_prep %>% 
    filter(fiscal_year >= identifypd(df, "year")) %>% 
    count(wt = cumulative)
  
  #count number of countries with PrEP
  df_cntry_cnt <- df_prep %>% 
    filter(cumulative > 0) %>% 
    distinct(fiscal_year, countryname) %>% 
    count(fiscal_year, name = "n_countries")
  
  #aggregate result to USAID level with targets
  df_prep_hist <- df_prep %>% 
    group_by(fiscal_year, fundingagency) %>%  
    gen_targets() %>% 
    group_by(fy) %>% 
    mutate(fy_id = row_number(),
           n = n()) %>% 
    ungroup() %>% 
    # Fill in the missing period for plotting ease
    complete(fy_id, n, fill = list(period = curr_pd,
                                   fundingagency = "USAID",
                                   period_type = "results",
                                   fy = paste0("FY", curr_fy))) %>% 
    arrange(period) %>% 
    group_by(fy) %>% 
    fill(target, .direction =  c("down")) %>% 
    filter(period_type == "results") %>% 
    mutate(value_cumul = cumsum(value),
           value_cumul = ifelse(value == 0, NA_real_, value_cumul)) %>% 
    ungroup() %>% 
    mutate(achv = no_zero_ach(value_cumul, target, value)) %>% 
      mutate(ts_spell = row_number(), 
             value = case_when(
               value == 0 & period_type == "results" ~ NA_real_,
               TRUE ~ value),
             callout_label = ifelse(achv > 1, "targets exceeded", NA_character_)
             ) %>% 
    # Add in Q4 gap pace to show what number is needed to hit targets
    mutate(gap_remaining = case_when(
      period == curr_pd ~ target - lag(value_cumul, order_by = period), 
      period == curr_pd ~ value,
      TRUE ~ NA_real_
    ))
    
  # OU dataframe
 df_prep_ou <-  df_prep %>% 
    filter(fiscal_year == curr_yr) %>% 
    group_by(fundingagency, fiscal_year, operatingunit) %>% 
    gen_targets() %>% 
    filter(period_type == "results") %>% 
    group_by(fy, operatingunit) %>% 
    mutate(value_cumul = cumsum(value),
           achv = no_zero_ach(value_cumul, target, value_cumul)) %>% 
    filter(period == curr_pd) %>% 
    ungroup() %>% 
    mutate(target_gap = value_cumul - target) %>% 
    arrange(target_gap) %>% 
    group_by(fundingagency) %>% 
    mutate(agency_targets = sum(target),
           agency_results = sum(value_cumul),
           agency_gap = agency_targets - agency_results,
           ) %>% 
    ungroup() %>% 
   # This adjusts for over-achievement counting in the Q4 target share
    mutate(over_achv = value_cumul > target) %>% 
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
 top_4 <- df_prep_ou %>% 
   select(operatingunit, ou_gap_sh) %>% 
   slice_max(ou_gap_sh, n = 4) %>% 
   pull(operatingunit)
  
  

# FULL LIST OF PERIODS ----------------------------------------------------

    fy_start <- df_prep_hist %>% select(period) %>% 
      filter(str_detect(period, "Q1")) %>% pull()
    
    pd_breaks <- df_prep_hist %>% select(period) %>% pull()
    

# VIZZZZZZ ----------------------------------------------------------------

    # Full Canvass is 9.7 w X 4 H
    
    df_prep_hist %>% 
      filter(ts_spell < 21) %>% 
      ggplot(aes(period, value, group = fundingagency)) +
      geom_area(aes(y = gap_remaining), fill = scooter, alpha = .1, size = 1, na.rm = TRUE) +
      geom_line(aes(y = gap_remaining), linetype = "dashed", color = scooter) +
      geom_point(aes(y = gap_remaining), shape = 21, fill = "#E8F3F6", size = 5, color = "white") +
      geom_point(aes(y = gap_remaining), shape = 1, size = 5, color = "black") +
      geom_area(fill = scooter, color = scooter, alpha = .2, size = 1, na.rm = TRUE) +
      geom_vline(xintercept = fy_start, color = "white", 
                 size = .9, linetype = "dotted") +
      geom_vline(xintercept = fy_start, color = "white", 
                 size = .9, linetype = "dotted") +
      geom_point(color = scooter, size = 5, na.rm = TRUE) +
      geom_point(color = "black", size = 5, na.rm = TRUE, shape = 1) +
      geom_label(aes(y = 0, label = percent(achv, 1), fill = achv, color = ifelse(achv > 0.3, "white", grey90k)), 
                 vjust = 1.3, size = 10/.pt, label.size = NA, family = "Source Sans Pro") +
      geom_text(aes(label = callout_label), 
               size = 10/.pt, family = "Source Sans Pro", vjust = -1, hjust = 1) +
      scale_fill_si(palette = "scooters", lim = c(0, 1), alpha = 0.85, labels = percent,
                    oob = squish) +
      scale_y_continuous(labels = comma) +
      scale_color_identity() +
      labs(x = NULL, y = NULL, 
           title = glue("USAID ..... ") %>% toupper,
           subtitle = "Pre-Exposure Prophylaxis (PrEP) Quarterly Results",
           caption = glue("Source: {source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"),
           fill = "Achievement") +
      scale_x_discrete(breaks = pd_breaks, labels = str_remove(pd_breaks, "FY[:digit:]{2}(?!Q1)")) +
      si_style_ygrid() +
      theme(legend.position = "none") 
    
      si_save(glue("Graphics/{curr_pd}_PrEP_NEW_trends_ou_gaps.svg"), height = 4, width = 6.7, scale = 1.4)

      
# Try a waffle chart of remaining ous - Cleaned up in AI
    df_prep_waffle <- 
      df_prep_ou %>% 
      select(operatingunit, ou_gap_sh, target_gap) %>% 
      mutate(value = round(ou_gap_sh * 100, 1)) %>% 
      filter(!is.na(value)) %>% 
      mutate(ou = case_when(
        operatingunit %in% top_4 ~ operatingunit, 
        TRUE ~ "Other"
      )) 
    
    df_prep_waffle %>% 
      ggplot(aes(fill = ou, values = value)) +
      waffle::geom_waffle(color = "white", size = .25, n_rows = 10, flip = T, make_proportional = T)  +
      scale_fill_si(palette = "scooter", discrete = T, reverse = F, alpha = 0.75) +
      si_style_map() 
    
    si_save(glue("Graphics/{curr_pd}_PrEP_NEW_ou_gaps.svg"), height = 4, width = 4, scale = 1.25)
    
    

    
