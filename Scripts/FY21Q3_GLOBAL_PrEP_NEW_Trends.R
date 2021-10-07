# PROJECT:  FY21Q3 Q. Review
# AUTHOR:   T. Essam | USAID
# PURPOSE:  Expansion of PrEP_NEW and q3 gaps
# LICENSE:  MIT
# DATE:     2021-10-05
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


# GLOBAL VARIABLES --------------------------------------------------------

  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  merdata <- si_path("path_msd")
  source <- source_info()
  
  # helper function to not return achv % when result is 0
  no_zero_ach <- function(x, y) {
    ifelse(x >0, (x/y), NA_real_)
  }


# IMPORT ------------------------------------------------------------------

  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()

  curr_pd <- identifypd(df)

# MUNGE -------------------------------------------------------------------

  #bind archived + current MSD and filter for PrEP
  df_prep <- df %>%
    bind_rows(df_arch) %>% 
    filter(fundingagency == "USAID",
           indicator == "PrEP_NEW",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year >= 2017)
  
  min_yr <- 17
  max_yr <- 22
  
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
    summarise(across(matches("qtr|tar"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    mutate(target = ifelse(period_type == "targets", value, NA_integer_),
           fy = substr(period, 1, 4)
           ) %>% 
    group_by(fy) %>% 
    fill(., target, .direction = "down") %>% 
    ungroup()%>% 
    filter(period %ni% c(paste0("FY", min_yr:max_yr))) %>% 
    group_by(fy) %>% 
    mutate(value_cumul = cumsum(value),
           achv = no_zero_ach(value_cumul, target)) %>% 
    group_by(fy) %>% 
    mutate(fy_id = row_number(),
           n = n()) %>% 
    ungroup() %>% 
    complete(fy_id, n, fill = list(period = "FY21Q4",
                                   fundingagency = "USAID",
                                   period_type = "results",
                                   fy = "FY21")) %>% 
    arrange(period) %>% 
      group_by(fy) %>% 
      fill(target, .direction =  c("down")) %>% 
      ungroup() %>% 
      mutate(ts_spell = row_number(), 
             value = case_when(
               value == 0 & period_type == "results" ~ NA_real_,
               TRUE ~ value),
             callout_label = ifelse(achv > 1, "targets exceeded", NA_character_)
             )
    
  

# FULL LIST OF PERIODS ----------------------------------------------------

    curr_pd_num <- curr_pd %>% 
    gsub("FY", "", .) %>% 
    gsub("Q", ".", .) %>% 
    as.numeric()

    curr_fy <- substr(curr_pd, 3, 4) %>% as.numeric() 


    fy_start <- df_prep_hist %>% select(period) %>% 
      filter(str_detect(period, "Q1")) %>% pull()
    
    pd_breaks <- df_prep_hist %>% select(period) %>% pull()
    

# VIZZZZZZ ----------------------------------------------------------------

    df_prep_hist %>% 
      filter(ts_spell < 20) %>% 
      ggplot(aes(period, value, group = fundingagency)) +
      geom_area(fill = scooter, color = scooter, alpha = .2, size = 1, na.rm = TRUE) +
      geom_vline(xintercept = fy_start, color = "white", 
                 size = .9, linetype = "dotted") +
      geom_vline(xintercept = fy_start, color = "white", 
                 size = .9, linetype = "dotted") +
      geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.5, na.rm = TRUE) +
      geom_label(aes(y = 0, label = percent(achv, 1), fill = achv, color = ifelse(achv > 0.3, "white", grey90k)), 
                 vjust = 1.3, size = 10/.pt, label.size = NA, family = "Source Sans Pro") +
      geom_text(aes(label = callout_label), 
               size = 10/.pt, family = "Source Sans Pro", vjust = -1, hjust = 1) +
      scale_fill_si(palette = "scooters", lim = c(0, 1), alpha = 0.85, labels = percent,
                    oob = squish) +
      scale_y_continuous(labels = comma, position = "right") +
      scale_color_identity() +
      si_legend_fill() +
      labs(x = NULL, y = NULL, 
           title = glue("USAID ..... ") %>% toupper,
           subtitle = "Pre-Exposure Prophylaxis (PrEP) Quarterly Results",
           caption = glue("Source: {source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"),
           fill = "Achievement") +
      scale_x_discrete(breaks = pd_breaks, labels = str_remove(pd_breaks, "FY[:digit:]{2}(?!Q1)")) +
      si_style_ygrid()
  
      

    
