# PURPOSE: Munge and Analysis of FY22Q2 HTS_SELF Visuals for TOP N OUs
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-06-02
# NOTES: 

# LOCALS & SETUP ============================================================================

# Libraries
  library(glitr)
  library(glamr)
  library(gisr)
  library(gophr)
  library(tidyverse)
  library(scales)
  library(sf)
  library(extrafont)
  library(tidytext)
  library(glue)


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
  
  df_hts <- df %>% 
    bind_rows(df_arch) %>% 
    filter(indicator == "HTS_TST_POS",
           standardizeddisaggregate == "Total Numerator") 
  
  df_hts_viz <- 
    df_hts %>%
    bind_rows(df_hts %>% mutate(funding_agency = "PEPFAR")) %>% 
    group_by(fiscal_year, funding_agency, operatingunit) %>% 
    summarise(across(c(cumulative), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(funding_agency %in% c("PEPFAR", "USAID")) %>% 
    mutate(source = "MSD") %>% 
    rename(value = cumulative) %>% 
    pivot_wider(names_from = funding_agency, values_from = value) %>%
    group_by(fiscal_year) %>%
    mutate(share = USAID / PEPFAR)  %>%
    pivot_longer(cols = PEPFAR:USAID, names_to = "funding_agency") %>% 
    group_by(funding_agency, fiscal_year) %>% 
    mutate(hts_rank = dense_rank(-value)) %>% 
    ungroup() %>% 
    mutate(rank_flag = case_when(
             hts_rank < 10 & fiscal_year == curr_yr ~ 1
           )
          ) %>% 
    group_by(operatingunit) %>% 
    fill(rank_flag, .direction = "downup") %>% 
    ungroup()
  

# VIZ ---------------------------------------------------------------------

  # Talking points
  df_hts_viz %>% 
    group_by(fiscal_year, funding_agency) %>% 
    summarise(tot_tests = sum(value, na.rm = T)) %>% 
    spread(funding_agency, tot_tests) %>% 
    mutate(share = USAID/PEPFAR)
  
  df_hts_viz %>% 
    filter(rank_flag == 1) %>% 
    group_by(operatingunit) %>% 
    mutate(total_tests = sum(value, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(fill_colors = ifelse(funding_agency == "PEPFAR", grey20k, golden_sand),
           facet_order = fct_reorder(operatingunit, total_tests, .desc = T)) %>% 
      ggplot(aes(x = fiscal_year)) + 
    geom_col(data = . %>% filter(funding_agency == "PEPFAR"),
             aes(y = value, group = funding_agency, fill = fill_colors)) +
    geom_col(data = . %>% filter(funding_agency == "USAID"),
             aes(y = value, group = funding_agency, fill = fill_colors)) +
    geom_text(data = . %>% filter(funding_agency == "USAID"), 
              aes(y = value, label = percent(share, 1)), 
              size = 8/.pt, 
              family = "Source Sans Pro", 
              vjust = -0.5) +
    facet_wrap(~facet_order, scales = "free_y", nrow = 2) +
    scale_fill_identity() +
    si_style_ygrid(facet_space = 0.5) +
    scale_y_continuous(labels = label_number_si()) +
    labs(x = NULL, y = NULL, 
         caption = glue::glue("Source: {source} (including FY15-18)
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))
  
  si_save(glue("Graphics/{curr_pd}_HTS_POS_trends_.svg"), height = 4, width = 10, scale = 1.3)  
  

