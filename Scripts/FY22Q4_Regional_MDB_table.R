# PURPOSE: Munge and Analysis of FY22Q4 LAC summary visual for OHA FO briefing
# AUTHOR: Jessica Hoehner, Tim Essam | SI
# LICENSE: MIT
# DATE: 2023-01-26
# NOTES: Updated by NP to simplify for slide deck
#        Updated by JH to filter and aggregate for LAC region

# LOCALS & SETUP ===============================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(selfdestructin5)
    library(gt)
    library(countrycode)
    library(webshot2)
    
  # SI specific paths/functions  
    load_secrets()
    
    file_path <- "OU_IM_FY20-23"

   # Grab metadata
    
   get_metadata()
    
   msd_source <- metadata$source
   curr_pd <- metadata$curr_pd
   fy <- metadata$curr_fy
   qtr <- metadata$curr_qtr
   caption <- metadata$caption
    
# LOAD DATA ==================================================================== 
   
   msd <- si_path() %>%
     return_latest(file_path) %>%
     read_msd()

   # Import list of PEPFAR countries in regions from DATIM
   df_meta <- grabr::get_outable(datim_user(),datim_pwd()) %>%
     select(country, country_iso)

# MUNGE ============================================================================
  
   # Cleaning up region classification to match USAID regions 
  df_meta <- df_meta %>%
    mutate(wb_region = countrycode(df_meta$country_iso, "iso3c", "region"),
            usaid_region = case_when(country == "Ukraine" ~ "Europe",
                                     wb_region == "Sub-Saharan Africa" ~ "Africa",
                                     wb_region == "Latin America & Caribbean" ~ "LAC",
                                     TRUE ~ "Asia")) %>% 
    select(-c(wb_region))
   
   # filtered and aggregated by region
  mdb_df <- make_mdb_df(msd) %>%
    full_join(., df_meta, by = c("operatingunit" = "country")) %>%
    filter(
      (usaid_region == "LAC") |
        (operatingunit == "Global" | operatingunit == "Western Hemisphere Region"),
      indicator %in% c("HTS_TST_POS", "TX_NEW", "KP_PREV")) %>%
    mutate(
      operatingunit = if_else(operatingunit %in% c(
        "Western Hemisphere Region",
        "Dominican Republic", "Haiti"),
      "LAC", operatingunit),
      indicator = factor(indicator,
        levels =
          c("HTS_TST_POS", "TX_NEW", "KP_PREV"))) %>%
    group_by(fiscal_year, agency, operatingunit, indicator, indicator_plain, agg_type) %>%
    summarise(across(targets:cumulative, sum, na.rm = TRUE)) %>%
    arrange(indicator)
    
  mdb_tbl  <- reshape_mdb_df(mdb_df, curr_pd)
   
  mdb_df_tx <- make_mdb_tx_df(msd) %>%
    left_join(., df_meta, by = c("operatingunit" = "country")) %>%
    filter((usaid_region == "LAC") |
      (operatingunit == "Global" | operatingunit == "Western Hemisphere Region")) %>%
    mutate(operatingunit = if_else(operatingunit %in% c(
      "Western Hemisphere Region",
      "Dominican Republic", "Haiti"),
    "LAC", operatingunit)) %>%
    group_by(fiscal_year, agency, operatingunit, indicator, agg_type) %>%
    summarise(across(targets:cumulative, sum, na.rm = TRUE))
  
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, curr_pd) %>%
    filter(indicator == "VLS")
  
# VIZ ==========================================================================

  create_mdb(mdb_tbl %>% filter(agency == "USAID"), 
             ou = "LAC", type = "main", curr_pd, msd_source) %>%
  cols_hide(columns= c(6:8,12:15)) %>%
  gtsave(path = "Images", filename = glue::glue("LAC_{curr_pd}_mdb_main.png"),
           vwidth = 900, vheight = 400, zoom = 2)
  
  create_mdb(mdb_tbl_tx, ou = "LAC", type = "treatment", pd, msd_source)
  