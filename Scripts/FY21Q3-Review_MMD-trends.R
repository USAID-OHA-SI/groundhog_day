# PROJECT:  catch-22
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  MMD by OU visual for FY21Q3 review
# LICENSE:  MIT
# DATE:     2021-10-12
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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
  msd_source <- source_info()


# FUNCTION ----------------------------------------------------------------

  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
    
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  
# MUNGE MMD ---------------------------------------------------------------
  
  #keep just TX_CURR/MMD
  df_mmd <- df %>% 
    filter(fundingagency == "USAID",
           indicator == "TX_CURR",
           operatingunit != "South Africa",
           fiscal_year >= 2020,
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
    )) 
  
  #add in agency agg and reshape
  df_mmd <- df_mmd %>%
    bind_rows(df_mmd  %>%
                mutate(countryname = "USAID")) %>% 
    group_by(fiscal_year, countryname, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    filter(value > 0)
  
  #create group for o3mo and o6mo via reshaping for plotting
  df_mmd <- df_mmd %>% 
    mutate(countryname = recode(countryname,
                                "Democratic Republic of the Congo" = "DRC",
                                "Dominican Republic" = "DR")) %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = otherdisaggregate) %>% 
    rowwise() %>% 
    mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
      #unknown = ifelse(unknown < 0, 0, unknown),
      o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    rename(o6mmd = `6 or more months`) %>% 
    select(-`Less than 3 months`, -`3 to 5 months`) %>% 
    pivot_longer(-c(period, countryname, indicator, total), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total) 
  
  
# MMD FOR COUNTRY TRENDS --------------------------------------------------
  
  #country trends for just o3mo
  df_mmd_ou <- df_mmd %>% 
    arrange(countryname, otherdisaggregate, period) 
  
  #create share on +3mo
  df_mmd_ou <- df_mmd_ou %>% 
    mutate(share = tx_mmd/tx_curr) 
  
  #data points for plotting
  df_mmd_ou <- df_mmd_ou %>% 
    mutate(max_tx = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_curr, 0),
           max_mmd = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_mmd, 0)) %>% 
    group_by(countryname) %>% 
    mutate(endpoints = case_when(period %in% c(max(period), min(period))~share),
           max_tx = max(max_tx),
           max_mmd = max(max_mmd)) %>% 
    ungroup() %>% 
    mutate(country_lab = case_when(max_tx == max(max_tx) ~ 
                                     glue("{countryname}<br><span style = 'font-size:8pt'>{clean_number(max_mmd)} / {clean_number(max_tx)} <span style = 'font-size:6pt'>(+6 MMD/TX_CURR)</span>"),
                                   TRUE ~ glue("{countryname}<br><span style = 'font-size:8pt'>{clean_number(max_mmd)} / {clean_number(max_tx)}</span>"))) %>% 
    filter(max_tx > 0)

  
  df_mmd_ou <- df_mmd_ou %>% 
    mutate(fill_color = case_when(countryname == "USAID" & otherdisaggregate == "o6mmd" ~ genoa,
                                  countryname == "USAID" ~ genoa_light,
                                  otherdisaggregate == "o6mmd" ~ scooter,
                                  TRUE ~ scooter_light))
   
    

# VIZ INFO ----------------------------------------------------------------

  #identify the MMD share for the largets 9 countires
  top <- df_mmd_ou %>% 
    filter(countryname != "USAID",
           otherdisaggregate == "o6mmd",
           period == max(period)) %>% 
    arrange(desc(tx_curr)) %>% 
    slice_max(n = 10, order_by = tx_curr) %>% 
    summarise(across(c(tx_curr, tx_mmd), sum, na.rm = TRUE),
              n = n()) %>% 
    mutate(share = percent(tx_mmd/tx_curr, 1))
  
  #top focus countries
  top_cntry <- df_mmd_ou %>% 
    filter(otherdisaggregate == "o6mmd",
           period == max(period)) %>% 
    slice_max(order_by = tx_curr, n = top$n + 1) %>% 
    pull(countryname)
  
# VIZ ---------------------------------------------------------------------

  df_mmd_ou %>%
    filter(countryname %in% top_cntry) %>%
    ggplot(aes(period, share, group = otherdisaggregate, color = fill_color, fill = fill_color)) +
    geom_area(alpha = .4, 
              size = .9, position = "identity"
              ) +
    geom_point(aes(y = endpoints), na.rm = TRUE) +
    facet_wrap(~fct_reorder2(country_lab, period, tx_curr, .desc = TRUE)) +
    scale_y_continuous(label = percent, 
                       breaks = seq(0, 1, .5)) +
    scale_x_discrete(breaks = c("FY20Q1", "FY20Q3", "FY21Q1", "FY21Q3")) +
    scale_color_identity(aesthetics = c("color","fill")) +
    labs(x = NULL, y = NULL,
         title = glue("IN {max(df_mmd_ou$period)}, USAID HAD {top$share} OF TREATMENT PATIENTS ON +6 MONTHS OF MMD IN THE LARGEST {top$n} COUNTRIES"),
         subtitle = "South Africa, representing a third of USAID's treatment portfolio, has been excluded",
         caption = glue("MMD 3 months or more = 3-5 months and 6 months or more | Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing.y = unit(.5, "line"),
          panel.spacing.x = unit(.5, "line"),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 7),
          panel.grid.major.y = element_line(color = "#E8E8E8"),
          panel.grid.minor.y = element_line(color = "#E8E8E8"),
          strip.text = element_markdown())    

  si_save("Graphics/20211012_fy21q3_mmd_trends_by_country.svg")    
  