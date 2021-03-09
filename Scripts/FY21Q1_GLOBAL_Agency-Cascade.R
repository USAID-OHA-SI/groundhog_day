# PROJECT:  FY21Q1 Review
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  Cascade trends
# LICENSE:  MIT
# DATE:     2021-03-05
# UPDATED:  2021-03-09

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(ICPIutilities)
  library(svglite)
  

# GLOBAL VARIABLES --------------------------------------------------------

ind_list <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")  

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19-21") %>% 
    read_rds()   

  df_cas <- df %>% 
    filter(indicator %in% ind_list,
           standardizeddisaggregate == "Total Numerator",
           !mech_code %in% c("16772", "81935", "84562", "84566", "84852"),
           fundingagency %in% c("USAID", "HHS/CDC")) %>% 
    group_by(fundingagency, indicator, fiscal_year) %>%
    summarise(across(c(starts_with("qtr"), targets), sum, na.rm = TRUE)) %>% 
    ungroup()

    
  df_cas <- df_cas %>% 
    pivot_longer(starts_with("qtr"),
                 names_to = "qtr",
                 names_prefix = "qtr",
                 values_to = "results") %>% 
    filter(results > 0) %>% 
    mutate(period = glue("FY{str_sub(fiscal_year, 3,4)}Q{qtr}"),
           fundingagency = str_remove(fundingagency, "HHS/"),
           fundingagency = as_factor(fundingagency) %>% fct_rev(),
           indicator = factor(indicator, ind_list),
           targets_qtrly = case_when(indicator == "TX_CURR" ~ targets,
                                     TRUE ~ targets * (.25* as.integer(qtr)))) %>% 
    group_by(fundingagency, indicator, fiscal_year) %>% 
    mutate(cumulative = cumsum(results),
           cumulative = ifelse(indicator == "TX_CURR", results, cumulative)) %>% 
    ungroup() %>% 
    mutate(achv = cumulative / targets,
           label = percent(achv, 1))


# PLOT --------------------------------------------------------------------

  df_cas %>% 
    filter(indicator != "HTS_TST") %>% 
    ggplot(aes(period, cumulative, fill = fundingagency)) +
    geom_col(aes(y = targets), fill = trolley_grey_light, alpha = .8) +
    geom_col() +
    geom_text(aes(y = 0, label =label), family = "Source Sans Pro", color = "white",
               size = 2.5, vjust = -.5) +
    geom_errorbar(aes(ymin = targets, ymax = targets), color = trolley_grey) +
    facet_grid(indicator~ fundingagency, scales = "free_y", switch = "y") +
    scale_y_continuous(labels = unit_format(.1, unit = "M", scale = 1e-6)) +
    scale_fill_manual(values = c(denim, scooter)) +
    scale_x_discrete(breaks = c("FY19Q1", "FY20Q1", "FY21Q1")) +
    labs(x = NULL, y = NULL,
         caption = "Excludes South Africa Dept of Health and 4 TBD mechanisms with double counted
         targets - 81935 Eswatini, 84566 Malawi, 84562 Malawi, and 84852 South Sudan
         Source: FY21Q1i MSD") +
    si_style_ygrid() +
    theme(legend.position = "none",
          panel.spacing = unit(.5, "lines"),
          strip.placement = "outside")
  
    si_save("Graphics/FY21Q1_Cascade.svg")
           