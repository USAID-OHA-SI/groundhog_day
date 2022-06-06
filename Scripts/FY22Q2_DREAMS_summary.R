# PURPOSE: Munge and Analysis of FY22Q2 DREAMS data
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-06-02
# NOTES: Extract from Tableau Dashboard

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
  library(readxl)


# IMPORT DATA -------------------------------------------------------------

  df <- read_csv("../../../Downloads/Overall_AGYW_PREV_Completion_Sheet_data.csv") %>% 
    rename(ou = `Operating Unit1`,
           fltr = `Primary Prevention View Filter`,
           pct = `Percent Contribution`, 
           value = `Cumulative`)
  
  msd_path <- return_latest(si_path(), "PSNU_IM_DREAMS_FY20-22") 
  msd_df <- read_msd(msd_path) %>% resolve_knownissues()

  msd_source <- source_info(msd_path)
  curr_fy <- source_info(return = "fiscal_year")
  curr_pd <- source_info(return = "period")

# MUNGE MSD ---------------------------------------------------------------

  drms_ban <- 
    msd_df %>% 
    filter(indicator == "AGYW_PREV", 
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
           fiscal_year == curr_fy) %>% 
    group_by(fiscal_year, standardizeddisaggregate) %>% 
    summarise(across(matches("cum"), sum, na.rm = T)) %>% 
    spread(standardizeddisaggregate, cumulative) %>% 
    mutate(pct_complete = `Total Numerator`/`Total Denominator`) %>% 
    arrange(desc(pct_complete))
  
  # 10-14 year olds
  age_fltr <- c("10-14")
  # drms_ban_ou <- 
    msd_df %>% 
    filter(indicator == "AGYW_PREV", 
           standardizeddisaggregate %in% c("Age/Sex/Time/Incomplete", "Age/Sex/Time/Complete"), 
           fiscal_year == curr_fy, ageasentered == age_fltr) %>% 
    group_by(fiscal_year, standardizeddisaggregate, operatingunit, ageasentered) %>% 
    summarise(across(matches("qtr"), sum, na.rm = T)) %>% 
    group_by(operatingunit) %>% 
    mutate(tot = sum(qtr2), share = qtr2/tot) %>% prinf()
    prinf()
    spread(standardizeddisaggregate, cumulative) %>% 
    mutate(pct_complete = `Total Numerator`/`Total Denominator`,
           ou_order = fct_reorder(operatingunit, `Total Denominator`)) %>% 
    arrange(desc(pct_complete))

# VIZ ---------------------------------------------------------------------

  df %>% 
    group_by(ou) %>% 
      mutate(agyw_tot = sum(value)) %>% 
    ungroup() %>% 
    filter(fltr == "Primary Package Completed") %>% 
    mutate(ou_order = fct_reorder(ou, pct)) %>% 
    ggplot(aes(y = ou_order)) +
    geom_col(aes(x = 1), fill = grey20k)+
    geom_col(aes(x = pct), fill = scooter_med) +
    geom_vline(xintercept = c(0.25, 0.5, 0.75), linetype = "dashed", 
               color = grey10k)+
    geom_vline(xintercept = c(0, 1), 
               color = grey90k)+
    geom_text(aes(x = 1.2, label = comma(agyw_tot, 1)), 
              hjust = 1,
              size = 12/.pt, 
              family = "Source Sans Pro",
              color = grey90k) +
    geom_text(aes(x = pct, label = percent(pct, 1)), 
              size = 10/.pt, 
              family = "Source Sans Pro",
              nudge_x = 0.04, 
              color = grey90k)+
    scale_x_continuous(limits = c(0, 1.2), labels = percent,
                       breaks = c(0, .25, .5, .75, 1)) +
    coord_cartesian(expand = F) +
    si_style_nolines() +
    labs(x = NULL, y = NULL)
  
  si_save(glue::glue("Graphics/{curr_pd}_DREAMS_completion.svg"), height = 2.95, width = 5.85, scale = 1.5)
  
    