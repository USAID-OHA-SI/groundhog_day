# PROJECT:  groundhog_day
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  review impact of Q1 threshold for CAP
# LICENSE:  MIT
# DATE:     2021-02-25
# UPDATED:  2021-03-02
# NOTES:    based on FY21Q1_GLOBAL_Reviewing-Impact-of-CAP-Threshold

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
library(googlesheets4)


# GLOBAL VARIABLES --------------------------------------------------------

  lp_list <- as_sheets_id("1tGk1TR8l3WacR8qMIK0AQvFynABijAaLHeIctE1nUoM")
  
  ind_sel <- "HTS_TST_POS"
  
  threshold <- .15
  

# CREDENTIALS -------------------------------------------------------------

  load_secrets()
  
# IMPORT ------------------------------------------------------------------
  
  #MSD
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds() 
  
  #LP list
  df_lp_list <- read_sheet(lp_list, col_types = c(.default = "c")) %>% 
    select(mech_code = `Mechanism ID`, partner_type = `Partner Type`)

# MUNGE -------------------------------------------------------------------

  #join LP info with MSD
    df <- df %>% 
      filter(fundingagency == "USAID",
             fiscal_year == 2021) %>%  
      left_join(df_lp_list)
  
  rm(df_lp_list)
    
  #limit to USAID LPs for select indicators
  df_lp <- df %>% 
    filter(indicator %in% ind_sel,
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID",
           partner_type == "Local",
           fiscal_year == 2021)

  #rename with latest partner/mech names
    df_lp <- rename_official(df_lp)
  
  #adjust names
    df_lp <- df_lp %>% 
      mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                       operatingunit =="Dominican Republic" ~ "DR",
                                       operatingunit == "Western Hemisphere Region" ~ "WHR",
                                       TRUE ~ operatingunit))
  
  #aggregate to mech level, calc achivement w/ cutoff threshold
    df_achv <- df_lp %>% 
      group_by(operatingunit, mech_code, primepartner, indicator) %>% 
      summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(achievement = cumulative/targets,
             achv_max = ifelse(achievement > 1.1, 1.1, achievement)) %>% 
      arrange(achievement)
  
  #capture mechanism count with no reporting for ind_sel in FY21 but have targets
    mech_noreporting <- df_achv %>% 
      filter(cumulative == 0) %>% 
      nrow()
  
  #capture mechanism count with no reporting for ind_sel in FY21 but have targets for plot notes
    mech_notargets <- df_achv %>% 
      filter(targets == 0) %>% 
      nrow()
  
  #drop mechanims with either no results or targets for plot notes
    df_achv <- df_achv %>% 
      filter(#cumulative > 0,
             targets > 0)
  
  #calc achievement at the OU level to plot overall achievement as comparison
    df_achv_ou <- df_achv %>% 
      group_by(operatingunit, indicator) %>% 
      summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(achievement_ou = cumulative/targets) %>%
      select(operatingunit, indicator, achievement_ou)
  
  #flag where achv is below threshold and count tot mech & those that meet flag for naming
    df_achv <- df_achv %>%
      mutate(flag = achievement < threshold) %>% 
      group_by(operatingunit) %>% 
      mutate(mech_tot = n(),
             mech_flag = sum(flag, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(ou_name = glue("{operatingunit} ({mech_flag}/{mech_tot})"))
  
  #add new names onto OU achievement to work with append + plot
    df_achv_ou <- df_achv %>% 
      distinct(operatingunit, ou_name) %>% 
      left_join(df_achv_ou) 
  
  #append OU achievement onto mech data 
    df_achv <- df_achv %>% 
      bind_rows(df_achv_ou)
  
  #capture mechanism total for use in plot title  
    total_mechs <-  df_achv %>% 
      filter(!is.na(mech_code)) %>% 
      nrow()
  
  #capture flagged mechanism count for use in plot title 
    flagged_mechs <-  df_achv %>% 
      filter(flag == TRUE) %>% 
      nrow()


# PLOT --------------------------------------------------------------------

  df_achv %>% 
    ggplot(aes(achv_max, fct_reorder(ou_name,achievement_ou, na.rm = TRUE), fill = achievement < threshold)) +
    geom_vline(aes(xintercept = threshold), linetype = "dotted", color = trolley_grey) +
    geom_vline(aes(xintercept = threshold + .2), linetype = "dotted", color = trolley_grey) +
    geom_vline(aes(xintercept = 1.1), color = trolley_grey_light) +
    geom_errorbar(aes(xmin = achievement_ou, xmax = achievement_ou), size = 1.5, color = trolley_grey) +
    geom_jitter(aes(size = targets), height = .3,  shape = 21, alpha = .6, color = "white", na.rm = TRUE) +
    scale_x_continuous(label = percent, limits = c(0, 1.11), 
                       breaks = seq(0, 1, .25),
                       expand = c(.005, .005)) +
    scale_fill_manual(values = c(scooter, burnt_sienna)) +
    labs(x = NULL, y = NULL,
         title = glue("At Q1, {flagged_mechs} out of USAID's {total_mechs} local mechanisms have fallen short of this year targets") %>% toupper,
         subtitle = glue("FY21 {ind_sel} | USAID Local Partners"),
         caption = glue("{mech_notargets} mechanisms were dropped due to reporting in Q1 but having no FY21 targets
                          Extent capped at 110%, all achievement above has been plotted at 110%
                          Source: FY21Q1i MSD")) +
    si_style() +
    theme(legend.position = "none")


# EXPORT ------------------------------------------------------------------

  si_save("Graphics/FY21Q1_LP_HTS_POS_Review.svg")

