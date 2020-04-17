##  PROJECT: Q1 review
##  AUTHOR:  achafetz | USAID
##  PURPOSE: review TZA PMTCT testing numbers
##  LICENCE: MIT
##  DATE:    2020-04-017
##  UPDATE:  


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(scales)
library(extrafont)


# GLOBAL ------------------------------------------------------------------


theme_set(theme_minimal(base_family = "Source Sans Pro"))


# IMPORT DATA -------------------------------------------------------------

  df <- list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
    read_rds()


# MUNGE -------------------------------------------------------------------

  #filter 
    df_fltr <- df %>% 
      filter(operatingunit == "Tanzania",
             fundingagency == "USAID",
             indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Modality/Age/Sex/Result",
             modality %in% c("PMTCT ANC", "Post ANC1"))
      
  #trend data for all of USAID TZA, HTS_TST + HTS_POS
    df_viz <- df_fltr %>% 
      group_by(fiscal_year, operatingunit, fundingagency, indicator, modality) %>% 
      summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      complete(period, nesting(modality, indicator), fill = list(val = 0)) %>% 
      arrange(indicator, modality, period) %>% 
      mutate(now = case_when(period == "FY20Q1" ~ val))

  #trend data by partner for HTS_TST
    df_viz_partner <- df_fltr %>% 
      filter(indicator == "HTS_TST") %>% 
      mutate(primepartner = case_when(primepartner == "DELOITTE CONSULTING LIMITED" ~ "Deloitte",
                                      primepartner == "JSI Research And Training Institute, INC." ~ "JSI",
                                      str_detect(primepartner, "Elizabeth") ~ "EGPAF",
                                      TRUE ~ primepartner)) %>% 
      group_by(fiscal_year, operatingunit, fundingagency, primepartner, indicator, modality) %>% 
      summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      complete(period, nesting(modality, indicator, primepartner), fill = list(val = 0)) %>% 
      arrange(indicator, modality, period) %>% 
      mutate(now = case_when(period == "FY20Q1" ~ val))

# PLOT --------------------------------------------------------------------

    #trend data for all of USAID TZA, HTS_TST + HTS_POS
      df_viz %>% 
        ggplot(aes(period, val, group = modality)) +
        geom_hline(aes(yintercept =0), color = "gray30") +
        geom_path(size = .9, color = "gray40") +
        geom_point(size = 3, color = "gray40") +
        geom_point(aes(y = now), color = "#CC5234", size = 4, na.rm = TRUE) +
        geom_text(aes(label = comma(now)), size = 4, family = "Source Sans Pro",
                  color = "#CC5234", vjust = -.4, hjust = 1.4,
                  na.rm = TRUE) +
        facet_grid(indicator ~ modality, scales = "free_y", switch = "y") +
        scale_y_continuous(label = comma) +
        scale_x_discrete(labels = c("FY18", "", "", "",
                                    "FY19", "", "", "",
                                    "FY20")) +
        labs(x = NULL, y = NULL,
             title = "USAID/Tanzania PMTCT Testing Trends") +
        theme(strip.placement = "outside",
              strip.text = element_text(face = "bold", size = 14),
              plot.title = element_text(face = "bold", size = 15),
        )



    #trend data by partner for HTS_TST
      df_viz_partner %>% 
        ggplot(aes(period, val, group = primepartner)) +
        geom_hline(aes(yintercept =0), color = "gray30") +
        geom_path(size = .9, color = "gray40") +
        geom_point(size = 3, color = "gray40") +
        geom_point(aes(y = now), color = "#CC5234", size = 4, na.rm = TRUE) +
        geom_text(aes(label = comma(now)), size = 4, family = "Source Sans Pro",
                  color = "#CC5234", vjust = -.4, hjust = 1.4,
                  na.rm = TRUE) +
        facet_grid(primepartner ~ modality, switch = "y") +
        scale_y_continuous(label = comma) +
        scale_x_discrete(labels = c("FY18", "", "", "",
                                    "FY19", "", "", "",
                                    "FY20")) +
        labs(x = NULL, y = NULL,
             title = "USAID/Tanzania PMTCT Testing Trends",
             subtitle = "HTS_TST by partner") +
        theme(strip.placement = "outside",
              strip.text = element_text(face = "bold", size = 14),
              plot.title = element_text(face = "bold", size = 15),
        )
