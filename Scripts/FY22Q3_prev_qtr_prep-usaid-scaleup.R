# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# REF ID:   `r Sys.time() |> digest::sha1() |> substr(start = 1, stop = 8)`
# PURPOSE:  scale up of prep
# LICENSE:  MIT
# DATE:     2021-12-01
# UPDATED:  2022-08-05
# NOTE:     adapted from agitprop/09a_usaid_prep_scaleup.R

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

  # Reference ID to be used for searching GitHub
  ref_id <- "d83879e3"

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  
  curr_fy <-source_info(return = "fiscal_year")
  curr_pd <- source_info(return = "period")
  #source info
  msd_source <- source_info()
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY20") %>% 
    read_msd()   
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()

# MUNGE -------------------------------------------------------------------
  
 # df_arch <- df_arch %>% 
 #  rename(funding_agency = fundingagency)

  #bind archived + current MSD and filter for PrEP
  df_prep <- df %>%
    bind_rows(df_arch) %>% 
    filter(funding_agency == "USAID",
           indicator == "PrEP_NEW",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year >= 2017)
  
  #curr fy prep (for viz title)
  prep_cum <- df_prep %>% 
    filter(fiscal_year == curr_fy) %>% 
    count(wt = cumulative) %>% 
    pull()
  
  #count number of countries with PrEP
  df_cntry_cnt <- df_prep %>% 
    filter(cumulative != 0) %>% 
    distinct(fiscal_year, country) %>% 
    count(fiscal_year, name = "n_countries")
  
  #aggregate result to USAID level
  df_prep <- df_prep %>% 
    group_by(fiscal_year, funding_agency) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    mutate(value = na_if(value, 0)) %>% 
    select(-period_type) %>% 
    arrange(period) 
  


  
# CREATE FULL LIST OF PERIODS ---------------------------------------------

#PrEP in and out of quarterly and semi-annual reporting so need a complete set  
  #current period as number
  curr_pd_num <- curr_pd %>% 
    str_remove("FY") %>% 
    str_replace("Q", ".") %>% 
    as.numeric()
  
  #identify current fiscal year for max date
  curr_fy <- str_sub(curr_pd, 3,4) %>% as.numeric()
  
  #propagate list of periods not in prep to add to df
  full_pds <- expand_grid(fiscal_year = c(17:curr_fy),
                           quarter = c(1:4)) %>% 
    unite(period, c(fiscal_year, quarter), sep = ".") %>% 
    mutate(period = as.numeric(period)) %>% 
    filter(period <= curr_pd_num) %>% 
    mutate(period = period %>% 
             paste0("FY", .) %>% 
             str_replace("\\.", "Q")) 
  
  extra_pds <- full_pds %>% 
    filter(!period %in% unique(df_prep$period))
  
  
# VIZ ---------------------------------------------------------------------
  
  fy_start <-  full_pds %>% 
    filter(str_detect(period, "Q1")) %>% 
    pull()
  
  pd_breaks <- full_pds %>% 
    # filter(str_detect(period, "Q(1|3)")) %>% 
    pull()
  
  df_viz <- df_prep %>% 
    bind_rows(extra_pds) %>% 
    arrange(period) %>% 
    mutate(achv=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Achieved FY19 \n Targets",
                  NA, NA, NA, "Achieved FY20 \n Targets", 
                  NA, NA, NA, "Achieved FY21 \n Targets", 
                  NA, NA, "Achieved FY22 \n Targets"))
   
  v <- df_viz %>% 
    ggplot(aes(period, value, group = funding_agency)) + 
    geom_area(fill = scooter, color = scooter, alpha = .2, size = 1, na.rm = TRUE) +
    geom_vline(xintercept = fy_start, color = "white", 
               size = .9, linetype = "dotted") +
    geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.5, na.rm = TRUE) +
    scale_y_continuous(label = label_number_si(), position = "right", expand = c(.01, .01)) +
    scale_x_discrete(breaks = pd_breaks, labels = str_remove(pd_breaks, "FY[:digit:]{2}(?!Q1)")) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL, 
         title = glue("USAID has initiated {label_number_si()(prep_cum)} \\
                      on PrEP this year across \\
                      {filter(df_cntry_cnt, fiscal_year == max(fiscal_year)) %>% pull()} \\
                      countries") %>% toupper,
         subtitle = glue("Up from {filter(df_cntry_cnt, fiscal_year == 2017) %>% pull()} \\
                      countries in 2017"),
         caption = glue("Source: {msd_source}
                        Created by: USAID OHA SI Team | {ref_id}")) +
    geom_label(aes(label=achv), hjust="center", color=scooter, label.size=NA, fill=NA,
               nudge_x=0, nudge_y = 20e3,
               family = "Source Sans Pro", fontface="bold", size = 11/.pt)+
    si_style_ygrid()
    

  v +
    annotate("text",
             x = 9.5, y = 15e3, 
             hjust = "left", lineheight = .9,
             label = "PrEP reported semi-annually in \nFY19-20, so Q1/Q3 were reported\n in aggregate with Q2/Q4 reporting",
             family = "Source Sans Pro", size = 9/.pt, color = matterhorn) +
    annotate("curve",
             arrow = arrow(length = unit(0.05, "inches"),
                           type = "closed", ends = "first"),
             x = 9, y = 22e3, xend = 9.4, yend = 13e3,
             color = matterhorn) +
    annotate("text",
             x = 16.8, y = 40e3,
             hjust = "right", lineheight = .9,
             label = "PrEP returned to quarterly\n reporting for the first time \nsince FY18, making a false \nimpression that there was \na decline in results",
             family = "Source Sans Pro", size = 9/.pt, color = matterhorn) +
    annotate("curve",
             arrow = arrow(length = unit(0.05, "inches"),
                           type = "closed"),
             x = 16.9, y = 60e3, xend = 17, yend = 75e3,
             color = matterhorn)
 
  si_save(glue("Graphics/{curr_pd}_prev_qtr_prep-usaid-scaleup.svg"))
  si_save(glue("Images/{curr_pd}_prev_qtr_prep-usaid-scaleup.png")) 
  
 

  