# PROJECT:  FY22Q3 Review
# AUTHOR:   N.Petrovic USAID
# PURPOSE:  treatment scale up since PEPFAR start
# LICENSE:  MIT
# DATE:     2021-05-14
# UPDATED:  2021-12-07

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
  library(janitor)
  library(lubridate)
  library(ggnewscale)
  library(waffle)
  library(gt)


# GLOBAL VARIABLES --------------------------------------------------------
  
  merdata <- si_path("path_msd")
  load_secrets()

  # Default should return current year
  make_year <- function(x = 0){
    paste0(20, curr_fy + x) %>% as.numeric()
  }
  
 # Reference ID to be used for searching GitHub
 ref_id <- "052998d6"
  
# IMPORT ------------------------------------------------------------------
  
  #Current MSD
  df <- si_path() %>% 
    return_latest("OU_IM_FY20-23") %>% 
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
  min_yr <- make_year(-4)
  max_yr <- make_year(1) 

# MUNGE -------------------------------------------------------------------

  pop_sel<-"ALL"
  std_dis<-"Total Numerator"
  #pop_sel<-"PEDS"
  #std_dis<-c("Age/Sex/HIVStatus", "Age/Sex/HIVStatus", "Modality/Age/Sex/Result")
  #pop_sel<-"AGYW"
  #std_dis<-c("Age/Sex/HIVStatus", "Age/Sex/HIVStatus", "Modality/Age/Sex/Result")
  #pop_sel<-"KP"
  #std_dis<-c("KeyPop/HIVStatus", "KeyPop/HIVStatus", "KeyPop/Result")
  
  #source info
  source <- source_info()
  
  ## Filters for 3 indicators w/ Total Numerator disagg and calculates total 
  ## results & targets for USAID
  tot_num<-df %>% 
    bind_rows(df_arch) %>% 
    filter(funding_agency == "USAID",
           indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST_POS"),
           standardizeddisaggregate %in% "Total Numerator",
           between(fiscal_year, min_yr + 1, max_yr - 1)) %>% 
    select(fiscal_year, funding_agency, indicator, cumulative, targets) %>% 
    group_by(fiscal_year, funding_agency, indicator) %>%
    summarise(across(matches("cumulative|target"), sum, na.rm = TRUE)) %>% 
    ungroup() %>%
    rename(res_all=cumulative, tar_all=targets)
  
  
  ## Filters for the three indicators, with the standardized disagg needed for each population subgroup
  ## & variables needed for filtering
  df_tx_all <- df %>% 
    bind_rows(df_arch) %>% 
    filter(funding_agency == "USAID",
           indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST_POS"),
           standardizeddisaggregate %in% std_dis,
           between(fiscal_year, min_yr + 1, max_yr - 1)) %>% 
           select(fiscal_year, funding_agency, indicator, sex, trendscoarse, 
                  ageasentered, otherdisaggregate, cumulative, targets) 
    
  
  ## Filter for selected group
  if (pop_sel=="PEDS"){
    df_tx<- df_tx_all %>%
    mutate(pop=ifelse(trendscoarse=="<15", TRUE, FALSE))
    group_label<-"USAID, Children under 15"
    } else if (pop_sel=="AGYW")  
    {df_tx<- df_tx_all %>% 
    mutate(pop=ifelse(ageasentered %in% c("15-19", "20-24") & sex=="Female", TRUE, FALSE))
    group_label<-"USAID, Females 15-24"
    } else if (pop_sel=="KP")  
    { df_tx<- df_tx_all %>% mutate(pop=TRUE)
    group_label<-"USAID, Key Populations"
    } else {
    df_tx<- df_tx_all %>% mutate(pop=TRUE)
    group_label<-"USAID, all populations"
    }
  

################
 cy<-make_year()
  
   df_viz<- df_tx %>% filter(pop==TRUE) %>%
           group_by(fiscal_year, funding_agency, indicator) %>%
           summarise(across(matches("cumulative|target"), sum, na.rm = TRUE)) %>% 
           ungroup() %>% 
           mutate(
           achv = case_when(targets>0 ~ cumulative/targets, targets==0 ~ NA_real_), 
           ind_color = case_when(indicator == "TX_CURR" ~ genoa, 
                                 indicator == "TX_NEW" ~ moody_blue,
                                 indicator == "HTS_TST_POS" ~ denim)) %>%
           #gap = ifelse(fiscal_year == curr_yr, glue("Gap:\n {comma(targets - cumulative)}"), 
            #                            NA_real_)) %>%
           ## Note currently hardwired for Q3
           mutate(achv_color = case_when(fiscal_year==cy & achv<0.50 ~ si_palettes$siei_ach[1],
                                          fiscal_year==cy & (achv>0.50 & achv<=0.65) ~ si_palettes$siei_ach[2],
                                          fiscal_year==cy & (achv>0.65 & achv<=0.85) ~ si_palettes$siei_ach[3],
                                          fiscal_year==cy & achv>0.85 ~ si_palettes$siei_ach[4],
                                          fiscal_year!=cy & achv<0.75 ~ si_palettes$siei_ach[1],
                                          fiscal_year!=cy & (achv>0.75 & achv<=0.90) ~ si_palettes$siei_ach[2],
                                          fiscal_year!=cy & (achv>0.90 & achv<=1.1) ~ si_palettes$siei_ach[3],
                                          fiscal_year!=cy & achv>1.1 ~ si_palettes$siei_ach[4])) %>%
                                                
            rename(period = fiscal_year, value = cumulative) %>% 
            mutate(period = str_replace(period, "20", "FY")) %>% 
            arrange(indicator, period) %>% 
            mutate(source = "MSD") %>%
            mutate(indicator=fct_relevel(indicator,"HTS_TST_POS", "TX_NEW", "TX_CURR")) %>%
            mutate(ind_label = case_when(indicator == "TX_CURR" ~ "Currently receiving ART",
                                             indicator == "HTS_TST_POS" ~ "Received positive result",
                                             TRUE ~ "Newly enrolled on ART"))
           
              
  

# VIZZZZZZ ----------------------------------------------------------------

nudge_space <- 0.125
  
  df_viz %>% 
    ggplot(aes(x = period, group = indicator)) +
    geom_col(aes(y = targets), fill = grey10k, width = 0.5, position = position_nudge(x = -nudge_space)) +
    geom_col(aes(y = value, fill = ind_color), width = 0.5, position = position_nudge(x = nudge_space)) +
    scale_fill_identity() +
    geom_label(data = . %>% filter(indicator == "TX_NEW"), 
                                   aes(y = 0, label = percent(achv, 1), fill = as.factor(achv_color), 
                                   color = grey90k),
                                   vjust = 0, 
                                   size = 16/.pt, 
                                   label.size = 0.2, family = "Source Sans Pro") +
    geom_label(data = . %>% filter(indicator != "TX_NEW"), 
                                   aes(y = 0, label = percent(achv, 1), fill = as.factor(achv_color), 
                                   color = grey90k), 
                                   vjust = 0, 
                                   size = 16/.pt,
                                   label.size = 0.2, family = "Source Sans Pro") +
    facet_wrap(indicator ~ ind_label, nrow = 1, scales = "free_y") +
    scale_color_identity() +
    si_style_ygrid() +
    scale_y_continuous(labels = label_number_si(), position = "left") +
    theme(axis.text.x = element_text(vjust=4), 
          axis.text = element_text(size = rel(1.3)),
          strip.text = element_text(size=rel(1.3), face="bold"))+
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL, fill = NULL,
         caption = c(glue("Data: {group_label}"), glue("Source: {source} (including FY15-18) 
                                  Created by: USAID OHA SI Team | {ref_id}")))+
     theme(plot.caption = element_text(hjust=c(0, 1)))
  
  si_save(glue("Graphics/{curr_pd}_TX_trends_ou_gaps_{pop_sel}.svg"), height = 4, width = 10, scale = 1.3)
  si_save(glue("Images/{curr_pd}_TX_trends_ou_gaps_{pop_sel}.png"), height = 4, width = 10, scale = 1.3)  
  
  ## Calculate percent
  
  ## Total results and targets for subgroup
  subgrp_tot<-df_tx %>% group_by(fiscal_year, funding_agency, indicator) %>%
    filter(pop==TRUE) %>%
    summarise(across(matches("cumulative|target"), sum, na.rm = TRUE)) %>% 
    ungroup() %>%
    rename(res_subgrp=cumulative, tar_subgrp=targets)
  
  percent_prog<-full_join(subgrp_tot,tot_num, by=c("fiscal_year", "funding_agency","indicator")) %>%
    mutate(res_per=round(res_subgrp/res_all*100),tar_per=round(tar_subgrp/tar_all*100)) %>%
    filter(fiscal_year=="2022") %>%
    select(indicator, res_all,res_subgrp,res_per,tar_all,tar_subgrp,tar_per)
  
  percent_prog %>%
    gt() %>%
    cols_label(res_all="Results All", res_subgrp=glue("Results {pop_sel}"), res_per="Results %",
               tar_all="Targets All", tar_subgrp=glue("Results {pop_sel}"), tar_per="Targets %") %>%
    gtsave(glue("Images/{curr_pd}_percent_programs_{pop_sel}.png"))
  
  ## Double checking
  #df_tst<-df_tx_all %>%
  #filter(indicator=="TX_CURR", fiscal_year=="2022") %>%
  #group_by(ageasentered) %>%
  #summarise(across(matches("cumulative|target"), sum, na.rm = TRUE)) %>% 
  #  ungroup() 
  #df_tst %>%
  #mutate(cum_per=cumulative/sum(cumulative)*100) %>%
  #ggplot(aes(x=ageasentered, y=cum_per)) +geom_bar(stat = "identity")

  
  ## NOTES
  
  ## AGYW -- check w/ Panorama
  ## 2019 results match, targets in Pano are not split by age group so cannot compare
  ## 2020 results match, targets slightly off
  ##      everything matches when resolve_known_issues is removed
  ## 2021 results are all close but slightly off, 
  ##      some of the targets match some are slightly off
  ##      everything matches when resolve_known_issues is removed
  ## 2022 results & targets match perfectly
  
  
 

  