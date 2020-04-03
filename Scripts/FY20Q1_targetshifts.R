##  PROJECT: Q1 review target analysis
##  AUTHOR:  achafetz tessam | USAID
##  PURPOSE: munge historic targets to fy21 targets, viz
##  LICENCE: MIT
##  DATE:    2020-04-01
##  UPDATE:  2020-04-03

#Dependancies----------------------------------------------------------

  library(tidyverse)
  library(vroom)
  library(scales)
  library(extrafont)
  library(ggrepel)


  source("Scripts/si_style.R")

#folders---------------------------------------------------------------

  data_in <- "Data"
  data_out <- "Dataout"
  viz_folder <- "Images"

# GLOBAL VARIABLES --------------------------------------------------------

  # Indicators of focus
    ind_grp1 <- c("HTS_TST_POS", "TX_NEW", "TX_CURR")
    ind_grp2 <- c("VMMC_CIRC", "PrEP_NEW")
    ind_grp3 <- c("OVC_SERV", "KP_PREV")
    indc <- c(ind_grp1, ind_grp2, ind_grp3)
  
# IMPORT ------------------------------------------------------------------
  
  #import
    df_21 <- vroom(file.path(data_in, "df_mer_21.csv"))
  
# MUNGE -------------------------------------------------------------------

  df_viz_withinou <- df_21 %>% 
    filter(indicator %in% indc, fiscal_year != 2015) %>% 
    group_by(operatingunit, fiscal_year, indicator, agency_other) %>% 
    summarise(targets = sum(targets, na.rm = TRUE)) %>% 
    group_by(indicator, fiscal_year, operatingunit) %>% 
    mutate(tot_targets = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(agency_share = targets / tot_targets) %>% 
    arrange(operatingunit, fiscal_year, indicator) %>% 
    group_by(indicator, operatingunit, agency_other) %>% 
    mutate(lag_share = lag(agency_share, n = 1, order_by = fiscal_year),
           share_diff = agency_share - lag_share, 
           abs_share_diff = abs(share_diff)) %>% 
    ungroup() %>% 
    group_by(agency_other, fiscal_year, indicator) %>% 
    mutate(rank_share_diff = percent_rank(abs_share_diff)) %>% 
    ungroup()
  
  #limit to USAID 2020 -21
    df_viz_withinou_usaid <- df_viz_withinou %>%
      filter(fiscal_year %in% c(2020, 2021), 
             agency_other == "USAID")
  
  #only want to plot large diff = >10% change
    ou_ind_keep <- df_viz_withinou_usaid %>% 
      filter(fiscal_year == 2021)  %>% 
      filter(abs_share_diff > .1) %>% 
      select(operatingunit, indicator)
    
  #limit using semi_join
    df_viz_withinou_fltr <- df_viz_withinou_usaid %>% 
      semi_join(ou_ind_keep, by = c("operatingunit", "indicator")) 
  
  #global USAID share
    df_viz_withinou_usaid_overall <- df_viz_withinou_usaid %>% 
      group_by(fiscal_year, indicator) %>% 
      summarize(usaid_share = sum(targets, na.rm = TRUE)/sum(tot_targets, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(operatingunit = case_when(indicator == "HTS_TST_POS" ~ "USAID Globally"))
  
  #bind OU + Global together
    df_viz_withinou_fltr <- df_viz_withinou_fltr %>% 
      bind_rows(df_viz_withinou_usaid_overall)
    
  #adjust var ordering
    df_viz_withinou_fltr <- df_viz_withinou_fltr %>% 
      mutate(indicator = factor(indicator, indc))
  

# PLOT --------------------------------------------------------------------

  plot_slope <- function(ind_sel, outpath = NULL){
    
    plot <- df_viz_withinou_fltr %>% 
      filter(indicator %in% {{ind_sel}}) %>% 
      ggplot(aes(x = fiscal_year, y = agency_share, group = operatingunit,
                 label = if_else(fiscal_year == max(fiscal_year), operatingunit, NA_character_))) +
      geom_line(aes(y = usaid_share), size = .5, colour = "gray80", na.rm = TRUE) +
      geom_point(aes(y = usaid_share), size = 5, shape = 21, fill = "#909090", colour = "gray80", stroke = 0.25, na.rm = TRUE) +
      geom_line(size = .5, colour = "gray80", na.rm = TRUE) +
      geom_point(aes(fill = agency_share), size = 5, shape = 21, colour = "#909090", stroke = 0.25, na.rm = TRUE) +
      geom_text_repel(hjust = 0,
                      force = 9, point.padding=unit(1, 'lines'),
                      direction = 'x',
                      nudge_x = 0.1,
                      segment.size = 0.1,
                      size = 3,
                      family = "Source Sans Pro",
                      na.rm = TRUE) +
      geom_text_repel(aes(y = usaid_share),
                      hjust = 0,
                      force = 1, point.padding=unit(1, 'lines'),
                      direction = 'x',
                      nudge_x = 0.1,
                      segment.size = 0.1,
                      size = 3,
                      family = "Source Sans Pro",
                      na.rm = TRUE) +
      facet_wrap(~indicator) +
      theme_minimal() + si_style() +
      theme(legend.position = "none",
            panel.grid.major.y = ggplot2::element_blank(),
            plot.caption = element_text(hjust = 0, face = "italic"),
            strip.text = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(margin = margin(0, 0, 10, 0))) +
      scale_fill_viridis_c(label = scales::percent, direction = -1, option = "A") +
      scale_x_continuous(breaks = c(2020, 2021), limits = c(2020, 2021.5)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = NULL, y = NULL, 
           title = "GLOBALLY NO MAJOR SHIFT IN TARGET SHARES",
           subtitle = "Largest target gains/losses (+10%) displayed against USAID overall share",
           caption = "  Note: Only OUs with shifts of +10% between 2020-21 targets are displayed
         Source: FY15-16 MSD, FY17-20 MSD, COP20 Data Pack")
    
    if(!is.null(outpath)){
      ind_sel <- str_remove_all(ind_sel, "_") %>% paste0(collapse = "_")
      ggsave(file.path(viz_folder, paste0("Q1Review_targetshifts_slope_",ind_sel, ".png")), 
            dpi = 330, width = 10, height = 5.66, scale = 1.2)
    }
    
    return(plot)
    
  }  
 

    
  #testing
    plot_slope(ind_grp1)
    
  #plot by group
    plot_slope(ind_grp1, viz_folder)
    plot_slope(ind_grp2, viz_folder)
    plot_slope(ind_grp3, viz_folder)
