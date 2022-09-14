# PROJECT: SI Retreat TDY Analysis
# PURPOSE: Munge and Analysis of TDY data scraped from leave tracker
# AUTHOR: Tim Essam | SI
# REF ID:   22f53f0a
# LICENSE: MIT
# DATE: 2022-09-13
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(googlesheets4)
    library(tidytext)
    library(glue)
    library(scales)
    library(extrafont)
    
  # SI specific paths/functions  
  load_secrets()
  
  # REF ID for plots
    ref_id <- "22f53f0a"
    drive_id <- "1E0C2lX_j2AUfSQX8EdjfvFCI-DnT6w1pb1qO904I0As"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_tdy <- read_sheet(ss = drive_id)

# MUNGE ============================================================================
  
  #  How many total TDY days? / Per OU?
    df_tdy_ou <-  
      df_tdy %>% 
      group_by(ou, type) %>% 
      summarize(total = sum(total_days, na.rm = T)) %>% 
      group_by(type) %>% 
      mutate(type_total = sum(total),
             share = total / type_total) %>% 
      ungroup()
    
   tot_hours <-  df_tdy_ou %>% 
    summarise(total = sum(total)) %>% 
     pull()
   
   sa_tot <- df_tdy_ou %>% 
     filter(ou == "South Africa") %>% 
     summarise(total = sum(total)) %>% 
     pull()
    
  df_tdy_ou %>% 
    mutate(ou_order = tidytext::reorder_within(ou, share, type),
           ou_color = ifelse(ou == "South Africa", scooter_med, trolley_grey)) %>% 
    ggplot(aes(x = total, y = ou_order, fill = ou_color)) +
    geom_col()+
    geom_text(aes(label = scales::percent(share, 1)), hjust = 1, 
              family = "Source Sans Pro", 
              color = "white") +
    scale_y_reordered() +
    facet_wrap(~type, scales = "free") +
    scale_fill_identity() +
    si_style_xgrid(facet_space = 0.5) +
    labs(title = glue::glue("SI provided {tot_hours} days of TDY support since January 2022"), 
         subtitle = glue::glue("South Africa received the most support at {sa_tot} days, over half of those days were in-person TDY support"),
         caption = "Data extracted from SIEI Division TDY & Leave Tracker", 
         x = NULL, 
         y = NULL) 
  
  si_save("Images/SI_retreat_TDY_support.png")
  
  str(df_tdy)
  
  df_tdy %>% 
    mutate(across(c(tdy_start, tdy_end), as.Date)) %>% 
    mutate(type_color = ifelse(type == "Virtual", golden_sand, denim)) %>% 
    ggplot(aes(y = ou)) +
    annotate(geom = "rect", 
             xmin = as.Date("2022-07-01"), 
             xmax = as.Date("2022-08-01"),
             ymin = -Inf,
             ymax = Inf,
             fill = grey10k, alpha = 0.55) +
    geom_linerange(aes(xmin = tdy_start, xmax = tdy_end,  
                     color = type_color, alpha = loe, group = tdy_id), 
                 size = 2, 
                 position = position_dodge(width  = 0.5)) +
    scale_color_identity() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    facet_wrap(~type)+
    si_style() +
    scale_alpha_binned(range = c(0.2, 1)) +
    theme(legend.position = "none") +
    labs(title = "SI conducted the most TDYs in July",
         subtitle = "The length of the bar is the duration of the TDY",
         caption = "Data extracted from SIEI Division TDY & Leave Tracker",
         x = NULL, y = NULL)
    
  si_save("Images/SI_retreat_TDY_time.png", scale = 1.25)
    

      
  group_by(OU, Type) %>% 
      mutate(ou_total = sum(total_days, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(ou_share = ou_total / grand_tally)
    
  # Collapse data down to OU level for high level summary
    df_
    
    
    
  df_tdy %>% 
    mutate(ou_order = fct_reorder(OU, total_days)) %>% 
    ggplot(aes(x = total_days, y = ou_order)) +
    geom_col() 
  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

