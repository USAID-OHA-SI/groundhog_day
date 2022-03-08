# PROJECT:  groundhogday
# AUTHOR:   A.Chafetz, K. Srikanth| USAID
# PURPOSE:  South Africa district review
# LICENSE:  MIT
# DATE:     2021-08-18
# UPDATED:  2022-03-08
# NOTES:    updated from FY21Q2_ZAF_District-Review.R

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
  library(googledrive)
  library(vroom)
  library(svglite)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
load_secrets()

drive_id <- as_id("1JG04zIjIxXeOlBaSLhWPMDTILOZuR-z9")

# DOWNLOAD ----------------------------------------------------------------

  #grab file name and id
    file <- drive_ls(drive_id, "zip")

  #create temp folder
    temp_folder()

  #download to temp folder
    drive_download(as_id(file$id),
                   path = file.path(folderpath_tmp, file$name), overwrite = TRUE)

# IMPORT ------------------------------------------------------------------

  #import data
    df <- vroom(file.path(folderpath_tmp, file$name))


# MUNGE -------------------------------------------------------------------

  #identify district type
    df <- df %>% 
        mutate(dist_type = ifelse(str_detect(snuprioritization, "Scale-Up"), 
               "PEPFAR Focus Districts", "Non-Focus Districts"))
      
  #remove extra details from PSNU names & adjust source
    df <- df %>% 
      clean_psnu() %>% 
      mutate(source_name = recode(source_name,
                                  "Derived" = "DATIM",
                                  "DHIS" = "DHIS_and_Private",
                                  "Private Sector" = "DHIS_and_Private",
                                  "naomi" = "NAOMI" ))
    
# TX_CURR GROWTH ----------------------------------------------------------

    # df %>% 
    #   filter(indicator == "TX_CURR",
    #          str_detect(period, "Q"),
    #          standardizeddisaggregate == "Total Numerator") %>% 
    #   count(source_name, period, indicator, standardizeddisaggregate, wt = value) %>% 
    #   prinf()
    # 
    df_tx_delta <- df %>% 
      filter(indicator == "TX_CURR",
             str_detect(period, "Q"),
             standardizeddisaggregate == "Total Numerator",
             source_name != "DATIM") %>% 
      mutate(period = str_replace(period, "20", "FY")) 
    
    
    df_tx_delta <- df_tx_delta %>% 
      group_by(period, psnu, dist_type, source_name, indicator) %>% 
      summarise(value = sum(value, na.rm = TRUE),
                .groups = "drop") %>% 
      arrange(psnu, source_name, period) 
    
    #drop DATIM values from centrally supported
    df_tx_delta <- df_tx_delta %>%
      filter(!(source_name == "DATIM" & dist_type != "PEPFAR Focus Districts"))
    
    df_tx_delta <- df_tx_delta %>% 
      group_by(psnu, source_name) %>% 
      mutate(tx_nn = value - lag(value, order_by = period),
             tx_nn_abs = abs(tx_nn),
             growth = (value / lag(value, order_by = period)) - 1,
             growth_abs = abs(growth),
             growth_abs = ifelse(growth_abs > .11, .11, growth_abs)) %>% 
      ungroup() %>% 
      mutate(growth = ifelse(is.infinite(growth), 1, growth),
             direction = ifelse(growth >= 0, "Positive", "Negative"))
    
    df_tx_delta <- df_tx_delta %>%
      mutate(order_datim = ifelse(period == max(period) & source_name == "DATIM", value, 0),
             psnu_datim = fct_reorder(psnu, order_datim, max),
             order_dhis = ifelse(period == max(period) & source_name == "DHIS_and_Private", value, 0),
             psnu_dhis = fct_reorder(psnu, order_datim, max),
             )

    
    df_nat_gr_vals <- df_tx_delta %>% 
      filter(source_name == "DHIS_and_Private",
             str_detect(period, "FY20Q4|FY21")) %>% 
      bind_rows(df_tx_delta %>% 
                  filter(source_name == "DHIS_and_Private",
                         str_detect(period, "FY20Q4|FY21")) %>% 
                  mutate(dist_type = "National")) %>% 
      group_by(period, dist_type) %>% 
      summarise(value= sum(value, na.rm = TRUE),
                .groups = "drop") %>% 
      filter(period == min(period) | period == max(period)) %>% 
      group_by(dist_type) %>% 
      mutate(growth = (value - lag(value))/lag(value)) %>% 
      ungroup() %>% 
      filter(period == max(period)) %>% 
      select(-value) %>% 
      pivot_wider(names_from = dist_type, 
                  values_from = growth)
    
    
    df_tx_delta %>% 
      filter(source_name == "DHIS_and_Private",
             psnu != "National",
             period != min(period)) %>% 
      ggplot(aes(period, fct_reorder(psnu_dhis, order_dhis, max, na.rm = TRUE), color = direction, size = growth_abs)) +
      geom_point(alpha = .6) +
      facet_grid(dist_type~., scales = "free_y", space = "free", switch = "y") +
      scale_x_discrete(position = "top") +
      scale_color_manual(values = c(old_rose, si_palettes$scooters[4])) +
      scale_size(label = percent_format(1)) +
      coord_cartesian(clip = "off") +
      labs(x= NULL, y = NULL,
           title = glue("SOUTH AFRICA EXPERIENCED POSITIVE A POSITIVE PATIENT GROWTH RATE OF {percent(df_nat_gr_vals$National,.1)} ACROSS FY21"),
           subtitle = glue("PEPFAR Focus Districts grew by {percent(df_nat_gr_vals$`PEPFAR Focus Districts`,.1)} compared with {percent(df_nat_gr_vals$`Non-Focus Districts`,.1)} for Non-Focus Districts"),
           color = "Growth Direction",
           size = "% Patients Gained/Lost",
           caption = "Notes: (a) District sorted on FY21 TX_CURR; (b) Growth rates capped at 11% | Source: South Africa DHIS [2022-03-08]
           SI Analytics: Aaron Chafetz | US Agency for International Development") +
      
      si_style_nolines() +
      theme(strip.placement = "outside",
            strip.text.y = element_text(hjust = .5),
            panel.spacing = unit(1, "lines"),
            plot.title.position = "plot") 
      
    
    si_save("Graphics/FY22Q4_ZAF_TX_Growth_share.svg",
            height = 9,  
            width = 10)
    