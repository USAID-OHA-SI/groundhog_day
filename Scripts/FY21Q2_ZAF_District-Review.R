# PROJECT:  groundhogday
# AUTHOR:   A.Chafetz, K. Srikanth| USAID
# PURPOSE:  South Africa district review
# LICENSE:  MIT
# DATE:     2021-08-18
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
  library(googledrive)
  library(vroom)
  

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
        mutate(dist_typee = ifelse(str_detect(snuprioritization, "Scale-Up"), 
               "PEPFAR Focus Districts", "Non-Focus Districts"))
      
  #remove extra details from PSNU names
    df <- df %>% 
      clean_psnu()
    
   
    
  df %>% 
    filter(indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           source_name %in% c("Derived"),
           period == "2021cum",
           str_detect(snuprioritization, "Scale-Up")) %>% 
    distinct(snuprioritization)
    
    
    
    df %>% 
      count(source_name, indicator) %>% 
      prinf()

    df %>% 
      filter(indicator == "PLHIV",
             period == "2021cum") %>% 
      count(period, indicator, standardizeddisaggregate, wt = value)
    
    df %>% 
      filter(indicator == "Number PLHIV aware",
             period == "2021cum") %>% 
      count(period, indicator, standardizeddisaggregate, wt = value)
  
    df %>% 
      filter(indicator == "TX_CURR",
             period == "2021cum") %>% 
      count(source_name, period, indicator, standardizeddisaggregate, wt = value)
    
    df %>% 
      filter(indicator == "HIV prevalence") %>% 
      count(period, indicator, standardizeddisaggregate, wt = value)
    
    
    df %>% 
      filter(indicator == "PLHIV") %>% 
      count(period, wt = value)
    
    
    #aggregate PLHIV and TX_CURR
    df_plhiv_grps <- df %>% 
      mutate(indicator = recode(indicator, 
                                 "Number PLHIV aware" = "PLHIV_AWARE"),
             source_name = recode(source_name,
                             "Derived" = "DATIM",
                             "DHIS" = "DHIS_and_Private",
                             "Private Sector" = "DHIS_and_Private",
                             "naomi" = "NAOMI"
                             )) %>% 
      filter(indicator %in% c("PLHIV", "TX_CURR", "PLHIV_AWARE"),
             standardizeddisaggregate == "Total Numerator",
             period == "2021cum") %>%
      mutate(indicator = ifelse(indicator == "TX_CURR", 
                                glue("{indicator}.{source_name}"), indicator)) %>% 
      group_by(period, psnu, dist_type, indicator) %>% 
      summarise(value = sum(value, na.rm = TRUE)) %>% 
      ungroup() 

    #drop DATIM values from centrally supported
    df_plhiv_grps <- df_plhiv_grps %>% 
      mutate(value = ifelse(indicator == "TX_CURR.DATIM" & dist_type != "PEPFAR Focus Districts", NA, value),
             value = round(value))
      
    #reshape for calculating and plotting
    df_plhiv_grps <- df_plhiv_grps %>% 
      pivot_wider(names_from = indicator) %>% 
      pivot_longer(starts_with("TX_CURR"),
                   names_to = "source_name",
                   names_prefix = "TX_CURR.",
                   values_to = "tx_curr") %>% 
      rename_all(tolower)
    
    #adjust PLHIV aware to max at PLHIV
    df_plhiv_grps <- df_plhiv_grps %>% 
      mutate(plhiv_aware = ifelse(plhiv_aware > plhiv, plhiv, plhiv_aware))
    
    #break into groups
    df_plhiv_grps <- df_plhiv_grps %>% 
      filter(!is.na(tx_curr)) %>% 
      mutate(Undiagnosed = plhiv - plhiv_aware,
             Diagnosed_not_on_ART = plhiv_aware - tx_curr,
             On_ART = tx_curr)

    
    #reshape
    df_plhiv_grps_viz <- df_plhiv_grps %>% 
      select(-c(plhiv_aware, tx_curr)) %>% 
      relocate(source_name, .before = plhiv) %>%
      pivot_longer(matches("(Diagnosed|on_ART)"), 
                   names_to = "group") %>% 
      mutate(psnu = fct_reorder(psnu, plhiv),
             group = str_replace_all(group, "_", " "),
             group = factor(group, c("Undiagnosed", "Diagnosed not on ART",
                                     "On ART")),
             group_alpha = case_when(group == "Undiagnosed" ~ 1,
                                     group == "Diagnosed not on ART" ~ .75,
                                     group == "On ART" ~ .5))
    
    
    #shares
    df_plhiv_grps_viz <- df_plhiv_grps_viz %>% 
      group_by(psnu, source_name) %>% 
      mutate(share = value/sum(value)) %>% 
      ungroup()

    df_plhiv_grps_viz %>% 
      filter(source_name == "DHIS_and_Private") %>% 
      ggplot(aes(value, psnu, fill = dist_type, alpha = group_alpha)) +
      geom_col() +
      facet_grid(~group, scales = "free", space = "free") +
      scale_x_continuous(label = label_number_si()) +
      scale_alpha_identity() +
      scale_fill_manual(values = c(golden_sand, scooter)) + 
      si_style_xgrid() +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme(panel.spacing.x = unit(.2, "lines"))
    
    df_plhiv_grps_viz %>% 
      filter(source_name == "DHIS_and_Private") %>% 
      ggplot(aes(share, psnu, fill = dist_type, alpha = group_alpha)) +
      geom_col() +
      facet_grid(~group, scales = "free", space = "free") +
      scale_x_continuous(label = percent_format(1)) +
      scale_alpha_identity() +
      scale_fill_manual(values = c(golden_sand, scooter)) + 
      si_style_xgrid() +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme(panel.spacing.x = unit(.2, "lines"))
      

    # df_plhiv_grps_viz %>%
    #   filter(source_name == "DHIS_and_Private",
    #          group != "On ART") %>%
    #   select(-group_alpha) %>%
    #   pivot_wider(names_from = group,
    #               values_from = c(value, share)) %>%
    #   ggplot(aes(share_Undiagnosed, `share_Diagnosed not on ART`,
    #              size = plhiv, color = dist_type)) +
    #   geom_abline(slope = 1) +
    #   geom_blank(aes(`share_Diagnosed not on ART`, share_Undiagnosed)) +
    #   geom_point(alpha = .7) +
    #   scale_color_manual(values = c(golden_sand, scooter)) +
    #   scale_x_continuous(label = percent_format(1)) +
    #   scale_y_continuous(label = percent_format(1)) +
    #   scale_size(label = label_number_si()) +
    #   labs(x = "Undiagnosed (Share of PLHIV)",
    #        y = "Diagnosed not on ART (Share of PLHIV)",
    #        color = NULL, size = "PLHIV") +
    #   si_style()
    
    df_plhiv_grps_viz %>% 
      filter(source_name == "DHIS_and_Private") %>% 
      ggplot(aes(share, dist_type, size = plhiv, color = dist_type)) +
      geom_jitter(position = position_jitter(width = 0, height = 0.1),
                  alpha = .7) +
      scale_color_manual(values = c(golden_sand, scooter)) + 
      facet_grid(dist_type~group, scale = "free_y") +
      si_style()
    
    
    # df_plhiv_grps_viz %>% 
    #   filter(source_name == "DHIS_and_Private") %>% 
    #   ggplot(aes(share, period, size = plhiv, color = dist_type)) +
    #   geom_jitter(position = position_jitter(width = 0, height = 0.1),
    #               alpha = .7) +
    #   scale_color_manual(values = c(golden_sand, scooter)) + 
    #   scale_x_continuous(label = percent) +
    #   scale_size(label = label_number_si()) +
    #   facet_grid(~group, scale = "free_y") +
    #   labs(y = NULL, x = "Share of PLHIV", color = NULL, size = "PLHIV") +
    #   si_style() +
    #   theme(axis.text.y = element_blank())
    
    

# TX_CURR GROWTH ----------------------------------------------------------

    df %>% 
      filter(indicator == "TX_CURR",
             str_detect(period, "Q"),
             standardizeddisaggregate == "Total Numerator") %>% 
      count(source_name, period, indicator, standardizeddisaggregate, wt = value) %>% 
      prinf()
    
    df %>% 
      filter(indicator == "TX_CURR",
             str_detect(period, "Q"),
             standardizeddisaggregate == "Total Numerator"
             source != "DATIM") %>% 
      mutate(source_name = recode(source_name,
                                  "Derived" = "DATIM",
                                  "DHIS" = "DHIS_and_Private",
                                  "Private Sector" = "DHIS_and_Private",
                                  "naomi" = "NAOMI")) %>% 
      group_by(period,psnu, dist_type, indicator )
      