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
      
  #remove extra details from PSNU names & adjust source
    df <- df %>% 
      clean_psnu() %>% 
      mutate(source_name = recode(source_name,
                                  "Derived" = "DATIM",
                                  "DHIS" = "DHIS_and_Private",
                                  "Private Sector" = "DHIS_and_Private",
                                  "naomi" = "NAOMI" ))
    
  #aggregate PLHIV and TX_CURR
    df_plhiv_grps <- df %>% 
      mutate(indicator = recode(indicator, 
                                 "Number PLHIV aware" = "PLHIV_AWARE")) %>% 
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
      labs(x = NULL, y = NULL, fill = NULL,
           title = "WHILE MOST PLHIV ARE ON ART, A QUARTER KNOW THEIR STATUS BUT ARE NOT ON ART",
           subtitle = "28% of PLHIV in Non-Focus Districts know their status but are not on ART compared with 23% in PEPFAR Focus Districts",
           caption = "Source: South Africa DHIS + Private Sector
           SI Analytics: Aaron Chafetz | US Agency for International Development") +
      theme(panel.spacing.x = unit(.2, "lines"),
            plot.title.position = "plot",
            legend.position = "none",
            axis.text.y = element_text(size = 7))
    
    si_save("Graphics/FY21Q2_ZAF_PLHIV-breakdown.svg")
    
    df_plhiv_grps_viz %>% 
      filter(source_name == "DHIS_and_Private") %>% 
      ggplot(aes(share, psnu, fill = dist_type, alpha = group_alpha)) +
      geom_col() +
      facet_grid(~group, scales = "free", space = "free") +
      scale_x_continuous(label = percent_format(1)) +
      scale_alpha_identity() +
      scale_fill_manual(values = c(golden_sand, scooter)) + 
      si_style_xgrid() +
      labs(x = NULL, y = NULL, fill = NULL,
           title = "WHILE MOST PLHIV ARE ON ART, 30% KNOW THEIR STATUS BUT ARE NOT ON ART",
           subtitle = "28% of PLHIV in Non-Focus Districts know their status but are not on ART compared with 23% in PEPFAR Focus Districts",
           caption = "Source: South Africa DHIS + Private Sector; NAOMI
           SI Analytics: Aaron Chafetz | US Agency for International Development") +
      theme(panel.spacing.x = unit(.2, "lines"),
            plot.title.position = "plot",
            legend.position = "none",
            axis.text.y = element_text(size = 7))
    
    si_save("Graphics/FY21Q2_ZAF_PLHIV-breakdown_share.svg")
      

    
    
    df_plhiv_grps_viz %>% 
      filter(source_name == "DATIM") %>% 
      ggplot(aes(value, psnu, fill = dist_type, alpha = group_alpha)) +
      geom_col() +
      facet_grid(~group, scales = "free", space = "free") +
      scale_x_continuous(label = label_number_si()) +
      scale_alpha_identity() +
      scale_fill_manual(values = c(scooter)) + 
      si_style_xgrid() +
      labs(x = NULL, y = NULL, fill = NULL,
           title = "WHILE MOST PLHIV ARE ON ART, A QUARTER KNOW THEIR STATUS BUT ARE NOT ON ART",
           subtitle = "Only 6% of PLHIV are undiagnosed according to the NAOMI model",
           caption = "Source: DATIM; NAOMI
           SI Analytics: Aaron Chafetz | US Agency for International Development") +
      theme(panel.spacing.x = unit(.2, "lines"),
            plot.title.position = "plot",
            legend.position = "none",
            # axis.text.y = element_text(size = 7)
            )
    
    si_save("Graphics/FY21Q2_ZAF_PLHIV-breakdown-DATIM.svg")
    
    
    df_plhiv_grps_viz %>% 
      filter(source_name == "DATIM") %>% 
      ggplot(aes(share, psnu, fill = dist_type, alpha = group_alpha)) +
      geom_col() +
      facet_grid(~group, scales = "free", space = "free") +
      scale_x_continuous(label = percent_format(1)) +
      scale_alpha_identity() +
      scale_fill_manual(values = c(scooter)) + 
      si_style_xgrid() +
      labs(x = NULL, y = NULL, fill = NULL,
           title = "WHILE MOST PLHIV ARE ON ART, A QUARTER KNOW THEIR STATUS BUT ARE NOT ON ART",
           subtitle = "Only 6% of PLHIV are undiagnosed according to the NAOMI model",
           caption = "Source: DATIM; NAOMI
           SI Analytics: Aaron Chafetz | US Agency for International Development") +
      theme(panel.spacing.x = unit(.2, "lines"),
            plot.title.position = "plot",
            legend.position = "none",
            # axis.text.y = element_text(size = 7)
            )
    
    si_save("Graphics/FY21Q2_ZAF_PLHIV-breakdown-DATIM_share.svg")
    
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
    
    df_tx_delta <- df %>% 
      filter(indicator == "TX_CURR",
             str_detect(period, "Q"),
             standardizeddisaggregate == "Total Numerator",
             source_name != "DATIM",
             period %ni% c("2021Q3", "2021Q4")) %>% 
      mutate(source_name = recode(source_name,
                                  "Derived" = "DATIM",
                                  "DHIS" = "DHIS_and_Private",
                                  "Private Sector" = "DHIS_and_Private",
                                  "naomi" = "NAOMI"),
             period = str_replace(period, "20", "FY")) %>% 
      group_by(period, psnu, dist_type, source_name, indicator) %>% 
      summarise(value = sum(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      arrange(psnu, source_name, period) 
    
    #drop DATIM values from centrally supported
    df_tx_delta <- df_tx_delta %>%
      filter(!(source_name == "DATIM" & dist_type != "PEPFAR Focus Districts"))
    
    df_tx_delta <- df_tx_delta %>% 
      group_by(psnu, source_name) %>% 
      mutate(tx_nn = value - lag(value, order_by = period),
             tx_nn_abs = abs(tx_nn),
             growth = (value / lag(value, order_by = period)) - 1,
             growth_abs = abs(growth)) %>% 
      ungroup() %>% 
      mutate(growth = ifelse(is.infinite(growth), 1, growth),
             direction = ifelse(growth >= 0, "Positive", "Negative"))
    
    df_tx_delta <- df_tx_delta %>%
      mutate(order_datim = ifelse(period == max(period) & source_name == "DATIM", value, 0),
             psnu_datim = fct_reorder(psnu, order_datim, max),
             order_dhis = ifelse(period == max(period) & source_name == "DHIS_and_Private", value, 0),
             psnu_dhis = fct_reorder(psnu, order_datim, max),
             )

    
    df_tx_delta %>% 
      filter(source_name == "DHIS_and_Private",
             period != min(period)) %>% 
      ggplot(aes(period, psnu_dhis, color = direction, size = tx_nn_abs)) +
      geom_point(alpha = .5) +
      facet_grid(dist_type~., scales = "free_y", space = "free") +
      scale_x_discrete(position = "top") +
      scale_color_manual(values = c(old_rose, genoa_light)) +
      scale_size(label = label_number_si()) +
      labs(x= NULL, y = NULL,
           color = "Growth Direction",
           size = "Patients Gained/Lost") +
      si_style_nolines()
    
    
    df_tx_delta %>% 
      filter(source_name == "DHIS_and_Private",
             period != min(period)) %>% 
      ggplot(aes(period, psnu_dhis, color = direction, size = growth_abs)) +
      geom_point(alpha = .6) +
      facet_grid(dist_type~., scales = "free_y", space = "free", switch = "y") +
      scale_x_discrete(position = "top") +
      scale_color_manual(values = c(old_rose, si_palettes$scooters[4])) +
      scale_size(label = percent_format(1)) +
      labs(x= NULL, y = NULL,
           title = "POSITIVE BUT SLOW GROWTH IN FY21, AVERAGING 1.3% EACH QUARTER",
           subtitle = "PEPFAR Focus Districts grew by an average of 1.4% in FY21 quarters compared with 0.8% for Non-Focus Districts",
           color = "Growth Direction",
           size = "% Patients Gained/Lost",
           caption = "Note: District sorted on FY21 TX_CURR; Source: South Africa DHIS + Private Sector
           SI Analytics: Aaron Chafetz | US Agency for International Development") +
      si_style_nolines() +
      theme(strip.placement = "outside",
            strip.text.y = element_text(hjust = .5),
            panel.spacing = unit(1, "lines"),
            plot.title.position = "plot") 
      
    
    si_save("Graphics/FY21Q2_ZAF_TX_Growth_share.svg",
            width = 8)
    
    
    df_tx_delta %>% 
      filter(source_name == "DATIM",
             period != min(period)) %>% 
      ggplot(aes(period, psnu_dhis, color = direction, size = growth_abs)) +
      geom_point(alpha = .6) +
      facet_grid(dist_type~., scales = "free_y", space = "free", switch = "y") +
      scale_x_discrete(position = "top") +
      scale_color_manual(values = c(old_rose, si_palettes$scooters[4])) +
      scale_size(label = percent_format(1)) +
      labs(x= NULL, y = NULL,
           title = "POSITIVE BUT SLOW GROWTH IN FY21, AVERAGING 2% EACH QUARTER",
           subtitle = "Growth is up in FY21 from FY20, but still not at FY19 levels",
           color = "Growth Direction",
           size = "% Patients Gained/Lost",
           caption = "Note: District sorted on FY21 TX_CURR; Source: DATIM
           SI Analytics: Aaron Chafetz | US Agency for International Development") +
      si_style_nolines() +
      theme(strip.placement = "outside",
            strip.text.y = element_text(hjust = .5),
            panel.spacing = unit(1, "lines"),
            plot.title.position = "plot") 
    
    
    si_save("Graphics/FY21Q2_ZAF_TX_Growth-DATIM_share.svg",
            width = 8)
    