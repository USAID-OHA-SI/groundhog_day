# PROJECT:  FY21Q1 Quarterly Review
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  TX partner Trends
# LICENSE:  MIT
# DATE:     2021-03-08
# UPDATED:  2021-03-09

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
  
  

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   
  

# SETUP DATASET -----------------------------------------------------------
  
  #filter for TX_CURR USAID and rename to latest partner names
  df_tx <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID") %>% 
    rename_official() 
  
  #clean up partner names for aggregation and partner share
  df_tx_clean <- df_tx %>% 
    mutate(primepartner = ifelse(str_detect(primepartner, "^TBD"), glue("{operatingunit} TBD {mech_code}"), primepartner),
           primepartner = case_when(str_detect(tolower(primepartner), "baylor") ~ "Baylor",
                                    str_detect(tolower(primepartner), "intrahealth") ~ "Intrahealth",
                                    str_detect(tolower(primepartner), "population services") ~ "PSI",
                                    str_detect(primepartner, "SOCIETY FOR FAMILY HEALTH") ~ "SOCIETY FOR FAMILY HEALTH",
                                    primepartner %in% c("John Snow, Incorporated", "JSI Research And Training Institute, INC.") ~ "Intrahealth",
                                    primepartner %in% c("FHI Development 360 LLC", "Family Health International") ~ "FHI",
                                    TRUE ~ primepartner),
           primepartner = str_remove_all(primepartner, "( \\(PTY\\) LTD| Inc\\.| CONSULTING LIMITED|ANIZATION|ELOPMENT)"),
           primepartner = case_when(primepartner == "Elizabeth Glaser Pediatric Aids Foundation" ~ "EGPAF",
                                    TRUE ~ primepartner),
           primepartner = toupper(primepartner))
  
  #aggregate and create target share and cumulative target share
  df_tx_agg <- df_tx_clean %>% 
    group_by(fiscal_year, primepartner) %>% 
    summarise(across(where(is.double), sum, na.rm = TRUE))  %>% 
    ungroup() %>% 
    arrange(desc(targets)) %>% 
    group_by(fiscal_year) %>% 
    mutate(share = targets/sum(targets)) %>% 
    arrange(fiscal_year, desc(share)) %>% 
    mutate(cumshare = cumsum(share)) %>% 
    ungroup()


# LARGEST FY21 PARTNER TARGET SHARES --------------------------------------

  #filter to FY21 and lump remaining 20% of partners
  df_tx_21 <- df_tx_agg %>% 
    filter(fiscal_year == 2021) %>%
    mutate(primepartner = fct_lump(primepartner, prop = .03, w = share, other_level = "ALL OTHER")) 
  
  #capture largest partners for trends in next section
  lrg_partners <- df_tx_21 %>%
    distinct(primepartner) %>% 
    mutate(primepartner = as.character(primepartner)) %>% 
    pull(primepartner)
  
  #label
  df_lab <- df_tx_clean %>% 
    filter(primepartner %in% lrg_partners,
           fiscal_year == 2021) %>% 
    count(primepartner, countryname, wt = targets) %>% 
    arrange(desc(n)) %>% 
    group_by(primepartner) %>%
    summarise(countryname = paste(countryname, collapse = ", "),
              n = n()) %>% 
    ungroup() %>% 
    mutate(label = ifelse(n <= 3, glue("{primepartner}<br>{countryname}"), glue("{primepartner}<br>{n} countries")))
  
  
  #plot
  (v1 <- df_tx_21 %>% 
      mutate(primepartner = fct_lump(primepartner, prop = .03, w = share)) %>% 
      group_by(primepartner) %>% 
      summarise(across(c(cumulative, targets), sum, na.rm = TRUE))  %>% 
      ungroup() %>% 
      left_join(df_lab) %>% 
      mutate(primepartner = ifelse(is.na(label), "ALL OTHER", label)) %>% 
      mutate(primepartner = fct_reorder(primepartner, targets),
             primepartner = fct_relevel(primepartner, "ALL OTHER", after = 0),
             achv = cumulative / targets,
             label = percent(achv, 1)) %>% 
      ggplot(aes(y = primepartner)) +
      geom_col(aes(targets), fill = trolley_grey_light) + 
      geom_col(aes(cumulative, fill = primepartner == "ALL OTHER")) + 
      geom_errorbar(aes(y = primepartner, xmin = targets, xmax =targets),
                    color = trolley_grey) +
      geom_text(aes(x = 100000, label = label), size = 2.5,
                family = "Source Sans Pro", color = "white") +
      scale_x_continuous(labels = unit_format(.1, unit = "M", scale = 1e-6), expand = c(.005, .005)) +
      scale_fill_manual(values = c(scooter, moody_blue)) +
      labs(x = NULL, y = NULL, 
           subtitle = "70% of FY21 USAID targets held by 11 partners",
           caption = "Source: FY21Q1i MSD") +
      si_style_xgrid() +
      theme(legend.position = "none",
            axis.text.y = element_markdown(size = 7)))


# TRENDS BY PARTNER TYPE --------------------------------------------------

  #aggregate up to largest and other partners
  df_tx_trends <- df_tx_agg %>%
    mutate(partner_type = ifelse(primepartner %in% lrg_partners, "Largest FY21 Partners", "All Other")) %>% 
    group_by(fiscal_year, partner_type) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE))  %>% 
    ungroup()  
  
  #reshape quarters long for quarterly targets achviement
  df_tx_trends <-  df_tx_trends %>%
    pivot_longer(starts_with("qtr"),
                 names_to = "qtr",
                 names_prefix = "qtr",
                 values_to = "cumulative") %>% 
    filter(cumulative > 0) %>% 
    mutate(period = glue("FY{str_sub(fiscal_year, 3,4)}Q{qtr}")) %>% 
    mutate(achv = cumulative / targets,
           label = percent(achv, 1))

 (v2 <- df_tx_trends %>% 
   ggplot(aes(period, fill = partner_type)) +
   geom_col(aes(y = targets), fill = trolley_grey_light) +
   geom_col(aes(y = cumulative)) +
   geom_errorbar(aes(ymin = targets, ymax = targets), color = trolley_grey) +
   geom_text(aes(y = 500000, label = label), size = 2.5,
             family = "Source Sans Pro", color = "white") +
   facet_grid(fct_rev(partner_type) ~ ., switch = "y") +
   expand_limits(y = 7000000) +
   scale_fill_manual(values = c(moody_blue, scooter)) +
   scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6)) +
   scale_x_discrete(breaks = c("FY19Q1", "FY20Q1", "FY21Q1")) +
   labs(x = NULL, y = NULL,
        subtitle = "Flatline During COVID in FY20",
        caption = "Source: FY21Q1i MSD") +
   si_style_ygrid() +
   theme(panel.spacing = unit(.1, "lines"),
         strip.placement = "outside",
         strip.text.y = element_text(hjust = .5),
         legend.position = "none"))

 v1 + v2 

 si_save("Images/FY21Q1_GLOBAL_LARGE_TX_CURR_PARTNERS.png",
         width = 9.5, height = 4.5)  
 