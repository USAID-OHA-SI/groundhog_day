# PROJECT:  FY21Q1 Quarterly Review
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  TX partner Trends
# LICENSE:  MIT
# DATE:     2021-03-08
# UPDATED: 

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
  

# LARGEST FY21 PARTNER TARGET SHARES --------------------------------------

  df_tx <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID",
           fiscal_year == "2021") %>% 
    rename_official() %>%
    mutate(primepartner = ifelse(str_detect(primepartner, "^TBD"), glue("{operatingunit} TBD"), primepartner)) %>% 
    group_by(primepartner) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE))  %>% 
    ungroup()  

  df_tx <- df_tx %>% 
    mutate(primepartner = case_when(str_detect(tolower(primepartner), "baylor") ~ "Baylor",
                               str_detect(tolower(primepartner), "intrahealth") ~ "Intrahealth",
                               str_detect(tolower(primepartner), "population services") ~ "PSI",
                               primepartner %in% c("John Snow, Incorporated", "JSI Research And Training Institute, INC.") ~ "Intrahealth",
                               primepartner %in% c("FHI Development 360 LLC", "Family Health International") ~ "FHI",
                               TRUE ~ primepartner)) %>% 
    group_by(primepartner) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE))  %>% 
    ungroup() %>% 
    arrange(desc(targets)) %>% 
    mutate(share = targets/sum(targets))
  
  df_tx <- df_tx %>% 
    arrange(desc(share)) %>% 
    mutate(cumshare = cumsum(share))
  
  df_tx <- df_tx %>% 
    mutate(primepartner = fct_lump(primepartner, prop = .03, w = share, other_level = "All Other")) 
  
  lrg_partners <- df_tx %>%
    distinct(primepartner) %>% 
    mutate(primepartner = as.character(primepartner)) %>% 
    pull(primepartner)
  
  df_tx <- df_tx %>% 
    mutate(primepartner = str_remove_all(primepartner, "( \\(PTY\\) LTD| Inc\\.| CONSULTING LIMITED|ANIZATION|ELOPMENT)"),
           primepartner = case_when(primepartner == "Elizabeth Glaser Pediatric Aids Foundation" ~ "EGPAF",
                                    TRUE ~ primepartner),
           primepartner = toupper(primepartner))
  
 v1 <- df_tx %>% 
   mutate(primepartner = fct_lump(primepartner, prop = .03, w = share)) %>% 
   group_by(primepartner) %>% 
   summarise(across(c(cumulative, targets), sum, na.rm = TRUE))  %>% 
   ungroup() %>% 
   mutate(primepartner = fct_reorder(primepartner, targets),
          primepartner = fct_relevel(primepartner, "ALL OTHER", after = 0),
          achv = cumulative / targets,
          label = percent(achv, 1)) %>% 
   ggplot(aes(y = primepartner)) +
   geom_col(aes(targets), fill = trolley_grey_light) + 
   geom_col(aes(cumulative, fill = primepartner == "ALL OTHER")) + 
   geom_errorbar(aes(y = primepartner, xmin = targets, xmax =targets),
                 color = trolley_grey) +
   geom_text(aes(x = 70000, label = label), size = 2.5,
             family = "Source Sans Pro", color = "white") +
   scale_x_continuous(labels = unit_format(.1, unit = "M", scale = 1e-6), expand = c(.005, .005)) +
   scale_fill_manual(values = c(scooter, denim)) +
   labs(x = NULL, y = NULL, 
        subtitle = "80% of FY21 USAID targets are held by 13 partners",
        caption = "Source: FY21Q1i MSD") +
   si_style_xgrid() +
   theme(legend.position = "none",
         axis.text.y = element_text(size = 7))


# TRENDS BY PARTNER TYPE --------------------------------------------------

  df_tx_trends <- df %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fundingagency == "USAID") %>% 
    rename_official() %>%
    mutate(primepartner = ifelse(str_detect(primepartner, "^TBD"), glue("{operatingunit} TBD"), primepartner),
           partner_type = ifelse(primepartner %in% lrg_partners, "Largest FY21 Partners", "All Other")) %>% 
    group_by(fiscal_year, partner_type) %>% 
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE))  %>% 
    ungroup()  

 df_tx_trends <-  df_tx_trends %>%
    pivot_longer(starts_with("qtr"),
                 names_to = "qtr",
                 names_prefix = "qtr",
                 values_to = "cumulative") %>% 
    filter(cumulative > 0) %>% 
    mutate(period = glue("FY{str_sub(fiscal_year, 3,4)}Q{qtr}")) %>% 
    mutate(achv = cumulative / targets,
           label = percent(achv, 1))

 v2 <- df_tx_trends %>% 
   ggplot(aes(period, fill = partner_type)) +
   geom_col(aes(y = targets), fill = trolley_grey_light) +
   geom_col(aes(y = cumulative)) +
   geom_errorbar(aes(ymin = targets, ymax = targets), color = trolley_grey) +
   geom_text(aes(y = 500000, label = label), size = 2.5,
             family = "Source Sans Pro", color = "white") +
   facet_grid(fct_rev(partner_type) ~ ., switch = "y") +
   expand_limits(y = 5000000) +
   scale_fill_manual(values = c(denim, scooter)) +
   scale_y_continuous(labels = unit_format(1, unit = "M", scale = 1e-6)) +
   scale_x_discrete(breaks = c("FY19Q1", "FY20Q1", "FY21Q1")) +
   labs(x = NULL, y = NULL,
        subtitle = "Flatline During COVID in FY20",
        caption = "Source: FY21Q1i MSD") +
   si_style_ygrid() +
   theme(panel.spacing = unit(.1, "lines"),
         strip.placement = "outside",
         strip.text.y = element_text(hjust = .5),
         legend.position = "none")

 v1 + v2 

 si_save("Images/FY21Q1_GLOBAL_LARGE_TX_CURR_PARTNERS.png",
         width = 9.5, height = 4.5)  
 
 
 