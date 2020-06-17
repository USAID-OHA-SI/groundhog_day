##  PROJECT: FY20Q2 MWI POART SUPPORT
##  AUTHOR:  achafetz | USAID
##  PURPOSE: requested viz
##  LICENCE: MIT
##  DATE:    2020-06-12
##  UPDATE:  2020-06-15


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(ICPIutilities)
library(scales)
library(extrafont)
library(glitr)
library(patchwork)


# GLOBAL ------------------------------------------------------------------


theme_set(theme_minimal(base_family = "Source Sans Pro"))

lrg_site <- .8

# IMPORT ------------------------------------------------------------------

  #genie data FY20Q2 (prior to MSD release)
    df_genie <- read_msd("Data/Genie-SiteByIMs-Malawi-Daily-2020-06-12.zip")

  #DHA data for TX for trends
    df_dha <- read_excel("Data/DHA_TX_CURR_Data.xlsx")
  

# MUNGE -------------------------------------------------------------------

  df_genie <- rename_official(df_genie)  
  
  df_genie <- df_genie %>% 
    mutate(partner = case_when(str_detect(primepartner, "FHI|Family Health") ~ "FHI360",
                               str_detect(primepartner, "BAYLOR") ~ "Baylor TSP",
                               str_detect(primepartner, "PAKACHERE") ~ "Pakachere",
                               #primepartner == "Partners In Hope" ~ "PIH"
                               TRUE ~ primepartner),
           psnu = str_remove(psnu, " District")
                               )
  
  df_dha <- df_dha %>% 
    gather(period, val, starts_with("FY")) %>% 
    rename_all(tolower) %>% 
    select(orgunituid = orgunit, period, val) %>% 
    filter(period %in% c("FY19Q1", "FY19Q2", "FY19Q3"),
           val > 0)


# FUNCTION - AGG BY GROUP AND CREAT ACHIEVEMENT ---------------------------

  gen_achv <- function(df, ind, disagg, grp){
    df_genie %>% 
      filter(fiscal_year == 2020,
             indicator == ind,
             standardizeddisaggregate == disagg) %>% 
      group_by_at(grp) %>% 
      summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(achievement = cumulative / targets)
  }



# FUNCTION - PLOT ACHIEVEMENT ---------------------------------------------


  plot_achv <- function(df, grp, 
                        main_title = "UPDATE TITLE BASED ON GRAPH MESSAGE", 
                        sub_title,
                        facet_grp = NULL){
    
    #targets & results plot
      v1 <- df %>% 
        ggplot(aes(cumulative, fct_reorder({{grp}}, targets))) +
        geom_blank(aes(x = 1.1 * targets)) +
        geom_vline(xintercept = 0, color = "gray30") +
        geom_col() +
        geom_errorbar(aes(targets, xmin = targets, xmax = targets), color = "gray20") +
        geom_text(aes(targets, label = percent(achievement, 1)), hjust = -.4,
                  family = "Source Sans Pro", color = "gray30") +
        scale_x_continuous(label = comma) +
        labs(x = NULL, y = NULL) +
        si_style_xgrid()
      
      if(!is.null(facet_grp))
        v1 <- v1 + facet_grid(reformulate(".", facet_grp))
    
    #percent achievement plot
      v2 <- df %>% 
        ggplot(aes(achievement, fct_reorder({{grp}}, targets))) +
        geom_blank(aes(x = 1.1 * achievement)) +
        geom_vline(xintercept = 0, color = "gray30") +
        geom_segment(aes(x = 0, xend = achievement, yend = fct_reorder({{grp}}, targets)), color = "gray30") +
        geom_point(size = 4, color = "gray30") +
        geom_text(aes(achievement, label = percent(achievement, 1)), hjust = -.4,
                  family = "Source Sans Pro", color = "gray30") +
        scale_x_continuous(label = percent) +
        labs(x = NULL, y = NULL) +
        si_style_xgrid()
      
      if(!is.null(facet_grp))
        v2 <- v2 + facet_grid(reformulate(".", facet_grp))
    

    #combine plots
      v1 + v2 +
        plot_annotation(
          title = main_title,
          subtitle = sub_title,
          caption = 'DATIM Genie Site x IM [2020-06-12]'
        ) & theme(plot.caption = element_text(color = "gray30"))
  }


# TX_CURR ACHIEVEMENT -----------------------------------------------------


  #partner achievement
    df_genie %>% 
      gen_achv("TX_CURR", "Total Numerator", "partner") %>% 
      plot_achv(partner,
                sub_title = "FY20Q2 TX_CURR Achievement USAID/Malawi")
  
    ggsave("FY20Q2_MWI_TXCURR_partner.png", 
           path = "Images", height = 5.625, width = 10, dpi = 330)
    
  #district achievement
    df_genie %>% 
      gen_achv("TX_CURR", "Total Numerator", "psnu") %>% 
      plot_achv(psnu,
                sub_title = "FY20Q2 TX_CURR Achievement USAID/Malawi")
    
    ggsave("FY20Q2_MWI_TXCURR_district.png", 
           path = "Images", height = 5.625, width = 10, dpi = 330)
      
  #age/sex achievement
    df_genie %>% 
      gen_achv("TX_CURR", "Age/Sex/HIVStatus", c("ageasentered", "sex")) %>% 
      filter(ageasentered != "Unknown Age") %>% 
      plot_achv(ageasentered,
                sub_title = "FY20Q2 TX_CURR Achievement USAID/Malawi",
                facet_grp = "sex"
                )
    
    ggsave("FY20Q2_MWI_TXCURR_agesex.png", 
           path = "Images", height = 5.625, width = 10, dpi = 330)
      
  #site achievement
    tx_achv_site <- gen_achv(df_genie, "TX_CURR", "Total Numerator", 
                             c("psnu", "facility", "orgunituid", "partner"))
  
    tx_achv_site <- tx_achv_site %>% 
      arrange(desc(targets)) %>% 
      mutate(target_share = targets/sum(targets),
             target_sharecum = cumsum(target_share))
    
    grp_achv <- function(share = 1){
      tx_achv_site %>% 
        filter(target_sharecum <= share) %>% 
        summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>% 
        mutate(achievement = cumulative/targets) %>% 
        pull()
    }
    
    
    
    tx_achv_site %>% 
      ggplot(aes(targets, achievement)) +
      geom_hline(yintercept = grp_achv()) +
      geom_point() +
      scale_x_log10() +
      scale_y_continuous(label = percent) 
      
    
    tx_achv_site %>% 
      mutate(name = paste0(psnu, "/", facility, " [", partner, "]")) %>% 
      filter(target_sharecum < .25) %>% 
      ggplot(aes(achievement, fct_reorder(name, targets))) +
      geom_vline(xintercept = grp_achv(.25)) +
      geom_col(size = .2) +
      geom_vline(xintercept = 0, color = "gray30") +
      scale_x_continuous(label = percent) +
      labs(x = NULL, y = NULL) +
      si_style_xgrid()
    
    tx_achv_site %>% 
      mutate(name = paste0(psnu, "/", facility, " [", partner, "]")) %>% 
      filter(target_sharecum < .25) %>% 
      ggplot(aes(achievement, fct_reorder(name, targets))) +
      geom_vline(xintercept = grp_achv(.25)) +
      geom_col(size = .2) +
      geom_vline(xintercept = 0, color = "gray30") +
      scale_x_continuous(label = percent) +
      labs(x = NULL, y = NULL) +
      si_style_xgrid()
    
    tx_achv_site %>% 
      filter(target_sharecum < .25) %>% 
      mutate(name = paste0(psnu, "/", facility, " [", partner, "]")) %>% 
      plot_achv(name,
                sub_title = "FY20Q2 TX_CURR Achievement USAID/Malawi"
      )
    
    ggsave("FY20Q2_MWI_TXCURR_largestsites.png", 
           path = "Images", height = 5.625, width = 10, dpi = 330)
    
  #large sites with the best achievement
    tx_achv_site %>% 
      filter(target_sharecum < lrg_site) %>% 
      arrange(desc(achievement)) %>% 
      slice_head(n = 10) %>% 
      unite(facility, c("psnu", "facility"), sep = "/") %>% 
      select(facility, partner, cumulative, targets, achievement)
  
  #large sites with the worst achievement
    tx_achv_site %>% 
      filter(target_sharecum < lrg_site) %>%
      arrange(desc(achievement)) %>% 
      slice_tail(n = 10) %>% 
      unite(facility, c("psnu", "facility"), sep = "/") %>% 
      select(facility, partner, cumulative, targets, achievement)
    
    

# VIRAL LOAD --------------------------------------------------------------

  df_vl <- df_genie %>% 
      mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
      filter(indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", 
                                             "Age/Sex/Indication/HIVStatus")) %>% 
      group_by(fiscal_year, indicator, psnu, ageasentered, sex) %>% 
      summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>%
      ungroup()
    
  df_vl <- df_vl %>% 
    reshape_msd(clean = TRUE) %>% 
    spread(indicator, val) %>%
    arrange(psnu, sex, ageasentered, period) %>% 
    group_by(psnu, sex, ageasentered) %>% 
    mutate(`VL coverage` = TX_PVLS_D / lag(TX_CURR, 2, order_by = period)) %>% 
    ungroup() %>% 
    mutate(`VL suppression` = TX_PVLS / TX_PVLS_D) %>% 
    filter(period == "FY20Q2",
           ageasentered != "Unknown Age")
  
  df_vl_grid <- df_vl %>% 
    select(-starts_with("TX")) %>% 
    gather(indicator, value, `VL coverage`, `VL suppression`)
  
  snu_order <- df_genie %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == 2020) %>% 
    count(psnu, wt = cumulative) %>% 
    arrange(n) %>% 
    pull(psnu)

  df_vl_grid <- df_vl_grid %>% 
    mutate(lab = percent(value, 1),
           type = case_when(is.na(value) ~ as.character(NA),
                            value <.75 ~ "<75%",
                            value < .85 ~ "75-85%",
                            TRUE ~ ">85%"),
           type = factor(type, c("<75%", "75-85%", ">85%")),
           psnu = factor(psnu, snu_order))
  
  
  plot_vl <- function(df, ind,
                      main_title = "UPDATE TITLE BASED ON GRAPH MESSAGE", 
                      sub_title){
    df %>% 
      filter(indicator == ind) %>% 
      ggplot(aes(ageasentered, psnu, fill = type, color = type)) +
      geom_tile(color = "white", na.rm = TRUE) +
      geom_text(aes(label = percent(value, 1)), 
                family = "Source Sans Pro", size = 2.5,
                na.rm = TRUE) +
      facet_wrap(~ sex) +
      scale_x_discrete(expand = c(0.005, 0.005),  position = "top") +
      scale_fill_grey(na.value = NA, name = NULL) +
      scale_color_manual(values = c("<75%" = "white", "75-85%" = "black", ">85%" = "black"),
                         guide = NULL) +
      labs(x = NULL, y = NULL,
           title = main_title,
           subtitle = sub_title,
           caption = 'DATIM Genie Site x IM [2020-06-12]') +
      si_style_nolines() +
      theme(strip.placement = "outside",
            legend.position = "right",
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            plot.caption = element_text(color = "gray30"))
  }
 
    
  plot_vl(df_vl_grid, "VL coverage", sub_title = "FY20Q2 VL Coverage USAID/Malawi")
  
  ggsave("FY20Q2_MWI_VLC_agesex.png", 
         path = "Images", height = 5.625, width = 10, dpi = 330)
  
  plot_vl(df_vl_grid, "VL suppression", sub_title = "FY20Q2 VL Suppression USAID/Malawi")

  ggsave("FY20Q2_MWI_VLS_agesex.png", 
         path = "Images", height = 5.625, width = 10, dpi = 330)

# VL SITE REVIEW ----------------------------------------------------------

  df_vl_site <- df_genie %>% 
    mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
    filter(indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", 
                                           "Age/Sex/Indication/HIVStatus")) %>% 
    group_by(fiscal_year, indicator, psnu, facility, orgunituid, mech_code, primepartner) %>% 
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>%
    ungroup()
  
  df_vl_site <- df_vl_site %>% 
    reshape_msd(clean = TRUE) %>% 
    spread(indicator, val) %>%
    arrange(orgunituid, mech_code, period) %>% 
    group_by(orgunituid, mech_code) %>% 
    mutate(`TX_CURR FY19Q4` = lag(TX_CURR, 2, order_by = period),
           `VL coverage` = TX_PVLS_D / lag(TX_CURR, 2, order_by = period)) %>% 
    ungroup() %>% 
    mutate(`VL suppression` = TX_PVLS / TX_PVLS_D) %>% 
    filter(period == "FY20Q2") %>% 
    select(-TX_CURR)
  
  df_large_sites <- df_genie %>% 
    filter(indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == 2020) %>%
    count(orgunituid, mech_code, wt = targets, name = "targets", sort = TRUE) %>% 
    mutate(target_share = targets/sum(targets),
           target_sharecum = cumsum(target_share)) %>% 
    filter(target_sharecum <= lrg_site)
  
  
  df_vl_site_lrg <- df_vl_site %>% 
    semi_join(df_large_sites, by = c("orgunituid", "mech_code"))
  
  
  #large sites with the best VLC
  df_vl_site_lrg %>%
    arrange(desc(`VL coverage`)) %>% 
    slice_head(n = 10) %>% 
    unite(facility, c("psnu", "facility"), sep = "/") %>% 
    select(facility, primepartner, `TX_CURR FY19Q4`, TX_PVLS_D, `VL coverage`)
  
  #large sites with the worst VLC
  df_vl_site_lrg %>%
    arrange(desc(`VL coverage`)) %>% 
    filter(!is.na(`VL coverage`)) %>% 
    slice_tail(n = 10) %>% 
    unite(facility, c("psnu", "facility"), sep = "/") %>% 
    select(facility, primepartner, `TX_CURR FY19Q4`, TX_PVLS_D, `VL coverage`)
  
  #large sites with the best VLS
  df_vl_site_lrg %>%
    arrange(desc(`VL suppression`)) %>% 
    slice_head(n = 10) %>% 
    unite(facility, c("psnu", "facility"), sep = "/") %>% 
    select(facility, primepartner, TX_PVLS_D, TX_PVLS, `VL suppression`)
  
  #large sites with the worst VLS
  df_vl_site_lrg %>%
    arrange(desc(`VL suppression`)) %>% 
    filter(!is.na(`VL suppression`)) %>% 
    slice_tail(n = 10) %>% 
    unite(facility, c("psnu", "facility"), sep = "/") %>% 
    select(facility, primepartner, TX_PVLS_D, TX_PVLS, `VL suppression`)
  
  
  

# NET NEW GROWTH ----------------------------------------------------------

  df_netnew_delta <- df_genie %>% 
    filter(indicator %in% c("TX_CURR", "TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == 2020) %>%
    group_by(fiscal_year, indicator, facility, orgunituid, psnu, mech_code, primepartner, mech_name) %>% 
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    spread(indicator, val) %>% 
    arrange(orgunituid, mech_code, period) %>% 
    mutate(grp = paste(orgunituid, mech_code),
           txcurrq2 = ifelse(period == "FY20Q2", TX_CURR, 0)) %>% 
    group_by(orgunituid, mech_code) %>% 
    mutate(delta = TX_NET_NEW - lag(TX_NET_NEW)) %>% 
    ungroup()
  
  largest_growth <- df_netnew_delta %>% 
    slice_max(n = 10, order_by = delta) %>% 
    pull(grp)
  
  largest_decline <- df_netnew_delta %>% 
    slice_min(n = 10, order_by = delta) %>% 
    pull(grp)
  
  df_netnew_delta <- df_netnew_delta %>% 
    mutate(flag = case_when(period == "FY20Q2" & 
                              grp %in% c(largest_growth, largest_decline) ~ facility))
  
  
  df_netnew_delta %>% 
    ggplot(aes(period, TX_NET_NEW, group = grp)) +
    geom_path(size = .7, color = "gray50") +
    geom_path(data = filter(df_netnew_delta, grp %in% largest_growth), color = "black", size = 1) +
    geom_path(data = filter(df_netnew_delta, grp %in% largest_decline), color = "black", size = 1) +
    geom_text(aes(label = flag), size = 1.5, color = "gray30", hjust = -.1, na.rm = TRUE) +
    facet_wrap(~fct_reorder(psnu, txcurrq2, sum,.desc = TRUE), scales = "free_y") +
    scale_x_discrete(expand = c(0, 0.05)) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = NULL,
         title = "UPDATE TITLE BASED ON GRAPH MESSAGE",
         subtitle = "FY20Q2 NET NEW USAID/Malawi",
         caption = 'DATIM Genie Site x IM [2020-06-12]') +
    expand_limits(x = 3) +
    si_style_xgrid() +
    theme(plot.caption = element_text(color = "gray30"))

  ggsave("FY20Q2_MWI_NetNew_site.png", 
         path = "Images", height = 5.625, width = 10, dpi = 330)  
  