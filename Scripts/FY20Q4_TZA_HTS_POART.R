## PROJECT:  TZA Q4 Review
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  testing
## DATE:     2020-11-16
## UPDATED:  


# DATA SOURCE -------------------------------------------------------------

# PSNU By IM
# DATIM data as of: 11/13/2020 22:08:05 UTC
# Genie report updated: 11/15/2020 06:03:36 UTC
# Current period(s): 2019 Target,  2019 Q1,  2019 Q2,  2019 Q3,  2019 Q4,  2020 Target,  2020 Q1,  2020 Q2,  2020 Q3,  2020 Q4
# Operating Unit: Tanzania
# Indicator: HTS_SELF, HTS_TST, HTS_TST_POS


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(glitr)
library(scales)
library(extrafont)
library(glue)
library(RColorBrewer)
library(sf)
library(patchwork)


# GLOBAL VARIABLES --------------------------------------------------------

  #data paths
    path <- "data/Genie-PSNUByIMs-Tanzania-Daily-2020-11-16.zip"
    path_msd <- list.files("~/Data", "PSNU_IM", full.names = TRUE)
    path_shp <- "GIS/Tanzania_PROD_4_Region_RegionLsib_2019_May.shp"
  
  #achievement colors
    pal_achv <- brewer.pal(5, "Spectral")[2:5]


# IMPORT DATA -------------------------------------------------------------

  #import geni
    df_hts <- read_msd(path)
  
  #import msd
    df_msd <- read_rds(path_msd)

  #import shapefile
    shp_tza <- st_read(path_shp) %>% 
      mutate(snu1 = as.character(orgunit_na)) %>% 
      select(snu1, geometry)
    
# MUNGE -------------------------------------------------------------------
  
  #filter msd for just TZA and hts
    df_msd_tza <- df_msd %>% 
      filter(operatingunit == "Tanzania",
             indicator %in% c("HTS_TST", "HTS_TST_POS", "HTS_SELF"),
             fiscal_year == "2018")
    
  #bind
    df_hts <- bind_rows(df_msd_tza, df_hts)
    
  #clean partner names
    df_hts <- rename_official(df_hts)
    
  #clean agency
    df_hts <- df_hts %>% 
      mutate(fundingagency = str_remove(fundingagency, "HHS/"))
    
  #mech map
    df_mech_map <- tibble::tribble(
      ~mech_code,            ~partner,
      "18488",           "AMREF",
      "17986",          "AGPAHI",
      "18131",     "Intrahealth",
      "80095",             "MDH",
      "17991",            "ICAP",
      "18627", "HJFMRI-Southern",
      "18628", "HJFMRI-Military",
      "70356",     "Mtoto Bomba",
      "18237",        "Deloitte",
      "18060",           "EGPAF",
      "81965",            "EpiC",
      "81962",   "Tohara Salama"
    )
    
    
    df_hts <- left_join(df_hts, df_mech_map)

# OVERALL PARTNER QUARTERLY PERFORMANCE -----------------------------------

  #HTS/HTS_POS partner aggregates
    df_perf <- df_hts %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Total Numerator",
             fiscal_year < 2021,
             fundingagency != "Dedup") %>% 
      group_by(fundingagency, partner, indicator, fiscal_year) %>% 
      summarise(across(where(is.double), sum,na.rm = TRUE)) %>% 
      ungroup()
    
  #reshape quarters long
    df_perf <- df_perf %>% 
      pivot_longer(starts_with("qtr"), names_to = "quarter", names_prefix = "qtr") %>% 
      filter(value > 0) %>% 
      mutate(period = glue("FY{str_sub(fiscal_year, -2)}Q{quarter}"),
             quarter = as.integer(quarter))

  #id which partners still exist in Q4 (subset for plot)
    df_perf <- df_perf %>% 
      mutate(exists_latest = case_when(period == max(period) ~ TRUE)) %>% 
      group_by(partner) %>% 
      fill(exists_latest, .direction = "up") %>% 
      ungroup()
  
  #order partners by HTS_TST targets
    df_perf <- df_perf %>% 
      mutate(vizorder = case_when(period == max(period) & indicator == "HTS_TST" ~ targets,
                               is.na(targets) ~ 0,
                               TRUE ~ 0),
             partner = glue("{fundingagency}/{partner}"),
             partner = fct_reorder(partner, vizorder, max, .desc = TRUE))

  #achievement levels
    df_perf <- df_perf %>% 
      mutate(goal = .25 * quarter) %>% 
      group_by(partner, indicator, fiscal_year) %>% 
      mutate(cumvalue = cumsum(value)) %>% 
      ungroup() %>% 
      mutate(achv = cumvalue/targets,
             achv_grp = case_when(achv < goal - .25 ~ pal_achv[1],
                                  achv < goal - .1 ~ pal_achv[2],
                                  achv < goal + 1.1 ~ pal_achv[3],
                                  achv >= goal + 1.1 ~ pal_achv[4]))
  

  #plot HTS_TST partner trends 
    df_perf %>% 
      filter(indicator == "HTS_TST",
             exists_latest == TRUE) %>% 
      ggplot(aes(period, value)) +
      annotate("rect", xmin = 4.5, xmax = 8.5, ymin = -150000, ymax = 1600000, 
               fill = "gray60", alpha = .2) +
      geom_col(aes(y = -150000, fill = achv_grp), alpha = .8) +
      geom_col(aes(y = -50000), fill = "white") +
      geom_col(fill = "gray60") +
      geom_hline(yintercept = 0) +
      facet_wrap(~ partner) +
      scale_y_continuous(label = comma) +
      scale_x_discrete(labels = c("FY18Q1", "", "18Q3", "",
                                  "FY19Q1", "", "19Q3", "",
                                  "FY20Q1", "", "20Q3", "")) +
      scale_fill_identity()+
      labs(x = NULL, y = NULL, 
           title = "TANZANIA PARTNER ACHIEVEMENT | HTS_TST",
           subtitle = "Quarterly trends in testing & cumulative target achievement",
           caption = "FY20Q3c MSD + Genie [pulled 2020-11-16]") +
      si_style_ygrid() +
      theme(panel.spacing = unit(.5, "lines"))
    
    si_save("Images/FY20Q4_TZA_HTS_partner_trend.png")
  
  #plot HTS_TST_POS partner trends 
    df_perf %>% 
      filter(indicator == "HTS_TST_POS",
             exists_latest == TRUE) %>% 
      ggplot(aes(period, value)) +
      annotate("rect", xmin = 4.5, xmax = 8.5, ymin = -5000, ymax = 45000, 
               fill = "gray60", alpha = .2) +
      geom_col(aes(y = -5000, fill = achv_grp), alpha = .8) +
      geom_col(aes(y = -1000), fill = "white") +
      geom_col(fill = "gray60") +
      geom_hline(yintercept = 0) +
      facet_wrap(~ partner) +
      scale_y_continuous(label = comma) +
      scale_x_discrete(labels = c("FY18Q1", "", "18Q3", "",
                                  "FY19Q1", "", "19Q3", "",
                                  "FY20Q1", "", "20Q3", "")) +
      scale_fill_identity()+
      labs(x = NULL, y = NULL, 
           title = "TANZANIA PARTNER ACHIEVEMENT | HTS_TST_POS",
           subtitle = "Quarterly trends in testing & cumulative target achievement",
           caption = "FY20Q3c MSD + Genie [pulled 2020-11-16]") +
      si_style_ygrid() +
      theme(panel.spacing = unit(.5, "lines"))
    
    si_save("Images/FY20Q4_TZA_HTS_POS_partner_trend.png")

# OVERALL REGIONAL PERFORMANCE --------------------------------------------


  #aggregate to snu1
    df_snu <- df_hts %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Total Numerator",
             snu1 != "_Military Tanzania",
             fiscal_year < 2021) %>%
      group_by(snu1, snu1uid, indicator, fiscal_year) %>% 
      summarise(across(c(cumulative, targets), sum,na.rm = TRUE)) %>% 
      ungroup()
  
  #achievement
    goal <- 1
    # df_snu  <- df_snu %>% 
    #   mutate(achv = cumulative/targets,
    #          achv_grp = case_when(achv < goal - .25 ~ pal_achv[1],
    #                               achv < goal - .1 ~ pal_achv[2],
    #                               achv < goal + 1.1 ~ pal_achv[3],
    #                               achv >= goal + 1.1 ~ pal_achv[4]))
    
    df_snu  <- df_snu %>% 
      mutate(achv = cumulative/targets,
             achv_grp = case_when(achv < goal - .25 ~ "<75%",
                                  achv < goal - .1 ~ "75-90%",
                                  achv < goal + 1.1 ~ "90-110%",
                                  achv >= goal + 1.1 ~ ">110%"),
             achv_grp = factor(achv_grp, c("<75%", "75-90%", "90-110%", ">110%")))

          
  #join to shape file
    df_snu <- full_join(df_snu, shp_tza)
    
  #plot
    maps <- df_snu %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = achv_grp), 
              color = "gray60", alpha = .8,  na.rm = TRUE) +
      # scale_fill_identity() +
      scale_fill_manual(values = pal_achv) +
      facet_grid(indicator~fiscal_year, switch = "y") +
      labs(title = "TRENDS IN TESTING TARGET ACHIEVEMENT",
           caption = "FY20Q3c MSD + Genie [pulled 2020-11-16]",
           subtitle = "Target Achievement",
           fill = NULL) +
      si_style_void() +
      theme(panel.grid = element_blank(),
            strip.text.x = element_text(hjust = .5),
            strip.text.y = element_text(hjust = .5),
            legend.title = element_text(family = "Source Sans Pro", size = 11, color = "#505050")
            # legend.position = "none"
            )
    
    
    bars <- df_snu %>% 
      filter(fiscal_year == 2020) %>% 
      mutate(across(c(cumulative, targets), ~./1000)) %>% 
      ggplot(aes(cumulative, fct_reorder(snu1, targets))) +
      geom_blank(aes(x = 1.1 * targets)) +
      geom_col(aes(targets, fct_reorder(snu1, targets)), width = .8, fill = "white", color = "gray60") +
      geom_col(width = .8, fill = "gray60", color = "gray60") +
      facet_wrap(~ indicator, scales = "free_x") +
      scale_x_continuous(labels = comma, expand = c(.005, .005)) +
      labs(x = NULL, y = NULL, subtitle = "FY20 Results against Targets") +
      si_style_xgrid()
    
    maps + bars
    
    si_save("Images/FY20Q4_TZA_HTS_snu_trend.png")

# MODALITIES --------------------------------------------------------------

    
  #aggregate modalities by quarter
    df_mod <- df_hts %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             fiscal_year %in% c(2019, 2020),
             standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
      group_by(fiscal_year, indicator, modality) %>%
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      arrange(indicator, modality, period) %>% 
      mutate(order = ifelse(period == "FY20", val, 0),
             modality = recode(modality, "OtherPITC" = "Other PITC",
                               "MobileMod" = "Mobile (Comm)",
                               "IndexMod" = "Index (Comm)"),
             mod_lump = fct_lump(modality, n = 5, w = order, other_level = "All Other")) %>% 
      group_by(period, indicator, mod_lump, period_type) %>% 
      summarise(across(c(val, order), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(mod_lump = factor(mod_lump, c("Other PITC", "PMTCT ANC", "Mobile (Comm)", 
                                           "Index", "Index (Comm)", "All Other")))

    
    df_mod_yield <- df_mod %>% 
      filter(period_type == "results") %>% 
      select(-period_type, -order) %>% 
      spread(indicator, val) %>% 
      mutate(val = HTS_TST_POS/HTS_TST,
             indicator = "Positivity",
             start = case_when(period == min(period) ~ val),
             end = case_when(period == max(period) ~ val)
      ) 
    
    v_mod_hts <- df_mod %>% 
      filter(indicator == "HTS_TST",
             period_type == "results") %>% 
      ggplot(aes(period, val)) +
      geom_col(fill = "gray60") +
      annotate("rect", xmin = 0, xmax = 4.5, ymin = 0, ymax = 1600000,
               fill = "gray60", alpha = .2) +
      facet_grid(indicator ~ mod_lump, switch = "y") +
      scale_y_continuous(label = comma) +
      scale_x_discrete(labels = c("FY19Q1", "", "", "",
                                  "FY20Q1", "", "", "")) +
      labs(x = NULL, y = NULL) +
      si_style_ygrid() +
      theme(strip.text.y = element_text(hjust = .5),
            axis.text.x = element_blank(),
            strip.placement = "outside")
    
    v_mod_pos <- df_mod %>% 
      filter(indicator == "HTS_TST_POS",
             period_type == "results") %>% 
      ggplot(aes(period, val)) +
      geom_col(fill = "gray60") +
      annotate("rect", xmin = 0, xmax = 4.5, ymin = 0, ymax = 35000,
               fill = "gray60", alpha = .2) +
      facet_grid(indicator ~ mod_lump, switch = "y") +
      scale_y_continuous(label = comma) +
      scale_x_discrete(labels = c("FY19Q1", "", "", "",
                                  "FY20Q1", "", "", "")) +
      labs(x = NULL, y = NULL) +
      si_style_ygrid() +
      theme(strip.text.y = element_text(hjust = .5),
            axis.text.x = element_blank(),
            strip.text.x = element_blank(),
            strip.placement = "outside")

   
    v_mod_yield <- df_mod_yield %>%
      ggplot(aes(period, val, group = mod_lump)) +
      geom_line(color = "gray60", size = 1.1) +
      geom_point(aes(y = start), size = 3, color = "gray60", na.rm = TRUE) +
      geom_point(aes(y = end), shape = 21, fill = "white", stroke = 1.5,
                 size = 3, color = "gray60", na.rm = TRUE) +
      annotate("rect", xmin = 0, xmax = 4.5, ymin = 0, ymax = .28,
               fill = "gray60", alpha = .2) +
      facet_grid(indicator ~ mod_lump, switch = "y") +
      scale_y_continuous(label = percent) +
      scale_x_discrete(labels = c("FY19Q1", "", "", "",
                                  "FY20Q1", "", "", "")) +
      labs(x = NULL, y = NULL) +
      si_style_ygrid() +
      theme(strip.text.y = element_text(hjust = .5),
            strip.text.x = element_blank(),
            strip.placement = "outside")
    
    v_mod_hts/v_mod_pos/v_mod_yield +
      plot_annotation(title = "TANZANIA TESTING AND POSITIVITY TRENDS",
                      caption = "FY20Q3c MSD + Genie [pulled 2020-11-16]") &
      si_style_ygrid() &
      theme(strip.text.y = element_text(hjust = .5),
            # strip.text.x = element_blank(),
            strip.placement = "outside")
    
    si_save("Images/FY20Q4_TZA_HTS_mods.png")

# PEDS IDENTIFICATION -----------------------------------------------------

    #aggregate modalities by quarter
    df_peds <- df_hts %>% 
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Modality/Age/Sex/Result",
             trendscoarse == "<15",
             fiscal_year < 2021
            ) %>% 
      group_by(fiscal_year, indicator) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      select(-period_type) %>% 
      spread(indicator, val) %>% 
      mutate(positivity = HTS_TST_POS/HTS_TST,
             start = case_when(period == min(period) ~ positivity),
             end = case_when(period == max(period) ~ positivity))

    
    v_peds_hts <- df_peds %>% 
      ggplot(aes(period, HTS_TST_POS)) +
      geom_col() +
      annotate("rect", xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 4700,
               fill = "gray60", alpha = .2) +
      labs(x = NULL, y = NULL, subtitle = "HTS_TST_POS") +
      scale_y_continuous(label = comma) +
      scale_x_discrete(labels = c("FY18Q1", "", "", "",
                                  "FY19Q1", "", "", "",
                                  "FY20Q1", "", "", "")) +
      si_style_ygrid()
    
    v_ped_yield <- df_peds %>% 
      ggplot(aes(period, positivity, group = "na")) +
      geom_line(color = "gray60", size = 1.1) +
      geom_point(aes(y = start), size = 3, color = "gray60", na.rm = TRUE) +
      geom_point(aes(y = end), shape = 21, fill = "white", stroke = 1.5,
                 size = 3, color = "gray60", na.rm = TRUE) +
      annotate("rect", xmin = 4.5, xmax = 8.5, ymin = 0, ymax = .05,
               fill = "gray60", alpha = .2) +
      labs(x = NULL, y = NULL, subtitle = "Positivity") +
      expand_limits(y = 0) +
      scale_y_continuous(label = percent_format(1)) +
      scale_x_discrete(labels = c("FY18Q1", "", "", "",
                                  "FY19Q1", "", "", "",
                                  "FY20Q1", "", "", "")) +
      si_style_ygrid()
    
    
    
    
    df_peds_ind <- df_hts %>% 
      filter(indicator == "HTS_TST_POS",
             standardizeddisaggregate == "Modality/Age/Sex/Result",
             trendscoarse == "<15",
             fiscal_year < 2021
      ) %>% 
      mutate(mod_type = ifelse(str_detect(modality, "Index"), "Index", "Other")) %>% 
      group_by(fiscal_year, mod_type) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      select(-period_type) %>% 
      group_by(period) %>% 
      mutate(contribution = val/sum(val)) %>% 
      ungroup() %>% 
      mutate(start = case_when(period == min(period) ~ contribution),
             end = case_when(period == max(period) ~ contribution))

    v_peds_index <- df_peds_ind %>% 
      ggplot(aes(period, val)) +
      geom_col(alpha = .2) +
      geom_col(data = df_peds_ind %>% filter(mod_type == "Index")) +
      annotate("rect", xmin = 4.5, xmax = 8.5, ymin = 0, ymax = 4700,
               fill = "gray60", alpha = .2) +
      labs(x = NULL, y = NULL, subtitle = "HTS_TST_POS from Index Testing") +
      scale_y_continuous(label = comma) +
      scale_x_discrete(labels = c("FY18Q1", "", "", "",
                                  "FY19Q1", "", "", "",
                                  "FY20Q1", "", "", "")) +
      si_style_ygrid()
    
    
    v_peds_contr <- df_peds_ind %>% 
      filter(mod_type == "Index") %>% 
      ggplot(aes(period, contribution , group = "na")) +
      geom_line(color = "gray60", size = 1.1) +
      geom_point(aes(y = start), size = 3, color = "gray60", na.rm = TRUE) +
      geom_point(aes(y = end), shape = 21, fill = "white", stroke = 1.5,
                 size = 3, color = "gray60", na.rm = TRUE) +
      annotate("rect", xmin = 4.5, xmax = 8.5, ymin = 0, ymax = .7,
               fill = "gray60", alpha = .2) +
      labs(x = NULL, y = NULL, subtitle = "Pos. contribution from Index Testing") +
      expand_limits(y = 0) +
      scale_y_continuous(label = percent_format(1)) +
      scale_x_discrete(labels = c("FY18Q1", "", "", "",
                                  "FY19Q1", "", "", "",
                                  "FY20Q1", "", "", "")) +
      si_style_ygrid()

    
    (v_peds_hts + v_peds_index) / (v_ped_yield + v_peds_contr)  +
      plot_annotation(title = "TANZANIA PEDS (<15) TESTING & INDEX CONTRIBUTION TRENDS",
                      caption = "FY20Q3c MSD + Genie [pulled 2020-11-16]") &
      si_style_ygrid() 

    si_save("Images/FY20Q4_TZA_HTS_peds_trend.png")
    

# HTS_SELF ----------------------------------------------------------------

    # df_hts %>% 
    #   filter(indicator == "HTS_SELF",
    #          fiscal_year < 2021) %>% 
    #   count(fiscal_year, standardizeddisaggregate, otherdisaggregate) %>% 
    #   spread(fiscal_year, n)
    
  #aggregate HTS_SELF
    df_self <- df_hts %>% 
      filter(indicator == "HTS_SELF",
             fiscal_year < 2021,
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(fiscal_year, indicator) %>% 
      summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      select(-period_type)

    
    df_self %>% 
      ggplot(aes(period, val)) +
      geom_col() +
      scale_y_continuous(label = comma) +
      # scale_x_discrete(labels = c("FY18Q3", "",
      #                             "FY19Q1", "", "", "",
      #                             "FY20Q1", "", "", "")) +
      labs(x = NULL, y = NULL,
           title = "TANZANIA SELF TESTING TRENDS",
           caption = "FY20Q3c MSD + Genie [pulled 2020-11-16]") +
      si_style_ygrid()
    
    si_save("Images/FY20Q4_TZA_HTS-SELF_fy_trends.png")
    
    df_self_snu <- df_hts %>% 
      filter(indicator == "HTS_SELF",
             fiscal_year < 2021,
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(fiscal_year, snu1, indicator) %>% 
      summarise(across(cumulative, sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      select(-period_type)
    
    df_snu_compl <- df_hts %>% 
      filter(snu1 != "_Military Tanzania",
             fiscal_year <2021) %>% 
      distinct(snu1, fiscal_year) %>% 
      mutate(period = glue("FY{str_sub(fiscal_year, -2)}")) %>% 
      select(-fiscal_year)
    
    df_self_snu <- df_self_snu %>% 
      full_join(df_snu_compl) %>% 
      full_join(shp_tza)

        
    
    v_self <- df_self_snu %>% 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = val/1000), 
              color = "gray60", 
              alpha = .8,  
              na.rm = TRUE) +
      # scale_fill_viridis_c(option = "D", direction = -1) +
      scale_fill_fermenter(direction = 1, palette = "YlGnBu", na.value = "gray80") +
      facet_grid(~period) +
      labs(title = "DISTRIBUTION OF SELF TEST KITS",
           caption = "FY20Q3c MSD + Genie [pulled 2020-11-16]",
           fill = "Trends in Kits (thousands)"
           # fill = NULL
           ) +
      si_style_void() +
      theme(panel.grid = element_blank(),
            strip.text.x = element_text(hjust = .5),
            strip.text.y = element_text(hjust = .5),
            legend.title = element_text(family = "Source Sans Pro", size = 11, color = "#505050")
            # legend.position = "bottom"
      )

    
    v_self_bar <- df_self_snu %>% 
      filter(period == "FY20",
             !is.na(val)) %>% 
      ggplot(aes(val, fct_reorder(snu1, val))) +
      geom_col(fill = "gray60") +
      scale_x_continuous(label = comma, expand = c(.005, .005)) +
      labs(x = NULL, y = NULL,
           subtitle = "FY20 Self Test Kits") +
      si_style_xgrid() 

    v_self + v_self_bar +plot_layout(widths = c(2, 1)) 

    
    si_save("Images/FY20Q4_TZA_HTS-SELF_regions.png")
    
    df_hts %>% 
      filter(indicator == "HTS_SELF",
             standardizeddisaggregate == "KeyPop/HIVSelfTest",
             fiscal_year < 2021) %>% 
      count(fiscal_year, otherdisaggregate_sub, wt = cumulative, name = "cumulative") %>% 
      mutate(period = glue("FY{str_sub(fiscal_year, -2)}"),
             start = case_when(period == min(period) ~ cumulative),
             end = case_when(period == max(period) ~ cumulative)) %>% 
      ggplot(aes(period, cumulative, group = otherdisaggregate_sub)) +
      geom_line(color = "gray60", size = 1.1) +
      geom_point(aes(y = start), size = 3, color = "gray60", na.rm = TRUE) +
      geom_point(aes(y = end), shape = 21, fill = "white", stroke = 1.5,
                 size = 3, color = "gray60", na.rm = TRUE) +
      facet_wrap(~otherdisaggregate_sub) +
      labs(title = "DISTRIBUTION OF SELF TEST KITS BY KP GROUP",
           caption = "FY20Q3c MSD + Genie [pulled 2020-11-16]") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete(expand = c(.04, .04)) +
      si_style()
    
    si_save("Images/FY20Q4_TZA_HTS-SELF_subpop.png")
    