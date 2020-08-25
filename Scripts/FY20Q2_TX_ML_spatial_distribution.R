##  PROJECT: Q2 Review target analysis
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  PURPOSE: Geo-depiction of TX_ML [% of patients transferring out]
##  LICENCE: MIT
##  DATE:    2020-06-22
##  UPDATE:  2020-06-24

# Dependancies----------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)
library(here)
library(glamr)
library(glitr)
library(gisr)
library(scales)
library(RColorBrewer)
library(patchwork)

# SETUP & Make data folders are excluded from git commits

glamr::folder_setup()

# GLOBALS -------------------------------------------------------------

  ## Data & Output folders
  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"
  dir_geodata <- "../../GEODATA/PEPFAR"
  dir_merdata <- "../../MERDATA"
  
  ## PSNUxIM Dataset
  file_psnu_im <- here(dir_merdata, "MER_Structured_Datasets_PSNU_IM_FY18-20_20200605_v1_1.rds")
  
  ## Geodata
  gis_5_sfc <- list.files(dir_geodata, pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
    set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
    map(read_sf)
  
  gis_4_sfc <- list.files(dir_geodata, pattern = ".*_4_.*.shp$", recursive = T, full.names = T) %>%
    set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
    map(read_sf)
  
# FUNCTIONS -------------------------------------------------------------

  #' Extract TX_ML from MER PSNU Dataset
  #' 
  #' @param fy fiscal year
  #' @param snu_prio snuprioritization
  #' 
  extract_tx_ml <- function(.data, fy = "2020", snu_prio = NULL, mechs = NULL) {
    
    ## For ZAF Only
    if (!is.null(snu_prio)) {
      .data %>%
        filter(snuprioritization %in% snu_prio)
    }
    
    if (!is.null(mechs)) {
      .data %>% 
        filter(mech_code %in% mechs)
    }
    
    ## Common Munging
    .data %>%
      filter(
        fiscal_year == {{fy}},
        indicator == "TX_ML",
        standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus",
        typemilitary == 'N',
        fundingagency == "USAID"
      ) %>%
      mutate(
        otherdisaggregate = str_remove(otherdisaggregate, "No Contact Outcome - "),
        otherdisagg = ifelse(str_detect(otherdisaggregate, "Lost to Follow-Up"), "Lost to Follow-Up", otherdisaggregate),
        otherdisagg = ifelse(str_detect(otherdisagg, "Refused"), "Refused or Stopped", otherdisagg),
        otherdisagg = factor(otherdisagg,
                             levels = c("Transferred Out", "Lost to Follow-Up", "Refused or Stopped", "Died"),
                             labels = c("TO", "LTFU", "Refused or Stopped", "Died"))
      ) %>%
      group_by(operatingunit, snu1, snu1uid, psnu, psnuuid, indicator, otherdisagg) %>%
      summarize_at(vars(targets:cumulative), sum, na.rm=TRUE) %>%
      ungroup() %>%
      mutate(
        prct_ch = round(((qtr2 - qtr1) - qtr1) / qtr1 * 100, 2)
      ) %>%
      dplyr::select(operatingunit, snu1, snu1uid, psnuuid, psnu, otherdisagg, qtr1, qtr2, prct_ch, cumulative) %>%
      group_by(operatingunit, snu1uid, snu1, psnuuid, psnu) %>%
      dplyr::mutate(
        ml_ttl = sum(cumulative, na.rm = T),
        to_ttl = first(cumulative),
        to_cum = round(first(cumulative) / sum(cumulative, na.rm = T) * 100, 2),
        prct = round( cumulative / sum(cumulative, na.rm = T) * 100, 2)
      ) %>%
      ungroup()
  }
  
  #' Create a bar graph of % TO
  #' 
  #' @param df Summarized country level TX_ML Data
  #' @param org_level snu1 or psnu
  #' 
  plot_tx_ml <- function(df, org_level="psnu", fcolor = NULL) {
    
    viz <- df %>%
      mutate(label = paste0(!!sym(org_level), " (", to_ttl, "/", ml_ttl, ")")) %>% 
      ggplot(aes(reorder(label, to_cum), prct, fill = otherdisagg)) +
      geom_col(position = position_fill(reverse = TRUE)) +
      geom_hline(yintercept = .25, color = grey10k, lwd = .3) +
      geom_hline(yintercept = .50, color = grey10k, lwd = .3) +
      geom_hline(yintercept = .75, color = grey10k, lwd = .3)
    
    if (is.null(fcolor)) {
      viz <- viz +
        scale_fill_brewer(palette = "Set3", direction = -1)
    } else {
      viz <- viz +
        scale_fill_manual(values = fcolor)
    }
    
    viz <- viz  +
      scale_y_continuous(position = "right", labels = percent) +
      coord_flip() +
      labs(x="", y="", subtitle = paste0(toupper(org_level), " (TO / ML)")) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    print(viz)
    
    return(viz)
  }
  
  
  #' Map % TO
  #' 
  #' @param df country dataset
  #' @param df_shp country geodata
  #' @param label_name colname to be used for labels
  #' @param uid_name colname foreign key from df
  #' 
  map_tx_ml <- function(df, sf_shp, label_name="level5name", uid_name="psnuuid") {
    
    gviz <- sf_shp %>%
      left_join(df, by=c("uid" = {{uid_name}})) %>%
      dplyr::filter(!is.na(otherdisagg)) %>%
      ggplot(data=., aes(fill = prct, label = {{label_name}})) +
      geom_sf(color = grey40k) +
      geom_sf(data = sf_shp, fill = NA, color = grey40k) +
      geom_sf_text(size = 1.5, color = grey60k) +
      scale_fill_viridis_c(direction = -1, na.value = NA) +
      facet_wrap(~otherdisagg, ncol = 2) +
      theme_void() +
      theme(
        legend.position = 'bottom',
        legend.box.just = "right",
        legend.title = element_blank(),
        legend.key.width = unit(2, "cm"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", color = grey80k),
        plot.subtitle = element_text(face = "italic", margin = unit(c(1,1,10,1), 'pt')),
        plot.caption = element_text(face = "italic")
      )
    
    print(gviz)
    
    return(gviz)
  }
  
  
  #' Combine Map + Graph
  #' 
  #' @param cntry_plot bar chart
  #' @param cntry_map map
  #' @param title graphic title
  #' @param caption graphic footnote
  #' 
  viz_tx_ml <- function(cntry_plot, cntry_map,
                        title = "<COUNTRY XYZ - Descriptive Title>",
                        #subtitle = "<Key takeway for audience>",
                        caption = "QAC Product") {
    
    viz_output <- cntry_map + cntry_plot +
      plot_layout(ncol = 2, widths = c(2, 1)) +
      plot_annotation(
        title = title,
        #subtitle = subtitle,
        caption = paste0("OHA/SIEI - ", caption, ", ", Sys.Date())
      ) +
      theme(title = element_text(face = "bold"))
    
    print(viz_output)
    
    return(viz_output)
  }
  
  
# DATA ---------------------------------------------------------------
  
  ## PSNUxIMs
  df_psnu <- read_rds(file_psnu_im)
  
  ## Munge TX_ML
  df_tx_ml <- df_psnu %>%
    extract_tx_ml()

# EXTRACT & VIZ

  ## Zimbabwe
  df_zim <- df_tx_ml %>%
    filter(operatingunit == "Zimbabwe")
  
  plot_tx_ml(df_zim)
  map_tx_ml(df_zim, gis_5_sfc$Zimbabwe)
  
  viz_zim <- viz_tx_ml(
    cntry_plot = plot_tx_ml(df_zim),
    cntry_map = map_tx_ml(df_zim, gis_5_sfc$Zimbabwe),
    title = "ZIMBABWE - Distribution of TX_ML by PSNU",
    caption = "HQ Q2 Review"
  )
  
  ggsave(here(dir_graphics, "ZIMBABWE_TX_ML_x_PSNU.png"),
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
  
  ## Nigeria
  df_nga <- df_tx_ml %>%
    filter(operatingunit == "Nigeria")
  
  plot_tx_ml(df_nga)
  map_tx_ml(df_nga, gis_4_sfc$Nigeria %>% mutate(uid = orgunit_in))
  
  viz_zim <- viz_tx_ml(
    cntry_plot = plot_tx_ml(df_nga),
    cntry_map = map_tx_ml(df_nga, gis_4_sfc$Nigeria %>% mutate(uid = orgunit_in)),
    title = "NIGERIA - Distribution of TX_ML by PSNU",
    caption = "HQ Q2 Review"
  )
  
  ggsave(here(dir_graphics, "NIGERIA_TX_ML_x_PSNU.png"),
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
  
  ## Tanzania + additional filter
  
  df_tza <- df_tx_ml %>%
    filter(operatingunit == "Tanzania") %>%
    group_by(snu1, snu1uid, otherdisagg) %>%
    summarise_at(vars(qtr1:cumulative), sum, na.rm = TRUE) %>%
    ungroup() %>%
    dplyr::select(-prct_ch) %>%
    group_by(snu1, snu1uid) %>%
    dplyr::mutate(
      ml_ttl = sum(cumulative, na.rm = T),
      to_ttl = first(cumulative),
      to_cum = round(first(cumulative) / sum(cumulative, na.rm = T) * 100, 2),
      prct = round( cumulative / sum(cumulative, na.rm = T) * 100, 2)
    ) %>%
    ungroup()
  
  plot_tx_ml(df_tza, org_level = "snu1")
  map_tx_ml(df_tza, gis_4_sfc$Tanzania, label_name = "orgunit_in", uid_name = "snu1uid")
  
  viz_tza <- viz_tx_ml(
    cntry_plot = plot_tx_ml(df_tza, org_level = "snu1"),
    cntry_map = map_tx_ml(df_tza, gis_4_sfc$Tanzania, label_name = "orgunit_in", uid_name = "snu1uid"),
    title = "TANZANIA - Distribution of TX_ML by SNU1",
    caption = "HQ Q2 Review"
  )
  
  ggsave(here(dir_graphics, "TANZANIA_TX_ML_x_PSNU.png"),
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
  
  ## South Africa
  
  df_zaf <- df_psnu %>%
    extract_tx_ml(
      snu_prio = c("2 - Scale-Up: Aggressive", "1 - Scale-Up: Saturation"),
      mechs = c("70310", "70287","70289","70290","70301")
    ) %>%
    filter(operatingunit == "South Africa")
  
  df_zaf <- df_psnu %>% 
    filter(
      fiscal_year == "2020",
      indicator == "TX_ML",
      standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus",
      typemilitary == 'N',
      fundingagency == "USAID",
      snuprioritization %in% c("2 - Scale-Up: Aggressive", "1 - Scale-Up: Saturation"),
      mech_code %in% c("70310", "70287","70289","70290","70301"),
      operatingunit == 'South Africa'
    ) %>%
    group_by(psnu, psnuuid, indicator, otherdisaggregate) %>%
    summarize_at(vars(targets:cumulative), sum, na.rm=TRUE) %>%
    ungroup() %>%
    mutate(
      otherdisaggregate = str_remove(otherdisaggregate, "No Contact Outcome - "),
      otherdisagg = ifelse(str_detect(otherdisaggregate, "Lost to Follow-Up"), "Lost to Follow-Up", otherdisaggregate),
      otherdisagg = ifelse(str_detect(otherdisagg, "Refused"), "Refused or Stopped", otherdisagg),
      otherdisagg = ordered(otherdisagg,
                           levels = c("Transferred Out", "Lost to Follow-Up", "Died"),
                           labels = c("TO", "LTFU", "Died")),
      prct_ch = round(((qtr2 - qtr1) - qtr1) / qtr1 * 100, 2)
    ) %>% 
    dplyr::select(operatingunit, snu1, snu1uid, psnuuid, psnu, otherdisagg, qtr1, qtr2, prct_ch, cumulative) %>%
    group_by(psnuuid, psnu) %>%
    dplyr::mutate(
      ml_ttl = sum(cumulative, na.rm = T),
      to_ttl = first(cumulative),
      to_cum = round(first(cumulative) / sum(cumulative, na.rm = T) * 100, 2),
      prct = round( cumulative / sum(cumulative, na.rm = T) * 100, 2)
    ) %>%
    ungroup()
  
  df_zaf %>% 
    distinct(otherdisagg) %>% 
    pull()
  
  df_zaf <- df_zaf %>%
    mutate(
      psnu = str_replace(psnu, " Metropolitan Municipality| District Municipality| Bay Municipality", ""),
      psnu = str_replace(psnu, "^.{3}", "")
    )
  
  
  plot_tx_ml(df_zaf, fcolor = c("#fb8072", "#bebada", "#8dd3c7"))
  
  df_zaf %>% 
    mutate(label = paste0(psnu, " (", to_ttl, "/", ml_ttl, ")")) %>% 
    ggplot(aes(reorder(label, to_cum), prct, fill = otherdisagg)) +
    geom_col(position = position_fill(reverse = T)) +
    geom_hline(yintercept = .25, color = grey10k, lwd = .3) +
    geom_hline(yintercept = .50, color = grey10k, lwd = .3) +
    geom_hline(yintercept = .75, color = grey10k, lwd = .3) +
    scale_fill_manual(values = c("#fb8072", "#bebada", "#8dd3c7")) +
    scale_y_continuous(position = "right", labels = percent) +
    coord_flip() +
    labs(x="", y="", subtitle = paste0(toupper("psnu"), " (TO / ML)")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.major.y = element_blank()
    )
  
  map_tx_ml(df_zaf, gis_5_sfc$SouthAfrica)
  
  viz_zaf <- viz_tx_ml(
    cntry_plot = plot_tx_ml(df_zaf),
    cntry_map = map_tx_ml(df_zaf, gis_5_sfc$SouthAfrica),
    title = "SOUTH AFRICA - Distribution of TX_ML by PSNU",caption = "HQ Q2 Review"
  )
  
  ggsave(here(dir_graphics, "SOUTH_AFRICA_TX_ML_x_PSNU.png"),
         scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
  