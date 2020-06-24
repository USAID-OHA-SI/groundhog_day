##  PROJECT: Q2 Review target analysis
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  PURPOSE: Geo-depiction of TX_ML [% of patients transferring out]
##  LICENCE: MIT
##  DATE:    2020-06-23
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
    
# FUNCTIONS -------------------------------------------------------------

  ## Stitch the plots together
  generate_plot <- function(df, shp, layout = "long", title = "") {
    
    bar_chart <- df %>% 
      ggplot() +
      geom_col(aes(reorder(psnu, vlc), full), fill = grey10k) +
      geom_col(data = df, aes(reorder(psnu, vlc), vlc, fill = vlc), show.legend = F) +
      geom_hline(yintercept = .25, color = "white", lwd = .3) +
      geom_hline(yintercept = .50, color = "white", lwd = .3) +
      geom_hline(yintercept = .75, color = "white", lwd = .3) +
      scale_fill_viridis_c(direction = -1) +
      scale_y_continuous(position = "right", labels = percent) +
      coord_flip() +
      labs(x="", y="") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    print(bar_chart)
    
    map_tx_curr <- shp %>% 
      left_join(df, by = c("uid" = "psnuuid")) %>% 
      mutate(vlc = round(vlc * 100)) %>% 
      ggplot(aes(fill = tx_curr)) +
      geom_sf(color = grey30k) +
      scale_fill_viridis_c(option = "A", direction = -1, na.value = NA, labels = function(x) format(paste0(x / 1000, "K"))) +
      labs(title = "TX_CURR (FY19 Q4)") +
      theme_void() +
      theme(
        legend.position = 'bottom',
        legend.box.just = "right",
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(hjust = .5, face = "bold", color = grey80k),
        plot.caption = element_text(face = "italic")
      )
    
    print(map_tx_curr)
    
    map_vlc <- shp %>% 
      left_join(df, c("uid" = "psnuuid")) %>% 
      mutate(vlc = round(vlc * 100)) %>% 
      ggplot(aes(fill = vlc)) +
      geom_sf(color = grey30k) +
      scale_fill_viridis_c(direction = -1, na.value = NA, labels = function(x) format(paste0(x, "%"))) +
      labs(title = "VLC (current)") +
      theme_void() +
      theme(
        legend.position = 'bottom',
        legend.box.just = "right",
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(hjust = .5, face = "bold", color = grey80k),
        plot.caption = element_text(face = "italic")
      )
    
    print(map_vlc)
    
    if (layout == "wide") {
      viz <- (map_tx_curr + map_vlc + bar_chart) +
        plot_layout(nrow = 1)
    }
    else {
      viz <- (map_tx_curr + map_vlc) / bar_chart +
        plot_layout(nrow = 2)
    }
      
    viz <- viz +
      plot_annotation(
        title = {{title}},
        caption = paste0("OHA/SIEI - HQ Q2 Review", Sys.Date())
      ) +
      theme(title = element_text(face = "bold"))
    
    print(viz)

    return(viz)
  }
    
# DATA ------------------------------------------------------------------

  ## GeoData
  
    gis_5_sfc <- list.files(dir_geodata, pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
      set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
      map(read_sf)
  
    gis_4_sfc <- list.files(dir_geodata, pattern = ".*_4_.*.shp$", recursive = T, full.names = T) %>%
      set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
      map(read_sf)

  ## MER PSNUxIM
  
    ## Raw data
    df_psnu <- read_rds(file_psnu_im)
    
    df_psnu %>% glimpse()
    
    df_psnu %>% 
      distinct(indicatortype, numeratordenom, indicator) %>% 
      arrange(indicator) %>% 
      prinf()
    
    ## Calculate VLC
    df_vlc <- df_psnu %>%
      filter(
        fiscal_year %in% c("2019", "2020"),
        fundingagency == "USAID",
        indicator %in% c("TX_PVLS", "TX_CURR"),
        standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
        typemilitary == 'N'
      ) %>% 
      group_by(operatingunit, fundingagency, fiscal_year, indicator, psnuuid, psnu, standardizeddisaggregate) %>% 
      summarise_at(vars(targets:cumulative), sum, na.rm = TRUE) %>% #View()
      ungroup() %>% 
      filter(
        indicator == "TX_PVLS" & standardizeddisaggregate == "Total Denominator" & fiscal_year == "2020" |
          indicator == "TX_CURR" & fiscal_year == "2019"
      ) %>%
      dplyr::select(operatingunit, psnuuid, psnu, indicator, fiscal_year, cumulative) %>% 
      spread(indicator, cumulative) %>% 
      group_by(operatingunit, psnuuid, psnu) %>% 
      mutate(
        TX_CURR = first(TX_CURR),
        VLC = TX_PVLS / lag(TX_CURR, 1),
      ) %>%
      ungroup() %>% 
      filter(fiscal_year == '2020') %>% 
      select(-c(fiscal_year)) %>% 
      janitor::clean_names() 
    
    
## VIZ ---------------------------------------------------------

    ## Nigeria 
    df_vlc_nga <- df_vlc %>% 
      mutate(full = 1.0) %>%
      filter(operatingunit == 'Nigeria')
    
    viz_vlc_nga <- generate_plot(df_vlc_nga, 
                                 gis_4_sfc$Nigeria %>% mutate(uid = orgunit_in), 
                                 title = "NIGERIA - VLC Spatial Distribution")
    
    ggsave(here(dir_graphics, "NIGERIA_VCL_x_PSNU.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
    
    ## Moz
    
    df_vlc_moz <- df_vlc %>% 
      mutate(full = 1.0) %>%
      filter(operatingunit == 'Mozambique')
    
    viz_vlc_moz <- generate_plot(df_vlc_moz, 
                                 gis_5_sfc$Mozambique, 
                                 layout = "wide",
                                 title = "MOZAMBIQUE - VLC Spatial Distribution")
    
    ggsave(here(dir_graphics, "MOZAMBIQUE_VCL_x_PSNU.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
    
    
    ## Zim
    
    df_vlc_zim <- df_vlc %>% 
      mutate(full = 1.0) %>%
      filter(operatingunit == 'Zimbabwe')
    
    viz_vlc_zim <- generate_plot(df_vlc_zim, gis_5_sfc$Zimbabwe, title = "ZIMBABWE - VLC Spatial Distribution")
    
    ggsave(here(dir_graphics, "ZIMBABWE_VCL_x_PSNU.png"),
           scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
    
    