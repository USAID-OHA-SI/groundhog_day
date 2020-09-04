##  PROJECT: Q2 Review target analysis
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  PURPOSE: Geo-depiction of VLC
##  LICENCE: MIT
##  DATE:    2020-08-26

# Dependancies----------------------------------------------------------

library(tidyverse)
library(readxl)
library(vroom)
library(sf)
library(rnaturalearth)
library(here)
library(glamr)
library(glitr)
library(gisr)
library(ICPIutilities)
library(janitor)
library(scales)
library(RColorBrewer)
library(patchwork)

# GLOBALS -------------------------------------------------------------

  ## Creds
  source("./_secrets/_credentials.R")
    
  ## Data & Output folders
  
    dir_data <- "Data"
    dir_dataout <- "Dataout"
    dir_gis <- "GIS"
    dir_graphics <- "Graphics"
    dir_geodata <- "../../GEODATA/PEPFAR"
    dir_merdata <- "../../MERDATA"
  
  ## PSNUxIM Dataset
  
    file_psnu_im <- here(dir_merdata, "MER_Structured_Datasets_PSNU_IM_FY18-20_20200814_v1_1.zip")
    
  ## Notes
  
    footnote <- paste0("USAID/OHA/SIEI - HQ Q3 Data Review, ", Sys.Date())
    
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

  ## USAID/PEPFAR OUs
  
    ous <- identify_ouuids(username = user, password = mypwd(key)) 
    
    ous <- ous %>% 
      dplyr::filter(!str_detect(country, "Region"), type == "OU") %>% 
      dplyr::select(-uid)
    
    ous
    
  ## GeoData
    
    spdf <- ne_countries(type = "sovereignty", scale = 110, returnclass = "sf") 
    
    spdf <- spdf %>% 
      filter(admin != "Antarctica") %>% 
      dplyr::select(sovereignt, admin, name, iso3 = adm0_a3) %>% 
      mutate(admin = case_when(
        admin == "Czech Republic" ~ "Czechia",
        admin == "Ivory Coast" ~ "Cote d'Ivoire",
        admin == "Czechia" ~ "Czech Republic",
        admin == "Myanmar" ~ "Burma",
        admin == "Northern Cyprus" ~ "Cyprus",
        admin == "Republic of Serbia" ~ "Serbia",
        admin == "Republic of Congo" ~ "Republic of the Congo",
        admin == "East Timor" ~ "Timor-Leste",
        admin == "The Bahamas" ~ "Bahamas",
        admin == "United Republic of Tanzania" ~ "Tanzania",
        admin == "United States of America" ~ "United States",
        admin == "Swaziland" ~ "Eswatini",
        TRUE ~ admin
      ))
    
    spdf %>% 
      distinct(admin) %>% 
      arrange(admin) %>% 
      pull(admin) 

  ## MER PSNUxIM
  
    ## Raw data
    df_psnu <- vroom(file_psnu_im)
    #df_psnu <- read_msd(file_psnu_im)
    
    df_psnu %>% glimpse()
    
    df_psnu %>% 
      distinct(indicatortype, numeratordenom, indicator) %>% 
      arrange(indicator) %>% 
      prinf()
    
    ## Calculate VLC
    df_vlc <- df_psnu %>%
      filter(
        fiscal_year == "2020",
        fundingagency == "USAID",
        indicator %in% c("TX_PVLS","TX_CURR"),
        standardizeddisaggregate %in% c("Total Numerator","Total Denominator")
      ) %>% 
      mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
      group_by(fiscal_year, operatingunit, indicator) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      select(-period_type) %>% 
      spread(indicator, val) %>% #View(title = "DF")
      group_by(operatingunit) %>% 
      mutate(
        VLC = TX_PVLS_D / dplyr::lag(TX_CURR, 2, order_by = period),
        VLC = ifelse(VLC > 1, 0, VLC),
        ou_label = paste0(operatingunit, " (", lag(TX_CURR, 2, order_by = period) %>% comma(), ")")
      ) %>% #View()
      ungroup() %>% 
      filter(period == 'FY20Q3') %>% 
      mutate(
        VLS = (TX_PVLS / TX_PVLS_D) * VLC, 
        Not_Cov = ifelse(VLS <= 1, abs(1 - VLS), 0)
      ) %>% 
      clean_names() %>% 
      relocate(ou_label, .after = operatingunit) %>% 
      select(-period) %>% 
      gather(key = "metrics", value = "value", -c(operatingunit, ou_label)) 
    
    df_vlc %>% 
      glimpse()
    
    df_vlc %>% 
      filter(!str_detect(operatingunit, "Region")) %>% 
      distinct(operatingunit) %>% 
      arrange(operatingunit) %>% 
      pull() %>% 
      setdiff(
        spdf %>% 
          distinct(admin) %>% 
          arrange(admin) %>% 
          pull(admin) 
      )
      
    spdf_vlc <- spdf %>% 
      left_join(df_vlc, by = c("admin" = "operatingunit"))
    
    
## VIZ ---------------------------------------------------------

  ## Global Maps: VLS, VLC
    ggplot() +
      geom_sf(data = spdf, fill = NA, color = grey50k, size = .2) +
      geom_sf(data = spdf_vlc %>% 
                filter(!admin %in% c("Ukraine", "South Africa"),
                       metrics %in% c("vls", "vlc")) %>% 
                mutate(metrics = str_replace(toupper(metrics), "_", " ")),
              aes(fill = value), color = grey50k, size = .2) +
      scale_fill_gradient2(
        low = "#edf8e9",
        high = "#006d2c",
        breaks = c(0, .25, .50, .75, 1.00),
        limits = c(0, 1.00),
        labels = percent_format(accuracy = 1)
      ) +
      facet_wrap(~metrics, ncol = 1) +
      labs(
        title = "VIRAL LOAD - Coverage & Suppression",
        subtitle = "Note - South Africa & Regional Mission's data is not included.",
        caption = footnote
      ) +
      si_style_map() +
      theme(
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(face = "bold")
      )
           
    ggsave(here(dir_graphics, "FY20Q3_VLC_VLS_Global_Maps.png"),
           plot = last_plot(), scale = 1.2, dpi = 310, width = 7, height = 10, units = "in")   
    
    ## Global Maps: Not VLC
    ggplot() +
      geom_sf(data = spdf, fill = NA, color = grey50k, size = .2) +
      geom_sf(data = spdf_vlc %>% 
                filter(!admin %in% c("Ukraine", "South Africa"), metrics == "not_cov") %>% 
                mutate(metrics = str_replace(toupper(metrics), "_", " ")),
              aes(fill = value), color = grey50k, size = .2) +
      scale_fill_gradient2(
        low = "yellow",
        high = "brown",
        breaks = c(0, .25, .50, .75, 1.00),
        limits = c(0, 1.00),
        labels = percent_format(accuracy = 1)
      ) +
      labs(
        title = "VIRAL LOAD - Not Covered",
        subtitle = "Note - South Africa & Regional Missions' data not included.",
        caption = footnote
      ) +
      si_style_map() +
      theme(
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm"),
        plot.title = element_text(face = "bold")
      )
    
    ggsave(here(dir_graphics, "FY20Q3_Not_VLC_Global_Maps.png"),
           plot = last_plot(), scale = 1.2, dpi = 310, width = 10, height = 7, units = "in") 
    
    
    