# PURPOSE: Historical Analysis of TESTING, TREATMENT with TRENDS
# AUTHOR: Tim Essam | SI  
# LICENSE: MIT
# DATE: 2021-03-05
# NOTES: FOR FY21Q1 REVIEW

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(ICPIutilities)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    library(ggdist)
    
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
     
  # Functions  
  indic_list <- c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW")
  
  # Collapse and sum targets and cumulative results by OU / indicator /FY
  calc_ach <- function(df) {
    df %>% 
      group_by(operatingunit, indicator, fiscal_year) %>% 
      summarise(across(c("targets", "cumulative"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(achievement = cumulative / targets)
  }

# LOAD DATA  & MUNGE ============================================================================  

  msd_18 <- 
      read_msd(file.path(merdata, "MER_Structured_Datasets_OU_IM_FY15-18_20210212_v1_1.zip")) %>% 
      filter(fundingagency == "USAID",
             indicator %in% indic_list,
             standardizeddisaggregate == "Total Numerator")
  
  msd_18_ach <- 
    msd_18 %>% 
    calc_ach()  
  
  msd_21 <- 
      read_msd(file.path(merdata, "MER_Structured_Datasets_OU_IM_FY19-21_20210212_v1_1.zip")) %>%
      filter(fundingagency == "USAID",
           indicator %in% indic_list,
           standardizeddisaggregate == "Total Numerator")
  
  msd_21_ach <- 
    msd_21 %>% 
    calc_ach()
  
  msd_hist <- 
    bind_rows(msd_18_ach, msd_21_ach) %>% 
    filter(fiscal_year != 2015,
           operatingunit != "South Africa") %>% 
    group_by(fiscal_year, indicator) %>% 
    mutate(tot_results = sum(cumulative, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(annual_sh = cumulative / tot_results) %>% 
    mutate(
      ou_color = case_when(
        achievement > 1.1 ~ grey30k,
        achievement >= 0.9 & achievement <= 1.1  ~ "#5bb5d5",
        achievement >= 0.75 & achievement < 0.9  ~ "#ffcaa2",
        achievement < 0.75 ~ "#ff939a")
    ) 
  
  msd_hist %>% 
    arrange(operatingunit, indicator, fiscal_year) %>% 
    spread(fiscal_year, annual_sh) 
    
  
  
# VIZ ============================================================================


  min <- min(msd_hist$achievement[msd_hist$fiscal_year == 2021])

  #  Plot each indiactor as a dot plot by time, weighting each dot by target volume  
  msd_hist %>% 
    filter(fiscal_year != 2021,
           targets != 0) %>%
    mutate(ou_label = case_when(
             indicator == "HTS_TST" & achievement<= 0.5 ~ operatingunit,
             indicator == "HTS_TST_POS" & achievement <.4 ~ operatingunit,
             indicator == "TX_CURR" & achievement <= 0.5 ~ operatingunit,
             indicator == "TX_NEW" & achievement <= .45 ~ operatingunit, 
             TRUE ~ NA_character_)
           ) %>% 
    ggplot(aes(x = fiscal_year, group = indicator, size = annual_sh)) +
    geom_hline(yintercept = 1, color = grey50k, size = 0.5) +
    geom_hline(yintercept = c(0.5, 1.5), color = grey20k, size = 0.25, linetype = "dashed") +
    #geom_point(shape = 21, fill = grey10k) +
    stat_halfeye(aes(y = achievement), alpha = 0.25, side = "right") +
    geom_point(aes(y = achievement, fill = ou_color), 
               shape = 21, color = "white", alpha = 0.75,
               position = position_jitter(w = 0.15, h = 0, seed = 42)) +
    geom_point(data = msd_hist %>% filter(fiscal_year == 2021),
                aes(y = achievement, fill = ou_color), shape = 21, color = "white", alpha = 0.25,
               position = position_jitter(w = 0.15, h = 0, seed = 42)) +
    geom_smooth(aes(weight = cumulative, y = achievement), color = grey80k, se = F) +
    facet_wrap(~indicator, scales = "free_y") +
    scale_y_continuous(limits  = c(0, 2.5), oob=scales::squish, labels = percent,
                       breaks = c(0.5, 1, 1.5)) +
    si_style_xline() +
    coord_cartesian(clip = "off", expand = T) +
    scale_fill_identity() +
    scale_size(range = c(0, 10)) +
    theme(legend.position = "none") +
    #ggrepel::geom_text_repel(aes(y = achievement, label = ou_label), size = 3, colour = color_caption)
    labs(x = NULL, y = NULL, title = "",
         caption = "Source: MSD FY16-FY21. South Africa excluded.  ")
  
  si_save(here(images, "FY21Q1_historical_trends.png"), scale = 1.25,
          width = 10, height = 5.5, dpi = 320)
  
  ggsave(here(graphs, "FY21Q1_historical_trends.svg"), scale = 1.25,
         width = 10, height = 5.625, dpi = 320)
  

  # Distribution shifts?    
  msd_hist %>% 
  ggplot(aes(x = fiscal_year, y = achievement)) +
    stat_halfeye(alpha = 0.5, side = "right") +
    facet_wrap(~indicator) +
    scale_y_continuous(limits = c(0, 2.5), oob = squish)
  
  
  msd_hist %>% 
    mutate(ou_order = reorder_within(operatingunit, achievement, indicator)) %>% 
    ggplot(aes(x = fiscal_year, y = ou_order, fill = achievement)) +
    geom_tile(color = "white") + facet_wrap(~indicator, scales = "free") +
    scale_y_reordered() +
    scale_fill_viridis_c(direction = -1, option = "D", alpha = 0.5,
                         oob = squish, limits = c(0, 2)) +
    si_style_nolines()
  

# SPINDOWN ============================================================================

