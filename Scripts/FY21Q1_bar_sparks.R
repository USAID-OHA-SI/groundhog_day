# PURPOSE: FY21Q1 Review
# AUTHOR: Tim Essam | SI, 
# LICENSE: MIT
# DATE: 2021-02-28
# NOTES:

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
    
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
    
  # Functions  
  merdata <- glamr::si_path("path_msd")


  # Indicators
  
  # Plot to create tiny progress graphs in bar form
  # Requires a long data frame named df_long
  bar_spark <- function(df, ind, share) {
    
    viz <- df %>% 
      filter(indicator == {{ind}}, fundingagency == "USAID") %>% 
      ggplot() + 
      geom_col(aes(y = fundingagency, x = 1), fill = trolley_grey_light) +
      # old color #e04745
      geom_col(aes(y = fundingagency, x = {{share}}), fill = denim) +
      theme_void() +
      theme(legend.position = "none",
            plot.background = element_rect(fill = "white",
                                           color = "white"),
      ) +
      coord_cartesian(expand = F)
    
    ggsave(file.path(graphs, paste0("FY20Q1_sparks", {{ind}}, "_", deparse(substitute(share)), ".png")),
            plot = viz, dpi = 330, width = 1.25, height = 0.1)
    
    print(viz)
  } 
  
# LOAD DATA ============================================================================  
  msd <- return_latest(folderpath = merdata, pattern = "OU_IM_FY19-21")
  df <- read_msd(msd) 

# MUNGE ============================================================================
  
  # Extract out USAID share an values for key indicators
  df %>% count(indicator, standardizeddisaggregate) %>% prinf()
  
  df %>% filter(indicator == "TX_CURR") %>% 
    count(indicator, otherdisaggregate, standardizeddisaggregate) %>% prinf()
  
  # Retrieve TX_MMD
  tx_mmd <- 
    df %>% 
    filter(indicator == "TX_CURR",
           str_detect(otherdisaggregate, "(3 to 5|6 or more)"), 
           fiscal_year == "2021") %>% 
    group_by(fundingagency, indicator) %>%
    summarise(across(c("targets", "cumulative"), sum, na.rm = T)) %>% 
    mutate(indicator = "TX_MMD")
  
  
  # Calculate Agency shares and totals
  df_long <- 
    df %>% 
    filter(
      standardizeddisaggregate == "Total Numerator",
      indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST_POS", "PrEP_NEW", "VMMC_CIRC"),
      fiscal_year == "2021"
    ) %>% 
    group_by(fundingagency, indicator) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = T)) %>%
    rbind(tx_mmd) %>% 
    mutate(achievement = cumulative / targets) %>% 
    group_by(indicator) %>% 
    mutate(across(c(targets, cumulative), sum, na.rm = T, .names = "all_{.col}")) %>%
    ungroup() %>% 
    mutate(agency_share = cumulative / all_cumulative) %>% arrange(indicator, fundingagency) %>% 
    filter(fundingagency == "USAID") %>% 
    mutate(agency_share = if_else(indicator == "TX_MMD", 0.75, agency_share))
  
# VIZ ============================================================================

  #  Sample plot
  unique(df_long$indicator) %>% 
    map(~bar_spark(df_long, ind = .x, agency_share))
  
  unique(df_long$indicator) %>% 
    map(~bar_spark(df_long, ind = .x, achievement))
  
  
  bar_spark(df_long, "HTS_TST_POS", agency_share)

# SPINDOWN ============================================================================

names(df_long)
  