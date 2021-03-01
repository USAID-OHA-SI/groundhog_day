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
  rasdata <- glamr::si_path("path_raster")
  shpdata <- glamr::si_path("path_vector")
  datim   <- glamr::si_path("path_datim")

  # Indicators
  indic <- c("TX_NEW", "TX_CURR", "HTS_TST_POS" )
  
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
  df %>% filter(
    standardizeddisaggregate == "Total Numerator",
    indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST_POS", "PrEP_NEW"),
    fiscal_year == "2021") %>% 
    group_by(fundingagency, indicator) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = T)) %>%
    rbind(tx_mmd) %>% 
    mutate(achievement = cumulative / targets) %>% 
    group_by(indicator) %>% 
    mutate(across(c(targets, cumulative), sum, na.rm = T, .names = "all_{.col}")) %>%
    ungroup() %>% 
    mutate(agency_share = cumulative / all_targets) %>% arrange(indicator, fundingagency) %>% 
    prinf()
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

