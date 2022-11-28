# PROJECT: Create Q4 MDB tables for all OUs
# PURPOSE: Munge and Analysis of OU X IM for MDB tables
# AUTHOR: Tim Essam | SI
# REF ID:   a98d0ed7
# LICENSE: MIT
# DATE: 2022-11-28
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(extrafont)
    library(selfdestructin5)
    library(glue)
    library(gt)
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "OU_IM_FY20-23_20221114")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "a98d0ed7"
    
  # Functions  
  # create batch tables
    distinct_agg_type <- function(df, type = "OU"){
      df %>% 
        filter(agg_type == {{type}}) %>% 
        distinct(operatingunit) %>% 
        pull()
    }
  

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) 
  

# MUNGE ============================================================================
  
  mdb_df   <- make_mdb_df(df_msd)
  mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)  
    
  # Create the treatment data frame needed for derived indicators
  mdb_df_tx    <- make_mdb_tx_df(df_msd)
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
  
  # Create list for OUs
  ous <- distinct_agg_type(mdb_tbl)
  ous_tx <- distinct_agg_type(mdb_tbl_tx, "OU")
# VIZ ============================================================================

  # Create global tables
  mdb_tbl %>% 
    create_mdb(ou = "Global", type = "main", metadata$curr_pd, metadata$caption) %>% 
    gtsave(path = "Images", filename = glue::glue("GLOBAL_{metadata$curr_pd}_mdb_main.png"),
           vwidth = 1366, vheight = 784, zoom = 2)
  
  mdb_tbl_tx %>% 
    create_mdb(ou = "Global", type = "treatment", metadata$curr_pd, metadata$caption) %>% 
    gtsave(path = "Images", filename = glue::glue("GLOBAL_{metadata$curr_pd}_mdb_main.png"),
           vwidth = 1366, vheight = 784, zoom = 2)
  
  
  # Batch OU tables
  purrr::walk(ous, ~create_mdb(mdb_tbl, ou = .x, type = "main", metadata$curr_pd, metadata$caption) %>% 
               gtsave(., path = "Images/OUs/", filename = glue::glue("{.x}_{metadata$curr_pd}_mdb_main.png")))
  
  purrr::walk(ous_tx, ~create_mdb(mdb_tbl_tx, ou = .x, type = "treatment", metadata$curr_pd, metadata$caption) %>% 
        gtsave(., path = "Images/OUs/tx", filename = glue::glue("{.x}_{metadata$curr_pd}_mdb_treatment.png")))
  

# SPINDOWN ============================================================================

