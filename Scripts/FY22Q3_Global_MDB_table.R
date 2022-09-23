# PURPOSE: Munge and Analysis of FY22Q2 summary visual
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2022-06-02
# NOTES: Updated by NP to simplify for slide deck

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(selfdestructin5)
    library(gt)
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "OU_IM_FY20")
    
    embiggen <- function(gt_obj){
      gt_obj %>% 
        tab_options(
          source_notes.font.size = 10,
          table.font.size = 15,
          footnotes.font.size = 10)
    }
      
   # Grab metadata
   msd_source <- source_info(file_path)
   curr_pd <- source_info(file_path, return = "period")
   pd <- source_info(file_path, return = "period")
   fy <- source_info(file_path, return = "fiscal_year")
   qtr <- source_info(file_path, return = "quarter")  
    
  # Functions  
  

# LOAD DATA ============================================================================  

  msd <- read_msd(file_path)
  

# MUNGE ============================================================================
  
  mdb_df   <- make_mdb_df(msd)
  mdb_tbl  <- reshape_mdb_df(mdb_df, curr_pd)  
   
  mdb_df_tx    <- make_mdb_tx_df(msd)
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, pd)  
  
# VIZ ============================================================================

  mdb_tbl %>% 
    create_mdb(ou = "Global", type = "main", curr_pd, msd_source) %>%
    cols_hide(columns= c(6:8,14,15)) %>%
    gtsave(path = "Images", filename = glue::glue("GLOBAL_{curr_pd}_mdb_main.png"),
           vwidth = 1366, vheight = 784, zoom = 2)
  
  create_mdb(mdb_tbl_tx, ou = "Global", type = "treatment", pd, msd_source) %>%
    bold_column(., Q1) %>% 
    bold_rowgroup(.) %>% 
    embiggen()
  

# SPINDOWN ============================================================================

