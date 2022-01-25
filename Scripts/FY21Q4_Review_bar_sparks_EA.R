##  PROJECT: Q2 review target analysis
##  AUTHOR:  tessam | USAID
##  PURPOSE: Tiny bar sparks for summary graphic. 
##  LICENCE: MIT
##  DATE:    2020-06-15
##  UPDATE:

# LIBRARIES ----------------------------------------------------------

  library(tidyverse)
  library(vroom)
  library(scales)
  library(extrafont)
  library(ggrepel)
  library(glitr)
  library(ICPIutilities)
  library(here)
  library(glamr)

# GLOBALS -----------------------------------------------------------------

  datim_in <- 
    merdata <- glamr::si_path("path_msd")
  rasdata <- glamr::si_path("path_raster")
  shpdata <- glamr::si_path("path_vector")
  datim   <- glamr::si_path("path_datim")    
    
  data_in <- "Data"
  data_out <- "Dataout"
  viz_out <- "Images"

  # Plot to create tiny progress graphs in bar form
  # Requires a long data frame named df_long
      bar_spark <- function(df, ind, qtr) {
        
        time <- paste0("FY21", qtr)
        
        viz <- df %>% 
          filter( fundingagency == "USAID") %>% 
          ggplot() + 
          geom_col(aes(y = fundingagency, x = target), fill = trolley_grey_light) +
          geom_col(aes(y = fundingagency, x = share), fill = moody_blue) +
          theme_void() +
          coord_cartesian(expand = F, clip = "on")
        
        ggsave(file.path(viz_out, paste0(time, {{ind}}, "review_barspark", ".png")),
               plot = viz, dpi = 330, width = 1.18, height = .08)
        
        return(viz)
      } 


# IMPORT AND MUNGE --------------------------------------------------------

  hts <- tibble::tribble(
           ~fundingagency, ~indicator, ~share, ~target,
                  "USAID",     "t",   0.41,       1
           )
      
      ct <- tibble::tribble(
        ~fundingagency, ~indicator, ~share, ~target,
        "USAID",     "ct",   0.43,       1
      )
      prev <- tibble::tribble(
        ~fundingagency, ~indicator, ~share, ~target,
        "USAID",     "prev",   0.97,       1
      )
      asp <- tibble::tribble(
        ~fundingagency, ~indicator, ~share, ~target,
        "USAID",     "asp",   0.12,       1
      )
      
      
 bar_spark(hts, "hts", "Q4")
 bar_spark(ct, "ct", "Q4")
 bar_spark(prev, "prev", "Q4")
 bar_spark(asp, "asp", "Q4")

  # tmp
  # tmp + ggsave(file.path(viz_out, "FY21Q1_MMD3_review_barspark.png"),
  #              dpi = 330,
  #              width = 2.36,
  #              height = 1)

