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
          filter(indicator == {{ind}}, fundingagency == "USAID") %>% 
          ggplot() + 
          geom_col(aes(y = fundingagency, x = target), fill = trolley_grey_light) +
          geom_col(aes(y = fundingagency, x = share), fill = scooter) +
          theme_void() +
          coord_cartesian(expand = F, clip = "off")
        
        ggsave(file.path(viz_out, paste0(time, {{ind}}, "review_barspark", ".png")),
               plot = viz, dpi = 330, width = 2.36, height = 1)
        
        return(viz)
      } 


# IMPORT AND MUNGE --------------------------------------------------------

  mmd <- tibble::tribble(
           ~fundingagency, ~indicator, ~share, ~target,
                  "USAID",     "MMD3",   0.76,       1
           )
      
      
 bar_spark(mmd, "MMD3", "Q1")

  # tmp
  # tmp + ggsave(file.path(viz_out, "FY21Q1_MMD3_review_barspark.png"),
  #              dpi = 330,
  #              width = 2.36,
  #              height = 1)

