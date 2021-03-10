# Purpose: Outlay Barchart in proper style
# Author: Ben Kasdan | EA, 
# Date: 2020-03-03
# Notes: Copy Tim's MMD facet chart, adjusted for outlay needs

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
    #library(patchwork)
    library(ggtext)
    library(here)
    library(readxl)
  
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graph <- "Graphics"
    
  merdata <- glamr::si_path("path_msd")
  rasdata <- glamr::si_path("path_raster")
  shpdata <- glamr::si_path("path_vector")
  datim   <- glamr::si_path("path_datim")  
  load_secrets()

# LOAD DATA ============================================================================  




  # Case when statements are to create fill colors to pass to scale_fill_identity to color certain ous within each category
  outlay<-
    read_csv(here(data, "Quarterly Outlay Status.csv"))
  outlay<-outlay%>%
    mutate( ou_order = reorder_within(ou, outlay_pct, planning_levels))
  outlay<-outlay%>%
    mutate(ou_color = case_when(
      outlay_pct  >= 0.35 ~ "#e07653",
     outlay_pct  <.15 ~ "#f2bc40",
    outlay_pct >=.15 & outlay_pct <.35 ~ "#287c6f"))
  
  
  
 
  # Plot data, creating four facets to show diff breakdown of MMD.
  # Note use of tidytext::reorder_within above on ou order to get sorted facets
  # also need the scale_y_reordered() + the scale = "free_y" in facets to get it to work
  outlay %>% 
    mutate(ou = fct_reorder(ou_order,(planning_levels)))%>%
    #mutate(ou_order=factor(planning_levels))%>%
    ggplot(aes(x = planning_levels, y = ou, fill = ou_color)) +
    geom_col(alpha = 1.5, na.rm = T) +
    geom_text(aes(label = percent(outlay_pct, 1)), hjust = 1,
              family = "Source Sans Pro",
              color = "white", size = 3) +
    facet_wrap(~status) +
    scale_y_reordered() +
    si_style_xgrid() +
    scale_x_continuous(labels = unit_format(1.0, unit = "M", scale = 1e-7)) +
    scale_fill_identity() +
    coord_cartesian(expand = F) +
    labs(x = "COP Planning Levels ($)", y = NULL) +
    theme(panel.spacing = unit(1, "lines"))
  
 
  
   ggsave(here(images, "FY21Q1_outlay_viz.png"), 
          height = 5,
           width = 9.54, 
          scale = 1.4)


