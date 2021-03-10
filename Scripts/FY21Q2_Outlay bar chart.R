# Purpose: Outlay Barchart in proper style
# Author: Ben Kasdan | EA, 
# Date: 2020-03-03
# Notes:

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


# MMD PLOTS ---------------------------------------------------------------------

  # Remaking a stacked bar graph into a small multiple with frosted top tips.
  # Want to show 6+ month MMD OUs greater than 50%; 3+ month MMD greater than 90%
  # For the 3+ month MMD need to combine the 6+ Month MMD + 3-5 Month MMD numbers 
  # Data are from Supply Chain Dashboard: Global.04: MMD Levels Crosstab export (remove % signs)
  
  # Grab crosstab and pivot longer so data can be easily faceted (long data makes for easier faceting)
  mmd_stack <- 
    read_csv(here(data, "OU_05_MMD.csv"))%>% 
    pivot_longer(., 
                 cols = na:`6+ Month MMD`,
                 names_to = "duration",
                 values_to = "pct") %>% 
    group_by(ou) %>% 
    mutate(rownum = row_number()) %>% 
    ungroup() 


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
  
  
  
  mmd <- 
    mmd_stack %>% 
    mutate(rownum = ifelse(rownum == 4, 3, rownum)) %>% 
    group_by(ou, rownum) %>% 
    mutate(mmd_sum = sum(pct)) %>% 
    ungroup() %>% 
    group_by(ou) %>% 
    mutate(rownum = row_number()) %>% 
    ungroup() %>% 
    mutate(pct = if_else(rownum == 3, mmd_sum, pct),
           ou_order = reorder_within(ou, pct, duration),
           mmd_order = factor(duration),
           mmd_order = fct_relevel(mmd_order, 
                                 "6+ Month MMD", 
                                 "3+ Month MMD",
                                 "<3 Months (non-MMD)",
                                 "na"),
           ou_color = case_when(
             duration == "6+ Month MMD" & pct >= 0.5 ~ genoa,
             duration == "6+ Month MMD" & pct < 0.5 ~ genoa_light,
             duration == "3+ Month MMD" & pct >= .90 ~ moody_blue,
             duration == "3+ Month MMD" & pct < .90 ~ moody_blue_light,
             duration == "<3 Months (non-MMD)" ~ burnt_sienna,
             TRUE ~ grey40k),
         ou_label = case_when(
            duration == "6+ Month MMD" & pct> 0.5 ~ pct,
            duration == "3+ Month MMD" & pct >= .90 ~ pct,
            TRUE ~ NA_real_),
         zero = 0,
         pct_new = if_else(rownum == 1 & pct < 0.0001, NA_real_, pct)
         ) 
  
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
  
  #____----------------------------------
   mmd %>% 
      filter(!is.na(pct_new)) %>% 
      ggplot(aes(x = pct, y = ou_order, fill = ou_color)) +
      geom_col(alpha = 0.85, na.rm = T) +
      geom_text(aes(label = percent(ou_label, 1)), hjust = 1.05,
                family = "Source Sans Pro",
                color = "white", size = 3) +
      facet_wrap(~mmd_order, scale = "free_y", drop = T) +
      scale_y_reordered() +
      si_style_xgrid() +
      scale_x_continuous(labels = percent, 
                         breaks = c(seq(0, 1, .2))) +
      scale_fill_identity() +
      coord_cartesian(expand = F) +
      labs(x = NULL, y = NULL) +
      theme(panel.spacing = unit(0.25, "lines"))
  
   ggsave(here(images, "FY21Q1_outlay_viz.png"), 
          height = 5,
           width = 9.54, 
          scale = 1.4)


