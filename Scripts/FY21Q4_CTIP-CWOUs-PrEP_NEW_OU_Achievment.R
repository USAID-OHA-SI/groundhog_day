# Project: FY21Q4 CTIP-CWOU that we are not involved with yet are making many graphs.
# Author: Tim Essam | SI
# Purpose: Verify a statement | Make PrEP folks happy.
# LICENSE:  MIT
# Date: 2021-12-09
# Updated: 


# GLOBALS -----------------------------------------------------------------

  library(glitr)
  library(glamr)
  library(gisr)
  library(tidyverse)
  library(gophr)
  library(scales)
  library(sf)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(glue)


# LOAD DATA ---------------------------------------------------------------

  merdata <- glamr::si_path("path_msd") 
  ou_im_path <- list.files(merdata, "OU_IM_FY19-22_20211112_v1_1", full.names = T)
  
  ou_im <- read_msd(ou_im_path)
  
  curr_pd <- source_info(ou_im_path, return = "period")

# MUNGE JUST PREP_NEW -----------------------------------------------------

  # Filter to just PrEP_NEW
  prep_df <- ou_im %>% 
    filter(indicator == "PrEP_NEW", fiscal_year == 2021, fundingagency == "USAID",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(operatingunit, indicator, fiscal_year, fundingagency) %>% 
    summarize(across(matches("targets|cumu"), sum, na.rm = T)) %>% 
    ungroup()
  
  # Create target capped results, spillover results, and results deficit
  # TODO: Do we need a separate deficit/surplus variable or should we use binaries to flag? May make plotting easier w/ separate vars.
  # TODO: Check edge case when results == targets (is this covered?)
  prep_adj <- 
    prep_df %>% 
    group_by(operatingunit, indicator) %>% 
    mutate(rslt_capped = ifelse(cumulative > targets, targets, cumulative),
           rslt_gap = cumulative - targets,
           rslt_deficit = ifelse(cumulative < targets, cumulative - targets, NA_real_),
           rslt_surplus = ifelse(cumulative >= targets, cumulative - targets, NA_real_),
           achv = cumulative / targets,
           achv_adj = rslt_capped / targets, 
           ou_over_achv = cumulative > targets) %>% 
    ungroup() %>% 
    arrange(rslt_gap) 
  
  # Create deficit share associated with each OU (or level of interest)
  # ou_gap_sh allows us to say of the 96K deficity, 65% belongs to Tanzania
  # And on the other side, of the 94K surplus, 43% belongs to Nigeria
  
  prep_adj <- 
    prep_adj %>% 
    group_by(ou_over_achv) %>% 
    mutate(rslt_gap_tot = sum(rslt_gap)) %>% 
    ungroup()%>% 
    mutate(ou_gap_sh = rslt_gap / rslt_gap_tot,
      gap_running_sh = cumsum(ou_gap_sh),
      gap_running_target = cumsum(rslt_gap)
    )
      
  
  # Create Global rollups for achv and adjusted achv
  prep_adj <- 
    prep_adj %>% 
    group_by(fundingagency) %>% 
    mutate(across(c(targets:rslt_surplus), sum, na.rm = T, .names = "{.col}_glbl")) %>% 
    ungroup() %>% 
    mutate(achv_glbl = cumulative_glbl / targets_glbl,
           achv_adj_glbl = rslt_capped_glbl / targets_glbl,
           deficit_sh = abs(rslt_deficit_glbl)/targets_glbl,
           surplus_sh = rslt_surplus_glbl / targets_glbl) %>% 
    mutate(ou_order = fct_reorder(operatingunit, cumulative),
           facet_label = ifelse(ou_over_achv == TRUE, "Achieved Targets", "Unachieved"), 
           ou_order2 = reorder_within(operatingunit, targets, facet_label))

  # What colors to use?
  si_rampr("scooters")[4] %>% show_col()
  
  # Pull global achv and global ach adj
 achv_lab     <- prep_adj %>% slice(1) %>% pull(achv_glbl)
 achv_adj_lab <- prep_adj %>% slice(1) %>% pull(achv_adj_glbl)
  
  # Plots
 glbl_plot <- 
   prep_adj %>% 
    slice(1) %>% 
    ggplot(aes(y = "Global")) +
    # What is benchmark?
    geom_col(aes(x = targets_glbl), fill = trolley_grey_light) +
    # What is total rollup?
    #geom_col(aes(x = cumulative_glbl), fill = scooter_med) +
    # What is target capped rollup
    geom_col(aes(x = cumulative_glbl), fill = scooter) +
    # What is target capped deficit?
    # geom_col(aes(x = rslt_deficit_glbl), fill = old_rose_light) +
    #geom_text(aes(x = rslt_capped_glbl, label = percent(achv_adj_glbl)),
              #hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    geom_text(aes(x = cumulative_glbl, label = percent(achv_glbl)),
              hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    # geom_text(aes(x = rslt_deficit_glbl, label = percent(deficit_sh)),
    #           hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
    geom_vline(xintercept = 0, size = 0.5, color = grey90k) +
    geom_vline(xintercept = seq(1e5, 5e5, 1e5), color = "white") +
    scale_x_continuous(labels = scales::label_number_si())+
    si_style_xgrid() +
    coord_cartesian(clip = "off", expand = F) +
    labs(x = NULL, y = NULL, title = glue("USAID ACHIEVEMENT FOR PrEP_NEW IS {scales::percent({achv_lab})} for FY21"))
 
 ou_count <- prep_adj %>% summarise(ou_achv = sum(rslt_surplus > 0, na.rm = T)) %>% pull()
                                    
 ou_plot <-  
 prep_adj %>% 
   ggplot(aes(y = ou_order2)) +
   # What is benchmark?
   geom_col(aes(x = targets), fill = trolley_grey_light) +
   # What is total rollup?
   geom_col(aes(x = cumulative), fill = scooter_med) +
   # What is target capped rollup
   # geom_col(aes(x = rslt_capped), fill = scooter) +
   # Show target threshold in white to indicate surplus
   geom_errorbar(aes(xmin = targets, xmax = targets, color = ifelse(!is.na(rslt_surplus), "white", NA_character_)), size = 0.5) +
   # What is target capped deficit?
   # geom_col(aes(x = rslt_deficit), fill = old_rose_light) +
   geom_text(aes(x = cumulative, label = percent(achv, 1)),
             hjust = -0.5, size = 10/.pt, family = "Source Sans Pro") +
   geom_vline(xintercept = 0, size = 0.5, color = grey90k) +
   facet_wrap(~facet_label, scales = "free_y", ncol = 1) +
   scale_x_continuous(labels = scales::label_number_si())+
   si_style_xgrid(facet_space = 0.25) +
   scale_y_reordered() +
   scale_color_identity()  +
   coord_cartesian(clip = "off", expand = F) +
   labs(x = NULL, y = NULL, title = glue("{ou_count} OUs ACHIEVED THEIR FY21 PrEP_NEW TARGETS"))
  
  cowplot::plot_grid(glbl_plot, ou_plot, ncol = 1, align = "hv", axis = "bt", rel_heights = c(1, 6))
 
   si_save("Images/adj_achievement.svg", height = 4.37, width = 10, scale = 1.45)

   
