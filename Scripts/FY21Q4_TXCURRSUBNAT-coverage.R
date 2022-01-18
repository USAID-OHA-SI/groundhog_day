# PROJECT:  groundhogday
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  coverage gap
# LICENSE:  MIT
# DATE:     2022-01-18
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  

# GLOBAL VARIABLES --------------------------------------------------------

  authors <- c("Aaron Chafetz")
  
  msd_source <- source_info(type = "NAT_SUBNAT")
  
  ou_sel <- c("Lesotho", "Botswana", "Uganda", "Kenya", "Eswatini", "Namibia")
  
  ind_sel <- c("PLHIV", "DIAGNOSED_SUBNAT" ,"TX_CURR_SUBNAT", "VL_SUPPRESSION_SUBNAT")

  
  
# IMPORT ------------------------------------------------------------------
  
  df_subnat <- si_path() %>% 
    return_latest("NAT_SUBNAT") %>% 
    read_rds()   

# MUNGE -------------------------------------------------------------------

  df_gap <- df_subnat %>% 
    filter(countryname %in% ou_sel,
           fiscal_year == 2022,
           indicator %in% ind_sel,
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    count(countryname, indicator, ageasentered, sex, wt = targets, name = "value") %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(cov_status = diagnosed_subnat/plhiv,
           cov_tx = tx_curr_subnat/plhiv,
           cov_vlc = vl_suppression_subnat/plhiv)
  
  df_viz <- df_gap %>% 
    mutate(plhiv_marker = case_when(tx_curr_subnat > plhiv ~ plhiv),
           fill_color = ifelse(sex == "Male", genoa, moody_blue)) %>% 
    group_by(countryname) %>% 
    mutate(ctry_name = glue("{countryname}<br>{label_number_si(.1)(sum(tx_curr_subnat, na.rm = TRUE))}/{label_number_si(.1)(sum(plhiv, na.rm = TRUE))}"),
           lab_gap = case_when(cov_tx < .95^2 ~ percent(cov_tx, 1))) %>% 
    ungroup()
  
# VIZ ---------------------------------------------------------------------


  df_viz %>% 
    ggplot(aes(plhiv, ageasentered, fill = fill_color, color = fill_color)) +
    geom_blank(aes(plhiv*1.1)) +
    geom_col(fill = NA, width = .8, alpha = .8) +
    geom_col(aes(tx_curr_subnat), width = .8, alpha = .8) +
    geom_errorbar(aes(xmin = plhiv_marker, xmax = plhiv_marker), 
                  na.rm = TRUE, color = "white", linetype = "dotted") +
    geom_text(aes(label = lab_gap), na.rm = TRUE,
              family = "Source Sans Pro", color = suva_grey,
              size = 10/.pt, hjust = -.5) +
    facet_grid(sex ~ fct_reorder(ctry_name, plhiv, sum, na.rm = TRUE, .desc = TRUE),
               switch = "y", scales = "free_x"
               ) +
    scale_x_continuous(labels = label_number_si(),
                       expand = c(.005, .005)) +
    scale_fill_identity(aesthetics = c("fill", "color")) +
    labs(x = NULL, y = NULL,
         title = "The largest gaps to treatment coverage fall largely with men" %>% toupper,
         subtitle = "Sustained Impact Countries' TX_CURR_SUBNAT coverage of PLHIV (COP21)",
         caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
    coord_cartesian(clip = "off") +
    si_style_xgrid() +
    theme(strip.text.y = element_text(hjust = .5),
          strip.text.x = element_markdown(),
          strip.placement = "outside",
          panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(.5, "lines"))
  
  si_save("Graphics/FY21Q4_TXCURRSUBNAT-coverage.svg")
  