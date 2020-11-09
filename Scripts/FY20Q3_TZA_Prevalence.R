## PROJECT: TANZANIA SUPPORT
## AUTHOR:  A.CHAFETZ | USAID
## PURPOSE: compare prevalence in different regions
## LICENCE: MIT
## DATE:    2020-09-12


# inspiration: https://www.behance.net/gallery/99114047/Population-Density


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(glamr)
  library(glitr)
  library(extrafont)
  library(RColorBrewer)



# IMPORT DATA -------------------------------------------------------------

  df <- list.files("~/Data", "NAT_SUBNAT", full.names = TRUE) %>% 
    read_rds()
  
  df_msd <- list.files("~/Data", "PSNU_IM", full.names = TRUE) %>% 
    read_rds()


# MUNGE -------------------------------------------------------------------

  #filter and calc prev
    df_prev <- df %>% 
      filter(fiscal_year == 2020,
             operatingunit == "Tanzania",
             indicator %in% c("PLHIV","POP_EST"),
             standardizeddisaggregate == "Age/Sex") %>%
      group_by(operatingunit, snu1, indicator) %>% 
      summarise(value = sum(targets, na.rm = TRUE)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = indicator, values_from = value) %>%
      rename_all(tolower) %>% 
      mutate(prevalence = plhiv/pop_est,
             prevalence_1k = round(prevalence * 1000, 0))
  
  #identify the primary agency in a region
    df_snu_agency <- df_msd %>% 
      filter(operatingunit == "Tanzania",
             indicator == "TX_CURR",
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == 2020,
             str_detect(snu1, "_Military", negate = TRUE)) %>% 
      count(snu1, fundingagency, wt = targets) %>% 
      group_by(snu1) %>%
      filter(n == max(n)) %>% 
      ungroup() %>% 
      mutate(fundingagency = str_remove(fundingagency, "HHS/")) %>% 
      select(-n)
  
  #gen random points per region
    plot_pts <- map2_dfr(.x = df_prev$snu1,
                         .y = df_prev$prevalence_1k,
                         ~ tibble(snu1 = .x,
                                  x_rand = runif(.y),
                                  y_rand = runif(.y)))
  #merge
    plot_pts <- plot_pts %>% 
      left_join(df_prev) %>% 
      left_join(df_snu_agency) %>% 
      filter(!is.na(fundingagency))


# PLOT --------------------------------------------------------------------


  colors <- brewer.pal(3, "BrBG")

  plot_pts %>% 
    ggplot(aes(x = x_rand, y = y_rand)) +
    # geom_area(aes(y = 1)) +
    geom_point(aes(color = fundingagency), alpha = .7) +
    geom_hline(yintercept = 1, size = .9, color = "gray50") +
    coord_polar() +
    labs(x = NULL, y = NULL,
         title = "CDC MAY BE IN REGIONS WITH HIGHER PLHIV COUNTS BUT USAID COVERS REGIONS WITH \nHIGHER PREVALENCE",
         subtitle = "2020 HIV prevalence per 1,000   |   regions ordered by PLHIV",
         caption = "Source: MSD + IMPATT") +
    facet_wrap(~fct_reorder(snu1, desc(plhiv)), strip.position = "bottom") +
    scale_color_manual(values = c("CDC" = colors[1], "USAID" = colors[3], "DOD" = "gray70"),
                       name = NULL) +
    expand_limits(x = c(0, 1), y = c(0,1)) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_text(hjust = .5),
          panel.spacing = unit(.5, "lines"),
          legend.title = element_text(family = "Source Sans Pro"))
   
