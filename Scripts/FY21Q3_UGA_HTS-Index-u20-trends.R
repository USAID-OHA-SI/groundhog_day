# PROJECT:  groundhogday
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  UGA Index testing remake
# LICENSE:  MIT
# DATE:     2021-10-18
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
  
msd_source <- source_info()


# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM") %>% 
    read_rds()   

# MUNGE -------------------------------------------------------------------

  df_uga <- df %>% 
    filter(operatingunit == "Uganda",
           fundingagency == "USAID",
           indicator %in% c("HTS_TST", "HTS_TST_POS"),
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           modality %in% c("Index", "IndexMod"),
           fiscal_year >= 2020,
           (ageasentered == "15-19" | trendscoarse == "<15")) %>% 
    group_by(operatingunit, fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    mutate(positivity = hts_tst_pos/hts_tst,
           fill_color = ifelse(period %in% c("FY21Q2", "FY21Q3"), burnt_sienna, scooter))

  v1 <- df_uga %>% 
    ggplot(aes(period, hts_tst, fill = fill_color, label = comma(hts_tst, 1))) +
    geom_col() +
    geom_hline(yintercept = 0) +
    geom_text(color = "#505050",
              family = "Source Sans Pro SemiBold", vjust = -.5) +
    # facet_wrap(~ sex) +
    scale_fill_identity() +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = "Index Testing") +
    si_style_nolines() +
    theme(axis.text.y = element_blank())
  
  v2 <- df_uga %>% 
    ggplot(aes(period, positivity, group = operatingunit,
               fill = fill_color, label = percent(positivity, .1))) +
    geom_hline(yintercept = 0) +
    geom_line(color = grey40k, size = 1.1) +
    geom_point(aes(fill = fill_color), size = 6, shape = 21, stroke = 1.1, color = "white") +
    geom_text(color = "#505050",vjust = -1,
               family = "Source Sans Pro SemiBold") +
    coord_cartesian(clip = "off") +
    expand_limits(y = 0) +
    scale_y_continuous(label = percent_format(1)) +
    scale_fill_identity() +
    # facet_wrap(~sex) +
    labs(x = NULL, y = "Index Positivity") +
    si_style_nolines() +
    theme(axis.text.y = element_blank())

  v1 / v2  + plot_annotation(
      title = 'INCREASED NUMBER OF C/ALHIV WITH KNOWN STATUS AFTER QI IMPLEMENTATION IN UGANDA',
      subtitle = 'Facility and community Index testing for peds and adolescents (<20)',
      caption = glue("Source: {msd_source}
                     US Agency for International Development"),
      theme = si_style_nolines()
    )
    
  si_save("Graphics/UGA_Index_peds_trends.svg", 
          width = 5.75, height = 4)
  