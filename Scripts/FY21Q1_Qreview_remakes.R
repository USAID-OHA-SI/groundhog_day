# Purpose: Remake of FY21Q1 Graphics in proper style
# Author: Tim Essam | SI, 
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
    read_csv(here(data, "OU_05_MMD.csv")) %>% 
    pivot_longer(., 
                 cols = na:`6+ Month MMD`,
                 names_to = "duration",
                 values_to = "pct") %>% 
    group_by(ou) %>% 
    mutate(rownum = row_number()) %>% 
    ungroup() 

  # Munge the loaded data. Need to coalesce 6+ Month with 3-5 Month to replace 3-5 Month with 3+ month
  # Want to keep 6+ month data too (reuest from SC team)
  # To do this, we've created a tracking index within each out corresponding to the MMD length
  # Manipulate this to create a new column that sums to the value of 6 an 3-5 mo MMD
  # Case when statements are to create fill colors to pass to scale_fill_identity to color certain ous within each category
  
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
           duration = if_else(duration == "na", "not reported", duration),
           mmd_order = factor(duration),
           mmd_order = fct_relevel(mmd_order, 
                                 "6+ Month MMD", 
                                 "3+ Month MMD",
                                 "<3 Months (non-MMD)",
                                 "not reported"),
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
  
   ggsave(here(images, "FY21Q1_6mmd_scaling.png"), 
          height = 5,
           width = 9.54, 
          scale = 1.4)


# DREAMS ------------------------------------------------------------------

  # SLIDE: DREAMS primary package completion reached the 90% benchmark among AGYW in DREAMS at least 13 months in 2 OUs in FY20
  source("../fy21q1_dreams.R")
   
  bar <- dreams %>% 
    mutate(fill_color = if_else(primary.package > .9, genoa, "#3bb7a4")) %>% 
    ggplot(aes(y = ou_order)) +
    geom_col(aes(x = 1), fill = grey10k, alpha = 0.85) +
    geom_col(aes(x = primary.package, fill = fill_color), alpha = 0.85) +
    geom_vline(xintercept = .9, color = "white", size = 1) +
    geom_text(aes(label = percent(primary.package, 1), x = primary.package), hjust = 1.05, color = "white") +
    scale_fill_identity() +
    scale_x_continuous(limits = c(0, 1.05), breaks = 0.9, labels = paste0("     ", percent(0.9), "Threshold")) +
    si_style_nolines() +
    coord_cartesian(expand = F) +
    labs(x = NULL, y = NULL)

   table <-  dreams %>% 
    pivot_longer(cols = `13m+ in DREAMS`:`Total in DREAMS`,
                 names_to = "type",
                 values_to = "total") %>%
      mutate(type = paste0("  ", type)) %>% 
    ggplot(aes(y = ou_order, x = type)) +
    geom_tile(fill = "white") +
      geom_text(aes(label = comma(total)), hjust = 1,
                family = "SourceSansPro-Regular",
                color = grey90k) +
      si_style_nolines() +
      scale_x_discrete(position = "top",
                       labels = c("13+ months", "Total")) +
      theme(axis.text.x.top = element_text(hjust = 1),
            axis.text.y = element_blank()) +
      labs(x = NULL, y = NULL) +
     coord_cartesian(clip = "off")
    
    bar + table + plot_layout(width = c(4, 1))
    
    si_save(here(images, "FY21Q1_review_DREAMS.png"),
            height = 4.5,
            width = 9.75)


# DREAMS Map --------------------------------------------------------------

  #SLIDE: USAID supports DREAMS implementation across the majority of DREAMS SNUs in FY20, reaching AGYW with a comprehensive prevention package  
    
  ou_dreams <- c("Ivory Coast", "Kenya", "Uganda", "Rwanda", "United Republic of Tanzania",
                 "Zambia", "Malawi", "Mozambique", "Botswana", "Zimbabwe",
                 "Namibia", "South Africa", "Lesotho", "Swaziland")
  
  #ous <- get_outable(datim_user(), datim_pwd())
  ous <- get_outable("", mypwd("")) %>% 
    mutate(countryname_iso = if_else(countryname_iso == "SSD", "SDS", countryname_iso))
  
  map <- rnaturalearth::ne_countries(continent = "africa", returnclass = "sf") %>% 
    left_join(., ous, by = c("sov_a3" = "countryname_iso")) %>% 
    mutate(pepfar_fill = case_when(
      !is.na(operatingunit) & !sovereignt %in% ou_dreams ~ "#9fbeed",
      sovereignt %in% ou_dreams ~ denim,
      TRUE ~ grey20k)
    )
  
  # Set up terrain  
  afr <- map %>% st_drop_geometry() %>% distinct(sovereignt) %>% pull()
  terr <- get_terrain(afr, terr = rasdata, mask = T)

  ggplot() +
    geom_tile(data = filter(terr, value < 210), aes(x = x, y = y, alpha = value)) + 
    scale_alpha(name = "", range = c(0.6, 0), guide = F) +
    geom_sf(data = map, aes(fill = pepfar_fill), color = "white", size = 0.1, alpha = 0.8) +
    scale_fill_identity() +
    si_style_map()
  
  ggsave(here(images, "FY21Q1_dreams_map.png"),
         height = 5 ,
         width = 4.6)
  
  # Add in Haiti
  hti <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large") %>% 
    filter(sovereignt %in% c("Haiti", "Dominican Republic")) %>% 
    mutate(ou_fill = if_else(sovereignt == "Haiti", denim, grey20k))
  
  hti_terr <- get_terrain(hti %>% st_drop_geometry() %>% distinct(sovereignt) %>% pull(),
                          terr = rasdata, mask = T)
  
  ggplot(hti) +
    geom_tile(data = filter(hti_terr, value < 210), aes(x = x, y = y, alpha = value)) + 
    scale_alpha(name = "", range = c(0.6, 0), guide = F) +
    geom_sf(aes(fill = ou_fill), color = "white", size = 0.1, alpha = 0.8) +
    scale_fill_identity() +
    si_style_map()
  
  ggsave(here(images, "FY21Q1_dreams_haiti_map.png"),
         height = 1,
         width = 2.3)

 
# TESTING -----------------------------------------------------------------

  #  Despite COVID-19 effects on testing, FY21 Q1  HTS POS percent achievement is on pace compared to prior years.
  # Tableau data doesn't match MER
    msd <- return_latest(folderpath = merdata, pattern = "OU_IM_FY19-21")
    df <- read_msd(msd) 
    
  tst <- df %>% 
      filter(
        standardizeddisaggregate == "Total Numerator",
        indicator %in% c("HTS_TST_POS"),
        fiscal_year %in% c("2021", "2020", "2019"),
        fundingagency == "USAID"
      ) %>% 
      group_by(fundingagency, indicator, fiscal_year) %>% 
      summarise(across(c(targets, qtr1, qtr2, qtr3, qtr4), sum, na.rm = T)) %>% 
    pivot_longer(cols = qtr1:qtr4,
                 names_to = "qtr", 
                 values_to = "HTS_TST_POS") %>% 
    mutate(Achievement = HTS_TST_POS / targets,
           `Fiscal Year` = paste0(fiscal_year, " ", str_to_upper(qtr)),
           `Fiscal Year` = str_remove_all(`Fiscal Year`, "TR"),
           diff = targets - lag(targets)) %>% 
    rename(Target = targets) %>% 
    ungroup() %>% 
    select(`Fiscal Year`, Target, HTS_TST_POS, Achievement) %>% 
    filter(HTS_TST_POS !=0) %>% 
    mutate(year = str_extract(`Fiscal Year`, "^.{4}")) %>% 
    group_by(year) %>% 
    mutate(HTS_TST_POS = cumsum(HTS_TST_POS))
  
    tst %>% 
      mutate(label = if_else(str_detect(`Fiscal Year`, "Q1"), Achievement, NA_real_),
             bar_fill = if_else(str_detect(`Fiscal Year`, "Q1"), denim, "#9fbeed")) %>% 
      ggplot(aes(x = `Fiscal Year`)) +
        geom_col(aes(y = Target), fill = grey10k) +
        geom_errorbar(aes(ymin = Target, ymax = Target), color = grey50k) +
        geom_col(aes(y = HTS_TST_POS, fill = bar_fill)) +
        geom_text(aes(y = HTS_TST_POS, label = percent(label, 1)), vjust = 1.25, color = "white",
                  family = "Source Sans Pro") +
        geom_hline(yintercept = c(0.5e6, 1e6), size = 0.25, color = "white", linetype = "dashed") +
      scale_fill_identity()+
      si_style_xline() +
      labs(x = NULL, y = NULL)+
      scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                         lim = c(0, 1.5e6)) +
      coord_cartesian(expand = F)
    
    ggsave(here(images, "FY21Q1_testing_targets.png"),
           height = 4.62,
           width = 10)



# TB SLIDES ---------------------------------------------------------------

  tb_art <- read_excel(here(data, "% tb art coverag.xlsx"))

  
  # No Targets: show the number of countries which fall below the average.
  tb_art_bar <- 
    tb_art %>% 
    rename(ou = indicator) %>% 
    filter(ou != "Cote d'Ivoire") %>% 
    mutate(ou_order = fct_reorder(ou, TB_ART),
           TB_ART_diff = TB_ART - .90,
           TB_ART_pos = if_else(TB_ART > 0.9, TB_ART - .90, NA_real_),
           ou_color = if_else(TB_ART_diff < 0, old_rose, trolley_grey),
           ou_order_statpos = fct_reorder(ou, TB_STAT_POS)) 
  

  max_art <-  tb_art_bar %>% pull(TB_ART_diff) %>% abs() %>% max()
  
    tb_art_bar %>% 
    ggplot(aes(y = ou_order_statpos)) +
    geom_col(aes(x = TB_STAT_POS), fill = grey40k) +
    geom_vline(xintercept = c(1000, 2000, 4000, 6000),
               color = "white", linetype = "dotted", size = 0.5) +
      geom_text(aes(x = TB_STAT_POS, label = comma(TB_STAT_POS, 1)),
                hjust = -0.1,
                family = "Source Sans Pro",
                color = color_caption) +
    si_style_nolines() +
    coord_cartesian(expand = T, clip = "off") +
    labs(x = NULL, y = NULL) +
    geom_point(aes(x = -500, fill = TB_ART_diff), shape = 21, size = 11) +
    geom_text(aes(x = -500, label = percent(TB_ART, 1)), size = 3) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'BrBG'),
                         limits = c(-1 * max_art, max_art)) +
    theme(legend.position = "none",
          axis.text.x = element_blank())
    
    
    ggsave(here(graphs, "FY21Q1_TB_ART_remake.svg"),
           width = 5, 
           height = 5.625,
           dpi = "retina",
           scale = 1.15)
    
    

    # TB_stat now
    # No Targets: show the number of countries which fall below the average.
    tb_stat <- read_excel(here(data, "% tb stat covera.xlsx"))  
    
    tb_stat_bar <- 
      tb_stat %>% 
      rename(ou = Indicator) %>% 
      filter(ou != "Cote d'Ivoire") %>% 
      mutate(ou_order = fct_reorder(ou, TB_STAT),
             TB_STAT_diff = TB_STAT - .90,
             TB_STAT_pos = if_else(TB_STAT > 0.9, TB_STAT - .90, NA_real_),
             ou_color = if_else(TB_STAT_diff < 0, old_rose, trolley_grey),
             ou_order_statpos = fct_reorder(ou, `TB_STAT Den`)) 

    
   max_stat <-  tb_stat_bar %>% pull(TB_STAT_diff) %>% abs() %>% max()
    

    tb_stat_bar %>% 
      ggplot(aes(y = ou_order_statpos)) +
      geom_col(aes(x = `TB_STAT Den`), fill = grey40k) +
      geom_vline(xintercept = c(1000, 3000, 6000, 12000),
                 color = "white", linetype = "dotted", size = 0.5) +
      geom_text(aes(x = `TB_STAT Den`, label = comma(`TB_STAT Den`, 1)),
                hjust = -0.1,
                family = "Source Sans Pro",
                color = color_caption) +
      si_style_nolines() +
      coord_cartesian(expand = T, clip = "off") +
      labs(x = NULL, y = NULL) +
      geom_point(aes(x = -1000, fill = TB_STAT_diff), shape = 21, size = 11) +
      geom_text(aes(x = -1000, label = percent(TB_STAT, 1)), size = 3) +
      scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'BrBG'),
                           limits = c(-1 * max_stat, max_stat)) +
      theme(legend.position = "none",
            axis.text.x = element_blank())
    
    ggsave(here(graphs, "FY21Q1_TB_STAT_remake.svg"),
           width = 5, 
           height = 5.625,
           dpi = "retina", 
           scale = 1.15)
    

# BUDGET REMAKE -----------------------------------------------------------
 
    library(ggforce)

      budget %>% 
      mutate(value = value / 1e6,
             type_order = factor(type),
             type_order = fct_reorder(type_order, order, .desc = T),
             mark = 
             ) %>% 
      ggplot(aes(x = value, y = time, group = (type_order), fill = color)) +
      geom_col() +
        geom_vline(xintercept = c(500, 1000, 1500), color = "white", linetype = "dotted") +
        geom_vline(xintercept = c(1061), color = "black", linetype = "dotted") +
        geom_text(aes(label = percent(share)),
                  family = "Source Sans Pro") +
      scale_fill_identity() +
        
      scale_x_continuous(labels = unit_format(unit = "M"), position = "top") +
      si_style_nolines() +
      coord_cartesian(expand = F) +
      labs(x = NULL, y = NULL, title = "")
      
      ggsave(here(graph, "FY21Q1_budget_tbds_remake_part1.svg"),
             width = 10, 
             height = 2.625,
             dpi = "retina")
        

      bdg2 %>% 
        mutate(time = fct_reorder(time, order)) %>% 
        ggplot(aes(x = time, y = value)) +
        geom_col(fill = scooter_light) +
        geom_text(aes(label = value), vjust = 1.25,
                  family = "Source Sans Pro",
                  color = color_caption, 
                  size = 6) +
        si_style_xline() +
        coord_cartesian(expand = F) +
        theme(axis.text.y = element_blank() ) +
        labs(x = NULL, y = NULL)
      
      ggsave(here(graph, "FY21Q1_budget_tbds_remake_part2.svg"),
             width = 10, 
             height = 3.4,
             dpi = "retina")
        

# LOCAL PARTNERS CASCADE --------------------------------------------------

  lp <- read_excel(here(data, "FY21Q1_local_partners_cascade.xlsx"))      
      
  lp %>% 
    mutate(type_color = if_else(type == "Local", genoa, grey40k),
           indic_order = factor(indicator),
           indic_order = fct_relevel(indic_order,
                                "HTS_TST",
                                "HTS_TST_POS",
                                "TX_NEW",
                                "TX_CURR",
                                "PrEP_NEW",
                                "VMMC_CIRC")) %>% 
    ggplot(aes(x = type)) +
    geom_col(aes(y = `FY21 Targets`), fill = grey10k) +
    geom_errorbar(aes(ymin = `FY21 Targets`, ymax = `FY21 Targets`), color = grey50k) +
    geom_col(aes(y = FY21Q1, fill = type_color), width = .75) +
    geom_text(aes(y = FY21Q1,
                  label = percent(Achievement, 1)),
              family = "Source Sans Pro",
              vjust = 1.25,
              color = "white") +
    facet_wrap(~indic_order, scales = "free_y") +
    scale_fill_identity() +
    scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
    si_style_xline() +
    coord_cartesian(expand = F, clip = "off") +
    labs(x = NULL, y = NULL, title = "",
         caption = "Source: FY21Q1i MSD")
  
  ggsave(here(graph, "FY21Q1_local_partners_cascade.svg"),
         height = 4.5,
         width = 9, 
         dpi = "retina", 
         scale = 1.15)  
    
    
    