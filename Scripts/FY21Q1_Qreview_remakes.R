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
  tb_stat <- read_excel(here(data, "% tb stat covera.xlsx"))  
  
    
    