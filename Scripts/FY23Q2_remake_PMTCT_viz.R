# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  remake PMTCT / PostANC1 Viz
# REF ID:   b9553d92 
# LICENSE:  MIT
# DATE:     2023-06-16
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()
  
  ref_id <- "b9553d92"

  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
# IMPORT ------------------------------------------------------------------
  
  filepath <- si_path() %>% return_latest("OU_IM_FY21")
  
  # Grab metadata
  get_metadata(filepath) 
  
  df_msd <- read_msd(filepath)
  

# MUNGE -------------------------------------------------------------------
  
 pmtct<- df_msd %>% 
   # clean_indicator() %>% 
    filter(indicator == "PMTCT_STAT",
           standardizeddisaggregate == "Age/Sex/KnownNewResult",
           otherdisaggregate == "Newly Identified",
           funding_agency == "USAID",
           fiscal_year %in% c(2022, 2023)) %>% 
   group_by(indicator, funding_agency, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd()
  
  
  
  df_viz <- df_msd %>% 
    # clean_indicator() %>% 
    filter(indicator == "HTS_TST",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           modality == "Post ANC1",
           funding_agency == "USAID",
           fiscal_year %in% c(2022, 2023)) %>% 
    group_by(modality, funding_agency, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    rename(indicator = modality) %>% 
    bind_rows(pmtct) %>% 
    pivot_wider(names_from = "indicator") %>% 
    mutate(total_plp_test = `Post ANC1` + PMTCT_STAT,
           share_post_anc = `Post ANC1` / total_plp_test) %>% 
   # pivot_longer(cols = c(4:6), names_to = "indicator") %>% 
    select(-c(period_type)) 
  
  
  # VIZ -------------------------------------------------------------------
  
  v2 <- df_viz %>% 
    ggplot(aes(period)) +
    geom_col(aes(y = total_plp_test), fill = trolley_grey_light, position = "identity", na.rm = TRUE) +
    geom_col(aes(y = PMTCT_STAT), fill = burnt_sienna, position = "identity", na.rm = TRUE) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    si_style_ygrid() +
    geom_text(aes(y = PMTCT_STAT,
                  label = clean_number(PMTCT_STAT)), color = burnt_sienna,
              family = "Source Sans Pro",
              vjust = -1,
              size = 10/.pt) +
    labs(x = NULL,
         y = NULL,
         subtitle = "ANC1 Testing for PLPs")
  
  v1 <- df_viz %>% 
    ggplot(aes(period)) +
    geom_col(aes(y = total_plp_test), fill = trolley_grey_light, position = "identity", na.rm = TRUE) +
    geom_col(aes(y = `Post ANC1`), fill = burnt_sienna_light, position = "identity", na.rm = TRUE) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    si_style_ygrid() +
    geom_text(aes(y = `Post ANC1`,
                  label = clean_number(`Post ANC1`)),
              family = "Source Sans Pro",
              vjust = -1,
              size = 10/.pt) +
    geom_text(aes(y = total_plp_test,
                  label = clean_number(total_plp_test,1)),
              family = "Source Sans Pro",
              color = trolley_grey,
              vjust = -1,
              size = 10/.pt) +
    labs(x = NULL,
         y = NULL,
         subtitle = "Post ANC1 Testing among PLPs")

  v1 / v2  +
    plot_annotation(
      title = "Testing for PLPs is increasing at and after ANC1, with close to 30% of all testing happening after ANC1" %>% toupper(),
      caption = glue("{metadata$caption}")) &
        theme(plot.title = element_text(family = "Source Sans Pro",
                                        size = 14,
                                        face = "bold",
                                        color =  "#202020",
                                        hjust = 0),
              plot.caption = element_text(family = "Source Sans Pro",
                                          size = 9,
                                          color = "#909090",
                                          hjust = 1, vjust = 1)) 

  si_save("Graphics/FY23Q2_PostANC_Remake.svg")  
  