# AUTHOR:   K. Srikanth | USAID
# Modifier: B. Betz | USAID
# PURPOSE:  remake PMTCT / PostANC1 Viz, rows by country
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

  df_viz <- df_msd %>% 
    # clean_indicator() %>% 
    filter(country %in% c("Kenya", "Malawi", "Tanzania"),
           indicator == "HTS_TST",
           standardizeddisaggregate == "Modality/Age/Sex/Result",
           modality == "Post ANC1",
           funding_agency == "USAID",
           fiscal_year %in% c(2022, 2023)) %>% 
    group_by(modality, funding_agency, fiscal_year, country) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = T), .groups = "drop") %>% 
    reshape_msd() %>% 
    rename(indicator = modality) %>% 
    bind_rows(pmtct) %>% 
    pivot_wider(names_from = "indicator") %>% 
    mutate(total_plp_test = `Post ANC1` + PMTCT_STAT,
           share_post_anc = `Post ANC1` / total_plp_test) %>% 
   # pivot_longer(cols = c(4:6), names_to = "indicator") %>% 
    select(-c(period_type)) |> 
    mutate(country = fct_relevel(country, "Malawi", after = Inf))
  
  
  # VIZ -------------------------------------------------------------------
  
  v2 <- df_viz %>% 
    ggplot(aes(period)) +
    geom_col(aes(y = total_plp_test), fill = burnt_sienna_light, position = "identity", na.rm = TRUE) +
    geom_col(aes(y = PMTCT_STAT), fill = trolley_grey_light, position = "identity", na.rm = TRUE) +
    facet_grid(rows=vars(country), switch = "y", scales = "free_y") +
    scale_y_continuous(label = label_number(scale = 1e-3, suffix = "k"),
                       # limits = range(df_viz$total_plp_test),
                       expand = c(0, 50000),
                       # n.breaks = 3,
                       breaks = c(0, 100000, 200000, 300000),
                       minor_breaks = waiver()
                       ) +
    si_style_ygrid() +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle=0, face = "bold", vjust = 0.6, hjust = 0, size=14),
          # strip.background.y = element_rect(fill=NA, color = grey10k)
          # panel.grid.minor.y = element_line(colour = grey10k)
          ) +
    geom_text(aes(y = PMTCT_STAT,
                  label = clean_number(PMTCT_STAT)), color = grey60k,
              family = "Source Sans Pro",
              vjust = 1.2,
              size = 10/.pt) +
    geom_text(aes(y = total_plp_test,
                  label = clean_number(`Post ANC1`)), color = burnt_sienna,
              family = "Source Sans Pro",
              fontface = "bold",
              vjust = -0.2,
              size = 12/.pt) +
    labs(x = NULL,
         y = NULL,
         # subtitle = "ANC1 Testing for PLPs"
         )
  
  v2
  
  
  v2  +
    plot_annotation(
      title = "Testing for PLPs is increasing after ANC1, especially in the 3 countries below" %>% toupper(),
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

  si_save("Images/OHA Q3/FY23Q3_PostANC_Remake.png", width = 8.5, height = 5.5)  
  