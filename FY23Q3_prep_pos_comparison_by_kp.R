# AUTHOR:   B. Betz | USAID
# PURPOSE:  Visualize proportions of KP for PoS and PrEP for FY23Q3 quarterly review
# REF ID:   e5c7ba95 
# LICENSE:  MIT
# DATE:     2023-09-26
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

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "e5c7ba95"

# IMPORT ------------------------------------------------------------------
  
  filepath <- si_path() %>% return_latest("OU")
  msd <- read_msd(filepath) 
  

# MUNGE -------------------------------------------------------------------
msd_viz <- msd |> filter(funding_agency=="USAID",
              indicator %in% c("HTS_TST_POS", "PrEP_NEW", "PrEP_CT"),
              str_detect(tolower(standardizeddisaggregate), "keypop"),
              fiscal_year %in% c(2022, 2023),
              country!="Uganda") |> 
    reshape_msd() |> 
    filter(period_type == "results",
           !period %in% c("FY22Q1","FY22Q2", "FY23Q4")) |> 
    mutate(indicator = recode(indicator, 
                                "PrEP_CT" = "PrEP_NEW + PrEP_CT",
                                "PrEP_NEW" = "PrEP_NEW + PrEP_CT")) |> 
    rename(population = otherdisaggregate) |> 
    group_by(period, population, indicator) |> 
    summarise(results=sum(value), .groups = "drop") |> 
    group_by(period, indicator) |> 
    mutate(total = sum(results)) |>
    ungroup() |> 
    mutate(results_percent = results/total) |> 
    filter(population %in% c("PWID")) |> 
    glimpse()

  # Viz   -------------------------------------------------------------------
  v <- msd_viz |> ggplot(aes(y=results_percent, x=period, fill = population)) +
    geom_col() + facet_grid(cols = vars(indicator)) + 
    geom_text(aes(label=percent(results_percent, accuracy = 1), vjust=-1, fontface="bold")) +
    # geom_text(aes(label=clean_number(results, digits = 1)), vjust=1.2) +
    si_style_ygrid() +
    theme(
          # strip.background = element_rect(fill = grey10k, ), 
          strip.text.x.top = element_text(hjust=0.5, size = 12, face = "bold"),
          legend.position =  "none",
          axis.title = element_blank()) +
    scale_y_continuous(labels = scales::percent_format(scale=100), limits = c(0, 0.125), breaks = c(0.03, 0.06, 0.09, 0.12))

  
  v  +
    plot_annotation(
      title = "Proportion of HTS_TST_POS (left) and PrEP_NEW + PrEP_CT (right) among PWID" %>% toupper(),
      caption = glue("{metadata$caption}")) &
    theme(plot.title = element_text(family = "Source Sans Pro",
                                    size = 16,
                                    face = "bold",
                                    color =  "#202020",
                                    hjust = 0),
          plot.caption = element_text(family = "Source Sans Pro",
                                      size = 9,
                                      color = "#909090",
                                      hjust = 1, vjust = 1)) 
  
  si_save("Images/OHA Q3/FY23Q3_prep_pos_Remake.png", width = 9, height = 5.5)  
  
  