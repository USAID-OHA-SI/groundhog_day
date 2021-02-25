## PROJECT: FY21Q1 REVIEW
## AUTHOR:  A.Chafetz | USAID
## PURPOSE: 95-95-95 Achievement
## LICENSE: MIT
## DATE:    2021-02-23
## UPDATED: 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(googlesheets4)
library(extrafont)
library(scales)
library(tidytext)
library(svglite)


# GLOBAL VARIABLES --------------------------------------------------------

  #creds  
    load_secrets()

  #unaids data (saved for Mind the Gap)
    gs_id <- as_sheets_id("1Ui1r5ynn9xYky86hHMO9kmdNZdWmANI5nAQ1N2b0wdM")


# IMPORT ------------------------------------------------------------------

  df_unaids <- read_sheet(as_sheets_id(gs_id), "UNAIDS") %>%
    dplyr::mutate(year = as.integer(year))
  
  df_impatt <- read_sheet(as_sheets_id(gs_id), "ARTshare")

  pepfar_cntry <- get_outable(datim_user(), datim_pwd()) %>% 
    filter(str_detect(operatingunit, "Region", negate = TRUE)) %>% 
    pull(countryname)


# MUNGE -------------------------------------------------------------------

  
  df_impatt <- df_impatt %>% 
    rename(country = countryname) %>% 
    filter(country %in% pepfar_cntry) %>% 
    group_by(country) %>% 
    summarise(PLHIV = sum(PLHIV, na.rm = TRUE)) %>% 
    ungroup()
    
  df_unaids <- df_unaids %>% 
    filter(year == max(year),
           sex == "All",
           country %in% pepfar_cntry)

  df_viz <- df_unaids %>% 
    left_join(df_impatt) %>% 
    mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "DRC",
                               country == "Dominican Republic" ~ "DR", 
                               TRUE ~ country),
           PLHIV = ifelse(is.na(PLHIV), 0, PLHIV))
  
  df_viz <- df_viz %>% 
    group_by(country) %>% 
    mutate(grouping = case_when(value == min(value, na.rm = TRUE) ~ indicator),
           grouping = case_when(country == "Eswatini" ~ "Z_Achieved",
                                country == "Zambia" & indicator == "Virally Suppressed" ~ NA_character_,
                                TRUE ~ grouping),
           gap = case_when(value == min(value, na.rm = TRUE) & value <.95 ~ .95-value,
                           TRUE ~ 0),
           dot_color = case_when(grouping == "Known Status" ~ old_rose,
                                 grouping == "On ART" ~ golden_sand,
                                 grouping == "Virally Suppressed" ~ scooter,
                                 TRUE ~ trolley_grey)) %>% 
    fill(grouping, .direction = "downup") %>% 
    ungroup() %>% 
    mutate(gap_bar = case_when(value < .95 ~ value))


# PLOT --------------------------------------------------------------------


df_viz %>% 
  ggplot(aes(value, reorder_within(country, gap, grouping, max, na.rm = TRUE), color = dot_color)) +
  geom_vline(xintercept = .95, linetype = "dashed") + 
  geom_linerange(aes(xmin = gap_bar, xmax = .95), color = "gray90",
                 size = 2.5) +
  geom_point(size = 4, alpha = .8) +
  scale_y_reordered() +
  scale_x_continuous(label = percent) +
  scale_color_identity() +
  facet_grid(grouping~indicator, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL, color = NULL,
       caption = "Source: UNAIDS 90-90-90 15+ (2019)") +
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))
  
  si_save("Images/UNAIDS_Epi_Progress.svg")
