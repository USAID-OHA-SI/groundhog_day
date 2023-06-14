# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
# REF ID:   c22a90c5 
# LICENSE:  MIT
# DATE:     2023-06-14
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

# data_folder
data_folder <- "Data/"

filepath <- data_folder %>% return_latest("recency_targets_dashboard_data")

ref_id <- "c22a90c5"

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

# IMPORT ------------------------------------------------------------------

df <- read_csv(filepath)

# MUNGE -------------------------------------------------------------------

df_viz <- df %>% 
  #filter(fiscal_year != "FY20") %>% 
  mutate(fill_color = case_when(funding_agency == "USAID" ~ denim,
                                funding_agency == "CDC" ~ scooter_light,
                                funding_agency == "DOD" ~ genoa_light)) 


df_viz %>% 
  ggplot(aes(fiscal_year, targets, group = funding_agency,
             color = fill_color, fill = fill_color)) +
  geom_blank(aes(y = 1.1 * targets)) +
  geom_line(size = 1.5) +
  geom_point(shape = 21, size = 10, stroke = 2) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_text(aes(label = df_viz$funding_agency,
                family = "Source Sans Pro",
                size = 10/.pt) +
              # facet_wrap(~funding_agency, nrow = 3) +
              #si_style_nogrid() +
              scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
              geom_text(aes(label = clean_number(targets)), color = "white",
                        family = "Source Sans Pro",
                        size = 10/.pt) +
              expand_limits(y = .2) +
              si_style_nolines() +
              labs(x = NULL, y = NULL) +
              theme(axis.text.y = element_blank())