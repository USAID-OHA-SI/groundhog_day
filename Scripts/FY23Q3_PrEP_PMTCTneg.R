# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY23Q3 - PrEP Initations for PMTCT
# REF ID:   260f0996 
# LICENSE:  MIT
# DATE:     2023-09-05
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
get_metadata() 

ref_id <- "260f0996"

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% return_latest("MER_Structured_Datasets_OU_IM_FY21-24") %>% 
  read_psd() %>% 
  filter(operatingunit %in% c("Botswana", "Cote d'Ivoire", "Eswatini", "Kenya", "Lesotho",
                              "Malawi", "Mozambique", "Namibia", "Nigeria", "Rwanda",
                              "South Africa", "Tanzania", "Uganda", "Zambia", "Zimbabwe"))


# MUNGE -------------------------------------------------------------------

df %>% count(operatingunit)

#PMTCT_STAT_NEG (do not include post ANC1 data) - new negs or recent?
df_pmtct_neg <- df %>% 
  # clean_indicator() %>% 
  filter(fiscal_year %in% c(2022, 2023),
         indicator %in% c("PMTCT_STAT"),
         #sex == "Female",
         statushiv == "Negative",
         ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
         standardizeddisaggregate %in% c("Age/Sex/KnownNewResult")) %>% 
  #count(ageasentered)
  group_by(fiscal_year, funding_agency,indicator, statushiv) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  mutate(indicator = "PMTCT_STAT_Neg") %>% 
  select(-c(statushiv)) %>% 
  reshape_msd()

#PMTCT_STAT_POS NEWs only

df_pmtct_pos <- df %>% 
  # clean_indicator() %>% 
  filter(fiscal_year %in% c(2022, 2023),
         indicator %in% c("PMTCT_STAT"),
         #sex == "Female",
         statushiv == "Positive",
         otherdisaggregate == "Newly Identified",
         ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
         standardizeddisaggregate %in% c("Age/Sex/KnownNewResult")) %>% 
  #count(ageasentered)
  group_by(fiscal_year, funding_agency,indicator, otherdisaggregate, statushiv) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  mutate(indicator = "PMTCT_STAT New Positive") %>% 
  select(-c(statushiv, otherdisaggregate)) %>% 
  reshape_msd()


# # POST ANC1 HTS DATA - update to not use
# df_hts <- df %>% 
#   # resolve_knownissues() %>% 
#   # clean_indicator() %>% 
#   filter(fiscal_year %in% c(2022, 2023),
#          indicator %in% c("HTS_TST"),
#          ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
#          modality == "Post ANC1",
#          statushiv == "Negative",
#          standardizeddisaggregate %in% c("Modality/Age/Sex/Result")) %>% 
#   group_by(fiscal_year, funding_agency, indicator, statushiv, modality) %>% 
#   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
#   select(-c(statushiv, modality)) %>% 
#   reshape_msd()

#PrEP_NEW for women aged 15-49
df_prep <- df %>% 
  #resolve_knownissues() %>% 
  # clean_indicator() %>% 
  filter(fiscal_year %in% c(2022, 2023),
         indicator %in% c("PrEP_NEW"),
         sex == "Female",
         ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
         #  modality == "Post ANC1",
         standardizeddisaggregate %in% c("Age/Sex")) %>% 
  group_by(fiscal_year, funding_agency, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd()


# df_prep_ct <- df %>% 
#   #resolve_knownissues() %>% 
#   # clean_indicator() %>% 
#   filter(fiscal_year %in% c(2022, 2023),
#          indicator %in% c("PrEP_CT"),
#          sex == "Female",
#          #ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
#          #  modality == "Post ANC1",
#          standardizeddisaggregate %in% c("Sex/PregnantBreastfeeding")) %>% 
#   group_by(fiscal_year, funding_agency, indicator) %>% 
#   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
#   reshape_msd()

df_viz <- df_prep %>% 
  bind_rows(df_pmtct_pos) %>% 
  bind_rows(df_pmtct_neg) %>% 
  filter(funding_agency == "USAID")


# VIZ ---------------------------------------------------

#overall viz for USAID

nudge_space <- 0.125

df_viz %>% 
  filter(funding_agency == "USAID") %>%
  ggplot(aes(x = period, y = value,  fill = indicator)) +
  geom_col(data = df_viz %>% filter(indicator %in% c("PMTCT_STAT New Positive", "PMTCT_STAT_Neg")), width = 0.75) +
  geom_col(data = df_viz %>% filter(indicator == "PrEP_NEW"),
           aes(y = value),fill = golden_sand, alpha = 1, position = position_nudge(x = nudge_space), width = 0.75) +
  #geom_col(data = df_viz %>% filter(indicator == "PrEP_CT"),
  #   aes(y = value),fill = burnt_sienna, alpha = 1, position = position_nudge(x = nudge_space*2), width = 0.75) +
  geom_text(data = df_viz %>% filter(indicator %ni% c("PrEP_NEW"), funding_agency == "USAID"),aes(y = value,
                                                                                                  label = clean_number(value)), size = 14/.pt, hjust = 0,
            position = "stack",
            color = "white",
            family = "Source Sans Pro Light") +
  # facet_wrap(~indicator, scales = "free_y") +
  geom_text(data = df_viz %>% filter(indicator == "PrEP_NEW", funding_agency == "USAID"),aes(y = value,
                                                                                             label = clean_number(value)), size = 14/.pt, hjust = 0,
            position = position_nudge(x = nudge_space),
            family = "Source Sans Pro Light") +
  scale_fill_manual(values = c("PMTCT_STAT New Positive" = scooter_light, "PMTCT_STAT_Neg" = scooter)) +
  si_style_ygrid() +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(title = "USAID PrEP and PVT programs show many missed opportunities for offering PrEP to women accessing ANC services" %>% toupper(),
       subtitle = "USAID results only, women aged 15-49",
       caption = glue("{metadata$caption}")) +
  theme(axis.title = element_blank(),
        legend.position = "none")

si_save("Graphics/FY23Q3_PMTCT_PREP_USAID.svg")  


# SNU Analysis --------------------------------------------

df_pmtct <- df %>% 
  # clean_indicator() %>% 
  filter(fiscal_year %in% c(2023),
         indicator %in% c("PMTCT_STAT"),
         #sex == "Female",
         statushiv == "Negative",
         ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
         standardizeddisaggregate %in% c("Age/Sex/KnownNewResult")) %>% 
  #count(ageasentered)
  group_by(operatingunit, fiscal_year, funding_agency, snu1, indicator, statushiv, otherdisaggregate) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  select(-c(statushiv, otherdisaggregate)) %>% 
  reshape_msd()



df_hts <- df %>% 
  # resolve_knownissues() %>% 
  # clean_indicator() %>% 
  filter(fiscal_year %in% c(2023),
         indicator %in% c("HTS_TST"),
         ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
         modality == "Post ANC1",
         statushiv == "Negative",
         standardizeddisaggregate %in% c("Modality/Age/Sex/Result")) %>% 
  group_by(operatingunit, fiscal_year, funding_agency, snu1, indicator, statushiv, modality) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  select(-c(statushiv, modality)) %>% 
  reshape_msd()


df_prep <- df %>% 
  #resolve_knownissues() %>% 
  # clean_indicator() %>% 
  filter(fiscal_year %in% c(2023),
         indicator %in% c("PrEP_NEW"),
         sex == "Female",
         ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
         #  modality == "Post ANC1",
         standardizeddisaggregate %in% c("Age/Sex")) %>% 
  group_by(operatingunit, fiscal_year, funding_agency, snu1, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd()

df_viz <- df_hts %>% 
  bind_rows(df_prep) %>% 
  bind_rows(df_pmtct) 

df_viz %>% 
  filter(funding_agency == "USAID") %>% 
  mutate(fill_color = case_when(indicator == "PrEP_NEW" ~ burnt_sienna,
                                indicator == "HTS_TST" ~ denim,
                                indicator == "PMTCT_STAT" ~ scooter)) %>%
  ggplot(aes(x = period, y = value,  fill = indicator)) +
  geom_col(data = df_viz %>% filter(indicator != "PrEP_NEW"), position = "stack", width = 0.75) +
  geom_col(data = df_viz %>% filter(indicator == "PrEP_NEW"),
           aes(y = value),fill = golden_sand, alpha = 1, position = position_nudge(x = nudge_space), width = 0.75) +
  geom_text(data = df_viz %>% filter(indicator != "PrEP_NEW", funding_agency == "USAID"),aes(y = value,
                                                                                             label = clean_number(value)), size = 12/.pt, hjust = 0,
            position = "stack",
            color = "white",
            family = "Source Sans Pro Light") +
  geom_text(data = df_viz %>% filter(indicator == "PrEP_NEW", funding_agency == "USAID"),aes(y = value,
                                                                                             label = clean_number(value)), size = 12/.pt, hjust = 0,
            position = position_nudge(x = nudge_space),
            family = "Source Sans Pro Light") +
  facet_wrap(~forcats::fct_reorder(snu1, value, .desc = TRUE), scales = "free_y") +
  #facet_wrap(~snu1) +
  scale_fill_manual(values = c("HTS_TST" = denim, "PMTCT_STAT" = scooter)) +
  si_style_ygrid() +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Number of women with known negative status at ANC1 + number of women who test negative at Post ANC1 compared to PrEP_NEW for women" %>% toupper(),
       subtitle = "USAID results only by SNU, women aged 15-49",
       caption = glue("{metadata$caption}")) +
  theme(axis.title = element_blank(),
        legend.position = "none")

si_save("Graphics/SA_PMTCT_PREP_SNU.svg")  


## WITH PREP CT ------------------------------------------------------

df_prep_viz <-df_prep_ct %>% 
  filter(funding_agency == "USAID") 

df_prep_viz %>% 
  filter(funding_agency == "USAID") %>%
  ggplot(aes(x = period, y = value,  fill = indicator)) +
  # geom_col(data = df_viz %>% filter(indicator %in% c("PMTCT_STAT", "HTS_TST")), width = 0.75) +
  # geom_col(data = df_viz %>% filter(indicator == "PrEP_NEW"),
  #          aes(y = value),fill = golden_sand, alpha = 1, position = position_nudge(x = nudge_space), width = 0.75) +
  geom_col(data = df_viz %>% filter(indicator == "PrEP_CT"),
           aes(y = value),fill = burnt_sienna, alpha = 1, position = position_nudge(x = nudge_space*2), width = 0.75) +
  geom_text(aes(y = value,
                label = clean_number(value, 1)), size = 14/.pt, hjust = 0,
            position = "stack",
            #  color = "white",
            family = "Source Sans Pro Light") +
  # facet_wrap(~indicator, scales = "free_y") +
  # geom_text(data = df_viz %>% filter(indicator == "PrEP_NEW", funding_agency == "USAID"),aes(y = value,
  #                                                                                           label = clean_number(value)), size = 14/.pt, hjust = 0,
  #         position = position_nudge(x = nudge_space),
  #       family = "Source Sans Pro Light") +
  scale_fill_manual(values = c("HTS_TST" = denim, "PMTCT_STAT" = scooter)) +
  si_style_ygrid() +
  #coord_flip()+
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Number of pregnant or breastfeeding people that return for a follow-up to receive PrEP" %>% toupper(),
       subtitle = "PrEP_CT USAID results only",
       caption = glue("{metadata$caption}")) +
  theme(axis.title = element_blank(),
        legend.position = "none")

si_save("Graphics/USAID_PrEP_Pregnant.svg")


