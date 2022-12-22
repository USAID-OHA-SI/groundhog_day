# AUTHOR:   K. Srikanth, J.Hoehner | USAID
# PURPOSE:  VLS by priority population
# REF ID:   0df6589b 
# LICENSE:  MIT
# DATE:     2022-12-22

# DEPENDENCIES ------------------------------------------------------------
  
  library(gagglr)
  library(tidyverse)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  library(cascade)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "OU_IM_FY20-23_20221114")
      
  # Grab metadata
   get_metadata(file_path)
  
    ref_id <- "0df6589b"

# IMPORT ------------------------------------------------------------------
  
  df <- read_msd(file_path)

# GLOBAL FUNCTIONS ---------------------------------------------------------
  
  #group and sum function
  sum_reshape <- function(.data, ...) {
    .data %>%
      gophr::clean_indicator() %>%
      dplyr::group_by(indicator, fiscal_year, ...) %>%
      dplyr::summarise(dplyr::across(tidyselect::matches("qtr"), sum, na.rm = T)) %>%
      gophr::reshape_msd() %>%
      dplyr::ungroup() %>% 
      dplyr::select(-period_type)
  }
  
  #mutate AYP groups to trendscoarse
  fltr_ayp <- function(.data) {
    .data %>%
      dplyr::mutate(trendscoarse = ifelse(ageasentered %in% c("15-19", "20-24"), "AYP", "Non AYP"))
  }
  
  #pivot and mutate VLS calcs
  get_vls <- function(.data, cat) {
  
    df <- .data %>% 
      tidyr::pivot_wider(names_from = indicator, values_from = value) %>% 
      dplyr::group_by(funding_agency, trendscoarse) %>% 
      dplyr::mutate(TX_CURR_LAG2 = lag(TX_CURR, 2, order_by = period),
                    VLC = TX_PVLS_D / TX_CURR_LAG2,
                    VLS = TX_PVLS/TX_PVLS_D) %>% 
      dplyr::ungroup() %>% 
      dplyr::relocate(TX_CURR_LAG2, .before = TX_CURR) %>% 
      dplyr::filter(!is.na(TX_PVLS_D), TX_PVLS_D > 0) 
    
   if (cat == "sex") {
    df_fin <- df %>% 
       mutate(group = sex) %>% 
       select(-c(trendscoarse, sex))
     
   } else {
     
    df_fin <- df %>% 
       mutate(group = trendscoarse) %>% 
       select(-c(trendscoarse))
   }
    
    return(df_fin)

  }

  # MUNGE -------------------------------------------------------------------
  
  # filter out UKR
  # Peds, AYP, Adult F, Adult M, PBFW
  
  df_peds <- df %>% 
   # clean_indicator() %>% 
    filter(fiscal_year %in% c(2021, 2022),
           funding_agency == "USAID",
           indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
           operatingunit != "Ukraine",
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                           "Age/Sex/Indication/HIVStatus")) %>% 
    sum_reshape(funding_agency, trendscoarse) %>% 
    filter(trendscoarse == "<15") %>% 
    get_vls(cat = "peds")
    
  df_sex <- df %>% 
    # clean_indicator() %>% 
    filter(fiscal_year %in% c(2021, 2022),
           funding_agency == "USAID",
           indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
           operatingunit != "Ukraine",
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                           "Age/Sex/Indication/HIVStatus")) %>% 
    sum_reshape(funding_agency, sex, trendscoarse) %>% 
    filter(trendscoarse == "15+") %>% 
    get_vls(cat = "sex") %>% 
    mutate(group = recode(group,
                          "Female" = "Adult Female",
                          "Male" = "Adult Male"))
  
df_ayp <- df %>% 
    # clean_indicator() %>% 
    filter(fiscal_year %in% c(2021, 2022),
           funding_agency == "USAID",
           indicator %in% c("TX_CURR", "TX_PVLS_D", "TX_PVLS"),
           operatingunit != "Ukraine",
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                           "Age/Sex/Indication/HIVStatus")) %>%
  fltr_ayp() %>%
    sum_reshape(funding_agency, trendscoarse) %>% 
  filter(trendscoarse == "AYP") %>% 
  get_vls(cat = "ayp")


df_viz <- bind_rows(df_peds, df_sex, df_ayp)

# PBFW 
# code from rebooTZ/R/FY22Q3_TZA_VL_PregnantWomen.R
# need to check if this is correct

df_vl_pmtct_viz <- df %>% 
  clean_indicator() %>% 
  filter(
    fiscal_year == 2022,
    funding_agency == "USAID",
    operatingunit != "Ukraine",
    (indicator == "PMTCT_ART" & numeratordenom == "N" & 
    otherdisaggregate == "Life-long ART, Already") | 
    (indicator == "TX_PVLS" & numeratordenom == "D" & 
    standardizeddisaggregate == "PregnantOrBreastfeeding/Indication/HIVStatus" & 
    otherdisaggregate %in% c("Pregnant, Routine", "Pregnant, Targeted"))) %>% 
  group_by(fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(names_from = indicator,
              names_glue = "{tolower(indicator)}") %>%
  mutate(pmtct_art_lag4 = lag(pmtct_art, 4, na.rm = TRUE), 
         vlc = tx_pvls_d/pmtct_art_lag4)

# VIZ -----------------------------------------------------------------------

# VLS

df_viz %>% 
  filter(period %in% c("FY21Q4", "FY22Q1", "FY22Q2", "FY22Q3", "FY22Q4")) %>%
  mutate(endpoints = case_when(period %in% c(max(period), min(period))~VLS)) %>% 
  ggplot(aes(period, VLS, group = group, color = scooter_med, fill = scooter_light))+
  geom_area(alpha = .4, linewidth = .9, position = "identity") +
  geom_hline(yintercept = .9, color = scooter, linetype = "dashed") +
  geom_area(aes(y = VLS), fill = scooter_light, color = scooter_med, alpha = .4) +
  facet_wrap(~fct_reorder2(group, period, VLS, .desc = TRUE)) +
  geom_point(aes(y = endpoints), na.rm = TRUE) +
  scale_fill_identity() +
  scale_y_continuous(label = percent, 
                     breaks = seq(0, 1, .3)) +
  scale_x_discrete(breaks = unique(df_viz$period)[grep("FY2(1|2)Q(2|4)", unique(df_viz$period))]) +
  scale_color_identity() +
  si_style_ygrid() +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL,
       title = glue("VLS remains strong, but AYP and children lag behind adults" %>% toupper()),
       #subtitle = "CHLIV from <1 to 19 years of age",
       caption = glue("VLS = TX_PVLS/TX_PVLS_D
                       Adult = ages 15+, AYP = Adolescent and Young People ages 15-24
                       {metadata$caption}| USAID SI Analytics: Karishma Srikanth/Jessica Hoehner"))

si_save(paste0(metadata$curr_pd, "_Q4Review_VLS_Age_Sex.png"),
        path = "Images", 
        width = 9.32, 
        height = 5)

# VLC

df_viz %>% 
  filter(period %in% c("FY21Q4", "FY22Q1", "FY22Q2", "FY22Q3", "FY22Q4")) %>%
  mutate(endpoints = case_when(period %in% c(max(period), min(period))~VLC)) %>% 
  ggplot(aes(period, VLC, group = group, color = burnt_sienna_light, fill = burnt_sienna_light))+
  geom_area(alpha = .4, linewidth = .9, position = "identity") +
  geom_hline(yintercept = .9, color = burnt_sienna, linetype = "dashed") +
  geom_area(aes(y = VLC), fill = burnt_sienna_light, color = burnt_sienna_light, alpha = .4) +
  facet_wrap(~factor(group, levels = c("Adult Female", "Adult Male", "AYP", "<15"))) +
  geom_point(aes(y = endpoints), na.rm = TRUE) +
  scale_fill_identity() +
  scale_y_continuous(label = percent, 
                     breaks = seq(0, 1, .3)) +
  scale_x_discrete(breaks = unique(df_viz$period)[grep("FY2(1|2)Q(2|4)", unique(df_viz$period))]) +
  scale_color_identity() +
  si_style_ygrid() +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL,
       title = glue("While all groups saw dips in Q2 VLC, they ended trending upwards" %>% toupper()),
       #subtitle = "CHLIV from <1 to 19 years of age",
       caption = glue("VLC = TX_PVLC_D / TX_CURR_LAG2
                       Adult = ages 15+, AYP = Adolescent and Young People ages 15-24
                       {metadata$caption}| USAID SI Analytics: Karishma Srikanth/Jessica Hoehner "))

si_save(paste0(metadata$curr_pd, "_Q4Review_VLC_Age_Sex.png"),
        path = "Images",
        width = 9.32, 
        height = 5)
