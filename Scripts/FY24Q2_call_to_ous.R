# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  create visuals for Q2 review for FY24Q2 Call to OUs
# REF ID:   7e3ec543 
# LICENSE:  MIT
# DATE:     2024-05-24
# UPDATED:  2024-06-03

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

  filepath <- si_path() %>% return_latest("MER_Structured_Datasets_OU_IM_FY22-24_20240517_v1_1")
  filepath_subnat <- si_path() %>% return_latest("MER_Structured_Datasets_NAT_SUBNAT_FY22-24_20240315_v2_1")
 
   arch_filepath <- si_path() %>% 
    return_latest("MER_Structured_Datasets_OU_IM_FY15")

  # Grab metadata
    metadata <- get_metadata(filepath) 
  
  ref_id <- "7e3ec543"

# IMPORT ------------------------------------------------------------------
  
  #import MSDS
  df_msd <- read_psd(filepath)
  df_subnat <- read_psd(filepath_subnat)
  
  #historic msd
  df_arch <- read_psd(arch_filepath)
  
  #rounding function
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
  #improt uNAIDS
  df_est <- mindthegap::pull_estimates(TRUE)
  df_tt <- mindthegap::pull_testtreat(TRUE)
  

# NET NEW VIZ -------------------------------------------------------------------
  
df_nn  <- df_msd %>% 
    resolve_knownissues() %>% 
    filter(fiscal_year >= 2023,
           indicatortype != "CS",
           operatingunit %ni% c("Ukraine", "Nigeria"),
           indicator  %in% c("TX_NET_NEW"),
           standardizeddisaggregate %in% c("Total Numerator"),
           funding_agency == "USAID") %>% 
    group_by(fiscal_year, indicator,funding_agency) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd()
  
  df_nn %>% 
    ggplot(aes(x = period, y = value)) +
    geom_col(fill = hw_hunter) +
    si_style_ygrid() +
    scale_y_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
    geom_text(aes(y = value, label = clean_number(value)),
              vjust = -0.5,
              size = 4.5,
              # color = "black",
              family = "Source Sans Pro") +
    labs(x = NULL, y = NULL,
         title = "USAID lost a sizeable number of PLHIV on treatment in Q1 of this year, but almost regained the
         same volume of patients in Q2" %>% toupper(),
         subtitle = "Note - Nigeria excluded due to data quality issues in FY23Q4",
         caption = glue::glue("{metadata$caption}
                              Note: Ukraine and Nigeria excluded"))
  
  si_save("Images/FY24Q2_CTOU_NN.png")
  
#Priority Pop Target Achv -------------------------------------------------  
 
   #adolescent and youth
  df_ayp <- df_msd %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST","HTS_TST_POS"),
           fiscal_year >= 2022,
           use_for_age == "Y",
           indicatortype != "CS",
           operatingunit != "Ukraine",
           target_age_2024 == "15-24",
           funding_agency == "USAID") %>% 
    group_by(fiscal_year, indicator, target_age_2024) %>%
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    # reshape_msd(include_type = FALSE, qtrs_keep_cumulative = TRUE) %>% 
    mutate(achv = cumulative/targets,
           pop = "AYP (15-24)") %>% 
    select(-target_age_2024)
  
  #peds
  df_peds <- df_msd %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST","HTS_TST_POS"),
           fiscal_year >= 2022,
           use_for_age == "Y",
           indicatortype != "CS",
           operatingunit != "Ukraine",
           trendscoarse == "<15",
           funding_agency == "USAID") %>% 
    group_by(fiscal_year, indicator, trendscoarse) %>%
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    # reshape_msd(include_type = FALSE, qtrs_keep_cumulative = TRUE) %>% 
    mutate(achv = cumulative/targets,
           pop = "Peds (<15)") %>% 
    select(-trendscoarse)
  
  #men
  df_men <- df_msd %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST", "HTS_TST_POS"),
           fiscal_year >= 2023,
           use_for_age == "Y",
           indicatortype != "CS",
           operatingunit != "Ukraine",
           sex == "Male",
           trendscoarse == "15+",
           funding_agency == "USAID") %>% 
    group_by(fiscal_year, indicator, sex, trendscoarse) %>%
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
  #  reshape_msd(include_type = FALSE) %>% 
    mutate(achv = cumulative/targets,
           pop = "Adult Male (15+)") %>% 
    select(-c(sex, trendscoarse))
  
  #KP
  df_kp <- df_msd %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST", "HTS_TST_POS"),
           fiscal_year >= 2023,
          # use_for_age == "Y",
           indicatortype != "CS",
           operatingunit != "Ukraine",
          str_detect(standardizeddisaggregate, "KeyPop"),
           funding_agency == "USAID") %>% 
    group_by(fiscal_year, indicator) %>%
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    #  reshape_msd(include_type = FALSE) %>% 
    mutate(achv = cumulative/targets,
           pop = "KP") 
  
  #add nudge bar for nudged bars
  nudge_space <- 0.25
  period <- "FY24Q2"
  
  indic_levels <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")
  
   df_ayp %>% 
    bind_rows(df_men, df_kp, df_peds) %>% 
    filter(fiscal_year == 2024) %>% 
    adorn_achievement(qtr = 2)%>% 
    mutate(achv_color = case_when(achv_color == "#e6e6e6" ~ "#697ebc", #adapt adorn achv later
                                  achv_color == "#ffcaa2" ~ "#fbdc99",
                                  achv_color == "#ff939a" ~ "#f8a27e",
                                  TRUE ~ achv_color)) %>% 
    ggplot(aes(x = fct_relevel(indicator, indic_levels))) +
    geom_col(aes(y = targets, fill = trolley_grey_light), width = 0.5) +
    geom_col(aes(y = cumulative, fill = achv_color), width = 0.5, position = position_nudge(x = nudge_space)) +
    facet_wrap(~pop, scales = "free_y", nrow = 1) +
    scale_y_continuous(label = scales::label_number(scale_cut = cut_short_scale())) +
    scale_fill_identity() +
    si_style_ygrid() +
    geom_text(aes(y = cumulative, label = glue::glue("({percent(achv, 1)})"),
              vjust = -0.5, hjust = -0.2,
              # color = "black",
              family = "Source Sans Pro"), size = 4) +
    geom_text(aes(y = cumulative, label = clean_number(cumulative, 1)),
              vjust = -2, hjust = -0.2,
              size = 4.5,
              # color = "black",
              family = "Source Sans Pro") +
    labs(x= NULL, y = NULL,
         title = glue::glue("At FY24Q2, USAID's testing and treatment initiation is on track to meet annual targets across most priority groups, while current on treatment
                            and peds lags behind" %>% toupper()),
         subtitle = glue::glue("{metadata$curr_pd} USAID Target Achievement"),
         caption = metadata$caption)

  si_save("Graphics/accelerate_achv_by_grp.svg")    
  
  
# VIRAL SUPPRESSION VIZ --------------------------------------------------------
  
  #USAID global VLS
 df_vls <- df_msd %>% clean_indicator() %>% 
    filter(indicator %in% c("TX_PVLS", "TX_PVLS_D"),
           fiscal_year >= 2022,
           standardizeddisaggregate %in% c("Age/Sex/Indication/HIVStatus", "Age/Sex/HIVStatus"),
           indicatortype != "CS",
           operatingunit %ni% c("Nigeria", "Ukraine"),
           funding_agency == "USAID") %>% 
    group_by(fiscal_year, indicator) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
    #filter(period == max(period)) %>% 
    pivot_wider(names_from = "indicator") %>% 
    mutate(vls = TX_PVLS / TX_PVLS_D) %>% 
    mutate(operatingunit = "Global")
  
  #USAID VLS by OU
  df_vls_ou <- df_msd %>% clean_indicator() %>% 
    filter(indicator %in% c("TX_PVLS", "TX_PVLS_D"),
           fiscal_year >= 2022,
           standardizeddisaggregate %in% c("Age/Sex/Indication/HIVStatus", "Age/Sex/HIVStatus"),
           indicatortype != "CS",
           operatingunit != "Ukraine",
           funding_agency == "USAID") %>% 
    group_by(fiscal_year, indicator, operatingunit) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator") %>% 
    mutate(vls = TX_PVLS / TX_PVLS_D) %>% 
    rbind(df_vls) %>% 
    filter(period == max(period)) %>% 
    mutate(fill_color = case_when(operatingunit == "Global" ~ hw_midnight_blue,
                                  vls >= .95 & operatingunit != "Global" ~ hw_viking,
                                  TRUE ~ "#9dd3e6"))
  
  
vls_line <-  df_vls %>% 
    mutate(group = 1) %>% 
    ggplot(aes(x = period, y = vls, color = hw_midnight_blue, group = group)) +
    geom_line(size = 1) + 
    geom_point(size = 3) +
    scale_y_continuous(label = percent, limits = c(0.94, 0.97)) +
    # scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
    geom_text(aes(label = percent(vls, 0.1)),
              vjust = -1, hjust = 0.1,
              # color = "black",
              family = "Source Sans Pro") +
    scale_color_identity() +
    si_style_nolines() +
    labs(x = NULL, y = NULL,
         title = glue::glue("USAID's global viral suppression of PLHIV with a recent VL test has steadily increased
                            in the last 10 quarters" %>% toupper()),
         subtitle = "Nigeria ommitted from total due to data quality issues in FY23Q4")
  
  si_save("Images/02_VLS_trends.png")
  
  vls_bar <- df_vls_ou %>% 
    mutate(operatingunit = recode(operatingunit, "Democratic Republic of the Congo" = "DRC"),
           operatingunit = recode(operatingunit, "Western Hemisphere Region" = "WH Region")) %>% 
    filter(!is.na(vls)) %>% 
    ggplot(aes(x = vls, y= fct_reorder(operatingunit, vls, .na_rm = FALSE), fill= fill_color)) +
    geom_col() +
    scale_fill_identity() +
    geom_vline(xintercept = 0.95, linetype = "dotted") +
    geom_text(aes(label = percent(vls, 1)),
            #  vjust = -1,
              hjust = -0.4,
            size = 3,
              # color = "black",
              family = "Source Sans Pro") +
    si_style_nolines() +
    labs(x = NULL, y=NULL,
         subtitle = glue::glue("USAID {metadata$curr_pd} Viral Suppression rates by OU"),
         caption = glue::glue("{metadata$caption}
                              Note: Ukraine ommitted from both plots")) +
    theme(axis.text.x = element_blank())
  
  si_save("Images/FY24Q2_CTOU_VLS.png")
  
  
vls_line / vls_bar +
  plot_layout(heights = c(1,3))
si_save("Graphics/FY24Q2_CTOU_VLS_combined.svg")


  # UNAIDS ART Coverage --------------------------------------------------------

#pull UNAIDS PLHIV
 plhiv <- df_est %>% 
    filter(indicator == "Number PLHIV",
           year == max(year),
           age == "All",
           sex == "All") %>% 
    select(c(country, iso, year, indicator, estimate)) 
  
#pull UNAIDS number on ART
  
df_tt %>% 
    filter(indicator == "Number on ART of PLHIV",
           year == max(year),
           age == "All",
           sex == "All") %>% 
    select(c(country, iso, year, indicator, estimate)) %>% 
    rbind(plhiv) %>% 
    spread(indicator, estimate) %>% 
    rename(tx_curr = `Number on ART of PLHIV`,
          plhiv = `Number PLHIV`) %>% 
    mutate(art_cov = tx_curr/plhiv) %>% 
    arrange(desc(art_cov))
  
#mutate ART coverage
  df_cov <- df_tt %>% 
    filter(indicator == "Percent on ART of PLHIV",
           year == max(year),
           age == "All",
           sex == "All") %>% 
    select(c(country, iso, year, indicator, estimate)) %>% 
    #rbind(plhiv) %>% 
    spread(indicator, estimate) %>% 
    arrange(desc(`Percent on ART of PLHIV`)) %>% 
    mutate(flag_over_90 = `Percent on ART of PLHIV` >= 90)
  
  #get countries TX_CURR growth, funding_agency agnostic
  df_art <- df_msd %>% 
    rbind(df_arch) %>% 
    filter(!str_detect(operatingunit, "Region"),
           fiscal_year %in% c(2018:2023),
           indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator") %>% 
    dplyr::count(fiscal_year, operatingunit, wt = cumulative, name = "TX_CURR") %>% 
    group_by(operatingunit) %>% 
    arrange(operatingunit, fiscal_year) %>% 
    mutate(tx_curr_lag = lag(TX_CURR, order_by = fiscal_year),
           increased = TX_CURR > tx_curr_lag ) %>%
    replace_na(list(increased = FALSE)) %>% 
    mutate(times_increased = sum(increased)) %>% 
    ungroup() %>% 
    left_join(df_cov, by = c("operatingunit" = "country"))

  
    
  sort_order <- c("Positive treatment growth, <90% Coverage", "Sustaining patients on treatment, >90% Coverage",
                  "Undergoing Data Quality Improvement", "Sustaining patients on treatment, <90% coverage", "Countries in crisis")
  
  df_art_viz <- df_art %>% 
    mutate(cntry_group = case_when(times_increased  == max(times_increased) & flag_over_90 == FALSE ~ "Positive treatment growth, <90% Coverage",
                                   flag_over_90 == TRUE ~ "Sustaining patients on treatment, >90% Coverage",
                                   operatingunit %in% c("Ukraine", "Haiti", "South Sudan", "Ethiopia") ~ "Countries in crisis",
                                   operatingunit %in% c("Tanzania", "Nigeria", "Democratic Republic of the Congo") ~ "Undergoing Data Quality Improvement",
                                   TRUE ~ "Sustaining patients on treatment, <90% coverage")) %>% 
    mutate(cntry_group_fct = factor(cntry_group, levels = sort_order))
  
  df_art_viz %>% 
    mutate(cntry_group = case_when(operatingunit %in% c("Ukraine", "Haiti", "South Sudan", "Ethiopia") ~ "Countries in crisis",
                                   operatingunit %in% c("Tanzania", "Nigeria", "Democratic Republic of the Congo") ~ "Undergoing Data Quality Improvement",
                                   TRUE ~ cntry_group)) %>% 
    mutate(cntry_group_fct = factor(cntry_group, levels = sort_order)) %>% 
    filter(fiscal_year == 2023) %>% 
    rename(art_cov = `Percent on ART of PLHIV`) %>% 
    mutate(fill_color = ifelse(art_cov >=90, hw_midnight_blue, hw_viking)) %>% 
    ggplot(aes(art_cov, fct_reorder(operatingunit, art_cov, .na_rm = FALSE), fill = fill_color, color = fill_color)) +
    ggplot2::geom_blank(aes(art_cov*1.1), na.rm = TRUE) +
    geom_col() +
    facet_wrap(~cntry_group_fct, scales = "free_y", ncol = 3) +
    scale_fill_identity() +
    scale_color_identity() +
    si_style_nolines() +
    ggplot2::geom_text(aes(label = glue::glue("{art_cov}%")), na.rm = TRUE,
                       family = "Source Sans Pro", color = glitr::suva_grey,
                       size = 11/.pt, hjust = -0.2) +
    labs(x = NULL, y = NULL,
         title = "PEPFAR countries' current momentum towards 2025 goal - ART Coverage in 2022" %>% toupper(),
         subtitle = "Current ART Coverage (90% ART is a proxy for 95-95-95) and recent trajectory are critical to developing the near-term strategy to close the remaining HIV gaps",
         caption = glue::glue("Source: {mindthegap::source_note}")) +
    theme(axis.text.x = element_blank())
  
  si_save("Graphics/FY24Q2_Call_to_OUs_art_cov.svg")
  

    
    
    
  # SUBNAT ART Coverage --------------------------------------------------------
  
  
  # df_cov <- df_subnat %>% 
  #   filter(fiscal_year == max(fiscal_year),
  #          operatingunit != "Ukraine",
  #          #funding_agency == "USAID",
  #          indicator %in% c("TX_CURR_SUBNAT", "PLHIV"),
  #          standardizeddisaggregate == "Age/Sex/HIVStatus",
  #          ageasentered != "Unknown Age") %>% 
  #   dplyr::count(fiscal_year, operatingunit, indicator, wt = targets, name = "value") %>% 
  #   tidyr::pivot_wider(names_from = indicator,
  #                      names_glue = "{tolower(indicator)}") %>% 
  #   dplyr::mutate(cov_tx = tx_curr_subnat/plhiv) %>% 
  #   mutate(fill_color = ifelse(cov_tx >=1, scooter, scooter_light)) %>% 
  #   arrange(desc(cov_tx)) %>%
  #   dplyr::mutate(plhiv_marker = dplyr::case_when(tx_curr_subnat > plhiv ~ plhiv)) %>% 
  #   dplyr::group_by(operatingunit) %>% 
  #   dplyr::mutate(lab_text = scales::percent(cov_tx, 1)) %>% 
  #   dplyr::ungroup() %>% 
  #   tidyr::pivot_longer(c("plhiv", "tx_curr_subnat"),
  #                       names_to = "indicator") %>% 
  #   dplyr::mutate(indicator = toupper(indicator),
  #                 dplyr::across(c(cov_tx, plhiv_marker, lab_text), \(x) ifelse(indicator == "PLHIV", x, NA)))
  # 
  # 
  # 
  # df_cov <- df_cov%>% 
  #   mutate(ou_fct = fct_reorder(operatingunit, value, .na_rm = TRUE, .desc = FALSE)) 
  
  # df_cov %>% 
  #   ggplot(aes(value, ou_fct, fill = fill_color, color = fill_color)) +
  #   ggplot2::geom_blank(aes(value*1.1), na.rm = TRUE) +
  #   ggplot2::geom_col(data = df_cov %>% filter(indicator == "PLHIV"),
  #                     fill = NA, width = .8, alpha = .8, na.rm = TRUE) +
  #   ggplot2::geom_col(data = df_cov %>% filter(indicator != "PLHIV"), 
  #                     width = .8, alpha = .8, na.rm = TRUE) +
  #   ggplot2::geom_errorbar(aes(xmin = plhiv_marker, xmax = plhiv_marker),
  #                          na.rm = TRUE, color = "white", linetype = "dotted") +
  #   ggplot2::geom_text(aes(label = lab_text), na.rm = TRUE,
  #                      family = "Source Sans Pro", color = glitr::suva_grey,
  #                      size = 10/.pt, hjust = -.5) +
  #   #ggplot2::facet(sex ~ facet_grp, switch = "y", scales = "free_x") +
  #   ggplot2::scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()),
  #                               expand = c(.005, .005)) +
  #   ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
  #   si_style_xgrid() +
  #   labs(x= NULL, y = NULL,
  #        title = "FY24 ART Coverage" %>% toupper(),
  #        subtitle = "TX_CURR_SUBNAT coverage of PLHIV",
  #        caption = metadata$caption) 
  # 
  # si_save("Graphics/03_fy24q2_art_cov.svg")
  
  # # IIT ----------------------------------------------------------------------- 
  # 
  # df_msd %>%
  #   #resolve_knownissues() %>% 
  #   filter(fiscal_year >= 2023,
  #          indicatortype != "CS",
  #          operatingunit %ni% c("Nigeria", "Ukraine"),
  #          indicator  %in% c("TX_CURR", "TX_NEW", "TX_ML", 'TX_CURR_Lag1'),
  #          standardizeddisaggregate %in% c("Total Numerator"),
  #          funding_agency == "USAID") %>% 
  #   group_by(fiscal_year, indicator) %>%
  #   #group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator) %>% 
  #   #group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>% 
  #   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  #   reshape_msd(include_type = FALSE) %>% 
  #   pivot_wider(names_from = "indicator",
  #               names_glue = "{tolower(indicator)}") %>% 
  #   rowwise() %>% 
  #   mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
  #   ungroup()