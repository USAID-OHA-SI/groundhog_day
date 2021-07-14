# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  USAID achievement
# LICENSE:  MIT
# DATE:     2021-05-19
# UPDATED: 
# NOTE:     adapted from agitprop/05_usaid_target_achievement.R

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(waffle) #devtools::install_github("hrbrmstr/waffle")
  


# GLOBAL VARIABLES --------------------------------------------------------
  
  ind_sel <- c("PrEP_NEW", "VMMC_CIRC", "HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")

  authors <- c("Aaron Chafetz", "Tim Essam")
  
  #msd_source <- msd_period()
  

# FUNCTIONS ---------------------------------------------------------------

  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   


# MUNGE -------------------------------------------------------------------

  curr_fy <- identifypd(df, "year")
  curr_qtr <- identifypd(df, "quarter")
  curr_pd <- identifypd(df)
  msd_source <- "FY21Q2c" #msd_period(period = curr_pd)
  trgt_rng <- 1*(curr_qtr/4)
  
  
  df_achv <- df %>% 
    filter(indicator %in% ind_sel,
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == curr_fy,
           fundingagency != "Dedup") %>%
    mutate(fundingagency = ifelse(fundingagency == "USAID", "USAID", "All Other Agencies"),
           fundingagency = factor(fundingagency, c("USAID", "All Other Agencies"))) %>% 
    group_by(fiscal_year, fundingagency, indicator) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
    ungroup()
    
    
  df_achv <- df_achv %>% 
    mutate(achievement = cumulative/targets,
           qtr_goal = ifelse(indicator == "TX_CURR", 1, 1*(curr_qtr/4)),
           achv_label = case_when(is.na(achievement) ~ NA_character_,
                                  achievement <= qtr_goal-.25 ~ glue("<{100*(qtr_goal-.25)}%") %>% as.character,
                                  achievement <= qtr_goal-.1 ~ glue("{100*(qtr_goal-.25)}-{100*(qtr_goal-.11)}%") %>% as.character,
                                  achievement <= qtr_goal+.1 ~ glue("{100*(qtr_goal-.1)}-{100*(qtr_goal+.1)}%") %>% as.character,
                                  TRUE ~ glue("+{100*(qtr_goal+.1)}%") %>% as.character),
           achv_color = case_when(is.na(achievement) ~ NA_character_,
                                  achievement <= qtr_goal-.25 ~ old_rose_light,
                                  achievement <= qtr_goal-.1 ~ burnt_sienna_light,
                                  achievement <= qtr_goal+.1 ~ "#5BB5D5",
                                  TRUE ~ trolley_grey_light)) %>% 
    select(-qtr_goal)
  

  df_viz <- df_achv %>%
    mutate(achv_round = round(achievement*100),
           achv_round = ifelse(achv_round > 100, 100, achv_round),
           gap = 100-achv_round) %>% 
    pivot_longer(c(achv_round, gap), names_to = "status") %>% 
    mutate(achv_color = ifelse(status == "gap", "#EBEBEB", achv_color),
           achv_color = ifelse(achv_color == trolley_grey_light, trolley_grey, achv_color),
           achv_alpha = ifelse(status == "gap", .1, 1),
           indicator = factor(indicator, ind_sel),
           ind_lab = case_when(indicator == "PrEP_NEW" ~ "Newly enrolled on antiretroviral pre-exposure prophylaxis",
                               indicator == "VMMC_CIRC" ~ "Voluntary medical male circumcision for HIV prevention",
                               indicator == "HTS_TST" ~ "Receiving HIV testing service and results",
                               indicator == "HTS_TST_POS" ~ "Receiving HIV testing services and positive results",
                               indicator == "TX_NEW" ~ "Newly enrolled on antiretroviral therapy",
                               indicator == "TX_CURR"~ "Currently receiving antiretroviral therapy"),
           ind_lab = str_wrap(ind_lab, width = 25),
           val_lab = ifelse(indicator == ind_sel[1],
                            glue("Results - {clean_number(cumulative)}\nTargets - {clean_number(targets)}"),
                            glue("{clean_number(cumulative)}\n{clean_number(targets)}")),
           full_lab = ifelse(fundingagency == "USAID", glue("{indicator}\n\n{val_lab}"),  val_lab)) %>% 
    arrange(indicator) %>% 
    mutate(full_lab = fct_inorder(full_lab))
  
  
  v_usaid <- df_viz %>% 
    filter(fundingagency == "USAID") %>% 
    ggplot(aes(fill = achv_color, values = value, alpha = achv_alpha)) +
    geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE) +
    geom_text(aes(x = 5, y  = 12, label = percent(achievement, 1), color = achv_color),
              family = "Source Sans Pro SemiBold", size = 14/.pt) +
    facet_wrap(~full_lab, nrow = 1, strip.position = "bottom") +
    expand_limits(y = 14) +
    scale_x_discrete() +
    scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_alpha_identity() +
    coord_equal() +
    labs(x= NULL, y = NULL,
         subtitle = "USAID") +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          strip.text.x = element_text(hjust = .5),
          panel.spacing = unit(1, "pt")) +
    guides(fill = guide_legend(reverse = TRUE))
  
  v_other <- df_viz %>% 
    filter(fundingagency != "USAID") %>% 
    ggplot(aes(fill = achv_color, values = value, alpha = achv_alpha)) +
    geom_waffle(color = "white", size = 1, n_rows = 10, flip = TRUE) +
    geom_text(aes(x = 5, y  = 12, label = percent(achievement, 1), color = achv_color),
              family = "Source Sans Pro SemiBold", size = 14/.pt) +
    facet_wrap(~full_lab, nrow = 1, strip.position = "bottom") +
    expand_limits(y = 14) +
    scale_x_discrete() +
    scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                       expand = c(0,0)) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_alpha_identity() +
    coord_equal() +
    labs(x= NULL, y = NULL,
         subtitle = "All Other Agencies") +
    si_style_nolines() +
    theme(axis.text.y = element_blank(),
          strip.text.x = element_text(hjust = .5),
          panel.spacing = unit(1, "pt")) +
    guides(fill = guide_legend(reverse = TRUE))
  

  v_usaid/v_other +
    plot_annotation(title = "USAID GLOBALLY IS IN GOOD STANDING FOR MER TARGET ACHIEVEMENT, WITH ROOM TO IMPROVE FOR TX_CURR",
                    subtitle = glue("as of {curr_pd}, goal of being at around {percent(trgt_rng)} of the FY target"),
                    caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"),
                    theme = si_style())
  
  si_save("Graphics/FY21Q2_achievement.svg")
  
  
  si_save("Images/05b_achievement.png", plot = v_usaid, height = 4)
  