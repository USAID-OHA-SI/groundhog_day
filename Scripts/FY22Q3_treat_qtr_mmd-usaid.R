# PROJECT:  agitprop
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  MMD
# REF ID:   a92e1bd0
# LICENSE:  MIT
# DATE:     2021-12-01
# UPDATED:  2022-06-01
# NOTE:     adapted from agitprop/11_MMD.R & catch-22/gpm_usaid_mmd-trends-by-country.R


# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  
# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")

  msd_source <- source_info()
  curr_pd <- source_info(return = "period")
  curr_fy <- source_info(return = "fiscal_year")
  curr_qtr <- source_info(return = "quarter")
  
  ref_id <- "a92e1bd0"
  
# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>% 
    return_latest("OU_IM_FY20") %>% 
    read_msd()   


# MUNGE MMD ---------------------------------------------------------------

  #keep just TX_CURR/MMD
  df_mmd <- df %>% 
    filter(funding_agency == "USAID",
           indicator == "TX_CURR",
           operatingunit != "South Africa",
           fiscal_year >= 2020,
           standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                         TRUE ~ str_remove(otherdisaggregate, "ARV Dispensing Quantity - ")
    )) 
  
  #add in agency agg and reshape
  df_mmd <- df_mmd %>%
    bind_rows(df_mmd  %>%
                mutate(operatingunit = "USAID",
                       country = "USAID")) %>% 
    group_by(fiscal_year, operatingunit, country, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    filter(value > 0)
  
  #create group for o3mo and o6mo via reshaping for plotting
  df_mmd <- df_mmd %>% 
    mutate(operatingunit = recode(operatingunit,
                                  "Democratic Republic of the Congo" = "DRC",
                                  "Dominican Republic" = "DR"),
           country = recode(country,
                                "Democratic Republic of the Congo" = "DRC",
                                "Dominican Republic" = "DR")) %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = otherdisaggregate) %>% 
    rowwise() %>% 
    mutate(#unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
      #unknown = ifelse(unknown < 0, 0, unknown),
      o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    rename(o6mmd = `6 or more months`) %>% 
    select(-`Less than 3 months`, -`3 to 5 months`) %>% 
    pivot_longer(-c(period, operatingunit, country, indicator, total), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total) 
  

# MMD FOR AGENCY ROLL UP --------------------------------------------------

  #aggregate up to agency level
  df_mmd_agency <- df_mmd %>%
    filter(operatingunit == "USAID") %>% 
    arrange(otherdisaggregate, period) %>% 
    mutate(otherdisaggregate = recode(otherdisaggregate,
                                      "o3mmd" = "MMD - 3 months or more",
                                      "o6mmd" = "MMD - 6 months or more")) %>% 
    mutate(share = tx_mmd / tx_curr)
  
  #adjust for viz
  df_mmd_agency <- df_mmd_agency %>% 
    mutate(bar_color = ifelse(otherdisaggregate == "MMD - 3 months or more", scooter, genoa),
           otherdisaggregate_md = glue("<span style='color:{bar_color}'>{otherdisaggregate}</span>"),
           lab_max = case_when(period == max(period) ~ share),
           lab_other = case_when(period != max(period) ~ share))

# MMD FOR COUNTRY TRENDS --------------------------------------------------

  #country trends for just o3mo
  df_mmd_ou <- df_mmd %>% 
    arrange(country, otherdisaggregate, period) 
  
  #create share on +3mo
  df_mmd_ou <- df_mmd_ou %>% 
    mutate(share = tx_mmd/tx_curr) 
  
  #data points for plotting
  df_mmd_ou <- df_mmd_ou %>% 
    mutate(max_tx = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_curr, 0),
           max_mmd = ifelse(period == max(period) & otherdisaggregate == "o6mmd", tx_mmd, 0)) %>% 
    group_by(country) %>% 
    mutate(endpoints = case_when(period %in% c(max(period), min(period))~share),
           max_tx = max(max_tx),
           max_mmd = max(max_mmd)) %>% 
    ungroup() %>% 
    mutate(country_lab = case_when(max_tx == max(max_tx) ~ 
                                     glue("{country}<br><span style = 'font-size:8pt'>{label_number_si()(max_mmd)} / {label_number_si()(max_tx)} <span style = 'font-size:6pt'>(+6 MMD/TX_CURR)</span>"),
                                   TRUE ~ glue("{country}<br><span style = 'font-size:8pt'>{label_number_si()(max_mmd)} / {label_number_si()(max_tx)}</span>")),
           country_lab = str_replace(country_lab, "NA", "0")) %>% 
    filter(max_tx > 0)
  
  
  df_mmd_ou <- df_mmd_ou %>% 
    mutate(fill_color = case_when(country == "USAID" & otherdisaggregate == "o6mmd" ~ "#0f4453",
                                  country == "USAID" ~ "#78b7c9",
                                  otherdisaggregate == "o6mmd" ~ scooter,
                                  TRUE ~ scooter_light),
           lab_share = case_when(country == "USAID" & period == max(period) ~ share))
  
  #identify the MMD share for the largest countries
  top <- df_mmd_ou %>% 
    filter(country != "USAID",
           otherdisaggregate == "o6mmd",
           period == max(period)) %>% 
    arrange(desc(tx_curr)) %>% 
    slice_max(n = 11, order_by = tx_curr) %>% 
    summarise(across(c(tx_curr, tx_mmd), sum, na.rm = TRUE),
              n = n()) %>% 
    mutate(share = percent(tx_mmd/tx_curr, 1))
  
  #top focus countries
  top_cntry <- df_mmd_ou %>% 
    filter(otherdisaggregate == "o6mmd",
           period == max(period)) %>% 
    slice_max(order_by = tx_curr, n = top$n + 1) %>% 
    pull(country)
  
# VIZ ---------------------------------------------------------------------
  
  #USAID Trend
  df_mmd_agency %>% 
    ggplot(aes(period, tx_mmd)) + 
    geom_col(aes(y = tx_curr), fill = trolley_grey_light, alpha = .5) +
    geom_col(aes(fill = bar_color)) +
    geom_text(aes(label = percent(lab_other, 1)), vjust = 1.2, na.rm = TRUE,
                  family = "Source Sans Pro SemiBold", color = "white") +
    geom_text(aes(label = percent(lab_max, 1)), vjust = -1, na.rm  = TRUE,
                  family = "Source Sans Pro SemiBold", color = matterhorn) +
    geom_errorbar(aes(ymax = tx_curr, ymin = tx_curr), color = trolley_grey) +
    # facet_wrap(~otherdisaggregate) +
    facet_wrap(~otherdisaggregate_md) +
    scale_x_discrete(breaks = c("FY20Q2", "FY20Q4", "FY21Q2", "FY21Q4", "FY22Q2")) +
    scale_fill_identity() +
    scale_y_continuous(labels = label_number_si(),
                       position = "right", expand = c(.005, .005)) +
    labs(x = NULL, y = NULL,
         title = "USAID HAS WORKED TO ENSURE MORE PATIENTS HAVE ACCESS TO MULTI MONTH DISPENSING (MMD)",
         subtitle = "South Africa, representing a third of USAID's treatment portfolio, has been excluded",
         caption = glue("MMD 3 months or more = 3-5 months and 6 months or more | Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')} | Ref ID: {ref_id}")) +
    si_style_ygrid() +
    theme(legend.position = "none",
          strip.text.x = element_markdown(family = "Source Sans Pro SemiBold", size = 13))
          # strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13))
          
  

  si_save(glue("Graphics/Treat_qtr_mmd-usaid_{curr_pd}.svg"))  
  si_save(glue("Images/Treat_qtr_mmd-usaid_{curr_pd}.png"))  
  
  
  #Country Trends
  df_mmd_ou %>%
    filter(country %in% top_cntry) %>%
    ggplot(aes(period, share, group = otherdisaggregate, color = fill_color, fill = fill_color)) +
    geom_area(alpha = .4, size = .9, position = "identity") +
    geom_point(aes(y = endpoints), na.rm = TRUE) +
    geom_text(aes(label = percent(lab_share, 1)), na.rm = TRUE,
              hjust = -.2, vjust = .1,family = "Source Sans Pro") +
    facet_wrap(~fct_reorder2(country_lab, period, tx_curr, .desc = TRUE)) +
    scale_y_continuous(label = percent, 
                       breaks = seq(0, 1, .5)) +
    scale_x_discrete(breaks = c("FY20Q1", "FY21Q1", "FY22Q1", "FY22Q3"),
                     labels = c("FY20Q1"="FY20", "FY21Q1"="FY21", "FY22Q1"="FY22", "FY22Q3"="Q3")) +
    scale_color_identity(aesthetics = c("color","fill")) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         title = glue("USAID HAS MADE LIMITED GAINS TOWARDS GETTING TREATMENT PATIENTS ON +6 MONTHS OF MMD SINCE FY20Q3"),
         subtitle = "South Africa, representing a third of USAID's treatment portfolio, has been excluded",
         caption = glue("MMD 3 months or more = 3-5 months and 6 months or more | Source: {msd_source}
                        Created by: USAID OHA SI Team | Ref ID: {ref_id}")) +
    si_style_ygrid() +
    theme(panel.spacing.y = unit(.5, "line"),
          panel.spacing.x = unit(.5, "line"),
          axis.text.x = element_text(size = 9),
          axis.text.y = element_text(size = 7),
          panel.grid.major.y = element_line(color = "#E8E8E8"),
          panel.grid.minor.y = element_line(color = "#E8E8E8"),
          strip.text = element_markdown())    
  
  si_save(glue("Graphics/{curr_pd}_Treat_qtr_mmd-countries.svg"))
  si_save(glue("Images/{curr_pd}_Treat_qtr_mmd-countries.png"))       
  
  