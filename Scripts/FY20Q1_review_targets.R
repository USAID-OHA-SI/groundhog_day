##  PROJECT: Q2 review target analysis
##  AUTHOR:  jdavis | USAID
##  PURPOSE: munge historic targets to fy21 targets, viz
##  LICENCE: MIT
##  DATE:    2020-03-30
##  UPDATE:

#Dependancies----------------------------------------------------------
  library(tidyverse)
  library(ICPIutilities)
  library(tameDP)
  #install.packages("janitor")
  library(janitor)
  library(ggplot2)
  library(skimr)
  library(scales)
  library(extrafont)
  library(patchwork)
  library(ggrepel)
  library(flextable)
  library(webshot)


#folders---------------------------------------------------------------
  
  data_in <- "Data"
  data_out <- "Dataout"
  viz_folder <- "Images"
  
  source("Scripts/si_style.R")

#create cop20 targets--------------------------------------------------

  files <- list.files(dps, full.names = TRUE)
  
  #read in all DPs and combine into one data frame
  df_all <- map_dfr(.x = files,
                    .f = ~ tame_dp(.x))


# GLOBALS -----------------------------------------------------------------

  # Collapse and summarize function that can be called at beginning to
  # group by a dfined 
   sum_targets <- function(df, group) {
       df %>%
         group_by_at({{group}}) %>% 
         summarise(targets = sum(targets, na.rm = TRUE)) %>% 
         ungroup() %>% 
         filter(targets != 0)
    }


  # Print all rows
  prinf <- function(df) {
      print(df, n = Inf)
  }

  # Grouping call
  grp_ou <- c("operatingunit", "fiscal_year", "indicator", "fundingagency")
  grp_ou_agency <- c("fiscal_year", "indicator", "agency_other") 
  
  # Indicators of focue
  indc <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "OVC_SERV", "KP_PREV")
  ind_v2 <- c("HTS_TST_POS", "TX_CURR", "TX_NEW", "VMMC_CIRC", "PrEP_NEW", "OVC_SERV", "KP_PREV")


#munge historic targets------------------------------------------------
# Bring in fy21 data
  
  # df_mer_21.csv seems to have everything we need
  df_21 <- vroom::vroom(file.path(data_in, "df_mer_21.csv"))
  
  df_15_16 <- readRDS(file.path(data_in, "MER_Structured_Datasets_OU_IM_FY15-17_20200320_v2_1.rds"))
  
  df_15_16_sum <- 
    df_15_16 %>% 
    filter(standardizeddisaggregate == "Total Numerator") %>% 
    sum_targets(., group = grp_ou)


#read_msd("C:/Users/Josh/Documents/data/fy20_q1_v1/MER_Structured_Datasets_OU_IM_FY18-20_20200320_v2_1.zip")

  df_curr <- readRDS(file.path(data_in, "MER_Structured_Datasets_OU_IM_FY18-20_20200320_v2_1.rds")) 
  
  df_curr_sum <- 
    df_curr %>% 
    filter(standardizeddisaggregate == "Total Numerator") %>% 
    sum_targets(., grp_ou)
  

  # Combine together for making beautifu plots
  df_mer <-
    bind_rows(df_curr_sum, df_15_16_sum) %>%
    mutate(agency_other = case_when(fundingagency == "USAID" ~ "USAID",
                                  fundingagency == "HHS/CDC" ~ "CDC",
                                  TRUE ~ "OTHER"),
         agency_other = factor(agency_other, c("USAID", "CDC", "Other")))

  
  # What OUs are missing targets?
  # Few targets in 2015 so may want to drop that out
  df_mer %>% filter(indicator == "TX_CURR",
                     fundingagency %in% c("USAID", "HHS/CDC")) %>% 
    distinct(agency_other, fiscal_year, targets, operatingunit) %>% 
    spread(fiscal_year, targets) %>% 
    prinf()

  # What indicators are missing targets?
  df_mer %>% 
    group_by(fiscal_year, indicator, agency_other) %>% 
    summarise(targets = sum(targets)) %>% 
    spread(fiscal_year, targets) %>% 
    prinf()

  
  
#Viz munge-----------------------------------------------------------

  # Create two types of shares:
  # TODO: functionalize the process
  # Share 1: What are the total targets for the Fiscal year, across all agencies
  # Share 2: What are the total targets across the Fiscal year, within agences (OU shares)
  # To make it easier, we'll create two data frames versus carry constants around at OU level
    
  df_viz_agency_21 <- df_21 %>% 
    filter(indicator %in% ind_v2, 
      fiscal_year != 2015) %>% 
    sum_targets(., group = grp_ou_agency) %>% 
    group_by(indicator, fiscal_year) %>% 
    mutate(tot_targets = sum(targets, na.rm = TRUE),
      agency_share = targets / tot_targets,
      ymax = cumsum(agency_share)) %>% 
    ungroup() %>% 
    mutate(agency_label = if_else(agency_other == "USAID" & fiscal_year %in% c(2016, 2021), 
      round(agency_share, 2), NA_real_))
    
  df_viz_ou <- df_21 %>% 
    filter(indicator %in% indc,
      fiscal_year != 2015, agency_other == "USAID") %>% 
    group_by(indicator, fiscal_year) %>% 
    mutate(tot_targets = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
      mutate(ou_share = targets / tot_targets) %>% 
    ungroup()
  
  
  # How does USAID OU share shift across all agencies
  # First, calculate total targets across by fiscal years and indicator
  df_viz_withinou <- 
    df_21 %>% 
    filter(indicator %in% indc, fiscal_year != 2015) %>% 
    group_by(operatingunit, fiscal_year, indicator, agency_other) %>% 
    summarise(targets = sum(targets, na.rm = TRUE)) %>% 
    group_by(indicator, fiscal_year, operatingunit) %>% 
    mutate(tot_targets = sum(targets, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(agency_share = targets / tot_targets) %>% 
    arrange(operatingunit, fiscal_year, indicator) %>% 
    group_by(indicator, operatingunit, agency_other) %>% 
    mutate(lag_share = lag(agency_share, n = 1, order_by = fiscal_year),
      share_diff = agency_share - lag_share, 
      abs_share_diff = abs(share_diff)) %>% 
    ungroup() %>% 
    group_by(agency_other, fiscal_year, indicator) %>% 
      mutate(rank_share_diff = percent_rank(abs_share_diff)) %>% 
  ungroup()



# AGENCY LEVEL VIZ --------------------------------------------------------
  
  # Where did things grow and shrink?
  df_viz_agency_21 %>% 
    select(agency_other, fiscal_year, agency_share, indicator) %>% 
    spread(fiscal_year, agency_share) %>% 
    mutate(diff = `2020` - `2016`,
      change_label = if_else(diff > 0, "grow", "shrink"))
  
    
  # Funcationalize ggplot
  area_share_plot <- function(df, share_var) {
    df %>% 
      group_by(indicator) %>% 
      mutate(agency_order = fct_reorder(agency_other, {{share_var}}),
        agency_order = fct_relevel(agency_order, "CDC", after = 1)) %>% 
      ungroup() %>% 
      ggplot(aes(x = fiscal_year, y = {{share_var}}, fill = agency_order, group = agency_order)) +
      geom_area() + 
      geom_label_repel(aes(label = scales::percent(agency_label, accuracy = 3)), fill = "white",
        family = "Source Sans Pro Light") +
      #position = position_stack()) +
      facet_wrap(~indicator, nrow = 1) +
      theme_minimal() +
      si_style() +
      theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        axis.title = element_blank()) +
      #scale_y_continuous(label = percent_format(accuracy = 1)) +
      scale_x_continuous(limits = c(2015.9, 2021.1), breaks = seq(2016, 2021, 1)) +
      scale_fill_manual(values = c("CDC" = '#a6bddb', "USAID" = "#045a8d", "Other" = "#E8E8E8")) 
  }
  
  # Agency shares - 3 groups
  testing_plot <-  
    df_viz_agency_21 %>% 
    filter(indicator %in% c("HTS_TST_POS", "TX_CURR", "TX_NEW")) %>% 
    area_share_plot(., agency_share) +
      labs(x = NULL, y = NULL,
        fill = "Agency share",
        title = "USAID SHARE OF TESTING AND TREATMENT TARGETS",
        subtitle = "USAID'S SHARE OF HTS_TST_POS GREW BY 27% FROM FY16 - FY2021 \n")
      
      ggsave(file.path(viz_folder, "Q1Review_test_treatement_shares.png"),
        plot = testing_plot, dpi = 330, width = 10, height = 5.66)
 
  (ovc_plot <-  
          df_viz_agency_21 %>% 
          filter(indicator %in% c("OVC_SERV", "KP_PREV")) %>% 
          area_share_plot(., agency_share) +
          labs(x = NULL, y = NULL,
            fill = "Agency share",
            title = "USAID SHARE OF KP_PREV AND OVC_SERVE",
            subtitle = "USAID'S SHARE OF KP_PREV SHRANK BY 33% FROM FY16 - FY2021 \n")) 
      
      ggsave(file.path(viz_folder, "Q1Review_ovc_shares.png"),
        plot = ovc_plot, dpi = 330, width = 10, height = 5.66)                      
           
  
  (vmmc_plot <-  
    df_viz_agency_21 %>% 
    filter(indicator %in% c("VMMC_CIRC", "PrEP_NEW")) %>% 
    area_share_plot(., agency_share) +
    labs(x = NULL, y = NULL,
      fill = "Agency share",
      title = "USAID SHARE OF VMMC_CIRC AND PrEP_NEW",
      subtitle = "USAID'S SHARE OF PrEP_NEW SHRANK BY 30% FROM FY16 - FY2021 \n")) 
          
  ggsave(file.path(viz_folder, "Q1Review_vmmc_shares.png"),
    plot = vmmc_plot, dpi = 330, width = 10, height = 5.66)
  
  

# OU LEVEL WITHIN USAID ---------------------------------------------------
  
  # See which ou's grew/shrank the most - this is based on the share across all agencies
  df_viz_ou %>%
    select(operatingunit, fiscal_year, ou_share, indicator) %>% 
    mutate(ou_share = round(ou_share, 3), min = min(ou_share)) %>% 
    spread(fiscal_year, ou_share) %>% 
    mutate(diff = `2020` - `2016`,
      change_label = case_when(
        diff > 0 ~ "grow",
        diff < 0 ~ "shrink",
        TRUE ~ "no change or NA" 
        )
      ) %>% 
    arrange(indicator, diff) %>% 
    prinf()
  

  # Looking only within USAID targets -- where are they focused?
  df_viz_ou <- 
    df_viz_ou %>% 
    select(-agency_other) %>% 
    complete(nesting(operatingunit, indicator), 
      fiscal_year, fill = list(ou_share = 0, fundingagency = "USAID")) %>% 
    #mutate(sort_value = if_else(fiscal_year == max(fiscal_year), ou_share, 0)) %>% 
    #group_by(indicator, operatingunit) %>% 
    #fill(sort_value, .direction = "up") %>%
    #ungroup()  %>% 
    mutate(ou_order = fct_reorder(operatingunit, ou_share))
    
  
  heatmap_ous <- df_viz_ou %>% 
    ggplot(aes(y = ou_order, x = fiscal_year, fill = ou_share)) +
    geom_tile(color = "white") +
    facet_wrap(~indicator, nrow = 1) +
      theme_minimal() +
      scale_fill_viridis_c(option = "A", direction = -1, 
        label = percent_format(accuracy = 2)) +
    coord_equal(ratio =  0.7) +
    labs(x = NULL, y = NULL, fill = "Target share",
      title = "TARGET SHARE BY OU ACROSS USAID") +
    theme(legend.position = "top",
      legend.text.align = 0,
      legend.justification = c(0, 0),
      axis.text.x = element_text(size = 6))

  ggsave(file.path(viz_folder, "Q1Review_ou_heatmap.png"),
    plot = heatmap_ous, dpi = 330, width = 10, height = 5.66)



# OU SHARE ACROSS AGENCIES ------------------------------------------------

  
  df_viz_withinou %>%
    filter(fiscal_year %in% c(2020, 2021)) %>% 
    mutate(ou_sort = fct_reorder(operatingunit, agency_share, .desc = TRUE)) %>% 
    ggplot(aes(x = fiscal_year, y = agency_share, group = operatingunit)) +
    geom_line() +
    facet_wrap(~indicator, scales = "free_y") + 
    theme_minimal()
  
  df_viz_withinou %>%
    filter(fiscal_year %in% c(2020, 2021), agency_other == "USAID") %>% 
    ggplot(aes(x = fiscal_year, y = agency_share, group = operatingunit,
      label = if_else(fiscal_year == max(fiscal_year) & rank_share_diff > 0.7, operatingunit, NA_character_))) +
    geom_line(size = 1, colour = "#909090") +
    geom_point(aes(fill = agency_share), size = 5, shape = 21, colour = "#909090", stroke = 0.25) +
    geom_text_repel(hjust = 0,
      force = 1, point.padding=unit(1, 'lines'),
      direction = 'x',
      nudge_x = 0.1,
      segment.size = 0.1) +
    facet_wrap(~indicator) +
    theme_minimal() + si_style() +
    theme(
      panel.grid.major.y = ggplot2::element_blank()
      ) +
    scale_fill_viridis_c(label = scales::percent, direction = -1, option = "A") +
    scale_x_continuous(breaks = c(2020, 2021), limits = c(2020, 2021.5)) +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    


# SUMMARY TABLE -----------------------------------------------------------

  df_table <- 
    df_21 %>% 
    filter(indicator %in% indc,
      fiscal_year == 2021,
      agency_other == "USAID") %>% 
    group_by(operatingunit, indicator) %>% 
    summarise(targets = sum(targets)) %>%
    ungroup() %>% 
    spread(indicator, targets) %>%
    rename(OU = operatingunit) %>%
    select(OU, HTS_TST, HTS_TST_POS, TX_NEW, TX_CURR, OVC_SERV, KP_PREV) %>%
    arrange(desc(TX_CURR))  
  
 ft <-  
   flextable(df_table) %>%
    set_header_labels() %>% 
    colformat_num(digits = 0) %>% 
    add_header_lines(values = "USAID FY21 targets by indicator") %>%
    flextable::theme_vanilla() %>% 
    font(fontname = "Gill Sans MT", part = "all") %>% 
    fontsize(size = 14, part = "header") %>% 
    autofit() %>% bg(bg = "#ffffff", part = "all")
  
  save_as_image(ft, path = file.path(viz_folder, "FY21_OUtargets_by_indicator.png"))

