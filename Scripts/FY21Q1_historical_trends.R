# PURPOSE: Historical Analysis of TESTING, TREATMENT with TRENDS
# AUTHOR: Tim Essam | SI  
# LICENSE: MIT
# DATE: 2021-03-05
# NOTES: FOR FY21Q1 REVIEW

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(ICPIutilities)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    library(ggdist)
    
  # Set paths  
    data   <- "Data"
    dataout <- "Dataout"
    images  <- "Images"
    graphs  <- "Graphics"
   
    merdata <- glamr::si_path("path_msd")
     
  # Functions  
  indic_list <- c("HTS_TST_POS", "TX_CURR", "TX_NEW", "PrEP_NEW")
  
  # Collapse and sum targets and cumulative results by OU / indicator /FY
  calc_ach <- function(df, ...) {
    df %>% 
      group_by(...) %>% 
      summarise(across(c("targets", "cumulative"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(achievement = cumulative / targets)
  }

  annual_summary <- function(df, ...) {
    df %>% 
    group_by(...) %>% 
    summarise(across(c(targets, cumulative), sum, na.rm = T)) %>% 
    mutate(pct = cumulative / targets) %>% 
    arrange(indicator, fiscal_year) %>% 
    select(-targets, -cumulative)
  }
  
  # Create achievement shares by OUs / indicator and FYs
  get_ach_shares <- function(...) {
    filter(...) %>% 
      group_by(fiscal_year, indicator)%>% 
      mutate(tot_results = sum(cumulative, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(annual_sh = cumulative / tot_results) %>% 
      mutate(
        ou_color = apply_ach_colors(achievement)
      ) %>% 
      group_by(fiscal_year, indicator) %>% 
      mutate(across(c(targets, cumulative), sum, na.rm = T, .names = "USAID_{col}"),
             usaid_ach = USAID_cumulative / USAID_targets) %>% 
      ungroup()
  }
  
  
  apply_ach_colors <- function(ach_var) {
    case_when(
      {{ach_var}} > 1.1 ~ "#BCBEC0",
      {{ach_var}}  >= 0.9 &  {{ach_var}}  <= 1.1  ~ "#5bb5d5",
      {{ach_var}}  >= 0.75 &  {{ach_var}}  < 0.9  ~ "#ffcaa2",
      {{ach_var}}  < 0.75 ~ "#ff939a")
  }
  
  
  
# LOAD DATA  & MUNGE ============================================================================  
  
  # Grab MSDS, isolate USAID and select indicators and focus on TN
  msd_18 <- 
      read_msd(file.path(merdata, "MER_Structured_Datasets_OU_IM_FY15-18_20210212_v1_1.zip")) %>% 
      filter(fundingagency == "USAID",
             indicator %in% indic_list,
             standardizeddisaggregate == "Total Numerator")
  
  msd_18_ach <- 
    msd_18 %>% 
    calc_ach(operatingunit, indicator, fiscal_year)  
  
  msd_21 <- 
      read_msd(file.path(merdata, "MER_Structured_Datasets_OU_IM_FY19-21_20210212_v1_1.zip")) %>%
      filter(fundingagency == "USAID",
           indicator %in% indic_list,
           standardizeddisaggregate == "Total Numerator")
  
  msd_21_ach <- 
    msd_21 %>% 
    calc_ach(operatingunit, indicator, fiscal_year)
  
  # Create two data frames, one w/ South Africa and one without for comparison
    msd_hist_noSA <- 
      bind_rows(msd_18_ach, msd_21_ach) %>% 
      get_ach_shares(fiscal_year != 2015, operatingunit != "South Africa")
      
    msd_hist_SA <- 
      bind_rows(msd_18_ach, msd_21_ach) %>% 
      get_ach_shares(fiscal_year != 2015)

  # Calculate summary achievement across FYs by USAID for comparison plot  
   ach_noSA <-  msd_hist_noSA %>% 
      annual_summary(fiscal_year, indicator) %>% 
      mutate(flag = "Without South Africa")
  
   ach_SA <- msd_hist_SA %>% 
     annual_summary(fiscal_year, indicator) %>% 
     mutate(flag = "With South Africa")

 # VIZ ============================================================================ 
  
  # PLOT different between including / excluding South Africa into overall USAID Achievement
   bind_rows(ach_SA, ach_noSA) %>% 
     filter(indicator != "HTS_TST") %>% 
     mutate(flag_color = ifelse(flag == "With South Africa", burnt_sienna, denim),
            indicator = fct_relevel(indicator, 
                                    "HTS_TST_POS",
                                    "TX_NEW", "TX_CURR")) %>% 
     group_by(indicator, fiscal_year) %>% 
     mutate(xmax = max(pct), 
            xmin = min(pct)) %>% 
     ungroup() %>% 
     ggplot(aes(x = fiscal_year, y = pct, color = flag_color, group = flag)) +
     geom_hline(yintercept = 1, size = 0.5, color = grey20k, linetype = "dotted") +
       geom_ribbon(data = . %>% filter(fiscal_year != 2021), 
                   aes(ymax = xmax, ymin = xmin), fill = grey10k, color = NA, alpha = 0.75) +
     geom_line(data = . %>% filter(flag == "With South Africa", fiscal_year != 2021), alpha = 0.75) +
     geom_line(data = . %>% filter(flag == "Without South Africa", fiscal_year != 2021), alpha = 0.75) +
     geom_point(aes(fill = flag_color), shape = 21, color = "white", stroke = .25, size = 4) +
     ggrepel::geom_text_repel(aes(label = percent(pct, 1)), family = "Source Sans Pro", force = 20) +
     facet_wrap(~indicator) +
     scale_color_identity()+
     scale_fill_identity() +
     si_style_xline() +
     coord_cartesian(clip = "off", expand = T) +
     theme(axis.text.y = element_blank()) +
     labs(x = NULL, y = NULL)
     
  si_save(file.path(graphs, "FY21Q1_global_achievement_SouthAfrica_comparison2.svg"), scale = 1.15)
  
  
  # Density plot of achievement + colored achievement share + TRENDS across all years

    min <- min(msd_hist$achievement[msd_hist$fiscal_year == 2021])

  #  Plot each indiactor as a dot plot by time, weighting each dot by target volume  
  # df: achievement dataframe w/ and w/out South Africa
  # indic: indicator for which plot will be produced
  
    trend_plot <- function(df, indic) {
    
    p <- df %>% 
      filter(fiscal_year != 2021,
             targets != 0,
             indicator == {{indic}}) %>%
      mutate(ou_label = case_when(
               indicator == "HTS_TST" & achievement<= 0.5 ~ operatingunit,
               indicator == "HTS_TST_POS" & achievement <.45 ~ operatingunit,
               indicator == "TX_CURR" & achievement <= 0.5 ~ operatingunit,
               indicator == "TX_NEW" & achievement <= .45 ~ operatingunit, 
               TRUE ~ NA_character_)
             ) %>% 
      ggplot(aes(x = fiscal_year, group = indicator, size = annual_sh)) +
      geom_hline(yintercept = 1, color = grey50k, size = 0.5) +
      geom_hline(yintercept = c(0.5, 1.5), color = grey20k, size = 0.25, linetype = "dashed") +
      #geom_point(shape = 21, fill = grey10k) +
      stat_halfeye(aes(y = achievement), alpha = 0.25, side = "right") +
      geom_point(aes(y = achievement, fill = ou_color), 
                 shape = 21, color = "white", alpha = 0.75,
                 position = position_jitter(w = 0.15, h = 0, seed = 42)) +
      geom_point(data = df %>% filter(fiscal_year == 2021, indicator == {{indic}}),
                  aes(y = achievement, fill = ou_color), shape = 21, color = "white", alpha = 0.25,
                 position = position_jitter(w = 0.15, h = 0, seed = 42)) +
      geom_smooth(aes(y = usaid_ach), color = grey80k, se = F) +
      facet_wrap(~indicator, scales = "free_y") +
      scale_y_continuous(limits = c(0, 2.5), oob=scales::squish, labels = percent,
                         breaks = c(0.5, 1, 1.5)) +
      si_style_xline() +
      coord_cartesian(clip = "off", expand = T) +
      scale_fill_identity() +
      scale_size(range = c(0, 10)) +
      theme(legend.position = "none") +
      ggrepel::geom_text_repel(aes(y = achievement, label = ou_label), size = 3, colour = color_caption) +
      labs(x = NULL, y = NULL, title = "",
           caption = "Source: MSD FY16-FY21.")
      return(p)
      
      si_save(here(images, paste0("FY21Q1_historical_trends", {{indic}})), 
              plot = p,
              scale = 1.1,
              width = 10, 
              height = 5.5, 
              dpi = 320)
    }
    
  # Indicators for which plots are generated
  list("HTS_TST_POS", "TX_NEW", "TX_CURR") %>% 
    map(~trend_plot(msd_hist_noSA, .x))
  
  trend_plot(msd_hist_noSA %>% filter(fiscal_year != 2016, targets != 0), "PrEP_NEW")

  # Distribution shifts?    
  msd_hist %>% 
  ggplot(aes(x = fiscal_year, y = achievement)) +
    stat_halfeye(alpha = 0.5, side = "right") +
    facet_wrap(~indicator) +
    scale_y_continuous(limits = c(0, 2.5), oob = squish)
  
  
  msd_hist %>% 
    mutate(ou_order = reorder_within(operatingunit, achievement, indicator)) %>% 
    ggplot(aes(x = fiscal_year, y = ou_order, fill = achievement)) +
    geom_tile(color = "white") + facet_wrap(~indicator, scales = "free") +
    scale_y_reordered() +
    scale_fill_viridis_c(direction = -1, option = "D", alpha = 0.5,
                         oob = squish, limits = c(0, 2)) +
    si_style_nolines()
  

# REPEAT for SOUTH AFRICA SPECIFIC ----------------------------------------
  msd_18_SA <- 
    read_msd(file.path(merdata, "MER_Structured_Dataset_PSNU_IM_FY17-18_20181221_v2_1_South Africa.zip")) %>% 
    filter(fundingagency == "USAID",
           operatingunit == "South Africa",
           indicator %in% indic_list,
           standardizeddisaggregate == "Total Numerator")
  
  msd_18_SA_ach <- 
    msd_18_SA %>% 
    calc_ach(operatingunit, indicator, fiscal_year, psnu)  
  
  
  msd_21_SA <- 
    read_msd(file.path(merdata, "MER_Structured_Datasets_PSNU_IM_FY19-21_20210212_v1_1_South Africa.zip")) %>%
    filter(fundingagency == "USAID",
           operatingunit == "South Africa",
           indicator %in% indic_list,
           standardizeddisaggregate == "Total Numerator")
  
  msd_21_SA_ach <- 
    msd_21_SA %>% 
    calc_ach(operatingunit, indicator, fiscal_year, psnu)
  
  
  msd_hist_psnu <- 
    bind_rows(msd_18_SA_ach, msd_21_SA_ach) %>% 
    get_ach_shares()
  

  msd_hist_psnu %>% 
    annual_summary(fiscal_year, indicator, psnu) %>% 
    pivot_wider(names_from = fiscal_year, 
                values_from = pct) %>% 
    prinf()

  
  
  # Do PSNU numbers aggregate up to USAID totals? NO!? YIKES.
  ach_psnu_collapse <- msd_hist_psnu %>% 
    annual_summary(fiscal_year, indicator) %>% 
    mutate(aggregator = "psnu")
  
 ach_ou_collapse <-  msd_hist_SA %>% 
    filter(operatingunit == "South Africa") %>% 
    annual_summary(fiscal_year, indicator) %>% 
   mutate(aggregator = "ou")
    
 bind_rows(ach_psnu_collapse, ach_ou_collapse) %>% 
   filter(fiscal_year != 2016) %>% 
   ggplot(aes(x = fiscal_year, y = pct, fill = aggregator, group = aggregator)) +
   geom_col(aes(y = 0.5), fill = grey10k, alpha = 0.75) +
   geom_col(data = . %>% filter(fiscal_year != 2019), position = position_dodge2(width = 0.5), alpha = 0.5) +
   geom_col(data = . %>% filter(fiscal_year == 2019), position = position_dodge2(width = 0.5)) +
   facet_wrap(~indicator, scale = "free_y") +
   scale_y_continuous(labels = percent) +
   scale_fill_manual(values = c(denim, golden_sand)) +
   si_style_ygrid() +
   labs(x = "Fiscal Year", y = "OU Achievement (results/targets)", title = "South Africa PSNU X IM data does not match the OU X IM achievement results",
        caption = "Source: MER_Structured_Datasets_PSNU_IM_FY19-21_20210212_v1_1_South Africa.zip & MER_Structured_Datasets_OU_IM_FY19-21_20210212_v1_1.zip")
   
 si_save(file.path(images, "DATIM_SouthAfrica_ticket.png"))
  

 
 # SPINDOWN ============================================================================

