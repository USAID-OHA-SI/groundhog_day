##  PROJECT: groundhogday
##  AUTHOR:  Tim Essam, Aaron Chafetz | USAID
##  PURPOSE: munge historic targets to fy21 targets, viz
##  LICENCE: MIT
##  DATE:    2020-04-01
##  UPDATE:  2021-07-14
##  NOTE:    adapted from groundhogday/FY20Q1_targetshifts.R


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
  library(vroom)
  library(ggrepel)


# GLOBAL OPTIONS ----------------------------------------------------------

  data_in <- "Data"
  data_out <- "Dataout"
  viz_folder <- "Graphics"

  authors <- c("Aaron Chafetz", "Tim Essam")
  
  load_secrets()
  
  # Indicators of focus
    ind_grp1 <- c("HTS_TST_POS", "TX_NEW", "TX_CURR")
    ind_grp2 <- c("VMMC_CIRC", "PrEP_NEW")
    ind_grp3 <- c("OVC_SERV", "KP_PREV")
    indc <- c(ind_grp1, ind_grp2, ind_grp3)
  
  #limit to changes in targets +/- x%
  delta_threshold <- .1
  
# IMPORT ------------------------------------------------------------------
  
    df_msd <- si_path() %>% 
      return_latest("OU_IM") %>% 
      read_rds()   
    
  #compiled data packs by ER team
    #https://drive.google.com/drive/u/0/folders/1CD3Asd5Uror4YEGUpcXzv-ah0_UV3dkU
    df_dp <- read_csv("../../../Downloads/Datapack_Master_06_09.csv",
                      col_types = c(.default = "c", fiscal_year = "i", targets = "d"))

# MUNGE MSD ---------------------------------------------------------------

  #filter dataset to select indicators
    df_msd <- df_msd %>% 
      filter(indicator %in% indc,
             mech_name != "Dedup",
             standardizeddisaggregate == "Total Numerator") 
    
  #resolve know issues
    df_msd <- df_msd %>% 
      resolve_knownissues()
  
  #limit to only necessary columns
    df_msd <- df_msd %>% 
      select(fiscal_year, fundingagency, operatingunit, indicator, targets)
  
# MUNGE DATA PACKS --------------------------------------------------------
  
  #filter dataset to select indicators (Drop dedup b/c not handled correctly in tameDP)
    df_dp <- df_dp %>% 
      filter(indicator %in% indc,
             !(indicator %in% c(ind_grp1, ind_grp2) & disagg == "KeyPop"),
             mech_code != "00000") %>% 
        select(fiscal_year, fundingagency, operatingunit, indicator, targets)
        

# JOIN DATA ---------------------------------------------------------------

  #join FY22 data onto MSDs
    df_targets <- bind_rows(df_msd, df_dp)


# MUNGE FULL DATASET ------------------------------------------------------

  #create a PEPFAR aggregate agency
    df_targets <- df_targets %>% 
      bind_rows(df_targets %>% mutate(fundingagency = "PEPFAR")) %>% 
      filter(fundingagency %in% c("USAID", "PEPFAR"))
    
  #create a global total
    df_targets <- df_targets %>% 
      bind_rows(df_targets %>% mutate(operatingunit = "Global"))
    
  #aggregate for PEPFAR and USAID targets totals
    df_targets <- df_targets %>% 
      group_by(fiscal_year, fundingagency, operatingunit, indicator) %>% 
      summarise(targets = sum(targets, na.rm = TRUE)) %>% 
      ungroup() 
    
  #reshape to create a target share
    df_targets <- df_targets %>% 
      pivot_wider(names_from = fundingagency,
                  names_glue = "targets_{tolower(fundingagency)}",
                  values_from = targets) %>% 
      mutate(agency_share = targets_usaid/targets_pepfar)
    
  #calculate the share change over time
    df_targets <- df_targets %>% 
      arrange(operatingunit, fiscal_year, indicator) %>% 
      group_by(indicator, operatingunit) %>% 
      mutate(share_diff = agency_share - lag(agency_share, n = 1, order_by = fiscal_year), 
             abs_share_diff = abs(share_diff)) %>% 
      ungroup()
    
  #Years
    yr_max <- max(df_targets$fiscal_year)
    yr_prior <- yr_max - 1 
    
  #limit change in the last year
    df_targets <- df_targets %>%
      filter(fiscal_year >= yr_prior)
    
  #only want to plot large diff = >10% change
    ou_ind_keep <- df_targets %>% 
      filter(fiscal_year == max(fiscal_year))  %>% 
      filter(abs_share_diff > delta_threshold) %>% 
      select(operatingunit, indicator) 
    
  #Keep global data as well
    ou_ind_keep <- ou_ind_keep %>% 
      bind_rows(expand.grid(operatingunit = "Global",
                            indicator = indc)) %>% 
      distinct(operatingunit, indicator)
    
  #limit using semi_join
    df_viz <- semi_join(df_targets, ou_ind_keep, by = c("operatingunit", "indicator")) 
    
  #adjust var ordering
    df_viz <- df_viz %>% 
      mutate(indicator = factor(indicator, indc))
    
  #global label
    df_viz <- df_viz %>% 
      mutate(usaid_share = case_when(operatingunit == "Global" ~ agency_share),
             placement = case_when(operatingunit == "Global" & fiscal_year == yr_max ~ -1,
                                   operatingunit == "Global" ~ 1),
             operatingunit = recode(operatingunit,
                                    "Democratic Republic of the Congo" = "DRC",
                                    "Western Hemisphere Region" = "WHR",
                                    "West Africa Region" = "WAR"),
             label_ou = if_else(fiscal_year == max(fiscal_year), operatingunit, NA_character_))
    

# PLOT --------------------------------------------------------------------

    # ind_sel <- ind_grp1
  
    plot_slope <- function(ind_sel, outpath = NULL){
    
      plot <-
        df_viz %>% 
        filter(indicator %in% {{ind_sel}}) %>% 
        ggplot(aes(x = fiscal_year, y = agency_share, group = operatingunit)) +
        geom_hline(yintercept = .5,  size = .25, colour = "gray80", linetype = "dashed") +
        geom_line(size = .5, colour = "gray80", na.rm = TRUE) +
        geom_point(aes(fill = agency_share), size = 5, shape = 21, colour = "#909090", stroke = 0.25, na.rm = TRUE) +
        geom_point(aes(y = usaid_share), size = 5, shape = 21, fill = "#909090", colour = "gray80", stroke = 0.25, na.rm = TRUE) +
        geom_text(aes(label = label_ou),
                  hjust = 0,
                  nudge_x = 0.1,
                  size = 3, color = "#505050",
                  family = "Source Sans Pro",
                  na.rm = TRUE) +
        geom_text(aes(y = usaid_share,label = percent(usaid_share, 1)),
                  size = 2, color = "white",
                  family = "Source Sans Pro",
                  na.rm = TRUE) +
        facet_wrap(~indicator, nrow = 1) +
        # scale_fill_viridis_c(label = scales::percent, direction = -1, option = "A") +
        scale_fill_si("moody_blues", discrete = FALSE) +
        scale_x_continuous(breaks = c(yr_prior, yr_max), limits = c(yr_prior-.25, yr_max + .5)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(x = NULL, y = NULL, 
             title = "GLOBALLY NO MAJOR SHIFT IN TARGET SHARES",
             subtitle = glue("Largest target gains/losses (+{percent(delta_threshold)}) displayed against USAID overall share"),
             caption = glue("Note: Only OUs with shifts of +{percent(delta_threshold)} between {yr_prior}-{str_sub(yr_max, -2)} targets are displayed; Target Dedups excluded; Excludes know target issues in FY21 
          Source: FY21Q2c MSD, COP21 Data Pack
          SI analytics: {paste(authors, collapse = '/')}
          USAID Agency for International Development")) +
        si_style() +
        theme(legend.position = "none",
              panel.grid.major.y = ggplot2::element_blank(),
              panel.spacing.x = unit(.75, "lines"),
              strip.text = element_text(face = "bold", size = 12),
              plot.subtitle = element_text(margin = margin(0, 0, 10, 0)))
      
    if(!is.null(outpath)){
      ind_sel <- str_remove_all(ind_sel, "_") %>% paste0(collapse = "_")
      ext <- ifelse(viz_folder == "Images", ".png", ".svg")
      ggsave(file.path(viz_folder, paste0("FY21Q2Review_targetshifts_slope_",ind_sel, ext)), 
            dpi = 330, width = 10, height = 5.66, scale = 1.2)
    }
    
    return(plot)
    
  }  
 

    viz_folder <- "Graphics"
  #testing
    plot_slope(c(ind_grp2, ind_grp3, ind_grp1))
    plot_slope(c(ind_grp2, ind_grp3, ind_grp1), viz_folder)
    
  #plot by group
    plot_slope(ind_grp1, viz_folder)
    plot_slope(ind_grp2, viz_folder)
    plot_slope(ind_grp3, viz_folder)
