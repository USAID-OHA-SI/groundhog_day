## PROJECT: FY21Q1 REVIEW
## AUTHOR:  A.Chafetz | USAID
## PURPOSE: Cascade Partner Review
## LICENSE: MIT
## DATE:    2021-02-05
## UPDATED: 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(googledrive)
library(readxl)
library(glamr)
library(glitr)
library(scales)
library(extrafont)
library(ICPIutilities)
library(glue)
library(tidytext)

# GLOBALS -----------------------------------------------------------------

load_secrets()

genie_file <- as_id("1PSYuXn3yLi6zic2TtnNPaP7A_ZvY65tI")
filepath_local <- drive_get(genie_file) %>% pull(name) %>% file.path("Data", .)


# IMPORT DATA -------------------------------------------------------------

  #download from drive if needed
    if(file.exists(filepath_local) == FALSE)
      drive_download(genie_file, path  = file.path("Data", filename))
  
  #import
    df <- read_excel(filepath_local, sheet = "Data")


# FY21 Q1 ACHIEVEMENT -----------------------------------------------------

    df_achv <- df %>% 
      filter(fiscal_year == 2021,
             indicator %in% c("TX_CURR", "TX_NEW"),
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(primepartner, indicator) %>% 
      summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(achv = cumulative/targets,
             partner_combo = glue("{primepartner} [{comma(cumulative)}/{comma(targets)}]"))

    df_achv %>% 
      ggplot(aes(achv, fct_reorder(partner_combo, targets, sum))) +
      geom_vline(xintercept = 0, size = 1, color = "#505050") +
      geom_linerange(aes(xmin = 0, xmax = achv), size = 1, color = scooter) +
      geom_point(shape = 21, fill = "white", stroke = 1.5,
                 size = 8, color = scooter) +
      geom_text(aes(label = percent(achv, 1)),
                family = "Source Sans Pro", color = "#505050", size = 3) +
      facet_wrap(~indicator, scales = "free_y") +
      scale_x_continuous(labels = percent) +
      labs(x = NULL, y = NULL, caption = "Source: FY21Q1 PSNUxIM Genie [2021-02-05]") +
      si_style()
    
    si_save("Images/FY21Q1_TZA_TX-Achv.png",
            width = 9.32, height = 3.74)
     
    
    df_achv_snu <- df %>% 
      filter(fiscal_year == 2021,
             indicator %in% c("TX_CURR", "TX_NEW"),
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(primepartner, snu1, indicator) %>% 
      summarise(across(c(targets, cumulative), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(achv = cumulative/targets,
             primepartner = fct_reorder(primepartner, targets, sum, .desc = TRUE))
    
    
    df_achv_snu %>% 
      filter(primepartner != "THPS") %>% 
      ggplot(aes(y = reorder_within(snu1, targets, primepartner, sum))) +
      geom_col(aes(x = targets), fill = trolley_grey_light) +
      geom_col(aes(x = cumulative), fill = scooter, alpha = .8) +
      geom_text(aes(x = cumulative, label = percent(achv, 1)),
                family = "Source Sans Pro", color = "#505050", size = 3, hjust = -.2) +
      facet_grid(primepartner ~ indicator, scales = "free", space = "free_y") +
      geom_vline(xintercept = 0, size = 1, color = "#505050") +
      scale_y_reordered() +
      scale_x_continuous(label = comma, expand = c(.005, .005)) +
      labs(x = NULL, y = NULL, caption = "Source: FY21Q1 PSNUxIM Genie [2021-02-05]") +
      si_style_xgrid() +
      theme(panel.spacing = unit(1, "lines"))
    
    si_save("Images/FY21Q1_TZA_TX-Achv-Region.png",
            width = 9.32, height = 3.74)
          

# QUARTERLY TRENDS --------------------------------------------------------

    df_trends <- df %>% 
      filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
             standardizeddisaggregate == "Total Numerator", 
             primepartner != "THPS") %>% 
      group_by(primepartner, indicator, fiscal_year) %>% 
      summarise(across(c(targets, starts_with("q")), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      pivot_longer(starts_with("qtr"), 
                   names_to = "period",
                   values_to = "results") %>% 
      unite(period, c(fiscal_year, period), sep = "") %>% 
      mutate(period = str_replace(period, "20", "FY") %>% str_replace(., "qtr", "Q"),
             results = na_if(results, 0),
             results = ifelse(indicator == "TX_NET_NEW" & period == "FY21Q2", NA, results),
             fiscal_year = str_sub(period, 3,4) %>% as.integer,
             quarter = str_sub(period, -1) %>% as.integer,
             # targets_qtrly = ifelse(indicator == "TX_CURR", targets, targets * quarter * .25),
             targets_qtrly = ifelse(indicator == "TX_CURR", targets, targets * .25),
             targets_qtrly = na_if(targets_qtrly, 0),
             ptnr_fy = glue("{primepartner} FY{fiscal_year}"),
             indicator = factor(indicator, c("TX_CURR", "TX_NEW", "TX_NET_NEW")),
             primepartner = fct_reorder(primepartner, targets, max, .desc = TRUE))


    df_trends %>% 
      ggplot(aes(period, results, color = primepartner, group = ptnr_fy)) +
      geom_hline(yintercept = 0, color = "#505050") +
      geom_path() +
      geom_path(aes(y = targets_qtrly), color = trolley_grey, linetype = "dashed") +
      geom_point() +
      facet_grid(indicator ~ primepartner, 
                 scale = "free_y") +
      scale_y_continuous(label = comma) +
      scale_x_discrete(breaks = c("FY20Q1", "FY21Q1")) +
      scale_color_si("scooter") +
      labs(x = NULL, y = NULL, caption = "Source: FY21Q1 PSNUxIM Genie [2021-02-05]") +
      si_style_ygrid() +
      theme(legend.position = "none")
    
    
    si_save("Images/FY21Q1_TZA_TX-Qtrly-Trends.png",
            width = 9.32, height = 3.74)
    
    
    
    
    df_trends_region <- df %>% 
      filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
             standardizeddisaggregate == "Total Numerator", 
             primepartner %in% c("Deloitte", "EGPAF", "Baylor")) %>% 
      group_by(primepartner, indicator, snu1, fiscal_year) %>% 
      summarise(across(c(targets, starts_with("q")), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      pivot_longer(starts_with("qtr"), 
                   names_to = "period",
                   values_to = "results") %>% 
      unite(period, c(fiscal_year, period), sep = "") %>% 
      mutate(period = str_replace(period, "20", "FY") %>% str_replace(., "qtr", "Q"),
             results = na_if(results, 0),
             results = ifelse(indicator == "TX_NET_NEW" & period == "FY21Q2", NA, results),
             fiscal_year = str_sub(period, 3,4) %>% as.integer,
             quarter = str_sub(period, -1) %>% as.integer,
             # targets_qtrly = ifelse(indicator == "TX_CURR", targets, targets * quarter * .25),
             targets_qtrly = ifelse(indicator == "TX_CURR", targets, targets * .25),
             targets_qtrly = na_if(targets_qtrly, 0),
             targets_fy21 = ifelse(fiscal_year == 21, targets, 0),
             ptnr_reg_fy = glue("{snu1} {primepartner} FY{fiscal_year}"),
             ptnr_reg =  glue("{snu1} - {primepartner}"),
             indicator = factor(indicator, c("TX_CURR", "TX_NEW", "TX_NET_NEW")),
             primepartner = fct_reorder(primepartner, targets, max, .desc = TRUE),
             ptnr_reg = fct_reorder(ptnr_reg, targets_fy21, max, .desc = TRUE))
    
    
    
    df_trends_region %>% 
      filter(indicator == "TX_CURR") %>% 
      ggplot(aes(period, results, color = primepartner, group = ptnr_reg_fy)) +
      geom_hline(yintercept = 0, color = "#505050") +
      geom_path() +
      geom_path(aes(y = targets_qtrly), color = trolley_grey, linetype = "dashed") +
      geom_point() +
      facet_wrap( ~ ptnr_reg, 
                 scale = "free_y") +
      scale_y_continuous(label = comma) +
      scale_x_discrete(breaks = c("FY20Q1", "FY21Q1")) +
      scale_color_manual(values = c(scooter, old_rose, genoa)) +
      labs(x = NULL, y = NULL, caption = "Source: FY21Q1 PSNUxIM Genie [2021-02-05]") +
      si_style_ygrid() +
      theme(legend.position = "none",
            panel.spacing = unit(.5, "lines")
            )
    
    si_save("Images/FY21Q1_TZA_TX_CURR-Qtrly-Trends-Region.png",
            width = 9.32, height = 3.74)
    
    
    df_trends_region %>% 
      filter(indicator == "TX_NEW") %>% 
      ggplot(aes(period, results, color = primepartner, group = ptnr_reg_fy)) +
      geom_hline(yintercept = 0, color = "#505050") +
      geom_path() +
      geom_path(aes(y = targets_qtrly), color = trolley_grey, linetype = "dashed") +
      geom_point() +
      facet_wrap( ~ ptnr_reg, 
                  scale = "free_y") +
      scale_y_continuous(label = comma) +
      scale_x_discrete(breaks = c("FY20Q1", "FY21Q1")) +
      scale_color_manual(values = c(scooter, old_rose, genoa)) +
      labs(x = NULL, y = NULL, caption = "Source: FY21Q1 PSNUxIM Genie [2021-02-05]") +
      si_style_ygrid() +
      theme(legend.position = "none",
            panel.spacing = unit(.5, "lines"))
    
    si_save("Images/FY21Q1_TZA_TX_NEW-Qtrly-Trends-Region.png",
            width = 9.32, height = 3.74)
    
    df_trends_region %>% 
      filter(indicator == "TX_NET_NEW") %>% 
      ggplot(aes(period, results, color = primepartner, group = ptnr_reg_fy)) +
      geom_hline(yintercept = 0, color = "#505050") +
      geom_path() +
      geom_path(aes(y = targets_qtrly), color = trolley_grey, linetype = "dashed") +
      geom_point() +
      facet_wrap( ~ ptnr_reg, 
                  scale = "free_y") +
      scale_y_continuous(label = comma_format(1)) +
      scale_x_discrete(breaks = c("FY20Q1", "FY21Q1")) +
      scale_color_manual(values = c(scooter, old_rose, genoa)) +
      labs(x = NULL, y = NULL, caption = "Source: FY21Q1 PSNUxIM Genie [2021-02-05]") +
      si_style_ygrid() +
      theme(legend.position = "none",
            panel.spacing = unit(.5, "lines"))
    
    si_save("Images/FY21Q1_TZA_TX_NN-Qtrly-Trends-Region.png",
            width = 9.32, height = 3.74)
    