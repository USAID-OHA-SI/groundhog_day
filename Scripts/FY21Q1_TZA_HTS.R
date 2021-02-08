## PROJECT: FY21Q1 REVIEW
## AUTHOR:  A.Chafetz | USAID
## PURPOSE: Cascade Partner Review - HTS
## LICENSE: MIT
## DATE:    2021-02-08
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



# MUNGE -------------------------------------------------------------------

  df_hts <- df %>%
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
      mutate(across(c(mech_code, pre_rgnlztn_hq_mech_code), as.character),
             fiscal_year = as.integer(fiscal_year),
             mod_grp = case_when(str_detect(modality, "Index") ~ "Index", 
                                      str_detect(modality, "Mod") ~ "Community",
                                      TRUE ~ "PITC")) %>% 
      group_by(fiscal_year, primepartner, snu1, indicator, mod_grp) %>% 
      summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      spread(period_type, val)
    
   df_hts_tot <-  df_hts %>% 
      mutate(mod_grp = "Total") %>% 
      group_by(period, primepartner, snu1, indicator, mod_grp) %>% 
      summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
      ungroup()
   
   df_hts <- df_hts %>% 
     filter(mod_grp != "Community") %>% 
     bind_rows(df_hts_tot) %>% 
     mutate(mod_grp = factor(mod_grp, c("Total", "Index", "PITC")))
        
# FY21 Q1 ACHIEVEMENT -----------------------------------------------------

   df_achv <- df_hts %>% 
     filter(period == "FY21") %>%
     group_by(period, primepartner, indicator, mod_grp) %>% 
     summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
     ungroup() %>% 
     mutate(achv = cumulative/targets,
            partner_combo = glue("{primepartner} [{comma(cumulative)}/{comma(targets)}]"))

    df_achv %>% 
      ggplot(aes(achv, fct_reorder(partner_combo, targets, sum))) +
      geom_linerange(aes(xmin = 0, xmax = achv), size = .8, color = scooter) +
      geom_vline(xintercept = 0, size = 1, color = "#505050") +
      geom_blank(aes(x = achv * 1.1)) +
      geom_point(shape = 21, fill = "white", stroke = 1,
                 size = 6, color = scooter) +
      geom_text(aes(label = percent(achv, 1)),
                family = "Source Sans Pro", color = "#505050", size = 2) +
      facet_wrap(~indicator + mod_grp, scales = "free_y",  labeller = label_wrap_gen(multi_line=FALSE)) +
      scale_x_continuous(labels = percent,
                         expand = c(.05, .05)) +
      labs(x = NULL, y = NULL, 
           caption = "Index (facility and community); PITC is all non-index, facility tests
           Source: FY21Q1 PSNUxIM Genie [2021-02-05]") +
      si_style_xgrid() +
      theme(axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            panel.spacing = unit(.5, "lines")
            )
    
    si_save("Images/FY21Q1_TZA_HTS-Achv.png",
            width = 9.32, height = 3.74)
     

# FY21 Q1 ACHIEVEMENT BY REGION -------------------------------------------

    
    df_achv_snu <- df_hts %>% 
      filter(period == "FY21") %>%
      group_by(period, snu1, primepartner, indicator, mod_grp) %>% 
      summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
      ungroup()%>% 
      mutate(achv = cumulative/targets,
             targets_total = ifelse(mod_grp == "Total", targets, 0), 
             primepartner = fct_reorder(primepartner, targets, sum, .desc = TRUE))
    
    viz_achv <- function(ind){
      df_achv_snu %>% 
        filter(primepartner != "THPS",
               indicator == {{ind}}) %>% 
        mutate(across(c(targets, cumulative), ~ ./1000)) %>% 
        ggplot(aes(y = reorder_within(snu1, targets_total, primepartner, sum))) +
        geom_col(aes(x = targets), fill = trolley_grey_light) +
        geom_col(aes(x = cumulative), fill = scooter, alpha = .8) +
        geom_text(aes(x = cumulative, label = percent(achv, 1)),
                  family = "Source Sans Pro", color = "#505050", size = 2, hjust = -.2) +
        facet_grid(primepartner ~ mod_grp, scales = "free_y", space = "free_y") +
        geom_vline(xintercept = 0, size = 1, color = "#505050") +
        scale_y_reordered() +
        scale_x_continuous(label = comma_format(1), expand = c(.005, .005)) +
        labs(x = "thousands of tests", y = NULL, 
             caption = "Index (facility and community); PITC is all non-index, facility tests
           Source: FY21Q1 PSNUxIM Genie [2021-02-05]") +
        si_style_xgrid() +
        theme(axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 8),
              strip.text.y = element_text(size = 9),
              axis.title.x = element_text(size = 8),
              panel.spacing = unit(.5, "lines"))
      
      name <- glue("FY21Q1_TZA_{ind}-Achv-Region.png")
      
      si_save(name, path = "Images/",
              width = 9.32, height = 3.74)
    }
   
    
    walk(c("HTS_TST", "HTS_TST_POS"), viz_achv)
    

# QUARTERLY TRENDS --------------------------------------------------------

    df_trends <- df %>%
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
      mutate(across(c(mech_code, pre_rgnlztn_hq_mech_code), as.character),
             fiscal_year = as.integer(fiscal_year),
             mod_grp = case_when(str_detect(modality, "Index") ~ "Index", 
                                 str_detect(modality, "Mod") ~ "Community",
                                 TRUE ~ "PITC")) %>% 
      group_by(fiscal_year, primepartner,  indicator, mod_grp) %>% 
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE)) %>% 
      ungroup()  %>% 
      pivot_longer(starts_with("qtr"), 
                   names_to = "period",
                   values_to = "results") %>% 
      unite(period, c(fiscal_year, period), sep = "") %>% 
      mutate(period = str_replace(period, "20", "FY") %>% str_replace(., "qtr", "Q"))
    
    df_trends_total <- df_trends %>% 
      mutate(mod_grp = "Total") %>% 
      group_by(period, primepartner, indicator, mod_grp) %>% 
      summarise(across(c(targets, results), sum, na.rm = TRUE)) %>% 
      ungroup()
      
    df_trends <- df_trends %>% 
      filter(mod_grp != "Community") %>% 
      bind_rows(df_trends_total) %>% 
      mutate(results = na_if(results, 0),
             results = ifelse(indicator == "TX_NET_NEW" & period == "FY21Q2", NA, results),
             fiscal_year = str_sub(period, 3,4) %>% as.integer,
             quarter = str_sub(period, -1) %>% as.integer,
             targets_qtrly = ifelse(indicator == "TX_CURR", targets, targets * .25),
             targets_qtrly = na_if(targets_qtrly, 0),
             ptnr_fy = glue("{primepartner} FY{fiscal_year}"),
             mod_grp = factor(mod_grp, c("Total", "Index", "PITC")),
             primepartner = fct_reorder(primepartner, targets, max, .desc = TRUE))

    viz_trends <- function(mod){
      df_trends %>% 
        filter(mod_grp == {{mod}},
               primepartner %in% c("Deloitte", "EGPAF", "FHI360/EPIC", "Baylor")) %>% 
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
      
      name <- glue("FY21Q1_TZA_{mod}-Qtrly-Trends-Total.png")
      
      si_save(name, path = "Images",
              width = 9.32, height = 3.74)
      
    }
    
    walk(c("Total", "Index", "PITC"), viz_trends)


# QUARTERLY TRENDS BY REGION ----------------------------------------------

    df_trends_region <- df %>%
      filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Modality/Age/Sex/Result",
             snu1 != "_Military Tanzania",
             primepartner %in% c("Deloitte", "EGPAF", "FHI360/EPIC", "Baylor")) %>% 
      mutate(across(c(mech_code, pre_rgnlztn_hq_mech_code), as.character),
             fiscal_year = as.integer(fiscal_year),
             mod_grp = case_when(str_detect(modality, "Index") ~ "Index", 
                                 str_detect(modality, "Mod") ~ "Community",
                                 TRUE ~ "PITC")) %>% 
      group_by(fiscal_year, snu1, primepartner,  indicator, mod_grp) %>% 
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE)) %>% 
      ungroup()  %>% 
      pivot_longer(starts_with("qtr"), 
                   names_to = "period",
                   values_to = "results") %>% 
      unite(period, c(fiscal_year, period), sep = "") %>% 
      mutate(period = str_replace(period, "20", "FY") %>% str_replace(., "qtr", "Q"))
    
    df_trends_region_total <- df_trends_region %>% 
      mutate(mod_grp = "Total") %>% 
      group_by(period, snu1, primepartner, indicator, mod_grp) %>% 
      summarise(across(c(targets, results), sum, na.rm = TRUE)) %>% 
      ungroup()
    
    df_trends_region <- df_trends_region %>% 
      filter(mod_grp != "Community") %>% 
      bind_rows(df_trends_region_total) %>% 
      mutate(results = na_if(results, 0),
             results = ifelse(indicator == "TX_NET_NEW" & period == "FY21Q2", NA, results),
             fiscal_year = str_sub(period, 3,4) %>% as.integer,
             quarter = str_sub(period, -1) %>% as.integer,
             targets_qtrly = ifelse(indicator == "TX_CURR", targets, targets * .25),
             targets_qtrly = na_if(targets_qtrly, 0),
             ptnr_fy = glue("{primepartner} FY{fiscal_year}"),
             mod_grp = factor(mod_grp, c("Total", "Index", "PITC")),
             targets_fy21 = ifelse(fiscal_year == 21 & targets > 0, targets, 0),
             ptnr_reg_fy = glue("{snu1} {primepartner} {fiscal_year}"),
             ptnr_reg =  glue("{snu1} - {primepartner}"),
             primepartner = fct_reorder(primepartner, targets, max, .desc = TRUE),
             ptnr_reg = fct_reorder(ptnr_reg, targets_fy21, max, .desc = TRUE))

    
    
    viz_trend_region <- function(ind, mod){
      
      df_trends_region %>% 
        filter(indicator == {{ind}},
               mod_grp == {{mod}}) %>% 
        group_by(ptnr_reg) %>% 
        filter(max(targets_fy21) > 0) %>% 
        ungroup() %>% 
        ggplot(aes(period, results, color = primepartner, group = ptnr_reg_fy)) +
        geom_blank(aes(y = results * 1.1)) +
        geom_blank(aes(y = 15)) +
        geom_hline(yintercept = 0, color = "#505050") +
        geom_path() +
        geom_path(aes(y = targets_qtrly), color = trolley_grey, linetype = "dashed") +
        geom_point() +
        facet_wrap( ~ ptnr_reg, 
                    scale = "free_y") +
        scale_y_continuous(label = comma_format(1)) +
        scale_x_discrete(breaks = c("FY20Q1", "FY21Q1")) +
        scale_color_manual(values = c(scooter, old_rose, moody_blue, genoa)) +
        labs(x = NULL, y = NULL, caption = "Source: FY21Q1 PSNUxIM Genie [2021-02-05]") +
        si_style_ygrid() +
        theme(legend.position = "none",
              panel.spacing = unit(.5, "lines"),
              axis.text.y = element_text(size = 7),
              axis.text.x = element_text(size = 8),
              strip.text = element_text(size = 8)
        )
      name <- glue("FY21Q1_TZA_{ind}-{mod}-Qtrly-Trends-Region.png")
      si_save(name, path = "Images",
              width = 9.32, height = 3.74)
    }
 
    
    walk2(distinct(df_trends_region, indicator, mod_grp) %>% pull(indicator),
          distinct(df_trends_region, indicator, mod_grp) %>% pull(mod_grp),
          viz_trend_region)
          
   
    