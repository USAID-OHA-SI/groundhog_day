# PROJECT:  FY21Q3 Q. Review
# AUTHOR:   T. Essam | USAID
# PURPOSE:  Expansion of PrEP_NEW and q3 gaps
# LICENSE:  MIT
# DATE:     2021-10-05
# UPDATED: 

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
  library(ggtext)


# GLOBAL VARIABLES --------------------------------------------------------

  authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")
  merdata <- si_path("path_msd")
  source <- source_info()


# IMPORT ------------------------------------------------------------------

  df <- si_path() %>% 
    return_latest("OU_IM_FY19") %>% 
    read_msd()   
  
  df_arch <- si_path() %>% 
    return_latest("OU_IM_FY15") %>% 
    read_msd()

  curr_pd <- identifypd(df)

# MUNGE -------------------------------------------------------------------

  #bind archived + current MSD and filter for PrEP
  df_prep <- df %>%
    bind_rows(df_arch) %>% 
    filter(fundingagency == "USAID",
           indicator == "PrEP_NEW",
           standardizeddisaggregate == "Total Numerator",
           fiscal_year >= 2017)
  
  min_yr <- 17
  max_yr <- 22
  
  #curr fy prep (for viz title)
  prep_cum <- df_prep %>% 
    filter(fiscal_year >= identifypd(df, "year")) %>% 
    count(wt = cumulative)
  
  #count number of countries with PrEP
  df_cntry_cnt <- df_prep %>% 
    filter(cumulative > 0) %>% 
    distinct(fiscal_year, countryname) %>% 
    count(fiscal_year, name = "n_countries")
  
  #aggregate result to USAID level with targets
  df_prep_hist <- df_prep %>% 
    group_by(fiscal_year, fundingagency) %>% 
    summarise(across(matches("qtr|tar"), sum, na.rm = TRUE)) %>% 
    ungroup() %>% 
    reshape_msd() %>% 
    mutate(target = ifelse(period_type == "targets", value, NA_integer_),
           fy = substr(period, 1, 4)) %>% 
    group_by(fy) %>% 
    fill(., target, .direction = "down") %>% 
    ungroup() %>% 
    filter(period %ni% c(paste0("FY", min_yr:max_yr)))
  

# FULL LIST OF PERIODS ----------------------------------------------------

    curr_pd_num <- curr_pd %>% 
    gsub("FY", "", .) %>% 
    gsub("Q", ".", .) %>% 
    as.numeric()

    curr_fy <- substr(curr_pd, 3, 4) %>% as.numeric() 

    full_pds <- expand_grid(fiscal_year = c(min_yr:max_yr),
                            quarter = c(1:4)) %>% 
      unite(period, c(fiscal_year, quarter), sep = ".") %>% 
      mutate(period = as.numeric(period)) %>% 
      mutate(period = period %>% 
               paste0("FY", .) %>% 
               str_replace("\\.", "Q"),
             period_num = row_number())

    

# VIZZZZZZ ----------------------------------------------------------------

    df_prep_hist %>% 
      ggplot(aes(period, value, group = fundingagency)) + 
      geom_area(fill = scooter, color = scooter, alpha = .2, size = 1, na.rm = TRUE) +
      geom_vline(xintercept = fy_start, color = "white", 
                 size = .9, linetype = "dotted") +
      geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.5, na.rm = TRUE) +
      scale_y_continuous(label = clean_number, position = "right", expand = c(.01, .01)) +
      scale_x_discrete(breaks = pd_breaks, labels = str_remove(pd_breaks, "FY[:digit:]{2}(?!Q1)")) +
      labs(x = NULL, y = NULL, 
           title = glue("USAID has initiated {clean_number(prep_cum, 0)} \\
                      onto PrEP this year across \\
                      {filter(df_cntry_cnt, fiscal_year == max(fiscal_year)) %>% pull()} \\
                      countries, up from {filter(df_cntry_cnt, fiscal_year == 2017) %>% pull()} \\
                      countries in 2017") %>% toupper,
           subtitle = "Pre-Exposure Prophylaxis (PrEP) Quarterly Results",
           caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
      si_style_ygrid()    
    
