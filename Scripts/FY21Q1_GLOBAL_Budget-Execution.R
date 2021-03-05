# PROJECT:  FY21Q1 Review
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  USAID Budget Execution Trends
# LICENSE:  MIT
# DATE:     2021-03-05
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(ICPIutilities)
  library(googlesheets4)
  


# GLOBAL VARIABLES --------------------------------------------------------

  sch_partner_sheet <- as_sheets_id("1mCJWDo4FPW2cQ6LpbsSjtnRjT7sUpPEOqxfT2zQNo64")

# LOAD CREDENTIALS --------------------------------------------------------

  load_secrets()

# IMPORT ------------------------------------------------------------------

  #import
  df_fsd <- si_path() %>% 
      return_latest("Financial") %>% 
      read_msd()   
  
  #list of GHSC mechanism to exclude
  sch_list <- read_sheet(sch_partner_sheet) %>% 
    pull(`Mech Code`)

# MUNGE -------------------------------------------------------------------

  #aggregate after filtering to USAID, excluding M&O and SC 
  df_be <- df_fsd %>% 
    filter(fundingagency == "USAID",
           record_type != "Management and Operations",
           !mech_code %in% sch_list) %>% 
    group_by(fiscal_year) %>% 
    summarise(across(c(cop_budget_total, expenditure_amt), sum, na.rm = TRUE)) %>% 
    ungroup()
  
  #clean up
  df_be <- df_be %>% 
    mutate(display_year = glue("FY{str_sub(fiscal_year, 3,4)}"),
           expenditure_amt = na_if(expenditure_amt, 0),
           ex_rate = expenditure_amt/cop_budget_total)

# PLOT --------------------------------------------------------------------

  df_be %>% 
    ggplot(aes(display_year, cop_budget_total)) +
    geom_col(fill = trolley_grey_light) +
    geom_col(aes(y = expenditure_amt), fill = genoa_light, na.rm = TRUE) +
    geom_errorbar(aes(x = display_year, ymin = cop_budget_total, ymax =cop_budget_total),
                  color = trolley_grey) +
    geom_hline(yintercept = 0, color = trolley_grey) +
    geom_hline(yintercept = seq(.5e9, 1.5e9, .5e9), color = "white", linetype = "dashed") +
    scale_x_discrete(expand = c(.05, .05)) + 
    scale_y_continuous(labels = unit_format(.1, unit = "B", scale = 1e-9)) +
    labs(x = NULL, y = NULL,
         subtitle = "USAID Budget Execution (USD)",
         caption = "Expenditure amount against COP Budget total
         Excludes M&O and commodities
         Source: FY20Q1i FSD") +
    si_style_nolines()
  

# EXPORT ------------------------------------------------------------------

  si_save("Images/BudgetExecution.png", width = 7.17, height = 4.22)
  