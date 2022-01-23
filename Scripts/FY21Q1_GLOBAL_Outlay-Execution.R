# PROJECT:  FY21Q1 Review
# AUTHOR:   B. Kasdan | USAID
# PURPOSE:  USAID Outlay Trends
# LICENSE:  MIT
# DATE:     2021-03-08
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
library(readxl)
library(here)
  




# IMPORT ------------------------------------------------------------------

  #import

df_outlay <- read_excel("Datasets/COP21 Outlays.xlsx", 
                        col_types = c("text", "numeric", "numeric"))
View(df_outlay)
  

# MUNGE -------------------------------------------------------------------
#create outlay %
  df_oe <- df_outlays %>% 
   
    mutate(agg_outlays = na_if(agg_outlays, 0),
           out_rate = agg_outlays/`COP_planning _levels`)



# PLOT --------------------------------------------------------------------

  df_oe %>% 
    ggplot(aes(FY_Quarter, `COP_planning _levels`)) +
    geom_col(fill = "#e6e6e6") +
    geom_col(aes(y = agg_outlays), fill = "#2057a7", na.rm = TRUE) +
    geom_errorbar(aes(x = FY_Quarter, ymin = `COP_planning _levels`, ymax =`COP_planning _levels`),
                  color = "#808080") +
    geom_hline(yintercept = 0, color = "#808080") +
    geom_hline(yintercept = seq(1.0e9, 2.0e9, 1.0e9), color = "white", linetype = "dashed") +
    scale_x_discrete(expand = c(.05, .05)) + 
    scale_y_continuous(labels = unit_format(.1, unit = "B", scale = 1e-9)) +
    labs(x = NULL, y = NULL,
         subtitle = "USAID Outlay Execution (USD)",
         caption = "Outlay amount against COP Budget total
         Source: Phoenix and WCF data as of March 4, 2021") +
    si_style_nolines()
  ggsave("Images/OutlayExecution.png")

# EXPORT ------------------------------------------------------------------

  
  ggsave("OutlayExecution.png", width=7.17, height=4.22, units="in")
  si_save("Images/OutlayExecution.png", width = 7.17, height = 4.22)
  