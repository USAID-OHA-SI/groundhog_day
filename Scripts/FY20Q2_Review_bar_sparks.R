##  PROJECT: Q2 review target analysis
##  AUTHOR:  tessam | USAID
##  PURPOSE: Tiny bar sparks for summary graphic. 
##  LICENCE: MIT
##  DATE:    2020-06-15
##  UPDATE:

#Dependancies----------------------------------------------------------

library(tidyverse)
library(vroom)
library(scales)
library(extrafont)
library(ggrepel)
library(glitr)
library(ICPIutilities)
library(here)
library(glamr)

# GLOBALS -----------------------------------------------------------------

data_in <- "Data"
data_out <- "Dataout"
viz_out <- "Images"

# Plot to create tiny progress graphs in bar form
# Requires a long data frame named df_long
bar_spark <- function(ind) {
  viz <- df_long %>% 
    filter(indicator == {{ind}}, Agency == "USAID") %>% 
    ggplot() + 
    geom_col(aes(Agency, target), fill = "#C0C0C0") +
    geom_col(aes(Agency, share), fill = "#e04745") +
    coord_flip() + 
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#f3f3f3",
                                         color = "#f3f3f3"),
    )
  
  ggsave(file.path(viz_out, paste0("Q1_sparks", {{ind}}, "_tgt_Q2", ".png")),
         plot = viz, dpi = 330, width = 1.25, height = 0.1)
  
  print(viz)
} 


ind_list <-  c("HTS_TST_POS", "TX_CURR", "TB_PREV", "TX_NET_NEW")

sum_indic <- function(df) {
  df %>% 
    group_by(fiscal_year, indicator) %>% 
    summarise(val = sum(cumulative, na.rm = TRUE)) %>% 
    ungroup()
}



# LOAD AND MUNG -----------------------------------------------------------

# Load most recent MSD
df <- read_msd(file.path(here(data_in, "MER_Structured_Datasets_OU_IM_FY18-20_20200605_v1_1.txt")))

df_pepfar <- 
  df %>% 
  filter(indicator %in% ind_list, 
         disaggregate == "Total Numerator", 
         fiscal_year == 2020) %>% 
  sum_indic() 

df_pepfar_mmd <- 
  df %>%  
  filter(indicator == "TX_CURR", 
         disaggregate == "Age/Sex/ARVDispense/HIVStatus",
         otherdisaggregate %in% c("ARV Dispensing Quantity - 3 to 5 months", "ARV Dispensing Quantity - 6 or more months")) %>% 
  sum_indic() %>% 
  mutate(indicator = "TX_MMD3") 

df_pepfar_all <- rbind(df_pepfar, df_pepfar_mmd) %>% 
  rename(PEPFAR = val)


df_usaid <- df %>% 
  filter(fundingagency == "USAID", 
         indicator %in% ind_list, 
         disaggregate == "Total Numerator", 
         fiscal_year == 2020) %>% 
  sum_indic() 


df_usaid_mmd <- df %>% 
  filter(fundingagency == "USAID",
         indicator == "TX_CURR", 
         disaggregate == "Age/Sex/ARVDispense/HIVStatus",
         otherdisaggregate %in% c("ARV Dispensing Quantity - 3 to 5 months", "ARV Dispensing Quantity - 6 or more months"))  %>% 
  sum_indic() %>% 
  mutate(indicator = "TX_MMD3")

df_usaid_all <- rbind(df_usaid, df_usaid_mmd) %>% 
  rename(USAID = val) %>% 
  left_join(., df_pepfar_all) %>% 
  mutate(share = USAID / PEPFAR,
         target = 1)

remove(df_pepfar, df_pepfar_all, df_pepfar_mmd, df_usaid, df_usaid_mmd)

df_long <- 
  df_usaid_all %>% pivot_longer(cols = USAID:PEPFAR,
                                names_to = "Agency",
                                values_to = "Value")


# Preview plot
bar_spark(ind ="TX_CURR")

#output for each ind
unique(df_long$indicator) %>% 
  walk(bar_spark)