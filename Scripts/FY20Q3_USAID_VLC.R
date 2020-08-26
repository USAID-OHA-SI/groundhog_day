## PROJECT: FY20Q3 REVIEW
## AUTHOR:  A.Chafetz | USAID
## PURPOSE: identify shortfalls in VLC (VLS)
## LICENSE: MIT
## DATE:    2020-08-26
## UPDATED: 

library(tidyverse)
library(scales)
library(extrafont)
library(glitr)
library(ICPIutilities)


df <- list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds()

df_vl <- df %>% 
  filter(fundingagency == "USAID",
         indicator %in% c("TX_CURR", "TX_PVLS"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
         !operatingunit %in% c("Ukraine", "South Africa")) %>% 
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
         operatingunit = recode(operatingunit, 
                                "Western Hemisphere Region" = "WHR",
                                "West Africa Region" = "WAR",
                                "Democratic Republic of the Congo" = "DRC",
                                "Dominican Republic" = "DR"
                                )) %>%
  group_by(operatingunit, fiscal_year, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  select(-period_type) %>% 
  spread(indicator, val)

df_vl <- df_vl %>%
  group_by(operatingunit) %>%
  mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
         ou_lab = paste0(operatingunit, " (", lag(TX_CURR, 2, order_by = period) %>% comma(1), ")")) %>%
  ungroup() %>%
  mutate(VLS = (TX_PVLS/TX_PVLS_D)*VLC,
         notcovered = 1 - VLC) %>% 
  filter(period == "FY20Q3")


df_vl %>% 
  ggplot(aes(y = fct_reorder(ou_lab, VLC, .desc = TRUE))) +
  geom_col(aes(x = 1), fill = "gray90") +
  geom_col(aes(x = VLC), fill = si_lblue) +
  geom_col(aes(x = VLS), fill = si_blue) +
  geom_text(aes(x = .98, label = percent(notcovered, 1), 
                color = ifelse(notcovered >=.39, USAID_red, "gray30")), 
            size = 2.5, family = "Source Sans Pro") +
  geom_vline(xintercept = c(.25, .5, .75), linetype = "dashed", color = "gray90") +
  scale_x_continuous(label = percent,expand = c(0.005, 0.005), position = "top") +
  scale_color_identity() +
  labs(x = NULL, y = NULL,
       # title = "VL COVERAGE AND SUPPRESSION",
       # subtitle = "USAID | FY20Q3",
       caption = "VLC = TX_PVLS / TX_CURR (2 periods prior); VLS = TX_PVLS / TX_PVLS_D * VLC
       USAID (sans South Africa)
       Source: FY20Q3i MSD") +
  si_style_nolines() +
  theme(axis.text.y = element_text(size = 10))



ggsave("../Downloads/FYQ3_USAID_VLC.png", dpi = 600, width = 9.51, height = 4.21)

df_vl %>% 
  filter(operatingunit == "DR") %>% 
  glimpse()
