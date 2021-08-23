# PROJECT:  groundhogday
# AUTHOR:   A.Chafetz, K.Srikanth | USAID
# PURPOSE:  Taget/Budget Share
# LICENSE:  MIT
# DATE:     2021-08-23
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
library(ggrepel)

# GLOBAL VARIABLES --------------------------------------------------------

source <- source_info()

load_secrets()


# IMPORT ------------------------------------------------------------------

df_fin <- si_path() %>% 
  return_latest("Finanical_Structured_Dataset") %>% 
  read_msd()

# MUNGE -----------------------------------------------------------------------'

#Remove M&O and Supply Chain Funding and filter to C&T
df_fin <- df_fin %>% 
  remove_mo() %>% 
  remove_sch() %>% 
  filter(program == "C&T")

#summarize budget totals
df_fin <- df_fin %>% 
  bind_rows(df_fin %>% mutate(operatingunit = "Global")) %>% 
  filter(operatingunit %in% c("South Africa", "Global"),
         fundingagency %in% c("USAID", "HHS/CDC")) %>%
  group_by(fiscal_year, operatingunit, fundingagency) %>% 
summarize(across(cop_budget_total, sum, na.rm = TRUE))
  
#create budget shares
df_fin <- df_fin %>% 
  group_by(fiscal_year, operatingunit) %>% 
  mutate(share = cop_budget_total/sum(cop_budget_total)) %>% 
  ungroup()

#colors
df_fin <- df_fin %>% 
  mutate(fill_color = case_when(fundingagency == "USAID" ~ usaid_blue,
                                fundingagency == "HHS/CDC" ~ usaid_lightblue))

df_fin <- df_fin %>% 
  mutate(fundingagency = fct_rev(fundingagency),
         fill_color = factor(fill_color, c(usaid_lightblue, usaid_blue)))

#labels
df_fin <- df_fin %>% 
  mutate(lab = case_when(fiscal_year == max(fiscal_year) ~ glue("{number(cop_budget_total, scale = 1e-6, accuracy = .1, suffix = 'M')} {fundingagency}")))

df_fin <- df_fin %>% 
  mutate(lab_share = case_when(fiscal_year %in% c(min(fiscal_year), max(fiscal_year)) & fundingagency == "USAID" ~ percent(share, 1)))

#PLOT -------------------------------------------------------------------------

df_fin %>% 
  ggplot(aes(fiscal_year, cop_budget_total, color = fill_color)) +
  geom_line(size = 1.2) +
  geom_point(data = df_fin %>% filter(fiscal_year %in% c(min(fiscal_year), max(fiscal_year))),
             size = 4) +
  geom_text(data = df_fin %>% filter(fiscal_year == min(fiscal_year)),
            aes(label = number(cop_budget_total, scale = 1e-6, accuracy = .1, suffix = "M")),
            family = "Source Sans Pro", size = 9/.pt, hjust = 1.4) +
  geom_text_repel(aes(label = lab), na.rm = TRUE, family = "Source Sans Pro", size = 9/.pt, hjust = -.2) +
  coord_cartesian(clip = "off") +
  expand_limits(y = 0, x = c(2015.75, 2022.5)) +
  scale_y_continuous(label = label_number_si()) + 
  scale_x_continuous(breaks = c(2016:2022)) +
  scale_color_identity() +
  facet_wrap(~operatingunit, scales = "free_y") +
  labs(x = NULL, y = NULL,
       caption = "Note: Budget totals for USAID and CDC only
         Source: FY21Q3i Financial Structured Dataset
         US Agency for International Development") +
  si_style() +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

si_save("Graphics/ZAF_TX_CURR_budget_trends.svg")

df_fin %>% 
  ggplot(aes(fiscal_year, share, fill = fill_color)) +
  geom_area(alpha = .75) +
  geom_text_repel(aes(label = lab_share), na.rm = TRUE,
            family = "Source Sans Pro", size = 12/.pt, color = "white") +
  geom_hline(yintercept = c(.25, .5, .75, 1), color = "white", linetype = "dotted") +
  scale_y_continuous(label = percent) + 
  scale_x_continuous(n.breaks = 7) +
  scale_fill_identity() +
  facet_wrap(~operatingunit, scales = "free_y") +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL,
       caption = "Note: Budget totals for USAID and CDC only
         Source: FY21Q3i Financial Structured Dataset
         US Agency for International Development") +
  si_style_nolines() +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

si_save("Graphics/ZAF_TX_CURR_budgetshare_trends.svg")
