## PROJECT: groundhog_day
## AUTHOR:  K. Srikanth | USAID
## PURPOSE: 95's Achievement
## LICENSE: MIT
## DATE:    2022-06-08
## NOTE: agitprop/Scripts/06_epi_ann-90s.R


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(googlesheets4)
library(extrafont)
library(scales)
library(tidytext)
library(glue)
library(mindthegap)

# GLOBAL VARIABLES --------------------------------------------------------

#creds  
load_secrets()

authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")

#goal
goal <- 95

#indicators
ind_sel <- c("KNOWN_STATUS","KNOWN_STATUS_ON_ART", "ON_ART_VLS")


# IMPORT ------------------------------------------------------------------

#Cascade %
df_unaids <- pull_unaids("Test & Treat - Percent", pepfar_only = TRUE)

#PLHIV number
df_est <- pull_unaids("HIV Estimates - Integer", pepfar_only = TRUE)

#PEPFAR select list
pepfar_cntry <- get_outable(datim_user(), datim_pwd()) %>% 
  filter(str_detect(operatingunit, "Region", negate = TRUE)) %>% 
  pull(country)


# MUNGE -------------------------------------------------------------------

#num PLHIV
df_est <- df_est %>% 
  filter(country %in% pepfar_cntry,
         indicator == "PLHIV",
         stat == "est",
         age == "all",
         sex == "all") %>% 
  group_by(country) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(PLHIV = value)

#Cascade
df_unaids <- df_unaids %>% 
  filter(year == max(year),
         sex == "all",
         stat == "est",
         age == "all",
         country %in% pepfar_cntry,
         indicator %in% ind_sel)

df_viz <- df_unaids %>% 
  left_join(df_est, by = c("country")) %>% 
  filter(country != "Vietnam") %>% 
  mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "DRC",
                             country == "Dominican Republic" ~ "DR", 
                             TRUE ~ country),
         indicator = case_when(indicator == "KNOWN_STATUS_ON_ART" ~ "On Treatment",
                               indicator == "KNOWN_STATUS" ~ "Known Status",
                               indicator == "ON_ART_VLS" ~ "Virally Suppressed",
                               TRUE ~ indicator),
         PLHIV = ifelse(is.na(PLHIV), 0, PLHIV))

df_viz <- df_viz %>% 
  group_by(country) %>% 
  mutate(value = round(value, 2),
         grouping = case_when(value == min(value, na.rm = TRUE) ~ indicator),
         grouping = case_when(min(value, na.rm = TRUE) >= goal ~ "Achieved", #"Z_Achieved",
                              #country == "Eswatini" ~ "Z_Achieved",
                              #country == "Zambia" & indicator == "Virally Suppressed" ~ NA_character_,
                              TRUE ~ grouping),
         gap = case_when(value == min(value, na.rm = TRUE) & value < goal ~ goal-value,
                         value == min(value, na.rm = TRUE) & grouping == "Achieved" ~ 1-value,
                         TRUE ~ 0),
         achv = case_when(value == min(value, na.rm = TRUE) & value < goal ~ value),
         dot_color = case_when(grouping == "Known Status" ~ "#009ee3",
                               grouping == "On Treatment" ~ "#009ee3",
                               grouping == "Virally Suppressed" ~ "#009ee3",
                               grouping == "Achieved" ~ "#dd052a",
                               # grouping == "Z_Achieved" ~ genoa,
                               TRUE ~ "#a8e5ff")) %>% 
  fill(grouping, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(gap_bar = case_when(value < goal ~ value),
         country = reorder_within(country, gap, grouping, max, na.rm = TRUE))

# PLOT --------------------------------------------------------------------

epi_ctrl_cnt <- df_viz %>% 
  filter(grouping == "Achieved") %>% 
  distinct(country) %>% 
  nrow()

df_viz %>% 
  ggplot(aes(value, country, color = dot_color)) +
  geom_vline(xintercept = goal, linetype = "dashed") + 
  geom_linerange(aes(xmin = gap_bar, xmax = goal), color = "gray90",
                 size = 2.5, na.rm = TRUE) +
  geom_point(size = 4, na.rm = TRUE) +
  scale_y_reordered(limits = rev) +
  scale_x_continuous(labels=function(x) paste0(x,"%")) +
  scale_color_identity() +
  facet_grid(grouping~indicator, scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL, color = NULL,
       title = glue("AS OF 2020, {epi_ctrl_cnt} PEPFAR COUNTRY HAS ACHIEVED THE UNAIDS' 2030 FAST TRACK TARGETS"),
       caption = glue("Source: UNAIDS 90-90-90 15+ (2020)
                      SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_xgrid() +
  theme(strip.text.y = element_blank(),
        panel.spacing = unit(.5, "lines"))

si_save("Graphics/06_epi_ann-90s.svg")
