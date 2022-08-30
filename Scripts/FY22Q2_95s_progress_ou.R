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
ind_sel <- c("Percent Known Status of PLHIV","Percent on ART with Known Status", "Percent VLS on ART")


# IMPORT ------------------------------------------------------------------

#Cascade %
df_unaids <- pull_unaids("HIV Test & Treat", pepfar_only = TRUE)

#PLHIV number
df_est <- pull_unaids("HIV Estimates", pepfar_only = TRUE)

#PEPFAR select list
pepfar_cntry <- pepfar_country_list %>% 
  filter(str_detect(operatingunit, "Region", negate = TRUE)) %>% 
  pull(country)


# MUNGE -------------------------------------------------------------------

#num PLHIV
df_est <- df_est %>% 
  filter(country %in% pepfar_cntry,
         indicator == "Number PLHIV",
       #  stat == "est",
         age == "All",
         sex == "All") %>% 
  group_by(country) %>% 
  summarise(estimate = sum(estimate, na.rm = TRUE)) %>% 
  ungroup() %>% 
  rename(PLHIV = estimate)

#Cascade
df_unaids <- df_unaids %>% 
  filter(year == max(year),
         sex == "All",
        # stat == "est",
         age == "All",
         country %in% pepfar_cntry,
         indicator %in% ind_sel)

df_viz <- df_unaids %>% 
  select(-c(lower_bound:upper_bound)) %>% 
  left_join(df_est, by = c("country")) %>% 
  filter(country != "Vietnam") %>% 
  mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "DRC",
                             country == "Dominican Republic" ~ "DR", 
                             TRUE ~ country),
         indicator = case_when(indicator == "Percent on ART with Known Status" ~ "On Treatment",
                               indicator == "Percent Known Status of PLHIV" ~ "Known Status",
                               indicator == "Percent VLS on ART" ~ "Virally Suppressed",
                               TRUE ~ indicator),
         PLHIV = ifelse(is.na(PLHIV), 0, PLHIV))

#BURUNDI AND MALAWI - issues because 1st 95 and 3 95 are the same
df_viz <- df_viz %>%
  rename(value = estimate) %>% 
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
         # gap = ifelse(iso %in% c("BDI", "MWI") & grouping == "Virally Suppressed", 0, gap),
         # achv = ifelse(iso %in% c("BDI", "MWI") & grouping == "Virally Suppressed", NA, achv),
         # grouping = ifelse(iso %in% c("BDI", "MWI"), "Known Status", grouping),
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
