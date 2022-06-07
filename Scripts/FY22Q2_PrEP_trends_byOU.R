# PROJECT:  groundhog_day
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  scale up of prep - by top 11 ous
# LICENSE:  MIT
# DATE:     2022-06-02
# UPDATED:  
# NOTE:     adapted from agitprop/09a_usaid_prep_scaleup.R

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


# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")

curr_fy <-source_info(return = "fiscal_year")
curr_pd <- source_info(return = "period")
msd_source <- source_info()

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("OU_IM_FY20") %>% 
  read_msd()   

df_arch <- si_path() %>% 
  return_latest("OU_IM_FY15") %>% 
  read_msd()

# MUNGE -------------------------------------------------------------------

df_arch <- df_arch %>% 
  rename(funding_agency = fundingagency)

#bind archived + current MSD and filter for PrEP
df_prep <- df %>%
  bind_rows(df_arch) %>% 
  filter(funding_agency == "USAID",
         indicator == "PrEP_NEW",
         standardizeddisaggregate == "Total Numerator",
         fiscal_year >= 2017)

#curr fy prep (for viz title)
prep_cum <- df_prep %>% 
  filter(fiscal_year == curr_fy) %>% 
  count(wt = cumulative) %>% 
  pull()

#count number of countries with PrEP
df_cntry_cnt <- df_prep %>% 
  filter(cumulative != 0) %>% 
  distinct(fiscal_year, countryname) %>% 
  count(fiscal_year, name = "n_countries")

#aggregate result to USAID level
df_prep_ou <- df_prep %>% 
  bind_rows(df_prep  %>%
              mutate(operatingunit = "USAID",
                     countryname = "USAID")) %>% 
  filter(fiscal_year >= 2021) %>% 
  group_by(fiscal_year, funding_agency, operatingunit, countryname) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd() %>% 
  mutate(value = na_if(value, 0)) %>% 
  select(-period_type) %>% 
  arrange(period) 


#data points for plotting
df_prep_ou <- df_prep_ou %>% 
  mutate(max_prep = ifelse(period == max(period), value, 0)) %>% 
  group_by(countryname) %>% 
  mutate(endpoints = case_when(period %in% c(max(period), min(period))~value),
         max_prep = max(max_prep)) %>% 
  ungroup() %>% 
  mutate(country_lab = case_when(countryname == "USAID" ~ 
                                   glue("{countryname}<br><span style = 'font-size:12pt'>{label_number_si()(max_prep)} <span style = 'font-size:10pt'>({curr_pd})</span>"),
                                 TRUE ~ glue("{countryname}<br><span style = 'font-size:12pt'>{label_number_si()(max_prep)}</span>")),
         country_lab = str_replace(country_lab, "NA", "0")) %>% 
  filter(max_prep > 0)


df_prep_ou <- df_prep_ou %>% 
  mutate(fill_color = case_when(countryname == "USAID" ~ scooter,
                                countryname == "USAID" ~ scooter,
                                #  otherdisaggregate == "o6mmd" ~ scooter,
                                TRUE ~ scooter_light),
         val_lab = case_when(countryname == "USAID" & period == max(period) ~ value))

#identify the MMD share for the largest countries
top <- df_prep_ou %>% 
  filter(operatingunit != "USAID",
         #  otherdisaggregate == "o6mmd",
         period == max(period)) %>% 
  arrange(desc(value)) %>% 
  slice_max(n = 11, order_by = value) %>% 
  summarise(across(c(value), sum, na.rm = TRUE),
            n = n()) 


#top focus countries
top_cntry <- df_prep_ou %>% 
  filter(
    period == max(period)) %>% 
  slice_max(order_by = value, n = top$n + 1) %>% 
  pull(operatingunit)



# CREATE FULL LIST OF PERIODS ---------------------------------------------

#PrEP in and out of quarterly and semi-annual reporting so need a complete set  
#current period as number
curr_pd_num <- curr_pd %>% 
  str_remove("FY") %>% 
  str_replace("Q", ".") %>% 
  as.numeric()

#identify current fiscal year for max date
curr_fy <- str_sub(curr_pd, 3,4) %>% as.numeric()

#propagate list of periods not in prep to add to df
full_pds <- expand_grid(fiscal_year = c(17:curr_fy),
                        quarter = c(1:4)) %>% 
  unite(period, c(fiscal_year, quarter), sep = ".") %>% 
  mutate(period = as.numeric(period)) %>% 
  filter(period <= curr_pd_num) %>% 
  mutate(period = period %>% 
           paste0("FY", .) %>% 
           str_replace("\\.", "Q")) 

extra_pds <- full_pds %>% 
  filter(!period %in% unique(df_prep$period))


# VIZ ---------------------------------------------------------------------

fy_start <-  full_pds %>% 
  filter(str_detect(period, "Q1")) %>% 
  pull()

pd_breaks <- full_pds %>% 
  # filter(str_detect(period, "Q(1|3)")) %>% 
  pull()

df_viz <- df_prep %>% 
  bind_rows(extra_pds) %>% 
  arrange(period)

v <- df_viz %>% 
  ggplot(aes(period, value, group = funding_agency)) + 
  geom_area(fill = scooter, color = scooter, alpha = .2, size = 1, na.rm = TRUE) +
  geom_vline(xintercept = fy_start, color = "white", 
             size = .9, linetype = "dotted") +
  geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.5, na.rm = TRUE) +
  scale_y_continuous(label = label_number_si(), position = "right", expand = c(.01, .01)) +
  scale_x_discrete(breaks = pd_breaks, labels = str_remove(pd_breaks, "FY[:digit:]{2}(?!Q1)")) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL, 
       title = glue("USAID has initiated {label_number_si()(prep_cum)} \\
                      onto PrEP this year across \\
                      {filter(df_cntry_cnt, fiscal_year == max(fiscal_year)) %>% pull()} \\
                      countries, up from {filter(df_cntry_cnt, fiscal_year == 2017) %>% pull()} \\
                      countries in 2017") %>% toupper,
       subtitle = "Pre-Exposure Prophylaxis (PrEP) Quarterly Results",
       caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_ygrid()


v +
  annotate("text",
           x = 9.5, y = 15e3, 
           hjust = "left", lineheight = .9,
           label = "PrEP reported semi-annually in \nFY19-20, so Q1/Q3 were reported\n in aggregate with Q2/Q4 reporting",
           family = "Source Sans Pro", size = 9/.pt, color = matterhorn) +
  annotate("curve",
           arrow = arrow(length = unit(0.05, "inches"),
                         type = "closed", ends = "first"),
           x = 9, y = 22e3, xend = 9.4, yend = 13e3,
           color = matterhorn) +
  annotate("text",
           x = 16.8, y = 50e3,
           hjust = "right", lineheight = .9,
           label = "PrEP returned to quarterly\n reporting for the first time \nsince FY18, making a false \nimpression that there was \na decline in results",
           family = "Source Sans Pro", size = 9/.pt, color = matterhorn) +
  annotate("curve",
           arrow = arrow(length = unit(0.05, "inches"),
                         type = "closed"),
           x = 16.9, y = 60e3, xend = 17, yend = 75e3,
           color = matterhorn)

si_save("Graphics/09_prev_qtr_prep-usaid-scaleup.svg")
si_save("Images/09_prev_qtr_prep-usaid-scaleup.png") 

df_prep_ou %>% 
  filter(countryname %in% top_cntry) %>%
  ggplot(aes(period, value, group = funding_agency)) + 
  geom_area(aes(fill = fill_color, color = fill_color), alpha = .2, size = 1, na.rm = TRUE) +
  geom_vline(xintercept = fy_start, color = "white", 
             size = .9, linetype = "dotted") +
  geom_point(shape = 21, fill = "white", color = scooter, stroke = 1.5, na.rm = TRUE) +
  scale_y_continuous(label = label_number_si(), position = "right", expand = c(.01, .01)) +
  scale_x_discrete(breaks = pd_breaks, labels = str_remove(pd_breaks, "FY[:digit:]{2}(?!Q1)")) +
  coord_cartesian(clip = "off") +
  #  facet_wrap(~countryname, scales = "free_y") +
  facet_wrap(~fct_reorder2(country_lab, period, value, .desc = TRUE), scales =  "free_y") +
  scale_color_identity() +
  scale_fill_identity() +
  labs(x = NULL, y = NULL, 
       title = glue("USAID has initiated {label_number_si()(prep_cum)} \\
                      onto PrEP this year across \\
                      {filter(df_cntry_cnt, fiscal_year == max(fiscal_year)) %>% pull()} \\
                      countries, up from {filter(df_cntry_cnt, fiscal_year == 2017) %>% pull()} \\
                      countries in 2017") %>% toupper,
       subtitle = "Pre-Exposure Prophylaxis (PrEP) Quarterly Results in largest 11 OUs",
       caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development")) +
  si_style_ygrid() +
  theme(panel.spacing.y = unit(.5, "line"),
        panel.spacing.x = unit(.5, "line"),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 7),
        panel.grid.major.y = element_line(color = "#E8E8E8"),
        panel.grid.minor.y = element_line(color = "#E8E8E8"),
        strip.text = element_markdown())  

si_save("Images/FY22Q2_PrEP_NEW_byOU.png")
si_save("Graphics/FY22Q2_PrEP_NEW_byOU.svg")

