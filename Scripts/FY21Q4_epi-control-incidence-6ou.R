# PROJECT:  groundhog_day
# AUTHOR:   K. Srikanth | A.Chafetz | T. Essam | USAID
# PURPOSE:  HIV incidence for young populations using UNAIDS data
# LICENSE:  MIT
# DATE:     2022-01-18 
# UPDATED:
# NOTES: (FY21Q4 review)

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
library(mindthegap)

# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Karishma Srikanth", "Aaron Chafetz", "Tim Essam")

#source info & definition of epidemic control
source <- "UNAIDS 2021 estimates" 
date_pulled <- "July 2021"


# IMPORT ------------------------------------------------------------------

#Pull HIV incidence for young populations (15-24) from AIDSInfo
df_young <- si_path() %>% 
  return_latest("New HIV infections_HIV incidence") %>% 
  read_csv(na = c("...")) %>% 
  dplyr::select(-(contains(c("Footnote"))))

#MUNGE ----------------------------------------------------------------------

#tidy incidence data
df_young <- df_young %>% 
  pivot_longer(!Country, names_to = "year", values_to = "value") %>% 
  separate(year, sep = "_", into = c("year", "type")) %>% 
  mutate(type = ifelse(is.na(type), "point_est", type)) %>% 
  dplyr::mutate(dplyr::across(value, ~gsub(" |<|>", "", .))) %>%
  mutate(value = as.numeric(value)) %>% 
  filter(Country %in% c("Eswatini", "Botswana", "Uganda", "Lesotho", "Kenya", "Namibia")) %>% 
  pivot_wider(names_from = type, values_from = value)

#VIZ --------------------------------------------------------------

df_viz <- df_young %>% 
  mutate(val_lab = case_when(year == max(year) ~ number(point_est, 0.01)),
         lab_pt = case_when(year == max(year) ~ point_est),
         age = "15-24")

df_viz %>% 
  mutate(year = as.numeric(year)) %>% 
  #filter(Country == "Uganda") %>% 
  ggplot(aes(year, point_est, group = Country)) + 
  geom_line(aes(y = point_est), color = scooter, size = 1) +
  facet_wrap(~Country, ncol =2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = scooter) +
  geom_point(aes(y = lab_pt), na.rm = TRUE,
             shape = 21, color = scooter, fill = scooter, size = 3) +
  geom_text(aes(label = val_lab), na.rm = TRUE,
            hjust =1,
            vjust=-1,
            family = "Source Sans Pro Light") +
 # scale_y_continuous(labels = ~ label_number_si()(abs(.))) +
  scale_x_continuous(breaks = seq(1990, 2025, 10)) +
  labs(x = NULL, y = "HIV Incidence per 1,000 people") +
  coord_cartesian(expand = T, clip = "off") +
  si_style_ygrid() +
  theme(axis.text.y = element_markdown(),
        panel.spacing.x = unit(20, "pt"),
        panel.spacing.y = unit(0, "pt")) +
  plot_annotation(title = 'STEADY DECLINE IN HIV INCIDENCE (PER 1000 PEOPLE) AMONG YOUNG PEOPLE AGES 15-24 IN SUSTAINED IMPACT COUNTRIES',
                  subtitle = "HIV incidence trends represented with upper and lower bounds of estimates",
                  caption = glue("Source: {source} [{date_pulled}]
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"),
                  theme = si_style_ygrid()) 

si_save("Graphics/fy21q4-epi-control-incidence.svg", width = 6)


  
