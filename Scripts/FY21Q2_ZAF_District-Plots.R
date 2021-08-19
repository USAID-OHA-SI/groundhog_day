# PROJECT:  groundhogday
# AUTHOR:   K. Srikanth, A. Chafetz | USAID
# PURPOSE:  South Africa district review
# LICENSE:  MIT
# DATE:     2021-08-18
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
library(googledrive)
library(vroom)

# load fonts
folderpath <- "C:/Users/STAR/Downloads/font folder" #not necessary, just use if computer doesnt have fonts locally
font_import(folderpath)
library(extrafont)

# GLOBAL VARIABLES --------------------------------------------------------

load_secrets()

drive_id <- as_id("1JG04zIjIxXeOlBaSLhWPMDTILOZuR-z9")

# DOWNLOAD ----------------------------------------------------------------

#grab file name and id
file <- drive_ls(drive_id, "zip")

#create temp folder
temp_folder()

#download to temp folder
drive_download(as_id(file$id),
               path = file.path(folderpath_tmp, file$name), overwrite = TRUE)

# IMPORT ------------------------------------------------------------------

#import data
df <- vroom(file.path(folderpath_tmp, file$name))


# MUNGE -------------------------------------------------------------------

#identify district type
df <- df %>% 
  mutate(dist_typ = ifelse(str_detect(snuprioritization, "Scale-Up"), 
                           "PEPFAR Focus Districts", "Non-Focus Districts"))

#INCIDENCE DF
hiv_in <- df %>% 
  filter(indicator == "HIV incidence",
         period == "2021cum",
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(psnu, snuprioritization, snu1, indicator, dist_typ) %>% 
  summarize(across(value, sum, na.rm = TRUE)) %>% 
  mutate(value = value * 1000)

#POPULATION DF
population <- df %>% 
  filter(indicator == "Population",
         period == "2021cum",
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(psnu, snuprioritization, snu1, indicator, dist_typ) %>% 
  summarize(across(value, sum, na.rm = TRUE))

#PREVALENCE DF
hiv_prev <- df %>% 
  filter(indicator == "HIV prevalence",
         period == "2021cum",
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(psnu, snuprioritization, snu1, indicator, dist_typ) %>% 
  summarize(across(value, sum, na.rm = TRUE)) %>%
  mutate(value = value * 1000) %>% 
  pivot_wider(names_from = indicator, values_from = value, names_glue = "{tolower(indicator)}")

#PLHIV DF
df_plhiv <- df %>% 
  filter(indicator == "PLHIV",
         period == "2021cum",
         standardizeddisaggregate == "Total Numerator") %>% 
group_by(psnu, snuprioritization, snu1, indicator, dist_typ) %>% 
  summarize(across(value, sum, na.rm = TRUE)) 
 
# JOIN ------------------------------------------------------------------

#incidence vis join
df_incidence_vis <- hiv_in %>% 
  bind_rows(population) %>% 
  pivot_wider(names_from = indicator, values_from = value, names_glue = "{tolower(indicator)}") %>% 
  mutate(point_color = ifelse(dist_typ == "PEPFAR Focus Districts", scooter, golden_sand)) 

#plhiv vis join 
df_plhiv_vis <- df_plhiv %>% 
  bind_rows(population) %>% 
  pivot_wider(names_from = indicator, values_from = value, names_glue = "{tolower(indicator)}") %>% 
  mutate(point_color = ifelse(dist_typ == "PEPFAR Focus Districts", scooter, golden_sand)) %>% 
  left_join(hiv_prev, by = c("psnu", "snuprioritization", "snu1", "dist_typ"))

#INCIDENCE SCATTERPLOT
df_incidence_vis %>% 
  ggplot(aes(population, `hiv incidence`)) +
  geom_point(aes(color = df_incidence_vis$point_color, size = 6), alpha = 0.7, show.legend = F)+ 
  geom_hline(yintercept = 1, size = .5) +
  scale_x_continuous(labels = label_number_si()) +
  si_style() +
  scale_color_identity() +
  labs(title = "HIV Incidence per 1,000 People across PEPFAR Focus Districts and Non-Focus Districts",
       y = "HIV Incidence per 1,000 people",
       x = "District Population",
       caption = "Source: South Africa District Data
         SI Analytics: Karishma Srikanth
         US Agency for International Development")

si_save("Graphics/incidence_districts.svg")

#PLHIV VIS
df_plhiv_vis %>% 
  ggplot(aes(population, plhiv)) +
  geom_point(aes(color = df_plhiv_vis$point_color, size = df_plhiv_vis$`hiv prevalence`), alpha = 0.7, show.legend = F)+ 
  geom_hline(yintercept = 0, size = .5) +
  scale_x_continuous(labels = label_number_si()) +
  scale_y_continuous(labels = label_number_si()) +
  si_style() +
  scale_color_identity() +
  labs(title = "Non-Focus Districts Have Smaller Populations and Fewer PLHIV",
       y = "People Living with HIV",
       x = "District Population",
       caption = "Source: South Africa District Data
         SI Analytics: Karishma Srikanth
         US Agency for International Development")

si_save("Graphics/plhiv_districts.svg")

