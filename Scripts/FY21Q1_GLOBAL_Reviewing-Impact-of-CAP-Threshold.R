# PROJECT:  groundhog_day
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  review impact of Q1 threshold for CAP
# LICENSE:  MIT
# DATE:     2021-02-25
# UPDATED:  2021-03-02

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
library(svglite)

# GLOBAL VARIABLES --------------------------------------------------------

threshold <- .8

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("OU_IM") %>% 
  read_rds()  

# MUNGE -------------------------------------------------------------------

#limit to TX_CURR & USAID
df_tx <- df %>% 
  filter(indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator",
         fundingagency == "USAID",
         fiscal_year == 2021)

#rename with latest partner/mech names
# df_tx <- rename_official(df_tx)

#adjust names
df_tx <- df_tx %>% 
  mutate(operatingunit = case_when(operatingunit == "Democratic Republic of the Congo" ~ "DRC",
                                   operatingunit =="Dominican Republic" ~ "DR",
                                   operatingunit == "Western Hemisphere Region" ~ "WHR",
                                   TRUE ~ operatingunit))

#aggregate to mech level, calc achivement w/ cutoff threshold
df_achv <- df_tx %>% 
  group_by(operatingunit, mech_code, primepartner) %>% 
  summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(achievement = cumulative/targets,
         achv_max = ifelse(achievement > 1.6, 1.6, achievement)) %>% 
  arrange(achievement)

#capture mechanism count with no TX_CURR reported in FY21 but have targets
mech_noreporting <- df_achv %>% 
  filter(cumulative == 0) %>% 
  nrow()

#capture mechanism count with no TX_CURR reported in FY21 but have targets for plot notes
mech_notargets <- df_achv %>% 
  filter(targets == 0) %>% 
  nrow()

#drop mechanims with either no results or targets for plot notes
df_achv <- df_achv %>% 
  filter(cumulative > 0,
         targets > 0)

#calc achievement at the OU level to plot overall achievement as comparison
df_achv_ou <- df_achv %>% 
  group_by(operatingunit) %>% 
  summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(achievement_ou = cumulative/targets) %>%
  select(operatingunit, achievement_ou)

#flag where achv is below threshold and count tot mech & those that meet flag for naming
df_achv <- df_achv %>%
  mutate(flag = achievement < threshold) %>% 
  group_by(operatingunit) %>% 
  mutate(mech_tot = n(),
         mech_flag = sum(flag, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(ou_name = glue("{operatingunit} ({mech_flag}/{mech_tot})"))

#add new names onto OU achievement to work with append + plot
df_achv_ou <- df_achv %>% 
  distinct(operatingunit, ou_name) %>% 
  left_join(df_achv_ou) 

#append OU achievement onto mech data 
df_achv <- df_achv %>% 
  bind_rows(df_achv_ou)

#capture mechanism total for use in plot title  
total_mechs <-  df_achv %>% 
  filter(!is.na(mech_code)) %>% 
  nrow()

#capture flagged mechanism count for use in plot title 
flagged_mechs <-  df_achv %>% 
  filter(flag == TRUE) %>% 
  nrow()


# PLOT --------------------------------------------------------------------

df_achv %>% 
  ggplot(aes(achv_max, fct_reorder(ou_name,achievement_ou, na.rm = TRUE), fill = achievement < threshold)) +
  geom_vline(aes(xintercept = threshold), linetype = "dotted", color = trolley_grey) +
  geom_vline(aes(xintercept = 1.6), color = trolley_grey_light) +
  geom_errorbar(aes(xmin = achievement_ou, xmax = achievement_ou), size = 1.5, color = trolley_grey) +
  geom_jitter(aes(size = targets), height = .3,  shape = 21, alpha = .6, color = "white", na.rm = TRUE) +
  scale_x_continuous(label = percent, limits = c(0, 1.61), 
                     breaks = seq(0, 1.5, .5),
                     expand = c(.005, .005)) +
  scale_fill_manual(values = c(scooter, burnt_sienna)) +
  labs(x = NULL, y = NULL,
       title = glue("With a {percent(threshold)} Q1 achievement cutoff, {flagged_mechs} out of USAID's {total_mechs} mechanism would be implicated") %>% toupper,
       subtitle = "FY21 TX_CURR | USAID mechanisms",
       caption = glue("{mech_noreporting} mechs dropped due to no results; {mech_notargets} mechs dropped due to no targets
                        Extent capped at 150%, all achievement above has been plotted at 160%
                        Source: FY21Q1i MSD")) +
  si_style() +
  theme(legend.position = "none")


# EXPORT ------------------------------------------------------------------

# si_save("../Downloads/TX_CURR_threshold.svg")

