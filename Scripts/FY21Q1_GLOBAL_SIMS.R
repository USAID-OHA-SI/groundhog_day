# PROJECT:  FY21Q1 Quarterly Review
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  SIMS visits
# LICENSE:  MIT
# DATE:     2021-03-09
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



# IMPORT ------------------------------------------------------------------

df <- read_csv("Data/FY21Q1_SIMS-visits.csv")


df %>% 
  mutate(completed = conducted/projected) %>% 
  ggplot(aes(fy, conducted)) +
  geom_col(aes(y = projected), fill = trolley_grey_light) +
  geom_col(aes(fill = fy)) +
  geom_hline(yintercept = seq(0,2000, 500), linetype = "dashed", color = "white") +
  geom_text(aes(label = percent(completed, 1)), family = "Source Sans Pro", 
            color = scooter, vjust = -1) +
  geom_errorbar(aes(ymin = projected, ymax = projected)) +
  scale_y_continuous(labels = comma, expand = c(.005, .005)) + 
  scale_x_discrete(expand = c(.05, .05)) + 
  scale_fill_manual(values = c("FY20" = scooter, "FY21" = scooter_light)) +
  labs(x = NULL, y = NULL) +
  si_style_nolines() +
  theme(legend.position = "none")

df %>% 
  mutate(completed = conducted/projected) %>% 
  ggplot(aes(fy, conducted)) +
  geom_col(aes(y = projected), fill = trolley_grey_light) +
  geom_col(aes(alpha = fy), fill = scooter) +
  geom_hline(yintercept = seq(0,2000, 500), linetype = "dashed", color = "white") +
  geom_text(aes(label = percent(completed, 1)), family = "Source Sans Pro", 
            color = scooter, vjust = -1) +
  geom_errorbar(aes(ymin = projected, ymax = projected)) +
  scale_y_continuous(labels = comma, expand = c(.005, .005)) + 
  scale_x_discrete(expand = c(.05, .05)) + 
  scale_alpha_manual(values = c("FY20" = 1, "FY21" = .6)) +
  labs(x = NULL, y = NULL,
       title = "SIMS <span style = 'color:#1e87a5;'>Completed</span> Assessments",
       caption = "Source: OHA SIMS Tableau dashboard (achievements) and 
       OUs SIMS Prioritization Lists (planning)") +
  si_style_nolines() +
  theme(legend.position = "none",
        plot.title = element_markdown())

si_save("Images/FY21_SIMS_completion.png",
        height = 4.16, width = 3.85)
