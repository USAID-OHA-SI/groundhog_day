## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  OVC
## DATE:     2020-12-02
## UPDATED:  2020-02-25
## NOTE:     adapted from achafetz/AgencySelfAssessment FY20_TZA_OVC.R

# DEPEDENCIES -------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(glue)
library(gt)
library(RColorBrewer)
library(extrafont)
library(glitr)
library(scales)
library(ggtext)


# IMPORT DATA -------------------------------------------------------------

df_k2 <- read_csv("data/TZA_k2_data.csv")

# MUNGE -------------------------------------------------------------------


df_k2 %>% 
  filter(period == "FY21Q1") %>% 
  mutate(category = fct_inorder(category)) %>% 
  ggplot(aes(value, fct_rev(category), label = comma(value))) +
  geom_blank(aes(value * 1.2)) +
  geom_col(fill = moody_blue) +
  geom_text(hjust = -.2, family = "Source Sans Pro", color = "gray30") +
  scale_x_continuous(expand = c(.005, .005)) + 
  labs(x = NULL, y = NULL,
       title = toupper("Kizazi Kipya is delivering an enhanced\nC/ALHIV service package"),
       subtitle = "FY21Q1",
       caption = "Source: Kizazi Kipya Viral Load Tracking") +
  si_style_nolines() +
  theme(axis.text.x = element_blank())

si_save("Images/TZA_K2_VL.png", width = 5.28, height= 4.84)
