# PROJECT:  FY21Q1 Quarterly Review
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  UE comparison
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

df <- read_csv("Data/FY21Q1_UEs.csv")


# MUNGE -------------------------------------------------------------------

df <- df %>% 
  mutate(agency = fct_rev(agency))

# PLOT --------------------------------------------------------------------
dodge <- position_dodge(width=.5)

df %>% 
  ggplot(aes(ue, fct_reorder(category, ue), fill = agency)) +
  geom_point(shape = 21, color = "white",stroke = 1.4, size = 9,
             position = dodge) +
  # geom_text(aes(label = ue))
  scale_y_reordered() +
  scale_fill_manual(values = c("USAID" = denim, "CDC" = trolley_grey_light)) +
  labs(x = "Unit Expenditure (FY20, $)", y = NULL,
       caption = "Note: Exclusives mechanisms that do not have associated targets and CDC NDOH mechanism 16772
Source: FY20Q4c MSD and FSD") +
  si_style_xgrid() +
  theme(legend.position = "none",
        axis.text.x = element_blank())

si_save("Images/FY20_UEs.png", height = 4.52, width = 8.4)
