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




# IMPORT ------------------------------------------------------------------

df <- read_csv("Data/FY21Q4_UEs.csv")%>%
  filter(agency=="USAID")%>%
  mutate(fy=as.character(fy))


# MUNGE -------------------------------------------------------------------

df <- df %>% 
  mutate(fy = fct_rev(fy))
 
f <- factor(c("Test","Postive Test","Current on Treatment","New on Treatment"))
df<-df%>%
  mutate(category=fct_rev(category))

# PLOT --------------------------------------------------------------------
dodge <- position_dodge(width=.5)

df %>% 
  ggplot(aes(ue, category, fill = fy)) +
  geom_point(shape = 21, color = "white",stroke = 1.4, size = 9,
             position = dodge) +
  # geom_text(aes(label = ue))
  #scale_y_manual(ue) +
  scale_fill_manual(values = c("21" = denim, "20" = "#bfddff")) +
  labs(x = "Unit Expenditure ($)", y = NULL,
       caption = "Note:Excludes mechanisms that do not have associated targets
Source: FY21Q4i MSD and FSD | Call with IPs/OUs Dec 2021") +
  si_style_xgrid() +
  theme(legend.position = "none",
        )

si_save("Images/FY21_UEs.svg", height = 4.52, width = 8.4)
