# PURPOSE: Remake Fy21Q3 Cop Budget Visual
# AUTHOR: K Srikanth | SI
# DATE: 2021-10-18
# NOTES: 

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(tidyverse)
library(gophr)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(here)
library(ggalt)
library(ggrepel)

#IMPORT / MUNGE ========================================================

#created adjusted csv file based on the previous visual in the Q3 deck
cop_df <- si_path() %>% 
  return_latest("copbudget") %>% 
  read_csv()

#quick function to label the first group with % sign
percent_first <- function(x) {
  x <- sprintf("%d%%", round(x*100))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}

#mutate categories to make them factors and get the difference 
cop_df <- cop_df %>% 
  mutate(budget_cat = as.factor(cop_df$budget_cat),
         diff = epi_control - non_epi)

#VIS ============================================================================

cop_df %>% 
ggplot() +
  geom_segment(aes(y=fct_reorder(budget_cat, non_epi), yend=budget_cat, x=0, xend=.7), color= trolley_grey, size=0.15) +
  geom_dumbbell(aes(y=budget_cat, x=epi_control, xend=non_epi),
                size=1.5, color="#b2b2b2", size_x=5, size_xend = 5, colour_x = usaid_red, colour_xend = denim) +
  geom_text(data=filter(cop_df, budget_cat=="Service Delivery"),
            aes(x=non_epi, y=budget_cat, label="Not at Epi Control"),
            color=denim, size=4, vjust=-1.5, hjust = 0, fontface="bold", family="Source Sans Pro") +
  geom_text(data=filter(cop_df, budget_cat=="Service Delivery"),
            aes(x=epi_control, y=budget_cat, label="At Epi Control"),
            color=usaid_red, size=4, vjust=-1.5, hjust = 1, fontface="bold", family="Source Sans Pro") +
  geom_text(aes(x=epi_control, y=budget_cat, label=percent_first(epi_control)),
            color=usaid_red, size=3.75, vjust=2.5, family="Source Sans Pro") +
  geom_text(color=denim, size=3.75, vjust=2.5, family="Source Sans Pro",
            aes(x=non_epi, y=budget_cat, label=percent_first(non_epi))) +
  geom_rect(aes(xmin=.75, xmax=.85, ymin=-Inf, ymax=Inf), fill= trolley_grey_light) +
  geom_text(aes(label=paste0(diff*100, "%"), y=budget_cat, x=.8), fontface="bold", size=3.75, family="Source Sans Pro") +
  geom_text(data=filter(cop_df, budget_cat=="Service Delivery"), 
            aes(x=.80, y=budget_cat, label="Difference"),
            color="black", size=4, vjust=-2, fontface="bold", family="Source Sans Pro") +
  si_style_ygrid() +
  scale_x_continuous(expand=c(0,0), limits=c(0, .875)) +
  labs(x= "COP21 Budget Share",
      y=NULL,
      title="In COP21, we see similar investment patterns across countries that have achieved
       epidemic control and those that have not" %>% toupper,
       subtitle="For COP22, USAID is already rethinking our strategy and resource allocation",
       caption="Source: OHA Financial & Programmatic Integrated Dashboard,
      US Agency for International Development")

si_save("copbudget_share.svg")
