# PROJECT:  agiprop
# AUTHOR:   B.Kasdan | USAID
# PURPOSE:  Spending by Funding Type (remake of the Powers memo)
# LICENSE:  MIT
# DATE:     2021-05-14
# UPDATED:  2021-05-27

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr)
  library(glamr)
  library(ICPIutilities)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(janitor)
  library(lubridate)
library(googlesheets4)
library(readxl)

#install treemaps
install.packages("treemapify")
library(treemapify)
devtools::install_github("adamdsmith/nrsmisc")
library(nrsmisc)



  # source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c( "Ben Kasdan")
  

# IMPORT ------------------------------------------------------------------
  


funding<-read_csv("Data/lesotho spend.csv")  
df1<-funding%>%
  pivot_longer(cols= `2019` : `2021`,
    names_to = "Fiscal Year",
    values_to= "Funding")%>%
  dplyr::filter(!agency_category=="Not Disaggregated")%>%
  dplyr::mutate(`color` = agency_category)%>%
 
  mutate(color = ifelse(agency_category == "HIV Clinical Services", "#1e87a5", 
                           ifelse(agency_category=="HIV Laboratory Services","#c43d4d","Other")))
                        
                                  # ifelse(agency_category=="Not Disaggregated","#8980cb","Other"))))
df1<-df1%>%
  mutate(`Funding`=(`Funding`/1e6))
#Adjust FY labels
my_labels <- seq(2019, 2021, 1) %>% 
  substr(., 3, 4) %>% paste0("FY", .)
# Keep every 4th label but replace those in between with blanks
cust_labels <- nrsmisc::every_nth(my_labels, 2, inverse = T)
# Add this to your ggplot
scale_x_discrete(labels = cust_labels)

#Reshape dataframe to group by variables to show background budget
df2<- df1 %>% 
  distinct(`Fiscal Year`, agency_category,`Funding`) %>% 
  count(`Fiscal Year`, agency_category,`Funding`) %>% 
  group_by(`Fiscal Year`) %>% 
  mutate(total_n = sum(`Funding`)) %>% 
  ungroup()%>%
  dplyr::mutate(`color` = agency_category)%>%
  
  mutate(color = ifelse(agency_category == "HIV Clinical Services", "#1e87a5", 
                        ifelse(agency_category=="HIV Laboratory Services","#c43d4d", "Other")))
                               # ifelse(agency_category=="Not Disaggregated","#8980cb","Other"))))
  

v <- df2 %>% 
  ggplot(aes(`Fiscal Year`,`Funding`, n, fill=`color`))+
geom_col(aes(y = total_n), fill = "#8C8985", alpha = .2)+
  geom_col()+
 facet_wrap(~agency_category)+
   scale_fill_identity()+
  si_style_nolines()+
  theme(legend.position = "none")+
  scale_y_continuous(labels = unit_format(1., unit = "M"))+
  scale_x_discrete(labels = cust_labels)+
  
 labs(
   title =  "Lesotho C&T Expenditure FY19-21 (USD $M)",
   caption = glue("Source: FY21Q4c FSD. Visual Excludes commodities and M&O
                  SI analytics: {paste(authors)}
              US Agency for International Development")) +
  
  si_style_nolines()+
  theme(legend.position = "none")
si_save(path="~/Images","Lesotho_trends.svg")
si_save(path="~/GitHub/agitprop","23_Powerfunding_trends.svg")
