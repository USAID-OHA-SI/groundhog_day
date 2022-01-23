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



  source("Scripts/99_utilities.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  authors <- c( "Ben Kasdan")
  

# IMPORT ------------------------------------------------------------------
  
id<- googlesheets4::as_sheets_id('11CRsy-77xtgxVWVhqUQEb7zxTThvPvuAVb0_WJLD734')
funding <- googlesheets4::read_sheet(id, "Machine Readable FY21Q4") 

funding<-read_csv("Data/agency funding.csv") %>%
  dplyr::filter(!agency_category=="NA")
df1<-funding%>%
  pivot_longer(cols= `2018` : `2022`,
    names_to = "Fiscal Year",
    values_to= "Funding")%>%
  dplyr::mutate(`color` = agency_category)%>%
 
  mutate(color = ifelse(agency_category == "USAID", "#002a6c", 
                           ifelse(agency_category=="CDC","#a7c6ed",
                                  ifelse(agency_category=="Other","#ba0c2f","Other"))))
df1<-df1%>%
  mutate(`Funding`=(`Funding`/1e9))
#Adjust FY labels
my_labels <- seq(2018, 20212, 1) %>% 
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
  
  mutate(color = ifelse(agency_category == "USAID", "#002a6c", 
                        ifelse(agency_category=="CDC","#a7c6ed",
                               ifelse(agency_category=="Other","#ba0c2f","Other"))))%>%
  mutate( agency_category = fct_relevel(agency_category, "USAID","CDC","Other"))
  

v <- df2 %>% 
  ggplot(aes(`Fiscal Year`,`Funding`, n, fill=`color`))+
geom_col(aes(y = total_n), fill = "#8C8985", alpha = .2)+
  geom_col()+
 facet_wrap(~agency_category)+
   scale_fill_identity()+
  si_style_nolines()+
  theme(legend.position = "none")+
  scale_y_continuous(labels = unit_format(1., unit = "B"))+
  scale_x_discrete(labels = cust_labels)+
  
 labs(
   title =  "Agency Funding for HIV Programming FY18-22 (USD $B)",
   caption = glue("Source: FY21Q4c FSD. Visual Includes commodities and M&O
                  SI analytics: {paste(authors)}
              US Agency for International Development")) +
  
  si_style_nolines()+
  theme(legend.position = "none")
si_save("agency_trends.svg")
si_save(path="~/GitHub/agitprop","23_Powerfunding_trends.svg")
