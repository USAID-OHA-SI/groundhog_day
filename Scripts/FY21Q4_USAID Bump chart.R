# PURPOSE:  PLL Bump Chart
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2022-01-18
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(tidyverse)
    library(gophr)
    library(scales)
    library(extrafont)
    library(tidytext)
    library(here)
    library(gt)
    library(janitor)
library(googledrive)
library(googlesheets4)
    
df.rankings <- df %>% 
  group_by(date) %>% 
  arrange(date, desc(gold), desc(silver), desc(bronze), country) %>% 
  mutate(ranking = row_number(),
         day = as.numeric(as.Date(date)) - 17571) %>% 
  as.data.frame()
  
 
  
  

# LOAD DATA ============================================================================  
id<- googlesheets4::as_sheets_id('1ckIspYD-G5rkOWuh40S-X4JbaJkzXjhrpGrr_Cbwjt8')
df_pll <- googlesheets4::read_sheet(id) 



fy<-c("2021","2022","2023")


# MUNGE ============================================================================
  
  df_pll<-df_pll%>%
select(-Dedup)
df_pll<-df_pll%>%
  pivot_longer(CDC:USAID,names_to="agency", values_to="funding")

df_pll<-df_pll%>%
  filter(agency=="USAID",
         `Fiscal Year`%in% fy)%>%
  group_by(`Fiscal Year`) %>% 
  mutate(`Fiscal Year`=as.numeric(`Fiscal Year`))%>%
  arrange(`Fiscal Year`, desc(funding), operating_unit) %>% 
  mutate(ranking = row_number(),
         day = `Fiscal Year` - 2020) %>% 
 
  as.data.frame()

df_pll <- df_pll %>%
  mutate(flag = ifelse(operating_unit %in% c("Mozambique","South Africa","Namibia","Tanzania","Eswatini","Nigeria,"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, operating_unit, "zzz"))
  
# VIZ ============================================================================
show.top.n <- 15
show.top.n <- 15

ggplot(data = df_pll, aes(x = day, y = ranking, group = operating_unit)) +
  geom_line(aes(color = country_col, alpha = 1), size = 2) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(1,nrow(df_pll)) +
  scale_x_continuous(breaks = 1:3, minor_breaks = 1:3, expand = c(.05, .05)) +
  geom_text(data = df_pll %>% filter(day == 1),
            aes(label = operating_unit, x = 0.5) , hjust = .5, fontface = "bold", color = "#888888", size = 3) +
  geom_text(data = df_pll %>% filter(day == 2),
            aes(label = operating_unit, x = 3.5) , hjust = 0.5, fontface = "bold", color = "#888888", size = 3) +
  glitr::si_style_nolines()+
  
 
     
  
  
    theme(legend.position = "none") +
  scale_color_manual(values = c(denim,trolley_grey))
         y = "Rank",
         title = "USAID COP Budgets",
         subtitle = "Countries ranked by overall funding each fiscal year") +
  coord_cartesian(ymax = c(1,show.top.n)) + 
    
    scale_x_continuous(breaks = 2018:2022, expand = c(.05, .5))+
    # geom_text(+
    geom_text(data = df_pll %>% filter(`Fiscal Year` == "2022"),
              aes(label = operating_unit, x = 16.5) , hjust = 4.15, fontface = "bold", color = "#888888", size = 4)+
    geom_text(data = df_pll %>% filter(`Fiscal Year` == "2018"),
              aes(label = operating_unit, x = 16.5) , hjust = 0.15, fontface = "bold", color = "#888888", size = 4)
  
  
  +
    
    

# SPINDOWN ============================================================================

