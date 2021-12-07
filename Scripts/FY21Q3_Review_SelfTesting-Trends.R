# PURPOSE: FY21Q3 Data Review - Self Testing
# AUTHOR: K Srikanth | SI
# LICENSE: MIT
# DATE: 2021-10-12
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

# Set paths  
data   <- "Data"
dataout <- "Dataout"
images  <- "Images"
graphs  <- "Graphics"


# LOAD DATA ============================================================================  

msd <- si_path() %>% 
  return_latest("OU_IM_FY19-22") %>% 
  read_msd()

# MUNGE =================================================================================

#DF for HTS_TST / HTS_SELF Trends
df_hts <- msd %>% 
  filter(fundingagency == "USAID",
         fiscal_year <= 2021,
         indicator %in% c("HTS_TST", "HTS_SELF"),
         standardizeddisaggregate == "Total Numerator") %>%
  group_by(indicator, fiscal_year, fundingagency) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd() %>%
  mutate(fill_color = ifelse(indicator == "HTS_SELF", scooter, denim))

#Df for HTS growth rates and comparison
df_growth <- msd %>% 
  filter(fundingagency == "USAID",
         fiscal_year <= 2021,
         indicator %in% c("HTS_TST", "HTS_SELF"),
         standardizeddisaggregate == "Total Numerator") %>%
  group_by(indicator, fiscal_year, fundingagency) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd() %>%
  pivot_wider(names_from = indicator, values_from = value, names_glue = "{tolower(indicator)}") %>%
  mutate(hts_tst_growth = 100*(hts_tst - lag(hts_tst))/lag(hts_tst),
         hts_self_growth = 100*(hts_self - lag(hts_self))/lag(hts_self))

#DF for trends across just KP and Men
df_kp_men <- msd %>% 
  filter(indicator %in% c("HTS_SELF"),
         fiscal_year <= 2021,
         fundingagency == "USAID",
         (standardizeddisaggregate == "KeyPop/HIVSelfTest" | 
            (standardizeddisaggregate == "Age/Sex/HIVSelfTest" & sex == "Male"))) %>% 
  mutate(sex = ifelse(standardizeddisaggregate == "KeyPop/HIVSelfTest", "KeyPop", sex)) %>% 
  group_by(indicator, fiscal_year, sex, fundingagency) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd() %>% 
  mutate(fill_color = ifelse(sex == "KeyPop", "#5BB5D5", scooter_light))

# VIS ========================================================================
# Adjust sizing for each to fit on the boxes in Q3 review slide deck

#HTS_SELF Quarterly Trends Bar Chart
df_hts %>% 
  filter(indicator == "HTS_SELF") %>%
  ggplot(aes(period, value, fill = fill_color)) +
  geom_col(position = "identity") +
  geom_text(aes(label = format(value, big.mark = ",")), vjust = -1, na.rm = TRUE,
            family = "Source Sans Pro", color = trolley_grey) +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL) +=
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

ggsave("hts_self_v1.svg", width = 2.23, height = 3.2, units = "in", dpi = 300)

#HTS_SELF quarterly trends: MEN
df_kp_men %>% 
  filter(sex == "Male") %>%
  ggplot(aes(period, value, fill = fill_color)) +
  geom_col(position = "identity") +
  geom_text(aes(label = format(value, big.mark = ",")), vjust = -1, na.rm = TRUE,
            family = "Source Sans Pro", color = trolley_grey) +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL, title = "Self Testing Trends for Men") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

ggsave("hts_self_v2_male.svg", width = 2.23, height = 1.5, units = "in", dpi = 300)

#HTS_SELF quarterly trends: KP
df_kp_men %>% 
  filter(sex == "KeyPop") %>%
  ggplot(aes(period, value, fill = fill_color)) +
  geom_col(position = "identity") +
  geom_text(aes(label = format(value, big.mark = ",")), vjust = -1, na.rm = TRUE,
            family = "Source Sans Pro", color = trolley_grey) +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL, title = "Self Testing Trends for Key Populations") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

ggsave("hts_self_v2_kp.svg", width = 2.23, height = 1.5, units = "in", dpi = 300)

#HTS_SELF growth rate Viz
df_growth %>% 
  mutate(line_color = scooter) %>% 
  ggplot(aes(x = period, y = hts_self_growth, color = line_color, group = NA)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_area(aes(y = hts_self_growth), fill = scooter, alpha = .25) +
  geom_hline(yintercept = 0, color = trolley_grey) +
  geom_vline(xintercept = "FY20Q2", color = trolley_grey, linetype = "dashed") +
  coord_cartesian(clip = "off") +
  #facet_wrap(~operatingunit, scales = "free_y") +
  scale_color_identity() +
  si_style()+
  labs(x = NULL, y = NULL, title = "Self-Testing Growth Rates") +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

ggsave("hts_self_growthrate.svg", width = 2.23, height = 3.2, units = "in", dpi = 300)

#OFFSET Bar Chart: HTS_SELF vs. HTS_TST
nudge_space <- 0.125 #set nudge for offset bars

df_growth %>% 
  mutate(share = hts_self/hts_tst) %>% 
  ggplot(aes(x = period)) +
  geom_col(aes(y = hts_tst),fill = "#cfc3ff", alpha = 1, position = position_nudge(x = -nudge_space)) +
  geom_col(aes(y = hts_self),fill = scooter, position = position_nudge(x = nudge_space)) +
  geom_label(aes(y = 0, label = percent(share, 1), fill = share, 
                 color = ifelse(share > 0.3, "white", grey90k)), 
             vjust = 1.3, 
             size = 10/.pt, 
             label.size = NA, family = "Source Sans Pro") +
  scale_y_continuous(labels = label_number_si()) +
  scale_fill_identity() +
  scale_color_identity() +
  si_style_ygrid() +
  labs(x = NULL, y = NULL) +
  theme(strip.text = element_text(family = "Source Sans Pro SemiBold"))

ggsave("hts_self_vs_test.svg", width = 2.23, height = 3.2, units = "in", dpi = 300)
