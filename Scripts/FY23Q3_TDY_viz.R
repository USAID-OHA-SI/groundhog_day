# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY23Q3 review: TDY analysis (update for Fy23Q4)
# REF ID:   a04654e7 
# LICENSE:  MIT
# DATE:     2023-09-25
# UPDATED: 2024-02-13

# DEPENDENCIES ------------------------------------------------------------
  
  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  library(gt)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata() 
    
    metadata$source <- "UTRAMS data [as of FY23Q4]"
  
  
  data_folder <- "Data/"
  
  ref_id <- "a04654e7"

# IMPORT ------------------------------------------------------------------
  
  g_id <- "1TasP3wkmDdTwaDqn9IjRQVOTPfZFtjSdfXo-myeBljk"
  
 df_tdy <- read_sheet(g_id, sheet = "UTRAMS Data")
  
  # df_tdy2 <- data_folder %>% 
  #   return_latest("OHA Travel Data Analysis.xlsx") %>% 
  #   read_xlsx(sheet = "Raw Data", col_types = "text")
  
cntry_xwalk <- data_folder %>% 
    return_latest("OHA Travel Data Analysis.xlsx") %>% 
    read_xlsx(sheet = "PEPFAR Crosswalk", col_types = "text")
  


  df_tdy_budget <- data_folder %>% 
    return_latest("OHA Travel Data Analysis.xlsx") %>% 
    read_xlsx(sheet = "FY23 TDYs v Budget") %>% 
    select(1:3) %>% 
    rename(ou = ...1,
           num_tdy = `FY2023 TDYs`,
           fy23_budget_usaid = `FY2023 USAID Budget`)


 df_tdy <- df_tdy %>%
    left_join(cntry_xwalk) %>% 
    janitor::clean_names() %>% 
    mutate(fiscal_year = substr(start_date_fiscal_quarter, start = 1, stop = 6))

# MUNGE -------------------------------------------------------------------
  
#dataset for TDY totals per FY
df_tdy_total <-  df_tdy %>%
  #  mutate(fiscal_year = substrRight(start_date_fiscal_quarter, 6))
    filter(
          bureau == "BUREAU - GH",
           office == "OHA",
           # pepfar_ou %ni% c("Denmark (incl. Greenland)", "Jordan", "Netherlands", "BUREAU - GH",
           #                  "United Kingdom (Britain)", "United States of America", "Madagascar", "Malaysia",
           #                  "United Arab Emirates", "Bangladesh", "Pacific Islands", "Sudan", "Switzerland"),
           purpose_category == "A. Field/Mission Managed Services & Technical Assistance",
           request_status %in% c("3. Complete")) %>%
  count(fiscal_year) %>% 
  rename(total_tdys = n)

df_viz1 <- df_tdy %>%
  janitor::clean_names() %>% 
  filter(
    bureau == "BUREAU - GH",
    office == "OHA",
    # pepfar_ou %ni% c("Denmark (incl. Greenland)", "Jordan", "Netherlands", "BUREAU - GH",
    #                  "United Kingdom (Britain)", "United States of America", "Madagascar", "Malaysia",
    #                  "United Arab Emirates", "Bangladesh", "Pacific Islands", "Sudan", "Switzerland"),
    purpose_category == "A. Field/Mission Managed Services & Technical Assistance",
    request_status %in% c("3. Complete")) %>%
  group_by(fiscal_year) %>% 
  mutate(duration = as.numeric(duration)) %>% 
  summarise(across(starts_with("duration"), sum, na.rm = TRUE), .groups = "drop") %>% 
  left_join(df_tdy_total, by = "fiscal_year")


# Total TDY days vs COP budget

#this currently includes USAID WCF + M&O
df_budget_viz <- df_tdy %>%
  janitor::clean_names() %>% 
  filter(
    bureau == "BUREAU - GH",
    fiscal_year == "FY2023",
    office == "OHA",
    pepfar_ou %ni% c("Denmark (incl. Greenland)", "Jordan", "Netherlands", "BUREAU - GH",
                     "United Kingdom (Britain)", "United States of America", "Madagascar", "Malaysia",
                     "United Arab Emirates", "Bangladesh", "Pacific Islands", "Sudan", "Switzerland", "West Africa Regional Mission"),
    purpose_category == "A. Field/Mission Managed Services & Technical Assistance",
    request_status %in% c("2. Lead Assigned", "2a. Supervisor approved", "3. Complete")) %>%
  mutate(pepfar_ou = case_when(pepfar_ou == "Congo, Democratic Republic of the" ~ "DRC",
                               pepfar_ou %in% c("Central & South America", "Caribbean") ~ "Western Hemisphere",
                               TRUE ~ pepfar_ou)) %>% 
  group_by(fiscal_year, pepfar_ou) %>% 
  mutate(duration = as.numeric(duration)) %>% 
  summarise(across(starts_with("duration"), sum, na.rm = TRUE), .groups = "drop") %>%
  left_join(df_tdy_budget, by = c('pepfar_ou' = "ou")) %>% 
  mutate(total_budget = sum(fy23_budget_usaid)) %>% 
  mutate(budget_share = fy23_budget_usaid / total_budget) %>% 
  arrange(desc(budget_share)) 

# TDY purpose visuals ------

df_purpose <- df_tdy %>%
  janitor::clean_names() %>% 
  filter(
    bureau == "BUREAU - GH",
    office == "OHA",
    fiscal_year == "FY2023",
    # pepfar_ou %ni% c("Denmark (incl. Greenland)", "Jordan", "Netherlands", "BUREAU - GH",
    #                  "United Kingdom (Britain)", "United States of America", "Madagascar", "Malaysia",
    #                  "United Arab Emirates", "Bangladesh", "Pacific Islands", "Sudan", "Switzerland"),
  #  purpose_category == "A. Field/Mission Managed Services & Technical Assistance",
    request_status %in% c("3. Complete")) %>% 
  count(purpose_category, purpose_name) %>% 
  group_by(purpose_category) %>% 
  mutate(total_cat = sum(n)) %>% 
  ungroup() %>% 
  rename(total_subcat = n)


# DIVISION TA VIZ -----------
df_div <- df_tdy %>%
  janitor::clean_names() %>% 
  filter(
    bureau == "BUREAU - GH",
    fiscal_year == "FY2023",
    office == "OHA",
    # pepfar_ou %ni% c("Denmark (incl. Greenland)", "Jordan", "Netherlands", "BUREAU - GH",
    #                  "United Kingdom (Britain)", "United States of America", "Madagascar", "Malaysia",
    #                  "United Arab Emirates", "Bangladesh", "Pacific Islands", "Sudan", "Switzerland"),
    purpose_category == "A. Field/Mission Managed Services & Technical Assistance",
    request_status %in% c("3. Complete")) %>%
  mutate(pepfar_ou = case_when(pepfar_ou == "Congo, Democratic Republic of the" ~ "DRC",
                               pepfar_ou %in% c("Central & South America", "Caribbean") ~ "Western Hemisphere",
                               TRUE ~ pepfar_ou)) %>% 
  count(pepfar_ou, division) %>% 
  group_by(division) %>% 
  mutate(div_total = sum(n)) %>% 
  ungroup() %>% 
  rename(ou_total = n)

top15_oha_tdy <- df_div %>% 
  group_by(pepfar_ou) %>% 
  summarise(across(starts_with("ou_total"), sum, na.rm = T), .groups = "drop") %>% 
  arrange(desc(ou_total)) %>% 
  slice(1:15) %>% 
  pull(pepfar_ou)
  
# DIVISION CONF VIZ -----------
df_div_conf <- df_tdy %>%
  janitor::clean_names() %>% 
  filter(
    bureau == "BUREAU - GH",
    fiscal_year == "FY2023",
    office == "OHA",
    # pepfar_ou %ni% c("Denmark (incl. Greenland)", "Jordan", "Netherlands", "BUREAU - GH",
    #                  "United Kingdom (Britain)", "United States of America", "Madagascar", "Malaysia",
    #                  "United Arab Emirates", "Bangladesh", "Pacific Islands", "Sudan", "Switzerland"),
    purpose_category == "B. Conference/Meeting/Workshop Attendance and Presenting",
    request_status %in% c("3. Complete")) %>%
  mutate(pepfar_ou = case_when(pepfar_ou == "Congo, Democratic Republic of the" ~ "DRC",
                               pepfar_ou %in% c("Central & South America", "Caribbean") ~ "Western Hemisphere",
                               TRUE ~ pepfar_ou)) %>% 
  count(pepfar_ou, division) %>% 
  group_by(division) %>% 
  mutate(div_total = sum(n)) %>% 
  ungroup() %>% 
  rename(ou_total = n)

top15_oha_conf <- df_div_conf %>% 
  group_by(pepfar_ou) %>% 
  summarise(across(starts_with("ou_total"), sum, na.rm = T), .groups = "drop") %>% 
  arrange(desc(ou_total)) %>% 
  slice(1:15) %>% 
  pull(pepfar_ou)


# VIZ ----------------------------------------------------------------------
fy23_tdys <- df_viz1 %>% 
  filter(fiscal_year =="FY2023") %>% 
  pull(total_tdys)

# TOTAL TDY COUNTS  
v1 <- df_viz1 %>% 
  filter(fiscal_year != "FY2024") %>% 
  mutate(color = scooter) %>% 
  ggplot(aes(x = fiscal_year, y = total_tdys, group= color, color = color, fill = color)) +
  #geom_col() +
  geom_line(linewidth = 1) +
  geom_point(size = 12) +
  geom_text(aes(y = total_tdys,
                label = total_tdys, size = 14/.pt, hjust = 0.5,
            #position = "stack",
            color = "white",
            family = "Source Sans Pro")) +
  geom_vline(xintercept = "FY2021", linetype = "dashed", color = trolley_grey) +
  scale_fill_identity() + 
  scale_color_identity() + 
  si_style_ygrid() +
  scale_y_continuous(limits = c(0, 800)) +
  labs(x = NULL,
       y = NULL,
       title = glue('In FY23, OHA Technical Assistance TDYs returned to pre-pandemic levels with {fy23_tdys} total TDYs planned') %>% toupper(),
       subtitle = "Total number of TDYs completed") +
  theme(axis.title = element_blank(),
        legend.position = "none")

v2 <- df_viz1 %>% 
  filter(fiscal_year != "FY2024") %>% 
  mutate(color = scooter_med) %>% 
  ggplot(aes(x = fiscal_year, y = duration, group= color, color = color, fill = color)) +
  geom_col() +
  scale_fill_identity() + 
  scale_color_identity() + 
  si_style_ygrid() +
  scale_y_continuous(limits = c(0, 8000)) +
  geom_text(aes(label = comma(duration), size = 14/.pt,
                vjust = -0.5,
                #position = "stack",
                color = grey80k,
                family = "Source Sans Pro"))  +
  labs(x = NULL,
       y = NULL,
      # title = 'FY22 actually counted more days of OHA TA TDYs than FY23' %>% toupper(),
       subtitle = "Total number of TDY days completed",
       caption = glue("Focused only on completed TDYs classified as TA
                      Source: {metadata$source} | Ref id: {ref_id}")) +
  theme(axis.title = element_blank(),
        legend.position = "none")

v1 / v2 +
  plot_layout(height = c(1,2)) 

si_save("Graphics/01_TDY_total.svg")


# TDY VS BUDGET VIZ --

df_budget_viz %>% 
  mutate(fill_color = case_when(budget_share > 0.05 & duration > 500 ~ genoa,
                                budget_share > 0.05 & duration <= 500 ~ golden_sand,
                                budget_share <= 0.05 & duration > 500 ~ scooter,
                                budget_share <= 0.05 & duration <= 500 ~ old_rose)) %>% 
  ggplot(aes(x=budget_share, y=duration, color = fill_color)) +
  geom_point(size=3) +
  geom_hline(yintercept = 500, colour = "#D3D3D3") +
  geom_vline(xintercept = .050, colour = "#D3D3D3") +
    scale_x_continuous(labels = scales::percent) +
  scale_color_identity() +
  ggrepel::geom_text_repel(aes(label = pepfar_ou), size = 4, na.rm = TRUE, family = "Source Sans Pro") +
  si_style_nolines() +
  labs(x = "Share of USAID COP23 Budget",
       y = "# TDY Days in FY23",
       title = "FY23 LENGTH OF TECHNICAL ASSISTANCE TDYS VS USAID COP23 BUDGET SHARE" %>% toupper(),
       subtitle = "Does not include conferences, donor coordination, etc",
       caption = glue("Source: {metadata$source} | Ref id: {ref_id}"))

si_save("Graphics/02_TDY_BUDGET.svg")


# Purpose viz ---

v3 <- df_purpose %>% 
  distinct(purpose_category, total_cat) %>% 
  mutate(purpose_category = case_when(purpose_category == "A. Field/Mission Managed Services & Technical Assistance" ~ "Field/Mission Services & TA",
                                      purpose_category == "B. Conference/Meeting/Workshop Attendance and Presenting" ~ "Conferences/Meetings/Workshops",
                                      purpose_category == "C. Donor coordination" ~ "Donor coordination",
                                      purpose_category == "E. Not for Direct Mission Support" ~ "Not Direct Mission Support",
                                      purpose_category == "D. Other" ~ "Other",
         TRUE ~ purpose_category)) %>% 
  mutate(fill_color = ifelse(purpose_category == "Field/Mission Services & TA", golden_sand, scooter)) %>% 
  mutate(total = sum(total_cat),
         share= total_cat/total) %>% 
  ggplot(aes(x = fct_reorder(purpose_category, total_cat), y = total_cat, fill = fill_color)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 700)) +
  si_style_xgrid() +
  scale_fill_identity() +
  geom_text(aes(y = total_cat,
                label = total_cat), size = 12/.pt, hjust = -0.2,
            #position = "stack",
            #  color = "white",
            family = "Source Sans Pro") +
  geom_text(aes(y = total_cat,
                label = percent(share, 1)), size = 12/.pt, hjust = -0.2,vjust = 2,
            #position = "stack",
            #  color = "white",
            family = "Source Sans Pro") +
  labs(x = NULL, y = NULL,
       title = "The primary function of OHA's travel is TA, but conference attendance and other purposes also takes up a significant portion of TDYs" %>% toupper(),
       subtitle = "Number of FY23 OHA TDYs per purpose type",
       caption = glue("Source: {metadata$source} | Ref id: {ref_id}"))


v4 <- df_purpose %>% 
  mutate(purpose_category = case_when(purpose_category == "A. Field/Mission Managed Services & Technical Assistance" ~ "Field/Mission Services & TA",
                                      purpose_category == "B. Conference/Meeting/Workshop Attendance and Presenting" ~ "Conferences/Meetings/Workshops",
                                      purpose_category == "C. Donor coordination" ~ "Donor coordination",
                                      purpose_category == "E. Not for Direct Mission Support" ~ "Not Direct Mission Support",
                                      purpose_category == "D. Other" ~ "Other",
                                      TRUE ~ purpose_category)) %>% 
  filter(purpose_category == "Field/Mission Services & TA") %>% 
  ggplot(aes(x = fct_reorder(purpose_name, total_subcat), y = total_subcat, fill = golden_sand, alpha = 0.7)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 300)) +
  si_style_nolines() +
  scale_fill_identity() +
  scale_alpha_identity() +
  geom_text(aes(y = total_subcat,
                label = total_subcat), size = 12/.pt, hjust = -0.2,
            #position = "stack",
            #  color = "white",
            family = "Source Sans Pro") +
  labs(x = NULL, y = NULL,
     #  title = "The primary function of OHA's travel is TA, but conference attendance and other purposes also takes up a significant portion of TDYs" %>% toupper(),
       subtitle = "Type of OHA Field Service/TA Support TDYs",
       caption = glue("Source: {metadata$source} | Ref id: {ref_id}"))


v3 + v4 +
  plot_layout(width = c(2,1)) 

si_save("Graphics/04_TDY_purpose.svg")

#Division level viz ----

df_div <- df_div %>% 
  mutate(division = str_remove(division, "OHA |OHA/"))

div1 <- df_div %>% 
  distinct(division, div_total) %>% 
  ggplot(aes(x = fct_reorder(division, div_total), y = div_total, fill = denim)) +
  geom_col() +
  coord_flip() +
  si_style_xgrid() +
  scale_fill_identity() +
  geom_text(aes(y = div_total,
                label = div_total), size = 12/.pt, hjust = -0.2,
            #position = "stack",
              color = grey80k,
            family = "Source Sans Pro") +
  labs(x = NULL, y = NULL,
       title = "Of the 615 TA TDYs in FY23, PCT and SIEI have the most TDYs" %>% toupper(),
       subtitle = "Number of FY23 OHA TA TDYs by division")

div2 <- df_div %>% 
  filter(pepfar_ou %in% top15_oha_tdy) %>% 
  distinct(pepfar_ou, division, ou_total) %>% 
  ggplot(aes(x = division, 
             y = fct_reorder(pepfar_ou, ou_total), 
             fill = ou_total)) +
  geom_tile(color = "white", 
            size = 0.5) +
  geom_text(aes(label = ou_total,
                color = if_else(ou_total >= 14, "white", grey90k)),
            size = 3,
            family= "Source Sans Pro") +
  scale_x_discrete(position = "top", 
                   guide = guide_axis(n.dodge = 2)) +
  scale_fill_si(palette = "denims", discrete = FALSE) +
  scale_color_identity() +
  si_style_nolines() +
  theme(panel.background = element_rect(fill = "#f6f6f6", color ="white"),
        legend.position = "none") +
  labs(x = NULL, 
       y = NULL,
      # title = "Of the 615 TA TDYs in FY23, PCT and SIEI have the most TDYs" %>% toupper(),
       subtitle = "Top 15 FY23 Technical Assistance TDY Destinations for OHA, by division",
      # subtitle = "From August 2023 patient audit tool",
       caption = glue("Source: {metadata$source} | Ref id: {ref_id}")) 


div1 / div2 +
  plot_layout(heights = c(1,3))

si_save("Graphics/05_division_TA.svg")

#Division level conf viz ----

df_div_conf <- df_div_conf %>% 
  mutate(division = str_remove(division, "OHA |OHA/"))

div1_conf <- df_div_conf %>% 
  distinct(division, div_total) %>% 
  ggplot(aes(x = fct_reorder(division, div_total), y = div_total, fill = "#b98abf")) +
  geom_col() +
  coord_flip() +
  si_style_xgrid() +
  scale_fill_identity() +
  geom_text(aes(y = div_total,
                label = div_total), size = 12/.pt, hjust = -0.2,
            #position = "stack",
            color = grey80k,
            family = "Source Sans Pro") +
  labs(x = NULL, y = NULL,
       title = "Of the 327 Conference TDYs in FY23, SIEI has the most TDYs" %>% toupper(),
       subtitle = "Number of FY23 OHA Conference TDYs by division")

div2_conf <- df_div_conf %>% 
  filter(pepfar_ou %in% top15_oha_tdy) %>% 
  distinct(pepfar_ou, division, ou_total) %>% 
  ggplot(aes(x = division, 
             y = fct_reorder(pepfar_ou, ou_total), 
             fill = ou_total)) +
  geom_tile(color = "white", 
            size = 0.5) +
  geom_text(aes(label = ou_total,
                color = if_else(ou_total >= 14, "white", grey90k)),
            size = 3,
            family= "Source Sans Pro") +
  scale_x_discrete(position = "top", 
                   guide = guide_axis(n.dodge = 2)) +
  scale_fill_si(palette = "moody_blues", discrete = FALSE) +
  scale_color_identity() +
  si_style_nolines() +
  theme(panel.background = element_rect(fill = "#f6f6f6", color ="white"),
        legend.position = "none") +
  labs(x = NULL, 
       y = NULL,
       # title = "Of the 615 TA TDYs in FY23, PCT and SIEI have the most TDYs" %>% toupper(),
       subtitle = "Top 15 FY23 Conference TDY Destinations for OHA, by division",
       # subtitle = "From August 2023 patient audit tool",
       caption = glue("Source: {metadata$source} | Ref id: {ref_id}")) 


div1_conf / div2_conf +
  plot_layout(heights = c(1,3))
