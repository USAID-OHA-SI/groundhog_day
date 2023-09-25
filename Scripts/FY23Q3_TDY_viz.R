# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY23Q3 review: TDY analysis
# REF ID:   a04654e7 
# LICENSE:  MIT
# DATE:     2023-09-25
# UPDATED: 

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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata() 
    
    metadata$source <- "UTRAMS data, pulled Sept 6, 2023"
  
  ref_id <- "a04654e7"
  
  data_folder <- "Data/"

# IMPORT ------------------------------------------------------------------
  
  df_tdy <- data_folder %>% 
    return_latest("OHA Travel Data Analysis.xlsx") %>% 
    read_xlsx(sheet = "Raw Data", col_types = "text")

  
df_tdy_2 <-  data_folder %>% 
    return_latest("tdy_data_condensed") %>% 
    read_csv()

# MUNGE -------------------------------------------------------------------
  
#dataset for TDY totals per FY
df_tdy_total <-  df_tdy %>%
    janitor::clean_names() %>% 
    filter(
          bureau == "BUREAU - GH",
           office == "OHA",
           pepfar_ou %ni% c("Denmark (incl. Greenland)", "Jordan", "Netherlands", "BUREAU - GH",
                            "United Kingdom (Britain)", "United States of America", "Madagascar", "Malaysia",
                            "United Arab Emirates", "Bangladesh", "Pacific Islands", "Sudan", "Switzerland"),
           purpose_category == "A. Field/Mission Managed Services & Technical Assistance",
           request_status %in% c("2. Lead Assigned", "2a. Supervisor approved", "3. Complete")) %>%
 # mutate(fy = substr(start_date_fiscal_quarter, start = 1, stop = 6)) %>% 
  count(fiscal_year) %>% 
  rename(total_tdys = n)

df_viz1 <- df_tdy %>%
  janitor::clean_names() %>% 
  filter(
    bureau == "BUREAU - GH",
    office == "OHA",
    pepfar_ou %ni% c("Denmark (incl. Greenland)", "Jordan", "Netherlands", "BUREAU - GH",
                     "United Kingdom (Britain)", "United States of America", "Madagascar", "Malaysia",
                     "United Arab Emirates", "Bangladesh", "Pacific Islands", "Sudan", "Switzerland"),
    purpose_category == "A. Field/Mission Managed Services & Technical Assistance",
    request_status %in% c("2. Lead Assigned", "2a. Supervisor approved", "3. Complete")) %>%
  group_by(fiscal_year) %>% 
  mutate(loe_days = as.numeric(loe_days)) %>% 
  summarise(across(starts_with("loe_days"), sum, na.rm = TRUE), .groups = "drop") %>% 
  left_join(df_tdy_total, by = "fiscal_year")



# VIZ ----------------------------------------------------------------------
  
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
  scale_y_continuous(limits = c(0, 600)) +
  labs(x = NULL,
       y = NULL,
       title = 'In FY23, OHA Technical Assistance TDYs returned to pre-pandemic levels with 540 total TDYs planned' %>% toupper(),
       subtitle = "Total number of TDYs completed") +
  theme(axis.title = element_blank(),
        legend.position = "none")

v2 <- df_viz1 %>% 
  filter(fiscal_year != "FY2024") %>% 
  mutate(color = scooter_med) %>% 
  ggplot(aes(x = fiscal_year, y = loe_days, group= color, color = color, fill = color)) +
  geom_col() +
  scale_fill_identity() + 
  scale_color_identity() + 
  si_style_ygrid() +
  geom_text(aes(label = comma(loe_days), size = 14/.pt,
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
