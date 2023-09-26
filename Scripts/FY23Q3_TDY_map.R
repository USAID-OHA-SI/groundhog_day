# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  FY23Q3 review: TDY analysis
# REF ID:   6329eea5 
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
library(sf)
library(rnaturalearth)
library(gisr)
library(rmapshaper)


# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()

# Grab metadata
get_metadata() 

metadata$source <- "UTRAMS data, pulled Sept 6, 2023"


data_folder <- "Data/"

ref_id <- "6329eea5"

# IMPORT ------------------------------------------------------------------

df_tdy <- data_folder %>% 
  return_latest("OHA Travel Data Analysis.xlsx") %>% 
  read_xlsx(sheet = "Raw Data", col_types = "text")

#shapefile
spdf <- ne_countries(type = "sovereignty", 
                     scale = 110, 
                     returnclass = "sf") %>% 
  select(sovereignt, admin, name, adm0_a3, continent, subregion) %>% 
  filter(admin != "Antarctica") %>% # Remove Antarctica
  clean_countries(colname = "admin")

# MUNGE -------------------------------------------------------------------

df_map <- df_tdy %>%
  janitor::clean_names() %>% 
  filter(
    bureau == "BUREAU - GH",
    office == "OHA",
    fiscal_year == "FY2023",
    pepfar_ou %ni% c("Denmark (incl. Greenland)", "Jordan", "Netherlands", "BUREAU - GH",
                     "United Kingdom (Britain)", "United States of America", "Madagascar", "Malaysia",
                     "United Arab Emirates", "Bangladesh", "Pacific Islands", "Sudan", "Switzerland"),
    purpose_category == "A. Field/Mission Managed Services & Technical Assistance",
    request_status %in% c("2. Lead Assigned", "2a. Supervisor approved", "3. Complete")) %>%
  count(pepfar_ou, destination, fiscal_year) %>% 
  rename(total_tdys = n) %>% 
  mutate(destination = case_when(destination == "Congo, Democratic Republic of the" ~ "Democratic Republic of the Congo",
                                TRUE ~ destination)) %>% 
  View()

# Define a projection to make Greenland a bit smaller & allow for zooom
spdf <- ms_simplify(spdf, keep = 0.75)

spdf_rob <- st_transform(spdf, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Join MSD with shapefiles
spdf_ou <- spdf_rob %>% 
  mutate(admin = ifelse(admin == "Swaziland", "Eswatini", admin)) %>% 
  right_join(df_map, 
             by = c("admin" ="destination")) %>% 
  mutate(fill_color = case_when(total_tdys > 80 ~ "#022e24",
                                total_tdys <= 30 & total_tdys > 25 ~ "#01564B",
                                total_tdys <= 25 & total_tdys > 20 ~ "#0D6C5F",
                                total_tdys <= 20 & total_tdys > 15 ~ "#2D8073",
                                total_tdys <= 15 & total_tdys > 10 ~ "#5CAC9E",
                                total_tdys <= 10 & total_tdys > 5 ~ "#89DACB",
                                total_tdys <= 5 & total_tdys > 0 ~ "#A0F2E2"))


## Global Map
ggplot() +
  geom_sf(data = spdf_rob, fill = "white", color = trolley_grey, size = .4) +
  geom_sf(data = spdf_ou, aes(fill = fill_color), color = NA,  alpha = 0.8) +
  geom_sf_text(data = spdf_ou, aes(label = total_tdys), size = 2,
               na.rm = TRUE, family = "Source Sans Pro", color = "white") +

  # scale_fill_viridis_c(option = "carto_div_tropic", alpha = 0.9, direction = -1,
  #                     breaks = c(0, 20,40,60,80),
  #                      limits = c(0, 90),
  #                 #     labels = percent
  #                    ) +
#  scale_fill_si(palette = "carto_sunset", labels = comma)+
  scale_fill_identity() +
  si_style_map() +
  theme(
    legend.direction = "horizontal",
    panel.background = element_rect(fill = "#bfddff40", color = "NA"),
    plot.title = element_text(hjust =  0),
    plot.caption = element_text(hjust = 1),
  ) +
  labs(label = "", title = "Number of TDYs in FY23 by PEPFAR Country" %>% toupper(),
       subtitle = "Only inclusive of Technical Assistance TDYs - does not include conferences, donor coordination, etc",
       x = NULL, y = NULL)

si_save("03b_tdy_global_map.svg")


