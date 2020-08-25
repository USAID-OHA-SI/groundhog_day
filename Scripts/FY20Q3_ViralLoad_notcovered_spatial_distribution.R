##  PROJECT: Q3 Review 
##  AUTHOR:  B.Kagniniwa & G.Sarfaty | USAID
##  PURPOSE: Geo-depiction of VL - % not covered
##  LICENCE: MIT
##  DATE:    2020-08-25

# Libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(tidytext)
library(sf)
library(gisr)
library(glitr)
library(glamr)
library(janitor)
library(scales)
library(patchwork)
library(ggrepel)
library(here)
library(ICPIutilities)
library(googlesheets4)
library(extrafont)

# GLOBALS -------------------------------------------------------------

## Data & Output folders
dir_data <- "Data"
dir_dataout <- "Dataout"
dir_gis <- "GIS"
dir_graphics <- "Graphics"
dir_geodata <- "../../GEODATA/PEPFAR"
dir_geo <- "../../GEODATA/PEPFAR"
dir_terr <- "../../GEODATA/RASTER"
dir_merdata <- "../../MERDATA"


# MER Data
file_psnu_im <- (list.files(path = dir_merdata,
                            #pattern = "Structured",
                            pattern = "*._PSNU_IM_FY18-20_20200814_v1_1.zip$",
                            recursive = TRUE,
                            full.names = TRUE)) 

#df <- read_msd(file_psnu_im)
df <- vroom::vroom(file_psnu_im)

# GEO Data
gis_5_sfc <- list.files(dir_geo, pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

gis_4_sfc <- list.files(dir_geo, pattern = ".*_4_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

nga1 <- get_adm_boundaries("NGA", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)

moz1 <- get_adm_boundaries("MOZ", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)

zim1 <- get_adm_boundaries("ZWE", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)


# MER Data Munge
df_VL <- df %>% 
  filter(fiscal_year=="2020",
         fundingagency=="USAID",
         indicator %in% c("TX_PVLS","TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator","Total Denominator"),
         operatingunit %in% c("Zimbabwe","Nigeria","Mozambique")) %>% 
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
  group_by(fiscal_year,operatingunit,psnuuid,psnu,indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  select(-period_type) %>% 
  spread(indicator, val)


df_VL <- df_VL %>% 
  group_by(operatingunit, psnuuid, psnu) %>% 
  mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
         ou_lab = paste0(operatingunit, " (", lag(TX_CURR, 2, order_by = period) %>% comma(), ")")) %>% 
  ungroup() %>% 
  mutate(
    VLS = (TX_PVLS / TX_PVLS_D) * VLC, 
    Not_Cov = abs(1 - VLS)
  )
  

# GEO Data Joins
zimgeo <- st_as_sf(gis_5_sfc$Zimbabwe) %>% 
  left_join(df_VL, by = c("uid" = "psnuuid"))

mozgeo <- st_as_sf(gis_5_sfc$Mozambique) %>% 
  left_join(df_VL, by = c("uid" = "psnuuid"))

ngageo <- st_as_sf(gis_4_sfc$Nigeria) %>% 
  left_join(df_VL, by = c("orgunit_in" = "psnuuid"))


# VIZ

moz_map <- terrain_map(countries = "Mozambique", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = mozgeo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
  geom_sf(data = moz1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_gradient2(
    low = "yellow",
    high = "brown", 
    breaks = c(0, .25, .50, .75, 1.00),
    limits = c(0, 1.00),
    #na.value = NA,
    labels = percent_format(accuracy = 1)
  )+
  si_style_map() +
  theme(
    legend.position =  c(.9, .2),
    legend.direction = "vertical",
    legend.key.width = ggplot2::unit(.5, "cm"),
    legend.key.height = ggplot2::unit(1, "cm")
  )

zim_map <- terrain_map(countries = "Zimbabwe", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zimgeo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
  geom_sf(data = zim1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_gradient2(
    low = "yellow",
    high = "brown",
    breaks = c(0, .25, .50, .75, 1.00),
    limits = c(0, 1.00),
    labels = percent
  ) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.direction = "horizontal",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm")
  )
  

nga_map <- terrain_map(countries = "Nigeria", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = ngageo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
  geom_sf(data = nga1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_gradient2(
    low = "yellow",
    high = "brown", 
    breaks = c(0, .25, .50, .75, 1.00),
    limits = c(0, 1.00),
    labels = percent
  ) +
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.direction = "horizontal",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm")
  )

moz_map + {
  zim_map +
  nga_map +
  plot_layout(ncol = 1)
} +
  plot_annotation(
    title = "Viral Load - % Not Covered",
    caption = "Source: FY20Q3i MSD,
VLC = TX_PVLS / TX_CURR (2 periods prior)
VLS = TX_PVLS / TX_PVLS_D * VLC"
    )



ggsave(here("Graphics", "FY20Q3_ViralLoad.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
