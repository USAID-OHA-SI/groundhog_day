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
file_psnu_im <- (list.files(path =dir_merdata,
                            pattern = "Structured",
                            recursive = TRUE,
                            full.names = TRUE)) 

df<-read_msd(file_psnu_im)

# GEO Data
gis_5_sfc <- list.files(dir_geo, pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

gis_4_sfc <- list.files(dir_geo, pattern = ".*_4_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

moz1 <- get_adm_boundaries("MOZ", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)

zim1 <- get_adm_boundaries("ZWE", adm_level = 1, geo_path = dir_geo) %>%
 st_as_sf() %>%
  select(country = name_0, province = name_1)

ken1 <- get_adm_boundaries("KEN", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)

les1<-get_adm_boundaries("LSO", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)



# MER Data Munge
df_VL<-df %>% 
  filter(fiscal_year=="2020",
         fundingagency=="USAID",
         indicator %in% c("TX_PVLS","TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator","Total Denominator"),
         operatingunit %in% c("Zimbabwe","Mozambique","Lesotho","Kenya")) %>% 
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
  group_by(fiscal_year,operatingunit,psnuuid,psnu,indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  select(-period_type) %>% 
  spread(indicator, val)
  


df_VL<-df_VL %>% 
  group_by(operatingunit,psnuuid,psnu) %>% 
  mutate(VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period),
         ou_lab = paste0(operatingunit, " (", lag(TX_CURR, 2, order_by = period) %>% comma(), ")")) %>% 
  ungroup() %>% 
  mutate(VLS = (TX_PVLS/TX_PVLS_D)*VLC) %>% 
  mutate(Not_Cov=case_when(VLC >1 ~ 0,
                           TRUE ~ 1-VLC)) %>% 
  filter(period == "FY20Q3") %>% 
  mutate(shortname = case_when(operatingunit=="Kenya" ~ str_remove(psnu, "County"),
                                TRUE ~ psnu)) %>% 
  mutate(lab_psnu = case_when(Not_Cov > .2 ~ shortname))


# GEO Data Joins
zimgeo<-st_as_sf(gis_5_sfc$Zimbabwe) %>%
  left_join(df_VL, by = c("uid" = "psnuuid"))


mozgeo<-st_as_sf(gis_5_sfc$Mozambique) %>% 
  left_join(df_VL, by = c("uid" = "psnuuid"))


ken_geo<-st_as_sf(gis_5_sfc$KenyaCounty) %>% 
  left_join(df_VL, by = c("uid" = "psnuuid"))

les_geo<-st_as_sf(gis_4_sfc$Lesotho) %>% 
  left_join(df_VL, by = c("uid" = "psnuuid"))

# VIZ - LOW PERFORMING OUs
moz_map<- terrain_map(countries = "Mozambique", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = mozgeo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
  geom_sf(data = moz1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_gradient2(
    low = "yellow",
    high = "brown",
    labels = percent)+
  si_style_map() +
  theme(
    legend.position =  c(.9, .2),
    legend.direction = "vertical",
    legend.key.width = ggplot2::unit(.5, "cm"),
    legend.key.height = ggplot2::unit(1, "cm")
  )

zim_map<-terrain_map(countries = "Zimbabwe", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zimgeo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
  geom_sf(data = zim1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_gradient2(
    low = "yellow",
    high = "brown",
    labels=percent_format(accuracy = 1))+
  si_style_map() +
  theme(
    legend.position =  "bottom",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key.height = ggplot2::unit(.5, "cm")
    )
  


(moz_map + zim_map) +
  plot_layout(widths = c(1,1)) +
  plot_annotation(
    caption = "Source: FY20Q3i MSD - USAID Only,
VLC = TX_PVLS / TX_CURR (2 periods prior)
Not Covered = 1-VLC")



ggsave(here("Graphics", "FY20Q3_ViralLoad_BottomOUs.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")


# VIZ - HIGH PERFORMING OUs
ken_map<- terrain_map(countries = "Kenya", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = ken_geo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
  geom_sf(data = ken1, fill = NA, lwd = .2, color = grey30k) +
  geom_sf_text(data = ken_geo %>% filter(!is.na(lab_psnu)), aes(label = paste0(lab_psnu, "\n(", round(Not_Cov * 100,0), "%)")), color = "white", size = 3.5)+
  scale_fill_gradient2(
    low = "yellow",
    high = "brown",
    labels=percent_format(accuracy = 1))+
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.direction = "horizontal",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm")
  )

les_map<-terrain_map(countries = "Lesotho", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = les_geo %>% filter(!is.na(Not_Cov)), aes(fill = Not_Cov), lwd = .2, color = grey10k) +
  geom_sf(data = les1, fill = NA, lwd = .2, color = grey30k) +
  geom_sf_text(data = les_geo %>% filter(!is.na(lab_psnu)), aes(label = paste0(lab_psnu,"\n(", round(Not_Cov * 100,0), "%)")), color = "white", size = 4)+
  scale_fill_gradient2(
    low = "yellow",
    high = "brown",
    labels=percent_format(accuracy = 1))+
  si_style_map() +
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm")
  )



(ken_map + les_map) +
  plot_layout(widths = c(1,1)) +
  plot_annotation(
    title = "Viral Load - % Not Covered",
    caption = "Source: FY20Q3i MSD,
VLC = TX_PVLS / TX_CURR (2 periods prior)
VLS = TX_PVLS / TX_PVLS_D * VLC"
    )



ggsave(here("Graphics", "FY20Q3_ViralLoad_TopOUs.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
