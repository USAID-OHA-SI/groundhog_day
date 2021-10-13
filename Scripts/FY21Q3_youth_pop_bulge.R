# PURPOSE: Youth Bulge SSA - AHOP visuals
# AUTHOR:  Karishma Srikanth, Aaron Chafetz | SI
# LICENSE: MIT
# DATE:    2021-10-01
# UPDATED: 2021-10-13
# NOTES:   adapted from 20211001_AHOP_youth_pop_buldge
# SOURCE:  https://population.un.org/wpp/Download/Standard/Population/

# DEPENDENCIES & GLOBALS ------------------------------------------------------------------

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(gophr)
    library(scales)
    library(sf)
    library(tidytext)
    library(here)
    library(readxl)
    library(janitor)
    library(ggrepel)
    library(countrycode)
    library(extrafont)
    library(glue)
   

# GLOBALS -----------------------------------------------------------------


    authors <- c("Karishma Srikanth", "Aaron Chafetz")
    
# IMPORT ------------------------------------------------------------------------------  

    #Read in population by age / sex data for population pyramid
    df_pyramid_male <- read_xlsx("Data/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx",
              sheet = "ESTIMATES",
              skip = 16) %>% 
      clean_names()
    
    df_pyramid_female <- read_xlsx("Data/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx",
                                 sheet = "ESTIMATES",
                                 skip = 16) %>% 
      clean_names()
    
    
    #pepfar country list
    pepfar_xwalk <- pepfar_country_xwalk
    
# MUNGE ------------------------------------------------------------------------------
  
 #CLEAN FOR POP PYRAMID VIS

  df_pyramid_clean_m <- df_pyramid_male %>% 
    rename(region_country = region_subregion_country_or_area,
           year = reference_date_as_of_1_july) %>% 
    pivot_longer(c(x0_4:x100), names_to = "age", values_to = "value") %>% 
    mutate(value = as.numeric(value),
           sex = "male",
           age = str_remove(age, "x")) %>% 
    select(-c(index, notes, parent_code)) %>% 
    filter(year == 2020)

  #PREP FOR PYRAMID VIS
  df_pyramid_clean_f <- df_pyramid_female %>% 
    rename(region_country = region_subregion_country_or_area,
           year = reference_date_as_of_1_july) %>% 
    pivot_longer(c(x0_4:x100), names_to = "age", values_to = "value") %>% 
    mutate(value = as.numeric(value),
           sex = "female",
           age = str_remove(age, "x")) %>% 
    select(-c(index, notes, parent_code)) %>% 
    filter(year == 2020)
  
  df_pyramid_vis <- df_pyramid_clean_m %>% 
    bind_rows(df_pyramid_clean_f) 
  
  df_pyramid_vis <- codelist %>% 
    select(countryname_iso = iso3c, country_code = unpd) %>% 
    left_join(pepfar_country_list, .) %>% 
    inner_join(df_pyramid_vis, .) %>% 
    count(year, sex, age, wt = value, name = "value") %>% 
    mutate(age = str_replace(age, "_", "-"),
           fill_color = case_when(sex == "male" & age %in% c("10-14", "15-19", "20-24") ~ genoa, 
                                  sex == "male" ~ si_palettes$genoas[2],
                                  sex == "female" & age %in% c("10-14", "15-19", "20-24") ~ moody_blue,
                                  sex == "female" ~ si_palettes$moody_blues[2]),
           total_pop = sum(value), 
           pct_pop = value/total_pop)
  
  #reorder age factor levels
  df_pyramid_vis$age <- factor(df_pyramid_vis$age, levels=c("0-4", "5-9", "10-14",
                                                            "15-19", "20-24", "25-29",
                                                            "30-34", "35-39", "40-44",
                                                            "45-49", "50-54", "55-59",
                                                            "60-64", "65-69", "70-74",
                                                            "75-79", "80-84", "85-89",
                                                            "90-94", "95-99", "100"))



# VIZ ------------------------------------------------------------------------------



#pop pyramid vis
df_pyramid_vis %>% 
  ggplot(aes(ifelse(sex == "male", -value, value),  age, fill = fill_color)) +
  geom_col() +
  geom_vline(aes(xintercept = 0), size = 1, color = "white") +
  geom_text(data = df_pyramid_vis %>% filter(age %in% c("10-14", "15-19", "20-24")),
            aes(label = percent(pct_pop,.1)),
            family = "Source Sans Pro", size = 12/.pt, hjust = 1.4) +
  scale_fill_identity() +
  si_style_nolines()+
  scale_x_continuous(breaks = seq(-180000, 180000, 60000),
                     labels = label_number_si()) +
  labs(title = toupper("In 2020, youth aged 10-24 years made up nearly 30% of the population in PEPFAR supported countries"), 
       x = NULL, y = NULL,
       caption = glue("Source: UN World Population Prospects, 2019
       SI analytics: {paste(authors, collapse = '/')}
         US Agency for International Development"))

si_save("Graphics/SSA_pop_pyramid.svg", width = 4.6, height = 3.7)

#check to see the percent population breakdown- over 60 of population is under 24, 30% between 15-24
df_pyramid_vis %>% 
  filter(age %in% c("10-14", "15-19", "20-24")) %>% 
  # group_by(sex) %>% 
  summarize_at(vars(pct_pop), sum, na.rm = TRUE)
  
