# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  
# REF ID:   814195f9 
# LICENSE:  MIT
# DATE:     2023-09-26
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
    get_metadata(filepath) 
  
  ref_id <- "814195f9"

# IMPORT ------------------------------------------------------------------
  
 df_prep <- data_folder %>% return_latest("fy23q3_review_catalyst_study") %>% 
    read_excel()
  
  

# MUNGE -------------------------------------------------------------------
  nudge_space <- 0.125

  df_prep <- df_prep %>% 
    mutate(sample_pop = str_to_title(sample_pop),
           prep_type = str_to_title(prep_type))
    
  df_prep %>% 
    ggplot(aes(x = fct_reorder(prep_type, desc(pct)), y = pct, fill = sample_pop)) + 
    geom_col(data = df_prep %>% filter(sample_pop %in% c("All Women At Enrollment")), width = 0.5) +
    geom_col(data = df_prep %>% filter(sample_pop == "Used Prep Before"),position = position_nudge(x = nudge_space), width = 0.5) +
    geom_col(data = df_prep %>% filter(sample_pop == "New To Prep"),position = position_nudge(x = nudge_space*2), width = 0.5) +
    scale_fill_manual(values = c("All Women At Enrollment" = golden_sand, "Used Prep Before" = scooter, "New To Prep" = moody_blue)) +
    si_style_ygrid() +
    scale_y_continuous(labels = scales::percent) +
    geom_text(data = df_prep %>% filter(sample_pop %in% c("All Women At Enrollment")),
              aes(y = pct,
                  label = percent(pct)), size = 14/.pt, hjust = 1,
              #position = "stack",
              #  color = "white",
              family = "Source Sans Pro Light") +
    geom_text(data = df_prep %>% filter(sample_pop == "Used Prep Before"),aes(y = pct,label = percent(pct)), size = 12/.pt, hjust = 1,
              position = position_nudge(x = nudge_space),
              family = "Source Sans Pro Light") +
    geom_text(data = df_prep %>% filter(sample_pop == "New To Prep"),aes(y = pct,label = percent(pct)), size = 12/.pt, hjust = 1,
              position = position_nudge(x = nudge_space*2),
              family = "Source Sans Pro Light") +
    labs(x = NULL, y = NULL,
         title = "Almost 60% of all women at enrollment chose Oral PrEP, however 53% of prior PrEP users opted for the PrEP ring" %>% toupper(),
         subtitle = "Real time (not final) data from Stage 1* of the CATALYST Implementation Science Study", 
         caption = "Preliminary and Confidential – CATALYST data as of 31 Aug 2023. Do not share.
        Source: FHI360, MOSAIC award:  Study is in the field and on-going.  Do not cite, Do not share, real-time data")
  
    si_save("Graphics/FY23Q3_catalyst.svg")
  
  
  df_prep %>% 
    ggplot(aes(x = sample_pop, y = pct, fill = sample_pop)) + 
    geom_col(width = 0.75) +
    #geom_col(data = df_prep %>% filter(sample_pop == "used prep before"),position = position_nudge(x = nudge_space), width = 0.75) +
    #geom_col(data = df_prep %>% filter(sample_pop == "new to prep"),position = position_nudge(x = nudge_space*2), width = 0.75) +
    facet_wrap(~prep_type)
  
