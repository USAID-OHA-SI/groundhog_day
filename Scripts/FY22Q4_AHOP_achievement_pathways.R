# PROJECT:  groundhog_day
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  visual on FY22 AHOP progress for FY22Q4 review
# REF ID:   8d9441b8 
# LICENSE:  MIT
# DATE:     2023-01-05
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------

  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(ggplot2)
  library(janitor)
  library(glue)
  library(ggtext)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "8d9441b8"

# IMPORT ----------------------------------------------------------------------
  
  df <- data.frame(pathway = factor(c("Pathway 1", "Pathway 1",
                                  "Pathway 2", "Pathway 2",
                                  "Pathway 3", "Pathway 3",
                                  "Pathway 4", "Pathway 4",
                                  "Pathway 5", "Pathway 5"), 
                                  levels = c("Pathway 1", "Pathway 2", 
                                             "Pathway 3", "Pathway 4", "Pathway 5"), 
                                  ordered = FALSE),
                   achievement_status = factor(c("Achieved", "Partially Achieved",
                                             "Achieved", "Partially Achieved",
                                             "Achieved", "Partially Achieved",
                                             "Achieved", "Partially Achieved",
                                             "Achieved", "Partially Achieved"),
                                             levels = c("Achieved", "Partially Achieved"), 
                                             ordered = TRUE),
                   milestone_status_FY22 = c(0, 3,  0, 0, 0, 8,0, 1, 2, 1),
                   total_milestones_FY22 = c(10, 10, 16, 16, 17, 17, 7, 7, 11, 11)) %>%
    arrange(pathway, achievement_status)

# MUNGE ------------------------------------------------------------------------
  
  df_viz <- df %>%
    group_by(pathway, achievement_status) %>%
    mutate(
      percent = round_half_up(as.numeric(milestone_status_FY22/total_milestones_FY22)*100), 
      achieved = if_else(achievement_status == "Achieved" &
                         milestone_status_FY22 > 0, 1, 0))
  
# VIZ --------------------------------------------------------------------------
  
  # need to reorder this but almost there
  
  df_viz %>%
    ggplot(aes(x = percent, y = fct_reorder2(pathway, desc(achieved), percent),
               fill = achievement_status, group = achievement_status)) +
    geom_bar(position = "stack", stat="identity") +
    scale_x_continuous(limits = c(0, 100), 
                       breaks = c(0, 50, 100)) +
    scale_fill_manual(values = c(scooter, scooter_light)) +
    si_style_xgrid() +
    labs(
      x = NULL, y = NULL, fill = NULL,
      title = glue("Pathways 5, 3, and 1 have made the most progress towards full or partial achievement "),
      caption = glue("Source: AHOP Goals, IRs, and Milestones | SI Analytics: Jessica Stephens/Jessica Hoehner | USAID")) +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  