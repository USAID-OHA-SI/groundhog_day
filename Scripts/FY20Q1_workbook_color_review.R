##  PROJECT: FY20Q1 
##  AUTHOR:  tessam achafetz | USAID
##  PURPOSE: review colors used in the FY20Q1 Slide deck
##  LICENCE: MIT
##  DATE:    2020-04-02
##  UPDATE:


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(readxl)

# GLOBAL VARIABLES --------------------------------------------------------


data_in <- ""


# IMPORT ------------------------------------------------------------------
  
  #data source: https://drive.google.com/file/d/1dmtJtUhjkpPSI_zeK1ZFOvReSrgsXnK7/view?usp=sharing

  df <- read_excel(file.path(data_in, "FY20Q1_color_review.xlsm"), sheet = "unordered")


# ALL COLORS --------------------------------------------------------------

  df %>% 
    mutate(size = 4) %>% 
    ggplot(aes(x = ColorHex, y = color, fill = factor(id))) + geom_tile() +
    scale_fill_manual(values = df$ColorHex) +
    theme_minimal() + theme(legend.position = "none") +
    facet_wrap(~mapping, scales = "free")+
    theme(axis.text.x = element_text(angle = 90))

# GROUPED COLORS ----------------------------------------------------------

  df_grps <- df %>% 
    gather(group, in_group, starts_with("grp")) %>% 
    separate(group, c(NA, "type", "element")) %>% 
    mutate(pair = paste(ColorHex, mapping))

## CATEGORIES
  viz_cat <- df_grps %>% 
    filter(type == "cat",
           !is.na(in_group))
  
  viz_cat %>% 
    ggplot(aes(x = 1, y = pair, fill = factor(id))) + geom_tile() +
    geom_text(aes(label =mapping)) +
    scale_fill_manual(values = viz_cat$ColorHex) +
    theme_minimal() + theme(legend.position = "none") +
    facet_wrap(~element, scales = "free")+
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(face = "bold", size = 13))

## DUPLICATES
  viz_dup <- df_grps %>% 
    filter(type == "dup",
           !is.na(in_group))
  
  viz_dup %>% 
    ggplot(aes(x = mapping, y = ColorHex, fill = factor(id))) + geom_tile() +
    scale_fill_manual(values = viz_dup$ColorHex) +
    theme_minimal() + theme(legend.position = "none") +
    facet_wrap(~mapping, scales = "free")+
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(face = "bold", size = 13))
  

