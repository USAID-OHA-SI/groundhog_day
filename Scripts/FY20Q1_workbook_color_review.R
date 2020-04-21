##  PROJECT: FY20Q1 
##  AUTHOR:  tessam achafetz | USAID
##  PURPOSE: review colors used in the FY20Q1 Slide deck
##  LICENCE: MIT
##  DATE:    2020-04-02
##  UPDATE:


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(tidytext)

# GLOBAL VARIABLES --------------------------------------------------------


data_in <- "Data"


# IMPORT ------------------------------------------------------------------
  
  #data source: https://drive.google.com/file/d/1dmtJtUhjkpPSI_zeK1ZFOvReSrgsXnK7/view?usp=sharing

  df <- read_excel(file.path(data_in, "FY20Q1_color_standards.xlsx")) %>% 
    mutate(red = col2rgb(ColorHex)[1,],
           green = col2rgb(ColorHex)[2,],
           blue = col2rgb(ColorHex)[3,]) %>% 
    arrange(red, green, blue) %>% 
    mutate(id = row_number()) %>% 
    mutate(color_sort = fct_reorder(ColorHex, id))

  df %>% group_by(ColorHex, color_sort) %>% 
    summarise_at(vars(red, green, blue), mean, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(id = row_number(),
           y = 1) %>% 
    ggplot(aes(x = id, y = y, fill = color_sort)) +
    geom_tile() +
    scale_fill_manual(values = df$ColorHex) + 
    theme(legend.position = "none")
  
  

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
    mutate(pair = paste(mapping, ColorHex)) %>% 
    arrange(red, green, blue) 
  

## CATEGORIES
  viz_cat <- df_grps %>% 
    filter(type == "cat",
           !is.na(in_group))
  
  viz_cat %>% 
    mutate(pair_sorted = reorder_within(pair, ColorHex, element)) %>% 
    ggplot(aes(x = 1, y = pair_sorted, fill = factor(id))) + geom_tile() +
    #geom_text(aes(label = mapping), size = 3) +
    scale_fill_manual(values = viz_cat$ColorHex) +
    theme_minimal() + theme(legend.position = "none") +
    facet_wrap(~element, scales = "free") + 
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(face = "bold", size = 8))

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
  

