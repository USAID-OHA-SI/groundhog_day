##  PROJECT: FY20Q1 
##  AUTHOR:  tessam achafetz | USAID
##  PURPOSE: review colors used in the FY20Q1 Slide deck
##  LICENCE: MIT
##  DATE:    2020-04-02
##  UPDATE:  2020-04-24; New base dataset added and munged


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(tidytext)

# GLOBAL VARIABLES --------------------------------------------------------


data_in <- "Data"
images <- "Images"


# IMPORT ------------------------------------------------------------------
  
  #data source: https://drive.google.com/file/d/1dmtJtUhjkpPSI_zeK1ZFOvReSrgsXnK7/view?usp=sharing
  #updated datasource: https://drive.google.com/open?id=1zm1-Os9drgP5e9wvcyt1gW-ImjWpL_St


  df <- read_excel(file.path(data_in, "FY20Q1_color_standards.xlsx")) %>% 
    mutate(red = col2rgb(ColorHex)[1,],
           green = col2rgb(ColorHex)[2,],
           blue = col2rgb(ColorHex)[3,]) %>% 
    arrange(color, red, green, blue) %>% 
    mutate(id = row_number()) %>% 
    mutate(color_sort = fct_reorder(ColorHex, id))
  

 yrange <-  unique(df$ColorHex) %>% length()
  
 # Group by color, print all different colors sorted and faceted
  colors_df <- 
    df %>% group_by(color_sort, ColorHex, color) %>% 
    summarise_at(vars(red, green, blue), mean, na.rm = TRUE) %>% 
    group_by(color) %>% 
    mutate(id = row_number(),
           y = ceiling(id / 20)
    ) %>% 
    group_by(y, color) %>% 
    mutate(x = row_number()) %>%
    ungroup()
  
      #y = if_else(id %% 3 %in% c(1, 2), id %% 3, 3)) %>% 
    #add_column(x = rep_along(1:yrange, 1:3)) %>% 
    colors_df %>% 
      ggplot(aes(x = x, y = y, fill = color_sort)) +
    geom_tile(color = "white") +
   scale_fill_identity() + 
      facet_wrap(~color, scales = "free", nrow = 2) +
    theme_void() +
    theme(legend.position = "none")
    
  # Color groups
    df %>% 
      count(color, sort = TRUE) %>% 
      ggplot(aes(x = fct_reorder(color, n, .desc = TRUE), y = 1, fill = color)) + 
               geom_tile() + 
      geom_text(aes(label = n), color = "white", size = 14) +
               si_style_nolines() + 
      coord_fixed() +
      theme(axis.text.y = element_blank(),
            axis.text.x = element_text(size = 18)) + 
               scale_fill_identity() +
      labs(title = "MAJOR COLOR GROUPINGS AND FREQUENCY OF OCCURENCE", x = NULL, y = NULL,
           caption = "Souce: Color data extracted from Q1 Review")
  

    df %>% filter(mapping %in% c("USAID")) %>% count(mapping, mapping, sort = TRUE) %>%
      mutate(color = "#002F6C") %>% 
      ggplot(aes(x = mapping, y = n, fill = color)) + 
      geom_col() + 
      coord_flip() +
      scale_fill_identity() +
      si_style()
    

# ALL COLORS --------------------------------------------------------------

  plot_color <- function(clr = "blue") {
    
  viz <-
      df %>% 
        filter(color == {{clr}}) %>% 
        mutate(size = 4) %>% 
        ggplot(aes(x = ColorHex, y = mapping, fill = ColorHex)) + 
          geom_tile() +
        scale_fill_identity() +
        theme_minimal() + theme(legend.position = "none") +
        facet_wrap(~color, scales = "free") +
        theme(axis.text.x = element_text(angle = 90),
              strip.text = element_text(size = 20)) +
        labs(x = NULL, y = NULL)
    
    ggsave(file.path(images, paste0("Q1_color_review_", {{clr}}, ".png")),
           plot = viz, 
           height = 6,
           width = 8, 
           dpi = "retina", 
           scale = 1.15)
    
    print(viz)
  }
  
# Print each color level summarizing it's use by mapping    
 unique(df$color) %>% 
   walk(plot_color)
 

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
    ggplot(aes(x = 1, y = pair_sorted, fill = ColorHex)) + 
    geom_tile(color = "white", size = 0.25) +
    #geom_text(aes(label = mapping), size = 3) +
    scale_fill_identity() +
    #scale_fill_manual(values = viz_cat$ColorHex) +
    theme_minimal() + theme(legend.position = "none") +
    facet_wrap(~element, scales = "free") + 
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(face = "bold", size = 18))

## DUPLICATES
  viz_dup <- df_grps %>% 
    filter(type == "dup",
           !is.na(in_group))
  
  viz_dup %>% 
    ggplot(aes(x = mapping, y = ColorHex, fill = factor(id))) + geom_tile(color = "white", size = 0.25) +
    scale_fill_manual(values = viz_dup$ColorHex) +
    theme_minimal() + theme(legend.position = "none") +
    facet_wrap(~mapping, scales = "free", nrow = 2)+
    labs(x = NULL, y = NULL) +
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(face = "bold", size = 16))
  

