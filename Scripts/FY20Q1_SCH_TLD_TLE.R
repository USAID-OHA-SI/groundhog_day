##  PROJECT: Q1 review
##  AUTHOR:  achafetz | USAID
##  PURPOSE: review TLD/TLE transition
##  LICENCE: MIT
##  DATE:    2020-04-04
##  UPDATE:  2020-04-06


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(scales)
  library(extrafont)
  library(RColorBrewer)


# IMPORT ------------------------------------------------------------------

  #Location on G Drive
  # https://drive.google.com/file/d/1Qlpud2OiKpTw9DpbLeBgvcjPeOCsUXSU/view?usp=sharing

  #import
    df <- read_excel("Data/FLARE Adult ARVs Aggregate 4-3-2020.xlsx", 
                     sheet = "Aggregated FLARE ARV TABLE")

# MUNGE -------------------------------------------------------------------

  #clean names
    df <- clean_names(df)
  
  #limit to TLD/TLE Last Month Issued (LMI) data & gather
    df_lmi <- df %>% 
      select(month, calendar_year, country, matches("(tl|lnz).*lmi")) %>% 
      gather(regimen_type, lmi, -month, -calendar_year, -country, na.rm = TRUE)
    
  #identify countries with incomplete reporting
    df_lmi %>%
      group_by(country) %>%
      filter(n() < 5) %>%
      ungroup() %>%
      distinct(country)

  #drop countries with incomplete reporting
    df_lmi <- df_lmi %>%
      group_by(country) %>%
      filter(n() >= 5) %>%
      ungroup()

  #aggregate to one obs per OU (UGA w/ three obs)
    df_lmi <- df_lmi %>% 
      mutate(country = str_remove(country, " \\(.*|-.*"),
             country = ifelse(country == "eSwatini", "Eswatini", country)) %>% 
      group_by(month, calendar_year, country, regimen_type) %>% 
      summarise_at(vars(lmi), sum, na.rm = TRUE) %>% 
      ungroup()
    
  #clean
    df_lmi <- df_lmi %>%
      unite(date, c(month, calendar_year), sep = " 1,") %>% 
      mutate(date = mdy(date),
             regimen_type = regimen_type %>%
               str_remove_all("_lmi") %>% 
               str_replace_all("_", " ") %>% 
               str_to_upper() %>% 
               str_replace("TABS", "tabs"),
             regimen_type = ifelse(regimen_type == "LNZ", "LNZ [LMI]", regimen_type),
             regimen_type = factor(regimen_type, c("TLD 30 tabs", "TLD 90 tabs", "TLD 180 tabs", "TLE400", "TLE600", "LNZ [LMI]")),
             regimen = str_extract(regimen_type, "(TL(D|E)|LNZ \\[LMI\\])"),
             regimen = factor(regimen, c("TLD", "TLE", "LNZ [LMI]"))) %>% 
      arrange(country, regimen, regimen_type, date)
    
  #convert to months of treatment issued
    df_mos_issued <- df_lmi %>% 
      mutate(mos_issued = case_when(regimen_type == "TLD 90 tabs" ~ lmi * 3,
                                    regimen_type == "TLD 180 tabs" ~ lmi * 6,
                                    TRUE ~ lmi))
    
  #arrange country by
    df_mos_issued <- df_mos_issued %>% 
      mutate(country = fct_reorder(country, mos_issued, sum, .desc = TRUE))
    
    df_mi_reg_share <- df_mos_issued %>% 
      group_by(date, country, regimen) %>% 
      summarise(mos_issued = sum(mos_issued, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(date, country) %>% 
      mutate(mi_reg_share = mos_issued/sum(mos_issued)) %>% 
      ungroup() %>% 
      group_by(country) %>% 
      mutate(tle_growth = case_when(date == max(date) & regimen == "TLE" ~ mos_issued > lag(mos_issued, order_by = date))) %>% 
      fill(tle_growth, .direction = "up")
      ungroup() %>% 
      arrange(country, regimen, date)
    
    df_mi_tld_shares <- df_mos_issued %>% 
      filter(regimen == "TLD") %>% 
      group_by(date, country, regimen_type) %>% 
      summarise(mos_issued = sum(mos_issued, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(date, country) %>% 
      mutate(mi_tld_share = mos_issued/sum(mos_issued)) %>% 
      ungroup() %>% 
      arrange(country, regimen_type, date)

# PLOT --------------------------------------------------------------------

  #set theme
    theme_set(theme_minimal(base_family = "Source Sans Pro"))
    
  #colors
    col_tld <- brewer.pal(5, "Blues")[3:5]
    col_tle <- brewer.pal(5, "YlOrBr")[3:4]
    col_lnz <- brewer.pal(5, "YlOrBr")[5]
    
  #overall trends
    df_mos_issued %>% 
      ggplot(aes(date, mos_issued,  group = regimen_type, color = regimen_type)) +
      geom_hline(aes(yintercept =0), color = "gray50") +
      geom_blank(aes(y =mos_issued * 1.1)) +
      geom_line(size = .6) +
      geom_point(size = 3) +
      facet_wrap(~ country, scales = "free_y") +
      labs(x = NULL, y = NULL, color = NULL,
           title = "NO MAJOR TRENDS IN REGIMEN TYPE",
           subtitle = "Months of Treatment Issued by Regimen Type",
           caption = "Note: LNZ reported as Last Month Issued 
           Ordered by # of months of treatment issues
           Source: Monthly SCH FLARE Reports") +
      scale_y_continuous(label = comma) +
      scale_color_manual(values = c(col_tld, col_tle, col_lnz)) +
      guides(colour = guide_legend(nrow = 1)) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold"),
            strip.text = element_text(size = 12, face = "bold"),
            plot.caption = element_text(size = 9, color = "gray30"))
    
    ggsave("Images/MI_Trends.png", dpi = 300,
           height = 5.66, width = 10)


  #share of TLD v TLE, removing those at 100% already    
    df_mi_reg_share %>% 
      filter(tle_growth == TRUE) %>% 
      ggplot(aes(date, mi_reg_share,  color = regimen)) +
      geom_hline(aes(yintercept =0), color = "gray50") +
      geom_line(size = .9) +
      geom_point(size = 5) +
      labs(x = NULL, y = NULL, color = NULL,
           title = "RECENT RISE IN TLE IN A HANDFUL OF COUNTRIES",
           subtitle = "Months of Treatment Issued by Regimen Type",
           caption = "Note: LNZ reported as Last Month Issued (LMI)
           Ordered by # of months of treatment issues
           Source:Monthly SCH FLARE Reports") +
      facet_wrap(~ country)  +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      scale_color_manual(values = c(col_tld[2], col_tle, col_lnz)) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold"),
            strip.text = element_text(size = 12, face = "bold"),
            plot.caption = element_text(size = 9, color = "gray30"))
    
    ggsave("Images/MI_Trends_Regimens.png", dpi = 300,
           height = 5.66, width = 10)
    
  #distribution of TLD, see if increase in tab count
    df_mi_tld_shares %>% 
      filter(country != "Cameroon") %>% 
      ggplot(aes(date, mi_tld_share,  color = regimen_type)) +
      geom_hline(aes(yintercept =0), color = "gray50") +
      geom_line(size = .6) +
      geom_point(size = 3) +
      facet_wrap(~ country) +
      labs(x = NULL, y = NULL, color = NULL,
           title = "NOT SEEING A MAJOR SHIFT TOWARDS INCREASED TABLETS",
           subtitle = "Months of Treatment Issued by Regimen Type",
           source = "Monthly SCH FLARE Reports") +
      scale_y_continuous(labels = percent, limits = c(0, 1.05)) +
      scale_color_manual(values = c(col_tld)) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold"),
            strip.text = element_text(size = 12, face = "bold"),
            plot.caption = element_text(size = 9, color = "gray30"))
    
    ggsave("Images/MI_Trends_TLD.png", dpi = 300,
           height = 5.66, width = 10)
    