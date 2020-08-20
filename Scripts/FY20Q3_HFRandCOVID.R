## PROJECT: FY20Q2 REVIEW
## AUTHOR:  A.Chafetz | USAID
## PURPOSE: HFR and COVID Trends
## LICENSE: MIT
## DATE:    2020-06-20
## UPDATED: 2020-06-24

# DEPENDENCIES ------------------------------------------------------------


  library(COVIDutilities)
  library(lubridate)
  library(scales)
  library(extrafont)
  library(glitr)
  library(glamr)
  library(ISOcodes)
  library(Wavelength)
  library(jsonlite)
  library(patchwork)
  library(tidyverse)
  library(ggrepel)


# GLOBAL VARIABLES --------------------------------------------------------

  #DATIM Access
    myuser <- "" #do not store when saving
    baseurl <- "https://final.datim.org/"
  
  #Stringency Index API url - start/end date 
    ox_start <- "2020-01-01"
    ox_end <- today()
    url_ox <- paste("https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range",
                    ox_start, ox_end, sep = "/")
    rm(ox_end, ox_start)

  #quarter starts
    qtrs <- as_date(c("2019-10-01", "2020-01-01", "2020-04-01", "2020-07-01", "2020-09-30"))
    
  #review slide background
    bckgrnd <- "#cfcdc9"
    
  #FT color scale
    
    
# IMPORT DATA -------------------------------------------------------------

  #COVID Restrictions (HDX)
    df_gov_measures <- extract_excel_data(hdx_govmes_url, 
                                          hdx_govmes_linkid, 
                                          "Database", 'xlsx')
  #Government Response (Oxford - https://covidtracker.bsg.ox.ac.uk/about-api)
    json <- url_ox %>%
      jsonlite::fromJSON(flatten = TRUE)
  
  #COVID cases (JHU)
    df_covid <- pull_jhu_covid()
    
  #TX_CURR data (DATIM)
    url_tx <- paste0(baseurl,
                  "api/29/analytics.json?",
                  "dimension=RUkVjD3BsS1:PE5QVF0w4xj&", #Top Level 
                  "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E;W8imnja2Owd&",
                  "dimension=pe:THIS_FINANCIAL_YEAR&",  #period
                  "dimension=LxhLO68FcXm:MvszPTQrUhy&", #Technical Area: TX_CURR
                  "dimension=bw8KHXzxd9i:NLV6dy7BE2O&", #Funding Agency: USAID
                  "dimension=ou:LEVEL-4;ybg3MO3hcf4&", 
                  "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    df_tx <- extract_datim(url_tx, myuser, mypwd(myuser))
  

    rm(url_ox, url_tx, baseurl)
    
# MUNGE TX DATA -----------------------------------------------------------

  #create country names
    df_tx <-df_tx %>% 
      mutate(countryname = ifelse(str_detect(orglvl_3, "Region"), 
                                  `Organisation unit`, orglvl_3))
  
  #limit vars, sum to OU level, and spread by results/targets
    df_tx <- df_tx %>% 
      select(period = Period,
             indicator = `Technical Area`,
             type = `Targets / Results`, 
             fundingagency = `Funding Agency`,
             countryname,
             value = Value) %>% 
      group_by_if(is.character) %>% 
      summarise(value = sum(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(type = str_remove(type, "MER ") %>% tolower) %>% 
      spread(type, value) 
  
  #add ISO code
    df_tx <- ISO_3166_1 %>% 
      select(Name, iso = Alpha_3) %>%
      mutate(Name = recode(Name, 
                           "Congo, The Democratic Republic of the" = "Democratic Republic of the Congo",
                           "Myanmar" = "Burma",
                           "C?te d'Ivoire" = "Cote d'Ivoire",
                           "Lao People's Democratic Republic" = "Laos",
                           "Tanzania, United Republic of" = "Tanzania",
                           "Viet Nam" = "Vietnam")) %>% 
      left_join(df_tx, ., by = c("countryname" = "Name")) %>% 
      relocate(iso, .after = countryname)
    
  #largest treatment programs
    top_tx <- df_tx %>% 
      slice_max(n = 12, order_by = targets) %>% 
      pull(countryname)
    
    top_tx_iso <- df_tx %>% 
      slice_max(n = 12, order_by = targets) %>% 
      pull(iso)
 
# MUNGE OXFORD DATA -------------------------------------------------------
    
  #covert from json to dataframe
    df_stringency <- json %>%
      unlist() %>%
      enframe()
    
  #clean up table
    df_stringency <- df_stringency %>% 
      rowwise() %>%
      mutate(
        parts = length(unlist(str_split(name, "[.]"))),
        tbl = first(unlist(str_split(name, "[.]"))),
        tbl = gsub("\\d", "", tbl)
      ) %>%
      filter(parts == 4) %>%    # Keep the data, section with the longest parts
      separate(name,
               into = c("name", "date", "iso", "variable"),
               sep = "[.]") %>%                   # Separate column into multiple parts
      select(date:value) %>%               # Get rid of extra columns
      filter(date != value, iso != value) %>%     # Exclude repetition
      mutate(date = ymd(date), value = as.numeric(value)) %>% 
      spread(variable, value) %>% 
      select(-contains("legacy"))
    
  #add colors from FT - https://ig.ft.com/coronavirus-lockdowns/)
    df_stringency <- df_stringency %>% 
      mutate(bins = case_when(is.na(stringency)  ~ "NA",
                              stringency < 1     ~ "<1",
                              stringency < 25    ~ "1-24",
                              stringency < 50    ~ "25-49",
                              stringency < 75    ~ "50-74",
                              stringency < 85    ~ "75-84",
                              TRUE               ~ "85-100"),
             color = case_when(is.na(stringency) ~ "#D9CDC3",
                               stringency < 1    ~ "#D3E8F0",
                               stringency < 25   ~ "#FAE1AF",
                               stringency < 50   ~ "#FDAC7A",
                               stringency < 75   ~ "#F6736B",
                               stringency < 85   ~ "#DA3C6A",
                               TRUE              ~ "#A90773"
                               ))
    
  #filter to PEPFAR countries
    df_stringency <- df_stringency %>% 
      filter(iso %in% iso_map$iso)
    
  #add country name
    df_stringency <- df_stringency %>% 
      left_join(iso_map) %>% 
      rename(countryname = operatingunit) %>% 
      select(-regional)
    
  #order colors
    df_stringency <- df_stringency %>% 
      mutate(bins = factor(bins, c("NA","<1", "1-24", "25-49", "50-74", "75-84", "85-100")),
             color = factor(color, c("#D9CDC3", "#D3E8F0","#FAE1AF", "#FDAC7A", "#F6736B", "#DA3C6A", "#A90773")))
    
  #order vars
    df_stringency <- df_stringency %>% 
      select(date, countryname, iso, everything())
    
    
    rm(json)
    
# COVID CALENDAR ----------------------------------------------------------

  covid <- who_pandemic() %>% pull(date)

  fy20_dates <- seq.Date(as_date("2019-10-01"), as_date("2020-09-30"), length.out = 365)
  
  df <- tibble(date = fy20_dates) %>% 
    mutate(value = ifelse(date < "2020-07-01", 1, 0),
           post_who = date > covid)
  
  df %>% 
    ggplot(aes(date, value, fill = post_who)) +
    geom_area() +
    geom_vline(xintercept = qtrs, color = "gray30") +
    geom_vline(xintercept = covid, size = 2) +
    scale_x_date(date_labels = "%b") +
    scale_fill_manual(values = c("gray60", USAID_red)) +
    labs(x = NULL, y = NULL,
         subtitle = "FY20 Calendar") +
    si_style_nolines() +
    theme(axis.text.y = element_blank()) +
    theme(legend.position = "none",
          plot.background = element_rect(fill = bckgrnd, colour = NA))
  
  ggsave("covid_calendar.png", path = "Images", dpi = 330,
         width = 7, height = 2)
  
  
# COVID Trends ------------------------------------------------------------

  #add ISO codes
    df_covid <- ISO_3166_1 %>% 
      select(Name, iso = Alpha_3) %>%
      mutate(Name = recode(Name, 
                           "Congo, The Democratic Republic of the" = "Congo (Kinshasa)",
                           "Myanmar" = "Burma",
                           "C?te d'Ivoire" = "Cote d'Ivoire",
                           "Lao People's Democratic Republic" = "Laos",
                           "Tanzania, United Republic of" = "Tanzania",
                           "Viet Nam" = "Vietnam")) %>% 
      left_join(df_covid, ., by = c("countryname" = "Name")) %>% 
      mutate(countryname = recode(countryname, 
                                  "Congo (Kinshasa)" = "Democratic Republic of the Congo"))
  
  #filter to just PEPFAR countries
    df_covid_pepfar <- df_covid %>% 
      filter(iso %in% iso_map$iso)
  
    df_covid_pepfar <- df_covid_pepfar %>%
      filter(cases >= 10)
    
    df_covid_pepfar_top <- df_covid_pepfar %>% 
      filter(countryname %in% top_tx) %>%
      group_by(countryname) %>% 
      mutate(lab = case_when(date == max(date) ~ iso)) %>% 
      ungroup()
    
    df_covid_pepfar %>% 
      mutate(cases = na_if(cases, 0)) %>% 
      ggplot(aes(days_since_ten_case,cases, group = countryname)) +
      geom_path(color = "gray50", na.rm = TRUE) +
      geom_path(data = df_covid_pepfar_top, color = USAID_medblue, size = .9, na.rm = TRUE) +
      geom_label_repel(data = df_covid_pepfar_top, aes(label = lab), nudge_x = 2,
                color = USAID_medblue, family = "Source Sans Pro", fontface = "bold",
                na.rm = TRUE) +
      scale_y_log10(label = comma) +
      si_style() +
      labs(x = "days since 10th case", y = NULL,
           title = "COVID TRENDS IN PEPFAR COUNTRIES",
           subtitle = "cumulative confirmed cases, log scale",
           caption = "source: JHU")
    
    ggsave("covid_cases_Q3.png", path = "Images", dpi = 330,
           width = 7.10, height = 4.15)
    
    
    df_restrictions <- df_gov_measures %>% 
      filter(is.na(admin_level_name),
             iso %in% top_tx_iso,
             measure %in% c("Domestic travel restrictions",
                            "Partial lockdown", "Full lockdown",
                            "Checkpoints within the country",
                            "Curfews"),
             log_type == "Introduction / extension of measures") %>% 
      select(iso, date = date_implemented, 
             restrict_cat = category, 
             restrict_measure = measure) 
    
    #add country name
    # df_restrictions <- df_tx %>% 
    #   distinct(iso, countryname) %>% 
    #   right_join(df_restrictions, by = "iso")
      
      
    df_top_trends <- df_covid %>%
      filter(countryname %in% top_tx) %>% 
      mutate(countryname = factor(countryname, top_tx),
             cases = na_if(cases, 0)) %>% 
      left_join(df_restrictions) %>% 
      mutate(restriction_date = case_when(!is.na(restrict_measure) ~ as_date(date)),
             date = as_date(date),
             point = case_when(date == max(date) ~ cases),
             countryname = recode(countryname, "Democratic Republic of the Congo" = "DRC"))
    
      
    df_top_trends %>% 
      ggplot(aes(date, cases, group = countryname)) +
      annotate(geom = "rect",
               xmin = as.Date("2020-01-01"), 
               xmax = as.Date("2020-04-01"), 
               ymin = 0,
               ymax = 100000,
               fill = grey30k, alpha = 0.25, #color = grey50k
               ) +
      geom_vline(aes(xintercept = restriction_date), color = "gray50", size = .5, na.rm = TRUE) +
      geom_path(color = USAID_medblue, size = .9, na.rm = TRUE) +
      geom_point(aes(y = point), color = USAID_medblue, na.rm = TRUE) +
      # geom_vline(xintercept = qtrs, color = "gray30") +
      scale_x_date(date_breaks = "1 months", date_labels = "%b") +
      scale_y_log10(label = comma) +
      si_style() +
      facet_wrap(~countryname, nrow = 3) +
      labs(x = NULL, y = NULL,
           title = "COVID TRENDS IN LARGEST TREATMENT PEPFAR COUNTRIES",
           subtitle = "cumulative confirmed cases, log scale, and introduction of travel restrictions",
           caption = 
           "travel restrictions include: lockdowns (full/partial), domestic travel restrictions/checkpoints and curfews
           source: JHU + HDX COVID-19 government measures") +
      theme(axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8)
            # plot.background = element_rect(fill = bckgrnd)
            )
    
    
    ggsave("covid_casesandrestrictions_Q3.png", path = "Images", dpi = 330,
           width = 7.10, height = 4.15)
    

# STRINGENCY INDEX --------------------------------------------------------

    df_stringency <- df_tx %>% 
      select(iso, targets) %>% 
      left_join(df_stringency)
    
    df_stringency <- df_stringency %>% 
      mutate(countryname = recode(countryname, "Democratic Republic of the Congo" = "DRC"))
    
    df_stringency %>% 
      ggplot(aes(date, fct_reorder(countryname, targets), fill = color)) +
      geom_tile(color = "white") +
      geom_vline(xintercept = qtrs,  color = "gray20", size = 1.2) +
      scale_fill_identity(guide = "legend", labels = rep("", 7)) +
      scale_x_date(expand = c(0.005, 0.005), position = "top") +
      scale_y_discrete(expand = c(.005, .005)) +
      labs(x = NULL, y = NULL,
           caption = "source: stringecy index from Blavatnik School of Government at Oxford University
           color scheme developed by Financial Times") + 
      si_style_nolines() +
      guides(fill = guide_legend(title = "stringency index",
                                 nrow = 1)) +
      theme(legend.spacing.x = unit(0, 'cm'),
            legend.title = element_text(color = "gray30", family = "Source Sans Pro"),
            legend.position = "none",
            axis.text.y = element_text(size = 7),
            plot.background = element_rect(fill = bckgrnd, color = bckgrnd)
            )
    
    ggsave("stringency_index_Q3.png", path = "Images", dpi = 330,
           width = 7.10, height = 4.15)
    