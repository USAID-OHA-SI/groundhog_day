##  PROJECT: TX Treatment Deep Dive
##  AUTHOR:  A.Chafetz | USAID
##  PURPOSE: pull and structure VLS data accounting for site shifts
##  LICENCE: MIT
##  DATE:    2020-05-18
##  UPDATE:  2020-06-17



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)
library(fs)
library(scales)
library(glitr)
library(patchwork)
library(extrafont)
library(ICPIutilities)

# GLOBAL VARIABLES --------------------------------------------------------

  myuser <- ""


# DATIM API FUNCTION ------------------------------------------------------

  pull_tx <- function(ou_uid, org_lvl, username, password, baseurl = "https://final.datim.org/"){
    print(paste("running ", ou_uid, " ... ", Sys.time()))
    core_url <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
             "dimension=pe:2017Q3;2017Q4;2018Q1;2018Q2;2018Q3;2018Q3;2018Q4;2019Q1;2019Q2;2019Q3;2019Q4;2020Q1&", #period
             "dimension=bw8KHXzxd9i&", #Funding Agency
             "dimension=SH885jaRe0o&", #Funding Mechanism
             "dimension=LxhLO68FcXm:MvszPTQrUhy;bZOF8bon1dD&", #technical area - TX_CURR, TX_PVLS
             "dimension=RUkVjD3BsS1:PE5QVF0w4xj;Azqe383ljKu&", #Top Level  - Numerator, Denominator
             "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    
    df <- get_datim_targets(core_url, username, password)
    
    return(df)
  }

  
  pull_txagesex <- function(ou_uid, org_lvl, username, password, baseurl = "https://final.datim.org/"){
    print(paste("running ", ou_uid, " ... ", Sys.time()))
    core_url <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
             "dimension=pe:2019Q3;2019Q4;2020Q1&", #period
             "dimension=bw8KHXzxd9i&", #Funding Agency
             "dimension=SH885jaRe0o&", #Funding Mechanism
             "dimension=LxhLO68FcXm:MvszPTQrUhy;bZOF8bon1dD&", #technical area - TX_CURR, TX_PVLS
             "dimension=lD2x0c8kywj&", #Numerator, Denominator
             "dimension=e485zBiR7vG&", #Age: Cascade Age bands
             "dimension=SEOZOio7f7o&", #Sex
             "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    
    df <- get_datim_targets(core_url, username, password)
    
    return(df)
  }

# IDENTIFY INPUTS FOR API -------------------------------------------------

  #identify site level in each ou
    df_lvls <- identify_levels(username = myuser, password = mypwd(myuser))
  
  #pull orgunit uids
    df_uids <- identify_ouuids(username = myuser, password = mypwd(myuser))
  
  #table for API use
    ctry_list <- left_join(df_lvls, df_uids, by = c("country_name" = "displayName")) %>% 
      select(operatingunit = name3, operatingunituid = id, countryname = country_name, 
             psnu_lvl = prioritization, site_lvl = facility)
    
    rm(df_lvls, df_uids)


# PULL DATA ---------------------------------------------------------------

  #run API for TZA
    ctry_list <- filter(ctry_list, operatingunit == "Tanzania")
    df <- pull_tx(ctry_list$operatingunituid, ctry_list$site_lvl, myuser, mypwd(myuser))
    df_agesex <- pull_txagesex(ctry_list$operatingunituid, ctry_list$site_lvl, myuser, mypwd(myuser))


# MUNGE -------------------------------------------------------------------

    df <- mutate(df, disagg = "Total")
    df_agesex <- df_agesex %>% 
      mutate(disagg = "Age/Sex") %>% 
      rename(`Top Level` = `Numerator / Denominator`)
    
    df <- bind_rows(df, df_agesex)
    
  #convert date from CY to FY
    df_clean <- df %>% 
      mutate(Period = Period %>% 
               str_extract("[:alpha:]{3} [:digit:]{4}") %>% 
               str_replace(" ", " 1, ") %>% 
               mdy %>% 
               quarter(with_year = TRUE, fiscal_start = 10) %>% 
               as.character() %>% 
               str_replace("20", "FY") %>% 
               str_replace("\\.", "Q")) 
  
  #rename columns
    df_clean <- df_clean %>% 
      select(-orglvl_1, -orglvl_2) %>% 
      rename(period = Period,
             operatingunit = orglvl_3, 
             fundingagency = `Funding Agency`,
             facility = `Organisation unit`, 
             mech = `Funding Mechanism`, 
             indicator = `Technical Area`,
             age = `Age: Cascade Age bands`,
             sex = Sex,
             value = Value) %>%
      mutate(snu1 = orglvl_4,
             countryname = case_when(str_detect(operatingunit, "Region") ~ snu1,
                                     TRUE                                ~ operatingunit))
  #rename indicator
    df_clean <- df_clean %>% 
      mutate(indicator = ifelse(str_detect(`Top Level`, "Denominator"), paste0(indicator, "_D"), indicator)) %>% 
      select(-`Top Level`)
    
  #clean ages
    df_clean <- df_clean %>% 
      mutate(age = str_remove(age, " \\(.*\\)"),
             age = recode(age, "5-9" = "05-09", "1-4" = "01-04", "<1" = "<01")) %>% 
      filter(!age %in% c("<15", "15+", "Unknown Age"))
    
  #clean agency
    df_clean <- df_clean %>% 
      mutate(fundingagency = str_remove(fundingagency, "HHS/"))
    
  #add psnu lvl to df for renaming purposes
    df_clean <- ctry_list %>% 
      select(countryname, psnu_lvl) %>% 
      left_join(df_clean, ., by = "countryname")
    
  #function for renaming psnu by org level
    rename_psnu <- function(df, lvl){
      
      oldname <- paste0("orglvl_", lvl) 
      
      df <- df %>% 
        filter(psnu_lvl == lvl) %>% 
        rename(psnu = oldname) %>% 
        select(-contains("lvl"))
      
      invisible(df)
    }
    
    df_clean <- map_dfr(unique(ctry_list$psnu_lvl) %>% setdiff(0), 
                   ~ rename_psnu(df_clean, .x))
    
    df_clean <- df_clean %>% 
      select(period, operatingunit, countryname, snu1, psnu, 
             facility, orgunituid, everything()) 
  
  #separate mech info
    df_clean <- df_clean %>% 
      mutate(mech = str_replace(mech, "^0000(0|1)", "ZZZ - 0000\\1 -")) %>% 
      separate(mech, c(NA, "mech_code", "mech_name"), extra = "merge", remove = FALSE)
    
    df_clean <- mutate(df_clean, value = as.numeric(value))

# EXPORT ------------------------------------------------------------------

  dir_create("Data")
  write_csv(df_clean, "Data/TZA_TX_CURR.PVLS_IM_Site.csv", na = "")
  # df_clean <- read_csv("Data/TZA_TX_CURR.PVLS_IM_Site.csv",
  #                      col_types = c(.default = "c", value = "d"))


# MUNGE FOR AGENCY DATA VIZ -----------------------------------------------

  #spread indicator for calculations
    df_tza <- df_clean %>% 
      select(period, orgunituid, fundingagency, mech_code, 
             indicator, disagg, age, sex, value) %>% 
      spread(indicator, value)

  #create a TX_CURR lag agnostic of mech/agency to roll up 
    df_tza <- df_tza %>% 
      filter(!period %in% c("FY17Q4", "FY18Q1", "FY18Q2")) %>% 
      complete(period, nesting(orgunituid, disagg, age, sex)) %>% 
      arrange(orgunituid, disagg, age, sex, period) %>% 
      group_by(orgunituid) %>% 
      fill(fundingagency, mech_code) %>% 
      group_by(orgunituid, disagg, sex, age) %>% 
      mutate(TX_CURR_lag2 = lag(TX_CURR, 2)) %>% 
      ungroup() %>% 
      filter(!is.na(fundingagency))
  
  #aggregate up to agency totals
    df_tza_agency <- df_tza %>% 
      group_by(fundingagency, period, disagg, age, sex) %>% 
      summarise_if(is_double, sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")))
  
  #calc VLC/S
    df_tza_trends <- df_tza_agency %>% 
      filter(disagg == "Total",
             str_detect(period, "FY(19|20)"),
             !is.na(fundingagency)) %>% 
      mutate(VL_COVERAGE = TX_PVLS_D/TX_CURR_lag2,
             VL_SUPPRESSION = TX_PVLS/TX_PVLS_D) %>% 
      select(fundingagency, period, VL_COVERAGE, VL_SUPPRESSION) %>% 
      gather(type, value, VL_COVERAGE, VL_SUPPRESSION) %>% 
      mutate(type = str_replace(type, "_", " "),
             endpoint = case_when(period %in% c(min(period), max(period)) ~ value),
             lab = case_when(period == max(period) ~ paste(percent(value, 1),fundingagency),
                             period == min(period) ~ percent(value, 1)))
    
  #df for agency vline in viz
    df_tza_trends_maxpd <- df_tza_trends %>% 
      filter(period == max(period)) %>% 
      select(-endpoint, -lab) %>% 
      mutate(fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")))
    
  #calc VLC/S for age/sex in FY20Q1
    df_tza_agesex <- df_tza_agency %>% 
      filter(disagg == "Age/Sex",
             period == max(period)) %>% 
      mutate(VL_COVERAGE = TX_PVLS_D/TX_CURR_lag2,
             VL_SUPPRESSION = TX_PVLS/TX_PVLS_D) %>% 
      select(-TX_CURR, -TX_PVLS, -TX_PVLS_D, -TX_CURR_lag2) %>% 
      gather(type, value, VL_COVERAGE, VL_SUPPRESSION) %>% 
      mutate(type = str_replace(type, "_", " "),
             agencysex = paste(fundingagency, sex),
             agencysex = fct_rev(agencysex))
  


# COLOR PALETTES ----------------------------------------------------------

  pal_woods <- add_color("woods")
  
  pal_agency <- c("USAID" = pal_woods[2], 
                  "CDC" = pal_woods[1], 
                  "DOD" = pal_woods[6])
  
  pal_agencysex <-   c("USAID Female" = "#5f8c86", "USAID Male" = "#89b599",
                       "CDC Female" = "#2e407b", "CDC Male" ="#5a91a6",
                       "DOD Female" = "#82816a", "DOD Male" ="#aa9d93")
  

# PLOT VL VISUALS ---------------------------------------------------------

  
  plot <- function(ind){
    
    df_trends <- filter(df_tza_trends, type == ind)
    df_trends_maxpd <- filter(df_tza_trends_maxpd, type == ind)
    df_agesex <- filter(df_tza_agesex, type == ind)
    
    pd_max <- max(df_trends$period)
    
    v_trends <- df_trends %>% 
      ggplot(aes(period, value, group = fundingagency, color = fundingagency)) +
      geom_path(size = .9) +
      geom_point(aes(y = endpoint), size = 5, na.rm = TRUE) +
      geom_text(aes(label = lab,                                 
                    hjust = ifelse(period == pd_max, -.2, 1.4)), 
                family = "Source Sans Pro", color = "gray30",
                na.rm = TRUE) +
      expand_limits(x = c(0, 7.5)) + 
      scale_y_continuous(label = percent_format(1)) +
      scale_color_manual(values = pal_agency) +
      labs(x = NULL, y = NULL,
           title = paste("AGENCY TRENDS IN", ind)) +
      si_style() +
      theme(legend.position = "none")
    
    
    v_agesex <- df_agesex %>% 
      ggplot(aes(value, age, group = age)) +
      geom_vline(data = df_trends_maxpd, aes(xintercept = value),
                 color = "gray50") + 
      geom_path(size = .9, color = "gray50") +
      geom_point(aes(fill = agencysex), shape = 21, color = "white", size = 4, na.rm = TRUE) +
      facet_grid(~ fundingagency) +
      labs(x = NULL, y = NULL,
           title = paste(pd_max, ind,"BY AGE AND SEX"),
           subtitle = "Females = darker shade, Males = lighter shade") +
      scale_x_continuous(label = percent_format(1)) +
      scale_fill_manual(values = pal_agencysex, name = NULL) +
      si_style() +
      theme(panel.border = element_rect(color = "gray30", fill = NA),
            legend.position = "none",
            axis.text.x = element_text(size = 8),
            plot.subtitle = element_text(size = 10),
            strip.text = element_text(face = "bold"))
    
    v_trends + v_agesex  +
      plot_annotation(caption = "Vertical line denotes agency VL total in FY20
                    Site transitions have been accounted for in data preparation
                    Source: DATIM API [2020-06-17]") &
      theme(plot.caption = element_text(family = "Source Sans Pro", color = "gray30"))
    
    filename <- paste0("Agency_", ind, ".png")
    ggsave(filename, path = "Images",
           width = 9.7, height = 4.37, dpi = 300)
  }

  plot("VL COVERAGE")  
  plot("VL SUPPRESSION")  


# MUNGE FOR MECH DATA VIZ -----------------------------------------------
  
  #spread indicator for calculations
    df_tza_mech <- df_tza %>% 
      group_by(fundingagency, mech_code, period, disagg, age, sex) %>% 
      summarise_if(is_double, sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")),
             partner = case_when(mech_code == "18237" ~ "Deloitte",
                                 mech_code == "18060" ~ "EGPAF")) %>% 
      filter(mech_code %in% c("18237", "18060"))
  
  #create VLC/S trends
    df_tza_mech_trends <- df_tza_mech %>% 
      filter(disagg == "Total",
             str_detect(period, "FY(19|20)"),
             !is.na(fundingagency)) %>% 
      mutate(VL_COVERAGE = TX_PVLS_D/TX_CURR_lag2,
             VL_SUPPRESSION = TX_PVLS/TX_PVLS_D) %>% 
      select(fundingagency, mech_code, partner, period, VL_COVERAGE, VL_SUPPRESSION) %>% 
      gather(type, value, VL_COVERAGE, VL_SUPPRESSION) %>% 
      mutate(type = str_replace(type, "_", " "),
             endpoint = case_when(period %in% c(min(period), max(period)) ~ value),
             lab = case_when(period == max(period) ~ paste(percent(value, 1),partner),
                             period == min(period) ~ percent(value, 1)))
  #df for partner vline in viz
    df_tza_mech_trends_maxpd <- df_tza_mech_trends %>% 
      filter(period == max(period)) %>% 
      select(-endpoint, -lab) %>% 
      mutate(fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")))
  
  #Age/Sex VLC/S calcs
  df_tza_mech_agesex <- df_tza_mech %>% 
    filter(disagg == "Age/Sex",
           period == max(period)) %>% 
    mutate(VL_COVERAGE = TX_PVLS_D/TX_CURR_lag2,
           VL_SUPPRESSION = TX_PVLS/TX_PVLS_D) %>% 
    select(-TX_CURR, -TX_PVLS, -TX_PVLS_D, -TX_CURR_lag2) %>% 
    gather(type, value, VL_COVERAGE, VL_SUPPRESSION) %>% 
    mutate(partner = case_when(mech_code == "18237" ~ "Deloitte",
                               mech_code == "18060" ~ "EGPAF"),
           type = str_replace(type, "_", " "),
           agencysex = paste(fundingagency, sex),
           agencysex = fct_rev(agencysex))

  

# PLOT MECH VL VISUALS ----------------------------------------------------

  
  plot_mech <- function(ind){
    
    df_mech_trends <- filter(df_tza_mech_trends, type == ind)
    df_mech_trends_maxpd <- filter(df_tza_mech_trends_maxpd, type == ind)
    df_mech_agesex <- filter(df_tza_mech_agesex, type == ind)
    
    pd_max <- max(df_mech_trends$period)
    
    
    v_trends <- df_mech_trends %>% 
      ggplot(aes(period, value, group = partner, color = fundingagency)) +
      geom_path(size = .9) +
      geom_point(aes(y = endpoint), size = 5, na.rm = TRUE) +
      geom_text(aes(label = lab,                                 
                    hjust = ifelse(period == pd_max,-.2, 1.4)), 
                family = "Source Sans Pro", color = "gray30",
                na.rm = TRUE) +
      expand_limits(x = c(0, 8)) + 
      scale_y_continuous(label = percent_format(1)) +
      scale_color_manual(values = pal_agency) +
      labs(x = NULL, y = NULL,
           title = paste("AGENCY TRENDS IN", ind)) +
      si_style() +
      theme(legend.position = "none")
    
    
    v_agesex <- df_mech_agesex %>% 
      ggplot(aes(value, age, group = age)) +
      geom_vline(data = df_mech_trends_maxpd, aes(xintercept = value),
                 color = "gray50") + 
      geom_path(size = .9, color = "gray50") +
      geom_point(aes(fill = agencysex), shape = 21, color = "white", size = 4, na.rm = TRUE) +
      facet_grid(~ partner) +
      labs(x = NULL, y = NULL,
           title = paste(pd_max, ind,"BY AGE AND SEX"),
           subtitle = "Females = darker shade, Males = lighter shade") +
      scale_x_continuous(label = percent_format(1)) +
      scale_fill_manual(values = pal_agencysex, name = NULL) + 
      si_style() +
      theme(panel.border = element_rect(color = "gray30", fill = NA),
            legend.position = "none",
            axis.text.x = element_text(size = 8),
            plot.subtitle = element_text(size = 10),
            strip.text = element_text(face = "bold"))
    
    v_trends + v_agesex  +
      plot_annotation(caption = "Vertical line denotes agency VL total in FY20
                    Site transitions have been accounted for in data preparation
                    Source: DATIM API [2020-06-17]") &
      theme(plot.caption = element_text(family = "Source Sans Pro", color = "gray30"))
    
    filename <- paste0("Partner_", ind, ".png")
    ggsave(filename, path = "Images",
           width = 9.7, height = 4.37, dpi = 300)
  }
  
plot_mech("VL COVERAGE")  
plot_mech("VL SUPPRESSION")  
