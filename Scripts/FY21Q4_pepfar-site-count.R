# PROJECT:  groundhogday
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  identify number of PEPFAR sites reporting
# LICENSE:  MIT
# DATE:     2021-12-10
# UPDATED:  2022-01-23

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(glitr) #remotes::install_github("USAID-OHA-SI/glitr", build_vignettes = TRUE)
  library(glamr) #remotes::install_github("USAID-OHA-SI/glamr", build_vignettes = TRUE)
  library(janitor)
  library(glue)

# GLOBAL VARIABLES --------------------------------------------------------

  #save DATIM credentials in OS password manager (one time)
  # set_datim()

  #store DATIM credentials in 
  load_secrets("datim")

  
# DATIM API FUNCTION ------------------------------------------------------

  
  pull_sites <- function(ou_name, ou_uid, org_type, org_lvl,
                         fy_pd = 2021, 
                         username, password, 
                         baseurl = "https://final.datim.org/"){
    
    print(paste("Running DATIM API for", ou_name, org_type,  Sys.time(),
                sep = " ... "))
    
    cy_pd <- paste0(fy_pd-1, "Oct", collapse = ";")
    
    type_uid <- ifelse(org_type == "facility", "POHZmzofoVx", "PvuaP6YALSA") #excludes military & Other organisation unit type
    
    core_url <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=pe:", cy_pd, "&", #period
             "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
             "dimension=SH885jaRe0o&", #Funding Mechanism
             "dimension=LxhLO68FcXm:ELZsYsR89rn;CZplmfCbnv2;vw3VoiA4D0s;NYAJ6QkEKbC;Uo2vBxak9im;RxyNwEV3oQf;Fvs28dwjL6e;pkZRNlMgL89;gma5vVZgK49;FfxbuFZVAM5;wdoUps1qb3V;qOgXk080fJH;CUblPgOMGaT;twyHxdQVjMC;hGUykTtC0Xm;f5IPTM7mieH;lYTgCwEjUX6;cwZbCmUvjp7;R59aGLjmKBO;ECGbKy8o3FC;BTIqHnjeG7l;rI3JlpiuwEK;bybAqM1Lnba;AaCcy7dVfWw;Z6TU9Os82Yw;MvszPTQrUhy;cSTYDtvP0Nt;udCop657yzi;o8GCardEcYz;tOiM2uxcnkj;bZOF8bon1dD;TYAjnC2isEk;jbyq87W19Qv;scxfIjoA6nt;oCwIxluUXok;lIUE50KyUIH&", #Technical Area
             "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets / Results - results
             "dimension=HWPJnUTMjEq&", #Disaggregation Type
             "dimension=mINJi7rR1a6:", type_uid,"&", #Type of organisational unit
             "dimension=TWXpUVE2MqL&", #Support Type
             "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    
    
    if(org_type == "agyw"){
      core_url <-
        paste0(baseurl,"api/29/analytics?",
               "dimension=pe:", cy_pd, "&", #period
               "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
               # "dimension=SH885jaRe0o&", #Funding Mechanism - no used with AGYW
               "dimension=LxhLO68FcXm:ELZsYsR89rn&", #Technical Area - AGYW
               "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets / Results - results
               "dimension=HWPJnUTMjEq&", #Disaggregation Type
               "dimension=mINJi7rR1a6:", type_uid,"&", #Type of organisational unit
               "dimension=TWXpUVE2MqL&", #Support Type
               "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true")
    }
    
    df <- glamr::datim_process_query(core_url, username, password)

    if(!is.null(df)){
      df <- df %>%
        dplyr::filter(`Technical Area` %in% c("SC_CURR", "SC_ARVDISP", "HRH_PRE") | Value != 0) %>%
        dplyr::mutate(operatingunit = ifelse(stringr::str_detect(orglvl_3, "Region"), paste0(orglvl_3, "/", orglvl_4), orglvl_3)) %>%
        dplyr::select(-orglvl_1, -orglvl_2) %>% 
        tidyr::unite(orgunit_hierarchy, starts_with("orglvl"), sep = "/", remove = FALSE) %>% 
        dplyr::mutate(orgunit_hierarchy = paste(orgunit_hierarchy, `Organisation unit`, sep = "/")) %>% 
        dplyr::distinct(operatingunit, orgunituid, orgunit_hierarchy, sitetype = `Type of organisational unit`, indicatortype = `Support Type`, indicator = `Technical Area`)
      
      df <- df %>% 
        dplyr::mutate(x = TRUE) %>% 
        tidyr::pivot_wider(names_from = indicator,
                           names_glue = "has_{tolower(indicator)}",
                           values_from = x)
    }
    
    return(df)
  }
  

  
# IDENTIFY INPUTS FOR API -------------------------------------------------
  
  #country and level list
  ctry_list <- get_outable(datim_user(), datim_pwd()) %>% 
    select(countryname, countryname_uid, 
           community = community_lvl, facility = facility_lvl) %>% 
    pivot_longer(c(community, facility),
                 names_to = "type",
                 values_to = "level") 
  
  #add a separate level for agyw
  ctry_list <- ctry_list %>% 
    bind_rows(ctry_list %>%
                filter(type == "community") %>% 
                mutate(type = "agyw"))  %>% 
    arrange(countryname, desc(type))
  

# RUN API -----------------------------------------------------------------


  df_sites <- ctry_list %>%
    pmap_dfr(~pull_sites(..1, ..2, ..3, ..4, 
                         username = datim_user(), password = datim_pwd()))
    

# SITE COUNT --------------------------------------------------------------

  df_sites %>% 
    distinct(orgunituid) %>% 
    count()
 
  df_sites %>% 
    group_by(orgunituid) %>% 
    mutate(indicatortype = ifelse(n() > 1, "multi", indicatortype)) %>% 
    ungroup() %>% 
    distinct(orgunituid, indicatortype, sitetype) %>% 
    count(sitetype, indicatortype) %>% 
    pivot_wider(names_from = indicatortype, 
                values_from = "n",
                values_fill = 0) %>% 
    adorn_totals(where = c("row", "col"))
    

# EXPORT ------------------------------------------------------------------

  df_ind <- tribble(
    ~category,      ~indicator,
    "Prevention",     "AGYW_PREV",
    "Prevention",    "FPINT_SITE",
    "Prevention",      "GEND_GBV",
    "Prevention",        "KP_MAT",
    "Prevention",       "KP_PREV",
    "Prevention",      "OVC_SERV",
    "Prevention",       "PP_PREV",
    "Prevention",     "PrEP_CURR",
    "Prevention",      "PrEP_NEW",
    "Prevention",       "TB_PREV",
    "Prevention",     "VMMC_CIRC",
    "Testing",     "CXCA_SCRN",
    "Testing",     "HTS_INDEX",
    "Testing",    "HTS_RECENT",
    "Testing",      "HTS_SELF",
    "Testing",       "HTS_TST",
    "Testing",   "OVC_HIVSTAT",
    "Testing",     "PMTCT_EID",
    "Testing",      "PMTCT_FO",
    "Testing", "PMTCT_HEI_POS",
    "Testing",    "PMTCT_STAT",
    "Testing",       "TB_STAT",
    "Treatment",       "CXCA_TX",
    "Treatment",     "PMTCT_ART",
    "Treatment",        "TB_ART",
    "Treatment",       "TX_CURR",
    "Treatment",         "TX_ML",
    "Treatment",        "TX_NEW",
    "Treatment",        "TX_RTT",
    "Treatment",         "TX_TB",
    "Viral Load Suppression",       "TX_PVLS",
    "Health Systems",      "EMR_SITE",
    "Health Systems",       "HRH_PRE",
    "Health Systems",     "LAB_PTCQI",
    "Health Systems",    "SC_ARVDISP",
    "Health Systems",       "SC_CURR")
  
  df_ind_adj <- df_ind %>% 
    mutate(category = recode(category,
                             "Prevention" = "prev",
                             "Testing" = "test",
                             "Treatment" = "treat",
                             "Viral Load Suppression" = "vls",
                             "Health Systems" = "hss"),
           indicator = paste0("has_", tolower(indicator)))
         
  ind_order <- df_ind_adj %>% 
    mutate(indicator = str_replace(indicator, "has", paste0("has_", category))) %>% 
    pull(indicator)
  
  df_sites_wide <- df_sites %>% 
    pivot_longer(starts_with("has_"), names_to = "indicator", values_drop_na = TRUE) %>% 
    tidylog::left_join(df_ind_adj) %>% 
    mutate(indicator = str_replace(indicator, "has", paste0("has_", category)),
           indicator = factor(indicator, ind_order)) %>% 
    arrange(indicator) %>% 
    select(-category) %>% 
    pivot_wider(names_from = indicator)
    
  
  pd <- pepfar_data_calendar %>%
    filter(entry_close <= Sys.Date()) %>%
    slice_tail() %>%
    mutate(period = glue::glue("FY{stringr::str_sub(fiscal_year, -2)}Q{quarter}{stringr::str_sub(type, end = 1)}")) %>% 
    pull()
                                      
  write_csv(df_sites_wide, 
            paste0("Dataout/", pd, "_PEPFAR-sites-and-types_", 
                   format(Sys.Date(), "%Y%m%d"), ".csv"), 
            na = "")
    
# PREP WORK ---------------------------------------------------------------


  # #pull list of DATIM dimensions for API
  # datim_dimensions() %>%
  #   arrange(dimension) %>% 
  #   prinf()
  # 
  # #pull items from dimensions to add to API
  # datim_dim_items("Support Type")
  # 
  # #all indicator uids
  # df_ind_datim <- datim_dim_items("Technical Area")
  # 
  # #indicators from MER 2.5
  # df_ind <- tribble(
  #                  ~category,      ~indicator,
  #               "Prevention",     "AGYW_PREV",
  #               "Prevention",    "FPINT_SITE",
  #               "Prevention",      "GEND_GBV",
  #               "Prevention",        "KP_MAT",
  #               "Prevention",       "KP_PREV",
  #               "Prevention",      "OVC_SERV",
  #               "Prevention",       "PP_PREV",
  #               "Prevention",     "PrEP_CURR",
  #               "Prevention",      "PrEP_NEW",
  #               "Prevention",       "TB_PREV",
  #               "Prevention",     "VMMC_CIRC",
  #                  "Testing",     "CXCA_SCRN",
  #                  "Testing",     "HTS_INDEX",
  #                  "Testing",    "HTS_RECENT",
  #                  "Testing",      "HTS_SELF",
  #                  "Testing",       "HTS_TST",
  #                  "Testing",   "OVC_HIVSTAT",
  #                  "Testing",     "PMTCT_EID",
  #                  "Testing",      "PMTCT_FO",
  #                  "Testing", "PMTCT_HEI_POS",
  #                  "Testing",    "PMTCT_STAT",
  #                  "Testing",       "TB_STAT",
  #                "Treatment",       "CXCA_TX",
  #                "Treatment",     "PMTCT_ART",
  #                "Treatment",        "TB_ART",
  #                "Treatment",       "TX_CURR",
  #                "Treatment",         "TX_ML",
  #                "Treatment",        "TX_NEW",
  #                "Treatment",        "TX_RTT",
  #                "Treatment",         "TX_TB",
  #   "Viral Load Suppression",       "TX_PVLS",
  #           "Health Systems",      "EMR_SITE",
  #           "Health Systems",       "HRH_PRE",
  #           "Health Systems",     "LAB_PTCQI",
  #           "Health Systems",    "SC_ARVDISP",
  #           "Health Systems",       "SC_CURR")
  # 
  # #join indicators with %>% 
  # lst_ind_uid <- df_ind %>% 
  #   left_join(df_ind_datim, by = c("indicator" = "item")) %>% 
  #   pull() %>% 
  #   paste0(collapse = ";")
  
