## PROJECT:  Epic DDD request
## AUTHOR:   A.Chafetz | USAID
## PURPOSE:  pull TX_CURR data and coordinates
## LICENSE:  MIT
## DATE:     2020-06-30
## UPDATED:  2020-08-26


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(glamr)
  library(Wavelength)
  library(ICPIutilities)


# GLOBAL VARIABLES --------------------------------------------------------

  myuser <- ""


# FUNCTION - DATIM API ----------------------------------------------------

  get_tx <-  function(ou_uid, org_lvl, username, password, baseurl = "https://final.datim.org/"){
    
    url_agesex <- paste0(baseurl,
                         "api/29/analytics.json?",
                         "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
                         "dimension=pe:2019Q2;2019Q3;2019Q4;2020Q1;2020Q2&",
                         "dimension=dx:ebCEt4u78PX;Hyvw9VnZ2ch;c03urRVExYe;qkjYvdfOakY&", #TX_CURR (N, *, Age [Agg]/Sex/HIVStatus)
                         "dimension=TWXpUVE2MqL:iM13vdNLWKb;cRAGKdWIDn4&", #Support Type - DSD/TA
                         "dimension=e485zBiR7vG:MnAgKVcL97H;Z8MTaDxRBP6;BURHq262iEL;RV1ZeOr98rI;tIZRQs0FK5P;QOawCj9oLNS;BDPEHrovntA;K9Cw4402aAh;JqZFtdn1sG3;ftAnvKhxRxl;kyKHoWxTzHR;MsbFixtB8mu;bePcXLCq9Ov;dHewx3ia8a6;aIbkjGjUZvE&", #"Age: Cascade Age bands"
                         "dimension=jyUTj5YC3OK&", #Cascade sex
                         "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=false")
    
    url_mmd <- paste0(baseurl,
                      "api/29/analytics.json?",
                      "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
                      "dimension=pe:2019Q4;2020Q1;2020Q2&",
                      "dimension=dx:z3JQrvdUCDM;VgUStUs4PM6;OnDTCTo7wM0&", #TX_CURR * months of ARVs Dispensed
                      "dimension=TWXpUVE2MqL:iM13vdNLWKb;cRAGKdWIDn4&", #Support Type - DSD/TA
                      "dimension=e485zBiR7vG:MnAgKVcL97H;dHewx3ia8a6;aIbkjGjUZvE&", #"Age: Cascade Age bands"
                      "dimension=jyUTj5YC3OK&", #Cascade sex
                      "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=false")
    
    
    url_kp <- paste0(baseurl,
                     "api/29/analytics.json?",
                     "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
                     "dimension=pe:2019Q4;2020Q1;2020Q2&",
                     "dimension=dx:ScQASwweWXL;rEKqsIDMOgB&", #TX_CURR (N, *, KeyPop/HIVStatus)
                     "dimension=TWXpUVE2MqL:iM13vdNLWKb;cRAGKdWIDn4&", #Support Type - DSD/TA
                     "dimension=VCCs1f22cOR:CKCeNqWcxXe;qY5T9mKjsz5;eiR4kOjT60r;UjruEYjGK4R;pi7naVKPzSM&", #Key Populations v3
                     "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=false")
    
    agesex <- extract_datim(url_agesex, username, password)
    mmd <- extract_datim(url_mmd, username, password)
    kp <- extract_datim(url_kp, username, password)
    
    df_tx <- bind_rows(agesex, mmd, kp)
    
    return(df_tx)
  
  }


# COUNTRY INFO ------------------------------------------------------------


  ctry_lst <- get_outable(myuser, mypwd(myuser))
  
  ctry_lst <- ctry_lst %>% 
    filter(operatingunit %in% c("Botswana", "Burundi", "Cameroon", 
                                "Cote d'Ivoire", "Democratic Republic of the Congo", 
                                "Eswatini", "Lesotho", "Malawi", "Mozambique")) %>% 
    select(operatingunit, operatingunit_uid, psnu_lvl = prioritization, facility_lvl)



# PULL DATA ---------------------------------------------------------------


  df_tx <- map2_dfr(.x = ctry_lst$operatingunit_uid,
                    .y = ctry_lst$facility_lvl,
                    .f = ~ get_tx(.x, .y, myuser, mypwd(myuser)))


  df_tx_clean <- df_tx %>% 
    select(-`Organisation unit`) %>% 
    rename(indicatortype = `Support Type`,
           ageasentered = `Age: Cascade Age bands`,
           sex = `Cascade sex`,
           keypop = `Key Populations v3`) %>%
    rename_with(tolower)
  
  df_tx_clean <- df_tx_clean %>% 
    mutate(disaggregate = case_when(str_detect(data, "KeyPop") ~ "KeyPop/HIVStatus",
                                    str_detect(data, "months") ~ "Age/Sex/ARVDispense/HIVStatus",
                                    str_detect(data, "Age Agg") ~ "Age Aggregated/Sex/HIVStatus",
                                    str_detect(data, "Sex") ~ "Age/Sex/HIVStatus"),
           otherdisaggregate = str_extract(data, "(?<=TX_CURR ).* months"),
           otherdisaggregate = ifelse(!is.na(keypop), keypop, otherdisaggregate),
           otherdisaggregate = str_remove(otherdisaggregate,  " and other enclosed settings"),
           ageasentered = str_remove(ageasentered, " \\(.*"),
           indicator = "TX_CURR") %>% 
    select(-data, -keypop)
  
# PULL COORDINATES --------------------------------------------------------
  
  #pull hierarchy
  df_orgs <- purrr::map_dfr(.x = ctry_lst$operatingunit_uid,
                            .f = ~ pull_hierarchy(.x, myuser, mypwd(myuser))) 

  df_orgs <- df_orgs %>% 
    select(-c(level, countryname, psnuuid))
  

# MERGE COORDINATES -------------------------------------------------------

  df_tx_clean <- left_join(df_tx_clean, df_orgs)
  
  df_tx_clean <- df_tx_clean %>% 
    select(operatingunit:longitude, orgunituid, 
           indicator, indicatortype, disaggregate,
           ageasentered, sex, otherdisaggregate,
           period, value) 
  

# EXPORT ------------------------------------------------------------------

  write_csv(df_tx_clean, "Dataout/SBU_Epic_TX_Request_SBU.csv", na = "")

# CHECK -------------------------------------------------------------------

  df <- list.files("~/Data", "OU_IM", full.names = TRUE) %>%
    read_rds()

  df_check <- df %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate != "Total Numerator",
           operatingunit %in% ctry_lst$operatingunit) %>%
    select(-cumulative, -targets) %>% 
    reshape_msd(clean = TRUE) %>% 
    filter(period %in% c("FY19Q3", "FY19Q4", "FY20Q1", "FY20Q2", "FY20Q3")) %>% 
    count(operatingunit, disaggregate, wt = val, name = "datim")
  
  
  df_tx_clean %>% 
    count(operatingunit, disaggregate, wt = value, name = "msd") %>% 
    full_join(df_check) %>% 
    mutate(delta = datim/msd) %>% 
    prinf()
  

  df_check <- df %>%
    filter(indicator == "TX_CURR",
           standardizeddisaggregate != "Total Numerator",
           operatingunit %in% ctry_lst$operatingunit,
           disaggregate == "KeyPop/HIVStatus") %>%
    mutate(otherdisaggregate = str_remove(otherdisaggregate,  " and other enclosed settings")) %>% 
    select(-cumulative, -targets) %>% 
    reshape_msd(clean = TRUE) %>% 
    filter(period %in% c("FY19Q3", "FY19Q4", "FY20Q1", "FY20Q2")) %>% 
    count(operatingunit, otherdisaggregate, wt = val, name = "msd")
  
  
  df_tx_clean %>% 
    filter(disaggregate == "KeyPop/HIVStatus") %>% 
    count(operatingunit, otherdisaggregate, wt = value, name = "datim") %>% 
    full_join(df_check) %>% 
    mutate(delta = datim/msd) %>% 
    prinf()
  