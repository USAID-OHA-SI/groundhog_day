##  PROJECT: jacquie table
##  AUTHOR:  jdavis | USAID
##  PURPOSE: code to create table of results a la jacquie
##  LICENCE: MIT
##  DATE:    2020-6-16
##  UPDATE: 

#-------------------------------------------------------
library(tidyverse)
library(ICPIutilities)

#------------------------------------------------------

data_out <- "C:/Users/Josh/Documents/data/fy20_q2_v1/zim"
data <- "C:/Users/Josh/Documents/data/fy20_q2_v1"

#read in data------------------------------------------------------

# df <- MSD

#globals-----------------------------------------------------------

prinf <- function(df) {
  print(df, n = Inf)
}

indc2 <- c("TX_CURR", "TX_NET_NEW", "TX_NEW", "HTS_TST", "HTS_TST_POS", "TX_RTT", "TX_ML")

ind_list <- c("TX_CURR", "TX_NET_NEW", "retention",
              "TX_NEW", "linkage", "HTS_TST_POS", "positivity",
              "HTS_TST", "TX_PVLS_D", "coverage", "suppression",
              "TX_ML", "TX_RTT")

pd_list <- c("fy2019q1",
             "fy2019q2",
             "fy2019q3",
             "fy2019q4",
             "fy2019cumulative",
             "fy2019_targets",
             "fy2020q1",
             "fy2020q2",
             "fy2020cumulative",
             "fy2020_targets")

#step 1: everything other than vl/retention

df1 <- df %>% 
  filter(operatingunit == "Zimbabwe",
         indicator %in% indc2,
         fiscal_year %in% c(2019, 2020),
         fundingagency != "Dedup") %>%
  select(fundingagency, psnu, standardizeddisaggregate, otherdisaggregate, sex, trendsfine, indicator, fiscal_year, targets:cumulative) %>%
  group_by(fundingagency, psnu, standardizeddisaggregate, otherdisaggregate, sex, trendsfine, fiscal_year, indicator) %>% 
  summarise_if(is.numeric,  ~sum(., na.rm = TRUE)) %>%
  ungroup() %>% 
  reshape_msd("long")

#step 2: retention/vl

df_vl <- df %>%
  filter(indicator %in% c("TX_CURR", "TX_PVLS"),
         fiscal_year %in% c(2019, 2020),
         fundingagency != "Dedup",
         operatingunit == "Zimbabwe") %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator),
         indicator = ifelse(numeratordenom == "N" , paste0(indicator, "_N"), indicator),
         indicator = case_when(indicator == "TX_CURR_N" ~ "TX_CURR",
                               TRUE ~ indicator)) %>% 
  select(fundingagency, psnu, standardizeddisaggregate, sex, trendsfine, indicator, fiscal_year, starts_with("qtr")) %>%
  group_by(fundingagency, psnu, standardizeddisaggregate, sex, trendsfine, fiscal_year, indicator) %>% 
  summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>%
  ungroup() %>% 
  reshape_msd("long") %>% 
  spread(indicator, val) %>% 
  group_by(fundingagency, psnu, standardizeddisaggregate, sex, trendsfine) %>% 
  mutate(TX_CURR_lag2 = lag(TX_CURR, 2),
         TX_CURR_lag1 = lag(TX_CURR, 1)) %>%
  select(-TX_CURR) %>% 
  ungroup() %>% 
  gather(indicator, value = val, TX_PVLS_D:TX_CURR_lag1, na.rm = TRUE)

##  step 3: append

df_zim <- bind_rows(df1, df_vl)

##  Step 4: create summary table, create vl suppression, coverage, retention
## summary table
df_zim %>%
  filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
         fundingagency %in% c("USAID", "HHS/CDC")) %>% 
  group_by(fundingagency, indicator, period) %>% 
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
  spread(indicator, val) %>%
  mutate(suppression = TX_PVLS_D/TX_CURR_lag2,
         coverage = TX_PVLS_N/TX_PVLS_D,
         retention = TX_CURR/(TX_CURR_lag1+TX_NEW),
         linkage = round(TX_NEW/HTS_TST_POS,2)*100,
         positivity = round(HTS_TST_POS/HTS_TST, 3)*100) %>%
  ungroup %>% 
  select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>% 
  gather(indicator, val, HTS_TST:positivity) %>%
  mutate(indicator = factor(indicator, ind_list),
         period = factor(period, pd_list),
         fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>% 
  spread(period, val)

# write_csv(file.path(data_out, "zim_overview_tbl_q2.csv"))


## table for peds
df_zim %>%
  filter(trendsfine %in% c("<01", "01-09", "10-14"),
         fundingagency %in% c("USAID", "HHS/CDC")) %>% 
  group_by(fundingagency, indicator, period) %>% 
  summarise_if(is.numeric, ~ sum(., na.rm = TRUE)) %>%
  spread(indicator, val) %>%
  mutate(suppression = TX_PVLS_D/TX_CURR_lag2,
         coverage = TX_PVLS_N/TX_PVLS_D,
         retention = TX_CURR/(TX_CURR_lag1+TX_NEW),
         linkage = round(TX_NEW/HTS_TST_POS,2)*100,
         positivity = round(HTS_TST_POS/HTS_TST, 3)*100) %>%
  ungroup %>% 
  select(-TX_CURR_lag1, -TX_CURR_lag2, -TX_PVLS_N) %>% 
  gather(indicator, val, HTS_TST:positivity) %>%
  mutate(indicator = factor(indicator, ind_list),
         period = factor(period, pd_list),
         fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>% 
  spread(period, val) %>%
  write_csv(file.path(data_out, "zim_overview_tbl_peds_q2.csv"))






