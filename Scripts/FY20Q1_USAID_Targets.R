## PROJECT:  Administor Briefer
## AUTHOR:   achafetz | USAID
## PURPOSE:  visuals to show Administor targets/achievement
## LICENSE:  MIT
## DATE:


library(tidyverse)
library(ICPIutilities)
library(scales)
library(extrafont)
library(glitr)

df_msd <- list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds()

df_genie <- "../Downloads/Genie-OUByIMs-Global-Daily-2020-05-21.zip" %>% 
  read_msd()

ind <- c("HTS_TST", "TX_CURR", "TX_NEW", "TX_PVLS_D",
          "TX_TB_D", "TB_PREV","OVC_SERV", "VMMC_CIRC",
         "PrEP_NEW")

df_q1 <- df_msd %>% 
  bind_rows(df_genie) %>% 
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
  filter(fundingagency == "USAID",
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
         fiscal_year >= 2020,
         indicator %in% ind) %>% 
  group_by(indicator, fiscal_year) %>% 
  summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>% 
  ungroup()

df_q1 <- df_q1 %>% 
  gather(type, value, cumulative, targets) %>% 
  unite(type, c(type, fiscal_year)) %>% 
  spread(type, value)


df_q1 <- df_q1 %>% 
  mutate(achievement = cumulative_2020/targets_2020,
         reminder = targets_2020 - cumulative_2020,
         lab_target20 = paste0(round(reminder/1000000, 1), "m"),
         # lab_target20 = paste0(round(targets_2020/1000000, 1), "m"),
         lab_target21 = paste0(round(targets_2021/1000000, 1), "m"),
         lab_ind = recode(indicator,
                          "HTS_TST" = "Testing \n(HTS_TST)",
                          "TX_CURR" = "Current on treatment \n(TX_CURR)",
                          "TX_NEW" = "New on treatment \n(TX_NEW)",
                          "TB_PREV" = "TB prevention* \n(TB_PREV)",
                          "TX_TB_D" = "ART patients screened for TB*\n(TX_TB_D)",
                          "TX_PVLS_D" = "ART patients with suppressed VL \n(TX_PVLS_D)",
                          "OVC_SERV" = "OVCs served* \n(OVC_SERV)",
                          "VMMC_CIRC" = "VMMC \n(VMMC_CIRC)",
                          "PrEP_NEW" = "New on PrEP* \n(PrEP_NEW)"
                          ),
         lab_target21 = ifelse(lab_target21 == "NAm", "TBD", lab_target21)
         )


df_q1 %>% 
  ggplot(aes(y = fct_reorder(lab_ind, targets_2020))) +
  geom_col(aes(1), fill = NA, color = USAID_blue) +
  geom_col(aes(achievement), fill= USAID_blue) +
  geom_text(aes(1.08, label = lab_target20),
            color = "gray30") +
  geom_text(aes(1.2, label = lab_target21),
            color = "gray30") +
  scale_x_continuous(label = percent, breaks = c(0, .25, .5, .75, 1)) +
  labs(x = "target achievment", y = NULL,
       caption = "* = semi-annual indicators
       Source: FY20Q1c MSD + DATIM GENIE [2020-05-21]") +
  si_style_xgrid()

ggsave("../Downloads/USAID_Target.png", dpi = 300, width = 10, height = 5)


