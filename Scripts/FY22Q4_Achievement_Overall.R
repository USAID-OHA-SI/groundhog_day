# PROJECT:  rebooTZ
# AUTHOR:   A.Chafetz | USAID
# PURPOSE:  provide additional FY21Q3 partner review slides
# LICENSE:  MIT
# DATE:     2021-08-19
# UPDATED:  2021-08-20


# SOURCE META DATA --------------------------------------------------------

# DATIM DATA GENIE
# PSNU By IM
# DATIM data as of: 08/14/2021 21:59:04 UTC
# Genie report updated: 08/19/2021 01:43:13 UTC
# 
# Current period(s): 2020 Target,  2020 Q1,  2020 Q2,  2020 Q3,  2020 Q4,  2021 Target,  2021 Q1,  2021 Q2,  2021 Q3 

# Operating Unit: Tanzania
# Daily/Frozen: Daily

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gisr)
library(sf)
library(ggrepel)


# GLOBAL VARIABLES --------------------------------------------------------

load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
pattern = "OU_IM_FY20-23")

#select indicators
ind_sel <- c("HTS_TST","HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS", 
             "PrEP_NEW", "VMMC_CIRC", "OVC_SERV", "KP_PREV", "PMTCT_EID", "TB_PREV")

#caption info for plotting
source <- source_info(file_path)

#current FY and quarter
curr_fy <- source_info(file_path, return = "fiscal_year")
curr_qtr <- source_info(file_path, return = "quarter")


# IMPORT ------------------------------------------------------------------

df <- read_msd(file_path)   


# MUNGE -------------------------------------------------------------------

#subset to key indicators
df_achv <- df %>% 
  clean_indicator() %>%
  #rowwise() %>% 
  #mutate(TX_IIT= sum(TX_ML_IIT_less_three_mo, TX_ML_IIT_more_three_mo, na.rm = T)) %>% 
  #ungroup() %>%
  filter(funding_agency == "USAID",
         operatingunit != "Ukraine",
         fiscal_year == curr_fy,
         indicator %in% ind_sel) 
 

# MUNGE - GLOBAL/OU ACHIEVEMENT ---------------------------------------------

## Aggregating results & targets at the global level for each indicator
df_achv <- df_achv %>% 
  bind_rows(df_achv %>% 
              mutate(operatingunit = "GLOBAL",
                     operatingunituid = "GLOBAL")) %>% 
  filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  group_by(fiscal_year, operatingunit, operatingunituid, indicator) %>% 
  summarize(across(c(targets, cumulative), sum, na.rm = TRUE), 
            .groups = "drop")

#calculate achievement and add colors 
df_achv <- df_achv %>% 
  adorn_achievement(curr_qtr)

#viz adjustments
df_achv_viz <- df_achv %>% 
  mutate(global_achv = case_when(operatingunit == "GLOBAL" ~ achievement),
         achievement = ifelse(operatingunit == "GLOBAL", NA, achievement),
         indicator = factor(indicator, ind_sel),
         baseline_pt_1 = 0,
         baseline_pt_2 = .25,
         baseline_pt_3 = .5,
         baseline_pt_4 = .75,
         baseline_pt_5 = 1,
  )

#adjust facet label to include indicator and national values
df_achv_viz <- df_achv_viz %>% 
  mutate(ind_w_glob_vals = case_when(operatingunit == "GLOBAL" & is.na(targets) ~ 
                                       glue("**{indicator}**<br><span style = 'font-size:11pt;'>No MER reporting</span>"),
                                     operatingunit == "GLOBAL" ~ 
                                       glue("**{indicator}**<br><span style = 'font-size:11pt;'>{comma(cumulative, 1)} / 
                                            {comma(targets, 1)}</span>")),
          operatingunit = case_when(operatingunit == "Western Hemisphere Region" ~ "WHR",
                                    operatingunit == "West Africa Region" ~ "WAR", 
                                    operatingunit == "Democratic Republic of the Congo" ~ "DRC", TRUE ~ operatingunit)) %>% 
  group_by(indicator) %>% 
  mutate(rank_worst=rank(achievement, ties.method="min")) %>%
  fill(ind_w_glob_vals, .direction = "downup") %>% 
  ungroup() %>% 
  arrange(indicator) %>% 
  mutate(ind_w_glob_vals = fct_inorder(ind_w_glob_vals))


# VIZ - ACHIEVEMENT GLOBAL -------------------------------------------------------
df_achv_viz %>% 
  ggplot(aes(achievement, indicator, color = achv_color)) +
  geom_blank() + # creates blank canvas +
  geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_1), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_2), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_3), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_4), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_5), shape = 3, color = "#D3D3D3") +
  geom_point(aes(global_achv), size = 12, alpha = 1, na.rm = TRUE, 
             position=position_nudge(y=0.3)) +
  geom_text(aes(global_achv, label = percent(global_achv, 1)), na.rm = TRUE,
            position=position_nudge(y=0.3),
            color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
  coord_cartesian(clip = "off") + # default decides how much to show - expands padding
  scale_x_continuous(limit=c(0,1.1),oob=scales::squish) + #capping achievement at 110
  scale_color_identity() + #whatever value is defined by color -- use that value from data frame
  facet_wrap(~ind_w_glob_vals, nrow=2, scales = "free_y") +
  labs(x = NULL, y = NULL,
       title = glue("FY{curr_fy}Q{curr_qtr} Global Achievement, USAID") %>% toupper,
       caption = glue("Target achievement capped at 110%
                        Source: {source}, US Agency for International Development")) +
  si_style_nolines() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.subtitle = element_markdown(),
        strip.text = element_markdown(),
        panel.spacing.y = unit(0, "lines"))

  si_save(glue("Graphics/FY{curr_fy}Q{curr_qtr}_achv_global.svg"))
  si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_achv_global.png"))

# VIZ - ACHIEVEMENT BY OU -------------------------------------------------------
df_achv_viz %>% 
    ggplot(aes(achievement, indicator, color = achv_color)) +
    geom_blank() + # creates blank canvas +
    geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
    geom_point(aes(x=baseline_pt_1), shape = 3, color = "#D3D3D3") +
    geom_point(aes(x=baseline_pt_2), shape = 3, color = "#D3D3D3") +
    geom_point(aes(x=baseline_pt_3), shape = 3, color = "#D3D3D3") +
    geom_point(aes(x=baseline_pt_4), shape = 3, color = "#D3D3D3") +
    geom_point(aes(x=baseline_pt_5), shape = 3, color = "#D3D3D3") +
    geom_jitter(position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
                alpha = .7, size = 4) + 
    geom_point(aes(global_achv), size = 12, alpha = 1, na.rm = TRUE, 
               position=position_nudge(y=0.3)) +
    geom_text(aes(global_achv, label = percent(global_achv, 1)), na.rm = TRUE,
              position=position_nudge(y=0.3),
              color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
    coord_cartesian(clip = "off") + # default decides how much to show - expands padding
    scale_x_continuous(limit=c(0,1.1),oob=scales::squish) + #capping achievement at 110
    scale_color_identity() + #whatever value is defined by color -- use that value from data frame
    facet_wrap(~ind_w_glob_vals, scales = "free_y", nrow=2) +
    labs(x = NULL, y = NULL,
         title = glue("FY{curr_fy}Q{curr_qtr} Operating Unit achievement, USAID ") %>% toupper,
         subtitle = glue("Global achievement (large, labeled points) with OU achievement reference points <br>"),
         caption = glue("Target achievement capped at 110%
                        Source: {source}, US Agency for International Development")) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.subtitle = element_markdown(),
          strip.text = element_markdown(),
          panel.spacing.y = unit(0, "lines"))

    si_save(glue("Graphics/FY{curr_fy}Q{curr_qtr}_achv_ou.svg"))
    si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_achv_ou.png"))


# VIZ - ACHIEVEMENT WORST BY OU -----------------------------------------

df_achv_viz %>% 
  filter((rank_worst<4 & achievement<0.9)|operatingunit == "GLOBAL") %>%
  ggplot(aes(achievement, indicator, color = achv_color)) +
  geom_blank() + # creates blank canvas +
  geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_1), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_2), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_3), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_4), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_5), shape = 3, color = "#D3D3D3") +
  geom_jitter(position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
              alpha = .7, size = 4) + 
  geom_point(aes(global_achv), size = 12, alpha = 1, na.rm = TRUE, 
             position=position_nudge(y=0.3)) +
  geom_text(aes(global_achv, label = percent(global_achv, 1)), na.rm = TRUE,
            position=position_nudge(y=0.3),
            color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
  geom_text_repel(aes(achievement, label=operatingunit), check_overlap=FALSE,
            color = "#202020", family = "Source Sans Pro", size = 10/.pt)+
  coord_cartesian(clip = "off") + # default decides how much to show - expands padding
  scale_x_continuous(limit=c(0,1.1),oob=scales::squish) + #capping achievement at 110
  scale_color_identity() + #whatever value is defined by color -- use that value from data frame
  facet_wrap(~ind_w_glob_vals, scales = "free_y", nrow=2) +
  labs(x = NULL, y = NULL,
       title = glue("FY{curr_fy}Q{curr_qtr} Operating Units with flagging progress, USAID") %>% toupper,
       subtitle = glue("Global achievement (large, labeled points) with up to three lowest ranked OUs <br>"),
       caption = glue("Target achievement capped at 110%
                        Source: {source}, US Agency for International Development")) +
  si_style_nolines() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.subtitle = element_markdown(),
        strip.text = element_markdown(),
        panel.spacing.y = unit(0, "lines"))

  si_save(glue("Graphics/FY{curr_fy}Q{curr_qtr}_achv_ou_lagging.svg"))
  si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_achv_ou_lagging.png"))

