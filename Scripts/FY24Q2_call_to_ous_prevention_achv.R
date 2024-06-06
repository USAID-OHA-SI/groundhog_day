# PROJECT:  groundhogday
# AUTHOR:   A.Chafetz, N.Petrovic, K.Srikanth | USAID
# PURPOSE:  FY24Q2 prevention indicators target achv
# REF ID:   3adb8fa1 
# LICENSE:  MIT
# DATE:  2024-05-29
# NOTE:     Adapted from rebootTZ FY21Q3 partner revide; Updated for FY23Q4 


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(systemfonts)
library(glitr)
library(glamr)
library(gophr)
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

ref_id <- "3adb8fa1" #id for adorning to plots, making it easier to find on GH

merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
                           pattern = "OU_IM_FY22")

#select indicators
ind_sel <- c("HTS_TST","HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS", 
             "PrEP_NEW", "VMMC_CIRC", "OVC_SERV", "KP_PREV", "GEND_GBV", "PMTCT_EID", "TB_PREV")

semi_annual <- c("OVC_SERV", "KP_PREV", "GEND_GBV", "TB_PREV")


#caption info for plotting
metadata<- get_metadata(file_path, caption_note = "US Agency for International Development")

#current FY and quarter
curr_fy <- metadata$curr_fy
curr_qtr <- metadata$curr_qtr

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


# IMPORT ------------------------------------------------------------------

df <- read_psd(file_path)   


# MUNGE -------------------------------------------------------------------

#subset to key indicators

df_achv <- df %>% 
  clean_indicator() %>%
  #rowwise() %>% 
  #mutate(TX_IIT= sum(TX_ML_IIT_less_three_mo, TX_ML_IIT_more_three_mo, na.rm = T)) %>% 
  #ungroup() %>%
  filter(funding_agency == "USAID",
         !operatingunit %in% c("Ukraine"), #exclude TZA and NGA globally for FY23Q4 because of DQA issues 
         fiscal_year == curr_fy,
         indicator %in% ind_sel)

#remove known issues

df_achv <- resolve_knownissues(df_achv)

# MUNGE - GLOBAL/OU ACHIEVEMENT ---------------------------------------------

## Aggregating results & targets at the global level for each indicator
df_achv <- df_achv %>% 
  bind_rows(df_achv %>% 
              mutate(country = "GLOBAL")) %>% 
  filter(standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  group_by(fiscal_year, country, indicator) %>% 
  summarize(across(c(targets, cumulative), \(x) sum(x, na.rm = TRUE)), 
            .groups = "drop")

#remove data points at Q1 for semi-annual indicators
if(metadata$curr_qtr == 1){
  df_achv <- df_achv %>%
    dplyr::mutate(cumulative = ifelse(indicator %in% semi_annual, 
                                      NA_real_, cumulative))
}

#calculate achievement and add colors 
df_achv <- df_achv %>% 
  adorn_achievement(metadata$curr_qtr)

#viz adjustments
df_achv_viz <- df_achv %>% 
  mutate(global_achv = case_when(country == "GLOBAL" ~ achievement),
         achievement = ifelse(country == "GLOBAL", NA, achievement),
         #indicator = factor(indicator, ind_sel),
         baseline_pt_1 = 0,
         baseline_pt_2 = .25,
         baseline_pt_3 = .5,
         baseline_pt_4 = .75,
         baseline_pt_5 = 1,
  )

#adjust facet label to include indicator and national values
# df_achv_viz <- df_achv_viz %>% 
#   mutate(ind_w_glob_vals = case_when(country == "GLOBAL" & is.na(targets) ~ 
#                                        glue("**{indicator}**<br><span style = 'font-size:11pt;'>No MER reporting</span>"),
#                                      country == "GLOBAL" ~ 
#                                        glue("**{indicator}**<br><span style = 'font-size:11pt;'>{clean_number(cumulative)} / 
#                                             {clean_number(targets)}</span>")),
#           country = case_when(country == "Western Hemisphere Region" ~ "WHR",
#                                     country == "West Africa Region" ~ "WAR", 
#                                     country == "Democratic Republic of the Congo" ~ "DRC", TRUE ~ country)) %>% 
#   group_by(indicator) %>% 
#   mutate(rank_worst=rank(achievement, ties.method="min")) %>%
#   fill(ind_w_glob_vals, .direction = "downup") %>% 
#   ungroup() %>% 
#   arrange(indicator) %>% 
#   mutate(ind_w_glob_vals = fct_inorder(ind_w_glob_vals))

df_achv_viz <- df_achv_viz %>% 
  mutate(indicator_ss = ifelse(indicator %in% snapshot_ind, paste(indicator, "(SS)"), indicator),
         ind_w_glob_vals = case_when(country == "GLOBAL" & is.na(targets) ~ glue("**{indicator_ss}**<br><span style = 'font-size:11pt;'>No MER reporting</span>"),
                                     country == "GLOBAL" ~ glue("**{indicator_ss}**<br><span style = 'font-size:11pt;'>{clean_number(cumulative)} / {clean_number(targets)}</span>")),
         indicator = factor(indicator, levels = ind_sel)) %>% 
  group_by(indicator) %>% 
  fill(ind_w_glob_vals, .direction = "downup") %>% 
  ungroup() %>% 
  arrange(indicator) %>% 
  mutate(ind_w_glob_vals = fct_inorder(ind_w_glob_vals)) 



# VIZ - ACHIEVEMENT GLOBAL -------------------------------------------------------


#@TO-DO: need to find a way to make this dynamic between quarters
lab_q4<-c("<75%","75-89%","90-110%","+110%")

lab_leg<-case_when(metadata$curr_qtr==1 ~  c("<15%","15-35%",">35%"),
                   metadata$curr_qtr==2 ~  c("<25%","25-40%","40-60%",">60%"),
                   metadata$curr_qtr==3 ~  c("<50%","50-65%","65-85%",">85%"),
                   TRUE ~ c("<75%","75-89%","90-110%","+110%"))%>%
  ## NOTE: Will need to add Q2, Q3, Q4 late,
  # metadata_msd$curr_qtr==3 ~  c("<75%","75-89%","90-110%","+110%"),
  # metadata_msd$curr_qtr==4 ~  c("<75%","75-89%","90-110%","+110%")) %>%
  paste("| SS:",lab_q4)

df_achv_viz %>% 
  filter(country == "GLOBAL") %>% 
  ggplot(aes(achievement, indicator, color = achv_color)) +
  geom_blank() + # creates blank canvas +
  geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_1), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_2), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_3), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_4), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_5), shape = 3, color = "#D3D3D3") +
  geom_point(aes(global_achv), size = 12, alpha = 1, na.rm = TRUE, 
             position=position_nudge(y=0)) +
  geom_text(aes(global_achv, label = percent(global_achv, 1)), na.rm = TRUE,
            position=position_nudge(y=0),
            color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
  coord_cartesian(clip = "off") + # default decides how much to show - expands padding
  scale_x_continuous(limit=c(0,1.1),oob=scales::squish) + #capping achievement at 110
  scale_color_identity(guide=guide_legend(direction = "horizontal", title.position = "top",
                                          title.hjust = 0), breaks=c("#ff939a","#ffcaa2","#5BB5D5","#e6e6e6"),
                       labels=lab_leg,
                       name="Achievement: Cumulative indicators | Snapshot indicators") + #whatever value is defined by color -- use that value from data frame
  facet_wrap(~ind_w_glob_vals, nrow=2, scales = "free_y") +
  labs(x = NULL, y = NULL,
       title = glue("{metadata$curr_pd} Global Achievement, USAID") %>% toupper,
       caption = glue("Target achievement capped at 110%
                        Source: {metadata$source} | USAID/OHA/SIEI | Ref ID: {ref_id}")) +
  si_style_nolines() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.subtitle = element_markdown(),
    strip.text = element_markdown(),
    panel.spacing.y = unit(0, "lines"),
    legend.position="bottom")

si_save(glue("Graphics/{metadata$curr_pd}_achv_global.svg"))
si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_achv_global.png"))

# VIZ - ACHIEVEMENT BY OU -------------------------------------------------------
df_achv_viz %>% 
  mutate(achv_color = case_when(achv_color == "#e6e6e6" ~ "#697ebc",
                                achv_color == "#ffcaa2" ~ "#fbdc99",
                                achv_color == "#ff939a" ~ "#f8a27e",
                                TRUE ~ achv_color)) %>% 
  ggplot(aes(achievement, indicator, color = achv_color)) +
  geom_blank() + # creates blank canvas +
  geom_linerange(aes(xmin = 0, xmax = 1.1, y = 1), color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_1), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_2), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_3), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_4), shape = 3, color = "#D3D3D3") +
  geom_point(aes(x=baseline_pt_5), shape = 3, color = "#D3D3D3") +
  geom_jitter(position = position_jitter(width = 0, height = 0.1), na.rm = TRUE,
              alpha = .7, size = 3) + 
  geom_point(aes(global_achv), size = 10, alpha = 1, na.rm = TRUE, 
             position=position_nudge(y=0.3)) +
  geom_text(aes(global_achv, label = percent(global_achv, 1)), na.rm = TRUE,
            position=position_nudge(y=0.3),
            color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
  coord_cartesian(clip = "off") + # default decides how much to show - expands padding
  scale_x_continuous(limit=c(0,1.1),oob=scales::squish, breaks = seq(0, 1.25, .25), label = percent_format(1)) + #capping achievement at 110
  scale_color_identity(guide=guide_legend(direction = "horizontal", title.position = "top",
                                          title.hjust = 0), breaks=c("#f8a27e", "#FBDC99","#5BB5D5","#697EBC"),
                       labels=lab_leg,
                       name="Achievement: Cumulative indicators | Snapshot indicators") +    facet_wrap(~ind_w_glob_vals, scales = "free_y", nrow=2) +
  labs(x = NULL, y = NULL,
       title = glue("{metadata$curr_pd} Operating Unit achievement, USAID ") %>% toupper,
       subtitle = glue("Global achievement (large, labeled points) with OU achievement reference points <br>"),
       caption = glue("Target achievement capped at 110%
                        Source: {metadata$source} | USAID/OHA/SIEI | Ref ID: {ref_id}")) +
  si_style_nolines() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.subtitle = element_markdown(),
    strip.text = element_markdown(),
    panel.spacing.y = unit(0, "lines"),
    legend.position="bottom")

si_save(glue("Graphics/{metadata$curr_pd}_achv_ou.svg"))
si_save(glue("Images/FY{curr_fy}Q{curr_qtr}_achv_ou.png"))
