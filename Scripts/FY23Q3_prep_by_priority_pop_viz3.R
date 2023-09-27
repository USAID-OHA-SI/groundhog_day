# AUTHOR:   B. Betz | USAID
# PURPOSE:  PrEP uptake
# REF ID:   b9553d92 
# LICENSE:  MIT
# DATE:     2023-06-16
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)


# GLOBAL VARIABLES --------------------------------------------------------

# SI specific paths/functions  
load_secrets()

ref_id <- "3"

# IMPORT ------------------------------------------------------------------

filepath <- si_path() %>% return_latest("OU_IM_FY21")

# Grab metadata
get_metadata(filepath) 

df_msd <- read_msd(filepath)



# THEMES ------------------------------------------------------------------
theme_clean_axis_legends <-  theme(axis.title = element_blank(),
                                   legend.position = "none",
                                   legend.title = element_blank())


# Munge -------------------------------------------------------------------


ayp <- c("15-19", "20-24", "25-29")
ind <- c("PrEP_NEW", "PrEP_CT")

prep_trends_pre <- df_msd |> 
  filter(indicator %in% c("PrEP_NEW", "PrEP_CT"),
         funding_agency == "USAID",
        !fiscal_year %in% c(2021, 2024)) |> 
  reshape_msd() |> 
  #standard KP data mods
  mutate(         partner = prime_partner_name,
                  disagg = str_extract(standardizeddisaggregate, "KeyPop|Total|Age|Preg"),
                  disagg = recode(disagg, "KeyPop" = "KP",
                                  "Preg" = "PregnantOrBreastfeeding",
                                  "Age" = "Age/Sex"),
                  keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
                  keypop = recode(keypop, "People in prisons" = "Prisoners"),
                  kp_prev_status = case_when(standardizeddisaggregate == "KeyPop/Status" ~ 
                                               str_extract(categoryoptioncomboname, 
                                                           "(?<=\\,\\s).+$"))) |> 
  #special modifications for this analysis
  mutate(pop = case_when(
    !is.na(keypop) ~ keypop,
    disagg == "PregnantOrBreastfeeding" ~ otherdisaggregate,
    sex == "Female" & ageasentered %in% ayp ~ "AGYW",
    sex == "Male" & ageasentered %in% ayp ~ "ABYM",
    standardizeddisaggregate == "Total Numerator" ~ "Total")
    ) |> 
  filter(
    #exclude Uganda KP data since they stopped reporting KP disaggs due to AHA
    !(country=="Uganda" & !is.na(keypop)),
    # !is.na(pop),
    period != "FY23Q4",
    period_type == "results"
  ) |> 
  group_by(indicator, pop, period) |> 
  summarise(results=sum(value), .groups = "drop") |>
  mutate(fy = str_extract(period, "^FY.{2}"),
         qtr = str_extract(period, "Q.{1}$"),
         ) |> glimpse()

prep_pop_trends <- prep_trends_pre |> filter(pop!="Total")

prep_trends <- prep_trends_pre |> filter(pop=="Total") |>  rename(total = results) |> select(-pop) |> 
  right_join(prep_pop_trends, multiple = "all", ) |> 
  mutate(pop = fct_relevel(pop,
                           "AGYW", "FSW", "MSM", "ABYM"),
         population = recode(pop,
                             "AGYW" = "Adolescent girls & young women (15-29)",
                             "FSW"  = "Female sex workers",
                             "MSM"  = "Men who have sex with men",
                             "ABYM" = "Adolescent boys & young men (15-29)",
                             "TG" = "Transgender",
                             "PWID" = "People who inject drugs"
         ),
         indicator1 = "PrEP initiations",
         indicator2 = "PrEP continuations") |>
  #label value for max and series start/end
  group_by(pop, indicator) |> 
  mutate(percent_of_total_series_ends = case_when(
    fy == min(fy) & qtr == "Q1" ~ scales::percent(results/total, accuracy = 1),
    fy == max(fy) & qtr == "Q3" ~ scales::percent(results/total, accuracy = 1),
    # results/total == max(results/total) ~ scales::percent(results/total, accuracy = 1),
  )) |> 
  ungroup() |> arrange(indicator, pop, period) |> glimpse()

rm(prep_trends_pre, prep_pop_trends)


# PrEP_NEW trends - by large priority pop -------------------------------------
akp <- c("#33adbb", "#005e7a", "#f28965", "#b85534")
agyw_last2_color <- c(grey60k, "#33adbb", grey30k)

prep_trends_smallx <- prep_trends |> filter(indicator == ind[1],
                                            !pop %in% c("Pregnant", "Breastfeeding", "TG", "PWID", "Prisoners")) |> 
  mutate(agyw_last2 = case_when(pop=="AGYW" &period %in% c("FY23Q2", "FY23Q3") ~ "focus",
                                pop!="AGYW" & period %in% c("FY23Q2", "FY23Q3") ~ "attention",
                                .default = "norm")) |> group_by(pop, agyw_last2) |> 
  ggplot(aes(x=period)) +
  # geom_bar(aes(y=total), alpha = 0.4, fill = "grey", stat = "summary", fun = "mean") +
  geom_bar(aes(y=results, fill = agyw_last2), stat = "identity",  position = "dodge2") +
  facet_wrap(~population, nrow = 1, labeller = label_wrap_gen(width = 20)) +
  geom_text(aes(y=results+5000, label=percent_of_total_series_ends,  color = agyw_last2), position = position_dodge2(width = 1), fontface="bold", size=4) +
  # geom_text(aes(y=results+20000, color = pop, label=percent_of_total,   position = position_dodge2(width = 1)) +
  scale_fill_manual(values=agyw_last2_color) +
  scale_color_manual(values=agyw_last2_color) +
  scale_y_continuous(labels = comma, breaks = c(25000, 50000, 75000, 100000, 125000, 150000),  limits = c(0, 160000), expand = c(0, 0)) +
  # scale_color_discrete(akp) +
  si_style_ygrid() +
  theme(
    #format axes
    axis.title = element_blank(),
    axis.text.x.bottom = element_text(angle=90, vjust = 0.3),
    # panel.grid.minor.y = element_line(color = grey10k),
    panel.grid.major.y = element_line(color = grey10k),
    #format facet titles
    strip.text.x.top = element_text(hjust = 0.5, vjust = 0, face = "bold", size = 16),
    #format legends
    legend.position = "none",
    legend.title = element_blank(),
    #format plot title
    plot.title = element_markdown(face="bold", size = 36),
    plot.subtitle = element_markdown(size=14),
    plot.caption = element_markdown(size=11, margin = margin(t=20))
    
  ) +
  labs(
    # title  = HTML(str_glue("PrEP initiations continue to increase, <span style='color: #33adbb;'>especially for AGYW")),
    subtitle  = "USAID global PrEP_NEW trends and percent of total by population",
    caption = str_glue("Data source: {file_name},  \nexcluding Uganda where KP disaggregate reporting was paused  \nNote: Populations may overlap (e.g. a 23yo FSW would also be in AGYW but a 34yo MSM would not be in ABYM)"),
    color = "Population") 

prep_trends_smallx

ggsave(plot=prep_trends_smallx, filename = paste0("Images/OHA Q3/prep_new_trends_smallx.png"), width = 11.5, height = 7)



# PrEP_NEW trends - small priority populations --------------------------------
last2_color <- c(grey60k, grey30k)

prep_trends_smallerx <- prep_trends |> filter(indicator == ind[1],
                                              pop %in% c("TG", "PWID")) |> 
  mutate(last2 = case_when(pop!="AGYW" & period %in% c("FY23Q2", "FY23Q3") ~ "attention",
                           .default = "norm")) |> 
  ggplot(aes(x=period)) +
  geom_bar(aes(y=results, fill=last2), stat = "identity",  position = "dodge2") +
  scale_fill_manual(values=last2_color) +
  facet_wrap(~pop, nrow = 1, labeller = label_wrap_gen(width = 20)) +
  scale_y_continuous(labels = comma, 
                     n.breaks = 3,
                     minor_breaks = waiver(), 
                     limits = c(0, 5200), 
                     expand = c(0, 0)) +
  si_style_ygrid() +
  theme(
    #format axes
    axis.title = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(size=6),
    # element_text(angle=90, vjust = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = grey10k),
    #format facet titles
    strip.text.x.top = element_text(hjust = 0.5, vjust = 0, face = "bold", size = 7),
    #format legends
    legend.position = "none",
    legend.title = element_blank(),
    #format plot title
    plot.title = element_markdown(face="bold", size = 36),
    plot.subtitle = element_markdown(size=14),
    plot.caption = element_markdown(size=11, margin = margin(t=20))) 

prep_trends_smallerx

ggsave(plot=prep_trends_smallerx, filename = paste0("Images/OHA Q3/prep_new_trends_smaller_pops.png"), width = 2, height = 1.5)




# Prep_CT trends by large priority pop ----------------------------------------
fsw_last2_color <- c(grey60k, "#005e7a", grey30k)


prep_ct_trends_smallx <- prep_trends |> filter(indicator == ind[2],
                                               !pop %in% c("Pregnant", "Breastfeeding", "TG", "PWID", "Prisoners")) |> 
  mutate(agyw_last2 = case_when(pop=="FSW" & period %in% c("FY23Q2", "FY23Q3") ~ "focus",
                                pop!="FSW" & period %in% c("FY23Q2", "FY23Q3") ~ "attention",
                                .default = "norm")) |> 
  ggplot(aes(x=period)) +
  geom_bar(aes(y=results, fill = agyw_last2), stat = "identity",  position = "dodge2") +
  facet_wrap(~population, nrow = 1, labeller = label_wrap_gen(width=20)) +
  geom_text(aes(y=results+5000, label=percent_of_total_series_ends,  color = agyw_last2), position = position_dodge2(width = 1), fontface="bold", size=4) +
  # geom_text(aes(y=results+20000, color = pop, label=percent_of_total,   position = position_dodge2(width = 1)) +
  scale_fill_manual(values=fsw_last2_color) +
  scale_color_manual(values=fsw_last2_color) +
  scale_y_continuous(labels = comma, breaks = c(25000, 50000, 75000, 100000, 125000), limits = c(0, 160000), expand = c(0, 0)) +
  # scale_color_discrete(akp) +
  si_style_ygrid() +
  theme(
    #format axes
    axis.title = element_blank(),
    axis.text.x.bottom = element_text(angle=90, vjust = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = grey10k),
    #format facet titles
    strip.text.x.top = element_text(hjust = 0.5, vjust=0, face = "bold", size = 16),
    #format legends
    legend.position = "none",
    legend.title = element_blank(),
    #format plot title
    plot.title = element_markdown(face="bold", size = 36),
    plot.subtitle = element_markdown(size=14),
    plot.caption = element_markdown(size=11, margin = margin(t=20))
    
  ) +
  labs(
    # title  = HTML(str_glue("PrEP initiations continue to increase, <span style='color: #33adbb;'>especially for AGYW")),
    subtitle  = "USAID global PrEP_CT trends and percent of total by population",
    caption = str_glue("Data source: {file_name},  \nexcluding Uganda where KP disaggregate reporting was paused  \nNote: Populations may overlap (e.g. a 23yo FSW would also be in AGYW but a 34yo MSM would not be in ABYM)"),
    color = "Population") 


prep_ct_trends_smallx

ggsave(plot=prep_ct_trends_smallx, filename = paste0("Images/OHA Q3/prep_ct_trends_smallx.png"), width = 11.5, height = 7)
