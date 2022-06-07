# PROJECT:  groundhog_day
# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  TX_CURR NAT_SUBNAT unmet need
# LICENSE:  MIT
# DATE:     2022-06-02
# UPDATED:  
# NOTE:     

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


# GLOBAL VARIABLES --------------------------------------------------------

authors <- c("Aaron Chafetz", "Tim Essam", "Karishma Srikanth")

curr_fy <-source_info(return = "fiscal_year")
curr_pd <- source_info(return = "period")
msd_source <- glue(source_info(), " + NAT+SUBNAT")

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("NAT_SUBNAT_FY15-22") %>% 
  read_msd()

six_plus_two <- c("Eswatini",
                  "Botswana",
                  "Uganda",
                  "Namibia",
                  "Kenya",
                  "Lesotho",
                  "Vietnam",
                  "Rwanda")

df_nat <- df %>%
  filter(indicator %in% c("TX_CURR_SUBNAT", "PLHIV"),
         fiscal_year == max(fiscal_year),
         standardizeddisaggregate == "Age/Sex/HIVStatus"
         ) %>%
  #count(indicator, standardizeddisaggregate)
group_by(fiscal_year, operatingunit, indicator) %>% 
  summarise(across(targets, sum, na.rm = TRUE)) %>% 
  pivot_wider(names_from = indicator, values_from = targets) %>% 
  mutate(gap = TX_CURR_SUBNAT/PLHIV)


nudge_space <- 0.125

df_nat %>% 
  filter(operatingunit %in% six_plus_two) %>% 
  ggplot(aes(x = fiscal_year, group = fiscal_year)) +
  geom_col(aes(y = PLHIV), fill = grey10k, width = 0.3, position = position_nudge(x = -nudge_space)) +
  geom_col(aes(y = TX_CURR_SUBNAT, fill = genoa), width = 0.3, position = position_nudge(x = nudge_space)) +
  #facet_wrap(~ operatingunit, nrow = 2, scales = "free_y") +
  facet_wrap(~fct_reorder2(operatingunit, fiscal_year, gap, .desc = TRUE), nrow = 2, scales =  "free_y") +
  scale_x_discrete() +
  si_style() + 
  geom_text(aes(y = TX_CURR_SUBNAT, label = percent(gap, 1)), hjust = -0.2,
            vjust = -.50, family = "Source Sans Pro", color = trolley_grey) +
  scale_y_continuous(labels = label_number_si(), position = "left") +
  scale_fill_identity() +
  scale_color_identity() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "TX_CURR_SUBNAT COVERAGE OF PLHIV FOR THE 8 SUSTAINING HIV IMPACT COUNTRIES",
       caption = glue("Source: {msd_source}
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"))

si_save(glue("Graphics/{curr_pd}_TX_PLHIV_gap.svg"))
