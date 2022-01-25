##  PROJECT: Q2 review target analysis
##  AUTHOR:  tessam | USAID
##  PURPOSE: Tiny bar sparks for summary graphic. 
##  LICENCE: MIT
##  DATE:    2020-06-15
##  UPDATE:

# LIBRARIES ----------------------------------------------------------

  library(tidyverse)
  library(vroom)
  library(scales)
  library(extrafont)
  library(ggrepel)
  library(glitr)
  library(ICPIutilities)
  library(here)
  library(glamr)
library(gt)
table_out<-"Images"

legend_snapshot <- 'https://github.com/USAID-OHA-SI/selfdestructin5/blob/main/man/figures/snapshot_legend.png?raw=true'
legend_chunk <- gt::md(glue::glue("Legend: <img src= '{legend_snapshot}' style='height:15px;'>"))
# data -----------------------------------------------------------------
df_l<-tibble::tribble(
  ~Indicator, ~`COP20 Target`, ~`COP20 Shadow Target`, ~`APR21 Result`,~Achievement,
  "HTS_TST",     179742,   147240,195486,1.3280,
  "HTS_TST_POS",     17129,   17669,13121,.743,
  "TX_CURR",     313028,   245327,232182,.946,
  "TX_NEW",    16497,   16785,14486,.878,
  "NET_NEW",     NA,   12913,-121,NA,
  "TX_PVLS",     292909,   231990,203660,.878,
  "PrEP_NEW",     20895,   NA,22695,1.086,
  "VMMC_CIRC",     20426,   NA,11345,.555,
)

df_l%>%gt()%>%
  gt::fmt_missing(
    columns = tidyselect::everything(),
    missing_text = "-"
  ) %>% 
  gt::fmt_percent(
    columns = tidyselect::matches("Achievement"), 
    decimal = 0
  ) %>% 
  tab_footnote(
    footnote = "Midway through COP21 planning the Chair and PPM agreed to set more meaningful COP20 & COP21 unofficial/"shadow" targets in light of PHIA results and the downward revision of Spectrum PLHIV estimates.",
    locations = cells_column_labels(
      columns =c("COP20 Shadow Target")))%>%
  gt::fmt_number(
    columns = c("COP20 Target", "COP20 Shadow Target","APR21 Result"),
    decimal = 0)%>%
  opt_row_striping()%>%
  gt::tab_source_note(
    source_note = gt::md(glue::glue("**Source**: FY21Q4c MSD")))%>%tab_header(
      title = ("PEPFAR Lesotho Performance Snapshot"),
      subtitle = legend_chunk)%>%
 
  
  tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
            locations = cells_body(               
              columns = (Achievement),
              rows = (Achievement) >= 0.9 & (Achievement) <= 1.1)) %>%
  tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
            locations = cells_body(               
              columns = (Achievement),
              rows =(Achievement) < 0.9 ))%>%
  tab_style(style = cell_fill(color = "#e6e6e6",alpha = .75),      
            locations = cells_body(               
              columns = (Achievement),
              rows = (Achievement)> 1.1))%>%
  tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
            locations = cells_body(               
              columns = (Achievement),
              rows = (Achievement) < .75 ))%>% 
  gt::tab_options(
                source_notes.font.size = 8,
                table.font.size = 13, 
                data_row.padding = gt::px(5),
                source_notes.padding = gt::px(1),) %>%
  cols_align(
    align = "center",
    columns = everything())%>%
  cols_align(
    align = "left",
    columns = "Indicator")%>%
 
  cols_width(
    everything() ~ px(90))%>%
  gtsave(., path=table_out, filename="pepfar_lesotho_agencies.png")


  
