library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
library(tidytext)
library(gt)
library(glue)
library(webshot)

df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  gophr::read_msd()
  


#use this function to print out budget executionfor just USAID in all OUs. 
#You should load the source files below before running

source("~/GitHub/stacks-of-hondos/Scripts/ea_style.R")
source("~/GitHub/stacks-of-hondos/Scripts/prep_fsd.R")
source("~/GitHub/stacks-of-hondos/Scripts/utilities.R")
table_out<-"GitHub/stacks-of-hondos/Images/Global Performance"

#munge data to get data frame for gt====
df_fsd<-df_fsd%>%
  prep_fsd()%>%
  filter(fundingagency=="USAID")%>%
  #dplyr::filter(fiscal_year=="2020"| fiscal_year=="2021")%>%
  group_by(operatingunit,fiscal_year)%>%
  #mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
  summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
  dplyr::mutate(budget_execution=expenditure_amt/cop_budget_total)%>%
  ungroup()%>%
  pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
  dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
  dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
  dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
  dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
  arrange(desc(budget_execution_2021))

df_top14<-df_fsd%>%
  slice_head(n = 14)%>%
  ea_style()%>%
  cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
    operatingunit = "Operating Unit")%>%
  
  tab_header(
    title = (" COP19 & COP20 Program Financial Summary: USAID "),
    subtitle = legend_chunk)%>%
  gtsave(., path=table_out, filename="global performance_usaid_top14.png")

df_b14<-df_fsd%>%
  slice_tail(n=14)%>%

    ea_style()%>%
    cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
      operatingunit = "Operating Unit")%>%
    
    tab_header(
      title = (" COP19 & COP20 Program Financial Summary: USAID "),
      subtitle = legend_chunk)%>%
  gtsave(., path=table_out, filename="global performance_usaid_bottom14.png")
  

  


#futZING WITH A SPARKLINE YOU CAN IGNORE
gt_sparkline_tab <- df_fsd %>%
  dplyr::group_by(operatingunit) %>%
  # must end up with list of data for each row in the input dataframe
  dplyr::summarize(er_data = list(cop_budget_total), .groups = "drop") %>%
  gt() %>%
  gt_sparkline(er_data)