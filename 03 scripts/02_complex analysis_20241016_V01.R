# Description --------------------------- --------------------------- ---------------------------
# Project: Analysis of baseline legacy data (i.e., 2023)
# Author: Dominik Bulla (dominik.bulla@gmail.com)
# Date: 2023-03-15
# Background: The client was a consortium of Germany’s biggest NGOs working with refugee/ IDP communities in Asia, Africa, and 
# Latin America. The consortium initiated a project to strengthen child protection among refugees/ IDPs as well as their host 
# communities. To guide programming, I was tasked to design a pre/ post evaluation design. As part of the evaluation design, a 
# baseline was commissioned as well. I designed the baseline framework, which included of a multi-stage sampling design. Data 
# was collected in country by local consultants. The data was collected using KOBO toolbox. The data was then pulled into R using 
# an API. I used the data collected in country to perform the global baseline analysis. Within the current excerpt from the global 
# baseline analysis, I determine the typical socio-demographic profile of primary caregivers that were interviewed across the 
# different levels. To do so, I used sampling weights.
# Purpose: Perform the complex survey analysis.



# Environment --------------------------- --------------------------- ---------------------------
setwd("C:/Users/domin/GitHub/second-sample-analysis")
MAR_ORIGINAL <- par("mar")
par(mar = c(5, 4, 1, 1))
rm(list = ls())
options(scipen = 999)



# Packages --------------------------- --------------------------- ---------------------------
library(dplyr)
library(survey)
library(gt)


# Load data --------------------------- --------------------------- ---------------------------
baseline <- read.csv("02 processed data/baseline_clean_20230323.csv")
project_population <- read.csv("02 processed data/in-country_project_population_data_20230323.csv")
baseline_sample <- read.csv("02 processed data/baseline_samples_20230323.csv")
weights <- read.csv("02 processed data/baseline_sampling_weights_20230323.csv")



# Create an overview table of the in-country project populations  (table 1) --------------------------- --------------------------- ---------------------------
project_population_table <- gt(project_population) %>%
  tab_options(
    heading.align = "left"
  ) %>%
  # load data
  fmt_number(columns = c(partner,
                         Hosts, IDPs, Refugees), decimals = 0) %>%
  tab_source_note(source_note = html("Source: project documentation © CONSORTIUM")) %>%
  # change the depiction of the column headers
  cols_label(
    partner = html("Implementing<br>partner"),
    Hosts = html("Host<br>communities"),
    IDPs = html("Internally<br>displaced people"), 
    Refugees = html("International<br>refugees")) %>%
  #format column headers
  cols_align(
    align = "center",
    columns = c(partner,
                Hosts, IDPs, Refugees)
  ) %>%
  # change font color to color of the client
  tab_style(
    style = cell_text(color = "blue", size = px(12)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "blue", size = px(10)),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "blue", size = px(12)),
    locations = cells_source_notes()
  ) %>%
  tab_style(
    style = cell_text(color = "blue", size = px(6)),
    locations = cells_source_notes()
  )  
gtsave(project_population_table, filename = "04 results/population_size.png")

data <- data.frame(
  group = c("Control", "Treatment"),
  description = c("Group 1 is the control\nIt has normal conditions", 
                  "Group 2 is the treatment\nIt has experimental conditions"),
  mean = c(3.5, 4.2),
  se = c(0.15, 0.20)
)

# determine the total number of project beneficiaries
project_population %>% 
  select(Hosts, IDPs, Refugees) %>%
  mutate(sumrow = rowSums(.))



# Create an overview table of the baseline samples (table 2) --------------------------- --------------------------- ---------------------------
baseline_sample_table <- gt(baseline_sample) %>%
  tab_options(
    heading.align = "left"
  ) %>%
  # load data
  fmt_number(columns = c(Partner,
                         Hosts, IDPs, Refugees), decimals = 0) %>%
  tab_source_note(source_note = html("Source: baseline 2022/23 © CONSORTIUM")) %>%
  # change the depiction of the column headers
  cols_label(
    Partner = html("Implementing<br>partner"),
    Hosts = html("Host<br>communities"),
    IDPs = html("Internally<br>displaced people"), 
    Refugees = html("International<br>refugees")) %>%
  #format column headers
  cols_align(
    align = "center",
    columns = c(Partner,
                Hosts, IDPs, Refugees)
  ) %>%
  # change font color to color of the client
  tab_style(
    style = cell_text(color = "blue", size = px(12)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "blue", size = px(10)),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "blue", size = px(12)),
    locations = cells_source_notes()
  ) %>%
  tab_style(
    style = cell_text(color = "blue", size = px(6)),
    locations = cells_source_notes()
  )  
gtsave(baseline_sample_table, filename = "04 results/baseline_samples.png")

# determine the total baseline sample size 
baseline_sample %>% 
  select(Hosts, IDPs, Refugees) %>%
  mutate(sumrow = rowSums(.))



# Create an overview table of the baseline sampling weights (table 3) --------------------------- --------------------------- ---------------------------
weights_table <- gt(weights) %>%
  tab_options(
    heading.align = "left"
  ) %>%
  # load data
  fmt_number(columns = c(Partner,
                         Hosts, IDPs, Refugees), decimals = 2) %>%
  tab_source_note(source_note = html("Source: baseline 2022/23 © CONSORTIUM")) %>%
  # change the depiction of the column headers
  cols_label(
    Partner = html("Implementing<br>partner"),
    Hosts = html("Host<br>communities"),
    IDPs = html("Internally<br>displaced people"), 
    Refugees = html("International<br>refugees")) %>%
  #format column headers
  cols_align(
    align = "center",
    columns = c(Partner,
                Hosts, IDPs, Refugees)
  ) %>%
  # change font color to color of the client
  tab_style(
    style = cell_text(color = "blue", size = px(12)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "blue", size = px(10)),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "blue", size = px(12)),
    locations = cells_source_notes()
  ) %>%
  tab_style(
    style = cell_text(color = "blue", size = px(6)),
    locations = cells_source_notes()
  )  
gtsave(weights_table, filename = "04 results/baseline_weights.png")



# The analysis of the caregiver socio-demographic data (table 4) --------------------------- --------------------------- ---------------------------
# Below are the steps that lead to table 4 in the main text. 
# The steps are as follows: 1) create summary stats that are partner specific; 2) create the global unweighted average;
# 3) create the global weighted average; 4) merge the summary tables and produce overview table
# Rseults f 2) and 3) highlight the effects of applying sampling weights. 
# Note: standard errors defined as standard deviation/ sqrt(length)

# Step 1: create summary stats that are partner specific
caregivers <- baseline %>%
  group_by(partner) %>%
  dplyr::summarise(Age = round(mean(age_cg, na.rm = TRUE), 2),
                   `Age (se)` = round(sd(age_cg, na.rm = TRUE)/ sqrt(length(age_cg)), 2),
                   `Female (in %)`  = round(mean(female_cg, na.rm = TRUE), 4) * 100,
                   `Female (se)`  = round(sd(female_cg, na.rm = TRUE)/ sqrt(length(female_cg)), 4) * 100,
                   `Single (in %)`  = round(mean(single_cg, na.rm = TRUE), 4) * 100,
                   `Single (se)`  = round(sd(single_cg, na.rm = TRUE)/ sqrt(length(single_cg)), 4) * 100,                   
                   `No education (in %)`  = round(mean(noschool_cg, na.rm = TRUE), 4) * 100,
                   `No education (se)`  = round(sd(noschool_cg, na.rm = TRUE)/ sqrt(length(noschool_cg)), 4) * 100,                   
                   `Working (in %)`  = round(mean(working_cg, na.rm = TRUE), 4) * 100,
                   `Working (se)`  = round(sd(working_cg, na.rm = TRUE)/ sqrt(length(working_cg)), 4) * 100,                   
                   `Disabled (in %)`  = round(mean(pwd_cg, na.rm = TRUE), 4) * 100,
                   `Disabled (se)`  = round(mean(pwd_cg, na.rm = TRUE)/ sqrt(length(pwd_cg)), 4) * 100,                   
                   `# of children` = round(mean(childrenMany_cg, na.rm = TRUE), 2),
                   `# of children (Se)` = round(sd(childrenMany_cg, na.rm = TRUE)/ sqrt(length(childrenMany_cg)), 2))



# Step 2: create the global unweighted average 
caregivers_global_unweighted <- baseline %>%
  dplyr::summarise(Age = round(mean(age_cg, na.rm = TRUE), 2),
                   `Age (se)` = round(sd(age_cg, na.rm = TRUE)/ sqrt(length(age_cg)), 2),
                   `Female (in %)`  = round(mean(female_cg, na.rm = TRUE), 4) * 100,
                   `Female (se)`  = round(sd(female_cg, na.rm = TRUE)/ sqrt(length(female_cg)), 4) * 100,
                   `Single (in %)`  = round(mean(single_cg, na.rm = TRUE), 4) * 100,
                   `Single (se)`  = round(sd(single_cg, na.rm = TRUE)/ sqrt(length(single_cg)), 4) * 100,                   
                   `No education (in %)`  = round(mean(noschool_cg, na.rm = TRUE), 4) * 100,
                   `No education (se)`  = round(sd(noschool_cg, na.rm = TRUE)/ sqrt(length(noschool_cg)), 4) * 100,                   
                   `Working (in %)`  = round(mean(working_cg, na.rm = TRUE), 4) * 100,
                   `Working (se)`  = round(sd(working_cg, na.rm = TRUE)/ sqrt(length(working_cg)), 4) * 100,                   
                   `Disabled (in %)`  = round(mean(pwd_cg, na.rm = TRUE), 4) * 100,
                   `Disabled (se)`  = round(mean(pwd_cg, na.rm = TRUE)/ sqrt(length(pwd_cg)), 4) * 100,                   
                   `# of children` = round(mean(childrenMany_cg, na.rm = TRUE), 2),
                   `# of children (Se)` = round(sd(childrenMany_cg, na.rm = TRUE)/ sqrt(length(childrenMany_cg)), 2)) %>%
  mutate(partner = "Global average (unweighted)")

# Step 3: create the global weighted average 
# Define the baseline dataset as a complex survey design. 
baseline_design <- svydesign(ids = ~1, weights = ~weights, data = baseline)

caregivers_global_weighted <- data.frame(partner = "Global average (weighted)",
                                Age = round(mean(svymean(~age_cg, baseline_design, na.rm = TRUE)), 2),
                                "Age (se)" = round((SE(svymean(~age_cg, baseline_design, na.rm = TRUE)))[1,1], 2),
                                `Female (in %)` = round(mean(svymean(~female_cg, baseline_design, na.rm = TRUE)), 4) * 100,
                                `Female (se)` = round((SE(svymean(~female_cg, baseline_design, na.rm = TRUE)))[1,1], 4) * 100,
                                `Single (in %)`  = round(mean(svymean(~single_cg, baseline_design, na.rm = TRUE)), 4) * 100,
                                `Single (se)`  = round((SE(svymean(~single_cg, baseline_design, na.rm = TRUE)))[1,1], 4) * 100,                   
                                `No education (in %)`  = round(mean(svymean(~noschool_cg, baseline_design, na.rm = TRUE)), 4) * 100,
                                `No education (se)`  = round((SE(svymean(~noschool_cg, baseline_design, na.rm = TRUE)))[1,1], 4) * 100,                   
                                `Working (in %)`  = round(mean(svymean(~working_cg, baseline_design, na.rm = TRUE)), 4) * 100,
                                `Working (se)`  = round((SE(svymean(~working_cg, baseline_design, na.rm = TRUE)))[1,1], 4) * 100,                   
                                `Disabled (in %)`  = round(mean(svymean(~pwd_cg, baseline_design, na.rm = TRUE)), 4) * 100,
                                `Disabled (se)`  = round((SE(svymean(~pwd_cg, baseline_design, na.rm = TRUE)))[1,1], 4) * 100,                   
                                `# of children` = round(mean(svymean(~childrenMany_cg, baseline_design, na.rm = TRUE)), 2),
                                `# of children (Se)` = round((SE(svymean(~childrenMany_cg, baseline_design, na.rm = TRUE)))[1,1], 2))
# Rename the table to make it match the naming of the previous two summary tables. 
colnames(caregivers_global_weighted) <- colnames(caregivers)

# Step 4: merge the summary tables and produce overview table
caregivers <- rbind(caregivers, caregivers_global_unweighted, caregivers_global_weighted)
rm(caregivers_global_unweighted, caregivers_global_weighted)

sample_size <- length(baseline$X)
caregivers_sociodemographics_table <- gt(caregivers) %>%
  tab_options(
    heading.align = "left"
  ) %>%
  # load data
  fmt_number(columns = c(partner,
                         Age, `Age (se)`, 
                         `Female (in %)`, `Female (se)`,
                         `Single (in %)`, `Single (se)`, 
                         `No education (in %)`, `No education (se)`, 
                         `Working (in %)`, `Working (se)`, 
                         `Disabled (in %)`, `Disabled (se)`,
                         `# of children`, `# of children (Se)`), decimals = 2) %>%
  tab_source_note(source_note = html("Note: 'se' refers to standard errors<br>
                                     Source: baseline 2022/23 © CONSORTIUM")) %>%
  # change the depiction of the column headers
  cols_label(
    partner = "Partner",
    Age = html("Age<br>  "),
    `Age (se)` = "se", 
    `Female (in %)` = html("Female<br>(in %)"), 
    `Female (se)` = "se",
    `Single (in %)` = html("Single<br>(in %)"), 
    `Single (se)` = "se", 
    `No education (in %)` = html("No edu.<br>(in %)"), 
    `No education (se)` = "se", 
    `Working (in %)` = html("Working<br>(in %)"), 
    `Working (se)` = "se", 
    `Disabled (in %)` = html("Disabled<br>(in %)"), 
    `Disabled (se)` = "se",
    `# of children` = html("# of<br>children"),
    `# of children (Se)` = "se") %>%
  #format column headers
  cols_align(
    align = "center",
    columns = c(Age, `Age (se)`, 
                `Female (in %)`, `Female (se)`,
                `Single (in %)`, `Single (se)`, 
                `No education (in %)`, `No education (se)`, 
                `Working (in %)`, `Working (se)`, 
                `Disabled (in %)`, `Disabled (se)`,
                `# of children`, `# of children (Se)`)
  ) %>%
  # change font color to color of the client
  tab_style(
    style = cell_text(color = "blue"),
    locations = cells_title(groups = c("title", "subtitle"))
  ) %>%
  tab_style(
    style = cell_text(color = "blue"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "blue"),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(color = "blue"),
    locations = cells_source_notes()
  ) %>%
  tab_style(
    style = cell_text(color = "blue"),
    locations = cells_source_notes())
gtsave(caregivers_sociodemographics_table, filename = "04 results/caregivers_sociodemographics.png")
rm(n)


