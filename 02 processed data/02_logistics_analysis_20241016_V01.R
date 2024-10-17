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



# The analysis of the caregiver socio-demographic data --------------------------- --------------------------- ---------------------------

caregivers <- surveyhh %>%
  group_by(country, IP) %>%
  dplyr::summarise(Age = round(mean(cg_age, na.rm = TRUE),2),
                   `Age (SD)` = round(sd(cg_age, na.rm = TRUE),2),
                   `Female (in %)`  = round(mean(cg_gender, na.rm = TRUE),4) * 100,
                   `Married (in %)`  = round(mean(cg_maritalstatus, na.rm = TRUE),4) * 100,
                   `No education (in %)`  = round(mean(cg_noschool, na.rm = TRUE),4) * 100,
                   `Working (in %)`  = round(mean(cg_working, na.rm = TRUE),4) * 100,
                   `Disabled (in %)`  = round(mean(cg_disability, na.rm = TRUE),4) * 100,                   
                   `# of children` = round(mean(cg_children, na.rm = TRUE),2),
                   `# of children (Sd)` = round(sd(cg_children, na.rm = TRUE),2))
write.csv(caregivers, "06 Data/0605 Tables/09 stats caregivers_20230329.csv", row.names = FALSE)


cg_age
cg_gender
cg_maritalstatus
cg_noschool
cg_working
cg_disability
cg_children
