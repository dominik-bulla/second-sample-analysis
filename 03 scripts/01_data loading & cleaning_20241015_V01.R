# Description --------------------------- --------------------------- ---------------------------
# Project: Analysis of baseline legacy data (i.e., 2023)
# Author: Dominik Bulla (dominik.bulla@gmail.com)
# Date: 2023-03-15
# Background: The client was a consortium of Germany’s biggest NGOs working with refugee/ IDP communities in Asia, Africa, and 
# Latin America. The consortium initiated a project to strengthen child protection amongst refugees/ IDPs as well as their host 
# communities. To guide programming, I was tasked to design a pre/ post evaluation design. As part of the evaluation design, a 
# baseline was commissioned as well. I designed the baseline framework, which included of a multi-stage sampling design. Data 
# was collected in country by local consultants. The data was collected using KOBO toolbox. The data was then pulled into R using 
# an API. I used the data collected in country to perform the global baseline analysis. Within the current excerpt from the global 
# baseline analysis, I determine the typical socio-demographic profile of primary caregivers that were interviewed across the 
# different levels. To do so, I used sampling weights.
# Purpose: Clean up and make the data set ready for complex survey analysis



# Environment --------------------------- --------------------------- ---------------------------
setwd("C:/Users/domin/GitHub/second-sample-analysis")
MAR_ORIGINAL <- par("mar")
par(mar = c(5, 4, 1, 1))
rm(list = ls())
options(scipen = 999)



# Packages --------------------------- --------------------------- ---------------------------
library(readxl)
library(tidyverse)
library(robotoolbox)



# Import baseline data --------------------------- --------------------------- ---------------------------
# Import the data from kobo toolbox. The credentials are listed below. 

kobo_setup(url = "https://kf.kobotoolbox.org/",
           token = kobo_token(username = "domib",
                              password = "!23KeZA2023",
                              url = "https://kf.kobotoolbox.org"))
assets <- kobo_asset_list()
uid <- assets %>%
  filter(assets$name == "JF-CPiE - The household survey_20221124_V05") %>%
  pull(uid) %>%
  first()
asset <- kobo_asset(uid)
HH <- kobo_submissions(kobo_asset(uid))
baseline <- HH$main
grid <- HH$g_hhmember
rm(asset, assets, HH, uid)


# Clean up baseline main data --------------------------- --------------------------- ---------------------------
# The baseline survey consists of altogether three sections: the sections on 1) household heads; 2) caregivers; 3) young people.
# Here, we only focus on the section on caregivers.
# Other sections will be removed. 

# Rename variables
baseline <- baseline %>%
  rename(index = "_index",
         organisation = GE0,
         country = GE1,
         unit = GE4,
         subgroup = GE7,
         date = GE8,
         hhsize = HHH6,
         hhchildren = HHH7,
         consent_hhh = HHH4,
         consent_cg = CG3.11,
         consent_ad = AD4) 

# Select relevant variables
baseline <- baseline %>%
  select(index, country,
         organisation, unit, subgroup, 
         date, 
         hhsize,
         hhchildren,
         consent_hhh, consent_cg, consent_ad) 

# Clean up names of organisations
baseline <- baseline %>%
  mutate(across(starts_with("consent_"), ~ ifelse(. == "yes", 1, 0))) %>%
  mutate(organisation = ifelse(organisation == "PlanInternational", "Plan International", organisation)) %>%
  mutate(organisation = ifelse(organisation == "SavetheChildren", "Save the Children", organisation)) %>%
  mutate(organisation = ifelse(organisation == "TerresdesHommes", "Terre des Hommes", organisation)) %>%
  mutate(organisation = ifelse(organisation == "WorldVision", "World Vision", organisation)) 

# Clean up country names
baseline <- baseline %>%
  mutate(country = ifelse(country == "RepubliqueCentrafricaine", "CAR", country)) %>%
  mutate(country = ifelse(country == "SouthSudan", "South Sudan", country)) %>%
  mutate(country = ifelse(country == "Burkina", "Burkina Faso", country)) %>%
  mutate(country = ifelse(country == "Bangladesh" & organisation == "ChildFund", "Burkina Faso", country)) %>%
  mutate(country = ifelse(country == "South Sudan" & organisation == "ChildFund", "Ethiopia", country)) 

# create variable on implementing partners.  
baseline <- baseline %>%
  mutate(partner = paste(country, organisation))

# Clean up population sub-groups 
baseline <- baseline %>%
  mutate(subgroup = ifelse(subgroup == "Refugee_HH", "Refugees", subgroup)) %>%
  mutate(subgroup = ifelse(subgroup == "Internally_displaced_HH", "IDPs", subgroup)) %>%  
  mutate(subgroup = ifelse(subgroup == "Host_community_HH", "Hosts", subgroup)) 

# Some sub-groups were wrongly classified (compare prop-baseline with prop_pop tables below)
baseline <- baseline %>%
  mutate(subgroup = ifelse(partner == "CAR Plan International", "IDPs", subgroup)) %>%
  mutate(subgroup = ifelse(partner == "CAR Plan International", "IDPs", subgroup))

# Remove those entries for whom no research consents were obtained 
baseline <- baseline %>%
  filter(consent_hhh == 1 & consent_cg == 1 & consent_ad == 1) %>%
  select(-c(consent_hhh, consent_cg, consent_ad)) 

# Sort data by partner
baseline <- baseline %>%
  arrange(partner)


# Clean up baseline grid --------------------------- --------------------------- ---------------------------
# The household survey consists of a survey grid to gauge basic socio-demographic data on the different household members
# In this part, the grid data will be cleaned up

# Rename variables
grid <- grid %>%
  rename(index = "_parent_index",
         index2 = "_index",
         name = HHH8.1,
         relationship = HHH8.2,
         age = HHH8.3,	
         gender = HHH8.4,	
         maritalstatus = HHH8.5,		
         children = HHH8.6,	
         childrenMany = HHH8.6.1,	
         enrollment = HHH8.7,		
         edu_level = HHH8.9,		
         working = HHH8.10,			
         dis_seeing = HHH8.121,	
         dis_hearing = HHH8.122,	
         dis_walking = HHH8.123,	
         dis_concentrating = HHH8.124,	
         dis_selfcare = HHH8.125,	
         dis_communicating = HHH8.126) 

# clean up data on disability status and create dummy on poeple with disabilities (pwd) 
grid <- grid %>%
  mutate(across(starts_with("dis_"), ~ ifelse(. == "No_–_no_difficulty", 0, 1))) %>%
  mutate(pwd = do.call(pmax, across(starts_with("dis_"))))

# Clean up some other socio-demographic variables 
grid <- grid %>%
  mutate(noschool = ifelse(edu_level == "Never_attended_school", 1, 0))  %>%
  mutate(working = ifelse(working == "yesd", 1, 0)) %>%
  mutate(gender = ifelse(gender == "Female", 1, 0)) %>%
  rename("female" = gender) %>%
  mutate(gender = ifelse(gender == "Female", 1, 0)) %>%

  
baseline$maritalstatus_cg
  
# Select relevant variables
grid <- grid %>%
  select(index, index2,
         name, relationship, age, female, maritalstatus,		
         children,	childrenMany,	
         enrollment, edu_level,	noschool,	
         working,			
         pwd) 

baseline$maritalstatus_cg

# Merge baseline grid and baseline main survey --------------------------- --------------------------- ---------------------------
# We need to merge the baseline grid with the baseline main survey. 
# The survey grid was set up in a way that the first family member with children below 18 years of age was the caregiver 
# to be surveyed

caregivers <- grid %>%
  filter(children == "yesd") %>%
  group_by(index) %>%
  summarise(index2 = min(index2)) %>%
  mutate(caregiver = 1) %>%
  select(-c(index))

grid <- merge(grid, caregivers, by = "index2", all.x = TRUE) %>%
  filter(caregiver == 1) %>%
  select(-c(caregiver, index2, name)) 

colnames(grid)[colnames(grid) != "index"] <- paste0(colnames(grid)[colnames(grid) != "index"], "_cg")    

baseline <- merge(baseline, grid, by = "index") 
rm(grid, caregivers)



# Determine sampling weights --------------------------- --------------------------- ---------------------------
# Create population overview table on percentage across partners and sub-groups. 
prop_pop <- read_excel("01 raw data/Beneficiary table_20221006.xlsx") %>%
  rename("organisation" = Partner) %>%
  mutate(organisation = ifelse(organisation == "Plan", "Plan International", organisation)) %>%
  mutate(organisation = ifelse(organisation == "Terres des Hommes", "Terre des Hommes", organisation)) %>%
  mutate(partner = paste(Country, organisation)) %>%
  select(partner, Hosts, IDPs, Refugees) %>%
  arrange(partner) 

# Turn overview table into percentage (sum arcoss columns and rows is 100%)
prop_pop[2:4] <- round(prop_pop[2:4] / sum(prop_pop[2:4]), 4)

# Create baseline overview table on percentage across partners and sub-groups. 
# Turn overview table into percentage (sum arcoss columns and rows is 100%)
prop_baseline <- round(prop.table(table(baseline$partner, baseline$subgroup)), 4)

# Create matrix with weights 
weights <- prop_pop[,2:4]/ prop_baseline
row.names(weights) <- row.names(prop_baseline)

# Incorporate sampling weights into the baseline survey
baseline$weights <- 0
for(partner in rownames(weights)) {
  for(group in colnames(weights)) {
    value <- weights[rownames(weights) == partner, colnames(weights) == group]
    baseline$weights[baseline$partner == partner & baseline$subgroup == group] <- value
  }
}
rm(group, partner, value, prop_baseline, prop_pop, weights)



# Save dataset --------------------------- --------------------------- ---------------------------
write.csv(baseline, "02 processed data/baseline_clean_20230323.csv")





