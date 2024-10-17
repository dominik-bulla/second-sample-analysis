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



# 10.7) Indicator 2: Table: % of caregivers who report increased knowledge of caring and --------------------------- --------------------------- ---------------------------
# protection behaviours towards children under their care compared to the beginning of the project.
# the indicator covers 2 aspects:
# 1) knowledge of caring behaviours [CGKC1 through CGKC12]: create a dummy on whether or caregiver has full score
# code Absolutely untrue/Mostly untrue/Can’t say true or untrue as 0; code Mostly true/Absolutely true as 1. 
# 2) knowledge of protection behaviours [CGKP1]: create a dummy that indicates whether or not a CM was aware of all CPR
# create indicator that sums up 1) and 2) and calculate percentage. then table results

indicator2 <- surveyhh %>%
  group_by(country, IP) %>%
  dplyr::summarise(Indicator2 = round(mean(indicator02S_0560, na.rm = TRUE),4)*100) %>%
  mutate(Sample = "all",
         location = paste0(paste0(country, "/"), IP))
ggplot(indicator2) +
  geom_col(aes(
    x = location,
    y = Indicator2
  ),
  fill =  unhcr_pal(n = 12, "pal_blue"),
  position = position_dodge(width = 0.7),
  width = 0.6
  ) +
  geom_text(aes(
    x = location,
    y = Indicator2,
    label = Indicator2
  ),
  position = position_dodge(width = 0.7),
  vjust = -1,
  size = 8 / .pt
  ) +
  labs(
    #    title = "Baseline values for indicator 2 | all partner countries",
    #    subtitle = "Results from the baseline surveys (caregivers only) | Proportion of respondents (in %)",
    caption = "Source: Baseline 2022/23 \n© JF-CPiE"
  ) +
  scale_x_discrete() +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  theme_unhcr(
    grid = FALSE,
    axis = "x",
    axis_title = FALSE,
    axis_text = "x"
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(axis.text.x=element_text(color = "black", size=11, angle=90, vjust=.8, hjust=0.8))

ggsave("Indicator2 (global).png",
       plot = last_plot(),
       device = png(),
       path = "06 Data/0604 Graphs/",
       scale = 1,
       dpi = 100,
       limitsize = TRUE,
       bg = 'white')
dev.off()

indicator2 <- surveyhh %>%
  group_by(country, IP) %>%
  dplyr::summarise(Indicator2 = round(mean(indicator02S_0560, na.rm = TRUE),4)*100) %>%
  mutate(Sample = "all")
indicator2F <- surveyhh[surveyhh$gender_CG == 1,] %>%
  group_by(country, IP) %>%
  dplyr::summarise(Indicator2 = round(mean(indicator02S_0560, na.rm = TRUE),4)*100) %>%
  mutate(Sample = "bfemale") %>%
  filter(!is.na(Indicator2))
indicator2M <- surveyhh[surveyhh$gender_CG == 0,] %>%
  group_by(country, IP) %>%
  dplyr::summarise(Indicator2 = round(mean(indicator02S_0560, na.rm = TRUE),4)*100) %>%
  mutate(Sample = "cmale") %>%
  filter(!is.na(Indicator2))
indicator2D <- surveyhh[surveyhh$disability_CG == 1,] %>%
  group_by(country, IP) %>%
  dplyr::summarise(Indicator2 = round(mean(indicator02S_0560, na.rm = TRUE),4)*100) %>%
  mutate(Sample = "disabled") %>%
  filter(!is.na(Indicator2))
indicator2ND <- surveyhh[surveyhh$disability_CG == 0,] %>%
  group_by(country, IP) %>%
  dplyr::summarise(Indicator2 = round(mean(indicator02S_0560, na.rm = TRUE),4)*100) %>%
  mutate(Sample = "not disabled") %>%
  filter(!is.na(Indicator2))
indicator2HC <- surveyhh[surveyCM$type_hh == "Host community HH",] %>%
  group_by(country, IP) %>%
  dplyr::summarise(Indicator2 = round(mean(indicator02S_0560, na.rm = TRUE),4)*100) %>%
  mutate(Sample = "xHost community") %>%
  filter(!is.na(Indicator2))
indicator2IDP <- surveyhh[surveyCM$type_hh == "Internally displaced HH",] %>%
  group_by(country, IP) %>%
  dplyr::summarise(Indicator2 = round(mean(indicator02S_0560, na.rm = TRUE),4)*100) %>%
  mutate(Sample = "yIDP") %>%
  filter(!is.na(Indicator2))
indicator2REF <- surveyhh[surveyCM$type_hh == "Refugee HH",] %>%
  group_by(country, IP) %>%
  dplyr::summarise(Indicator2 = round(mean(indicator02S_0560, na.rm = TRUE),4)*100) %>%
  mutate(Sample = "zRefugees") %>%
  filter(!is.na(Indicator2))
indicator2 <- rbind(indicator2, 
                    indicator2F, indicator2M, 
                    indicator2D, indicator2ND,
                    indicator2HC, indicator2IDP, indicator2REF)
indicator2 <- indicator2 %>% 
  filter(!is.na(country)) %>%
  arrange(country, IP, Sample) 
indicator2 <- indicator2 %>%
  mutate(Sample = ifelse(Sample == "bfemale", "female", Sample),
         Sample = ifelse(Sample == "cmale", "male", Sample),
         Sample = ifelse(Sample == "xHost community", "host community", Sample),
         Sample = ifelse(Sample == "yIDP", "IDP", Sample),
         Sample = ifelse(Sample == "zRefugees", "refugees", Sample))
indicator2 <- as.data.frame(indicator2)
rm(indicator2F, indicator2M, indicator2D, indicator2ND)
indicator2$Sample <- factor((indicator2$Sample), levels = c("all", "female", "male", "disabled", "not disabled", "host community", "IDP", "refugees"))

locs <- unique(indicator2$country)
for (loc in locs) {
  print(ggplot(indicator2[indicator2$country == loc,]) +
          geom_col(aes(
            x = IP,
            y = Indicator2,
            fill = Sample
          ),
          position = position_dodge(width = 0.7),
          width = 0.6
          ) +
          geom_text(aes(
            x = IP,
            y = Indicator2,
            group = Sample,
            label = Indicator2
          ),
          position = position_dodge(width = 0.7),
          vjust = -1,
          size = 8 / .pt
          ) +
          scale_fill_unhcr_d(
            palette = "pal_unhcr",
            nmax = length(unique(indicator2$Sample)),
            order = c(1 : length(unique(indicator2$Sample)))
          ) +
          labs(
            #      title = paste0("Baseline values for indicator 2 | ", paste0(loc, " only")),
            #      subtitle = "Results from the household survey (caregivers only) | Proportion of respondents (in %)",
            caption = "Source: Baseline 2022/23 \n© JF-CPiE"
          ) +
          scale_x_discrete() +
          scale_y_continuous(expand = expansion(c(0, 0.1))) +
          theme_unhcr(
            grid = FALSE,
            axis = "x",
            axis_title = FALSE,
            axis_text = "x"
          ) +
          guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
          theme(axis.text.x=element_text(color = "black", size=11, angle=90, vjust=.8, hjust=0.8))
  )
  
  ggsave(paste0(paste0("10.7_indicator2_",loc),".png"),
         plot = last_plot(),
         device = png(),
         path = "06 Data/0604 Graphs/",
         scale = 1,
         dpi = 100,
         limitsize = TRUE,
         bg = 'white')
  dev.off()
}

indicator2 <- indicator2 %>%
  select(country, IP, Sample, Indicator2) %>%
  rename(Country = "country",
         Partner = "IP",
         Population = "Sample",
         `Baseline value` = Indicator2) 
indicator2 <- as.data.frame(indicator2)
write.csv(indicator2, "06 Data/0605 Tables/10.7 indicator2 breakdown_20230330.csv", row.names = FALSE)



