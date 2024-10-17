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



# Import data --------------------------- --------------------------- ---------------------------
