

#---------------------------------------
# EE-BD-stool.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The stool-based biomarker outcomes for 
# EED Kenya sub-study
#---------------------------------------


rm(list=ls())
library(tidyverse)
library(foreign)
library(washb)
library(lubridate)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")

#Child dates of birth
dob <- readRDS("WBK-EE-childDOB.rds")

#Baseline covariates
enrol <- readRDS("WBK-EE-covariates.rds")
head(enrol)

write.csv(miss_child, file= "C:/Users/andre/Downloads/miss_child.csv") 

miss_child <- read.csv("C:/Users/andre/Downloads/miss_child.csv")

table(enrol$hhid %in% miss_child$hhid)
table(miss_child$hhid %in% enrol$hhid)
