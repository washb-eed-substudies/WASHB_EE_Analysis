
#---------------------------------------
# EE-BD-age.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The ages of children with urine and stool
# samples in the the EED substudy of the WASH
# Benefits Bangladesh trial
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-BD-EE-blind-tr.Rdata")
levels(treatment$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
levels(treatment$tr)
#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
urine<-read.csv("BD-EE-urine.csv",stringsAsFactors = TRUE)
stool<-read.csv("BD-EE-stool.csv",stringsAsFactors = TRUE)

#Temp join in fake outcomes
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
outcomesStool<-read.dta("washb-BD-EE-sim-stool-outcomes-stata12.dta")
outcomesUrine<-read.dta("washb-BD-EE-sim-urine-outcomes-stata12.dta")

outcomesStool$childid<-as.numeric(outcomesStool$childid)
outcomesUrine$childid<-as.numeric(outcomesUrine$childid)


urine<-left_join(urine,outcomesUrine, by="childid")
stool<-left_join(stool,outcomesStool, by="childid")

stool<-subset(stool, select=dataid, childNo, aat1, aat2, aat3, mpo1, mpo2, mpo3, neo1, neo2, neo3, reg1b2)

#Merge urine and stool
dim(urine)
dim(stool)
d<-merge(urine, stool, by=c("dataid", "childNo"), all.x=T, all.y=T)
dim(d)

#Merge treatment information 
dim(d)
d<-left_join(d,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)