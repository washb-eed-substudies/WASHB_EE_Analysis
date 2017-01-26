
#---------------------------------------
# EE-BD-telo.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The analysis script for the WASH Benefits
# Telomere substudy
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
enrol<-read.csv("washb-bangladesh-enrol.csv",stringsAsFactors = TRUE)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv")


#Merge treatment information 
dim(telo)
d<-left_join(telo,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)


#Merge in enrollment information
dim(d)
dim(enrol)
d<-left_join(d,enrol, by="dataid")
dim(d)

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 



#table number of fully collected aliqouts by arm and year
head(d)

t1_N<-d%>% subset(aliquot1=="Full Aliquot"|aliquot1=="Partial Aliquot")%>%group_by(tr) %>%summarize(sample1=n()) 
t2_N<-d%>% subset(aliquot2=="Full Aliquot"|aliquot2=="Partial Aliquot")%>%group_by(tr) %>%summarize(sample1=n()) 
t3_N<-d%>% subset(aliquot3=="Full Aliquot"|aliquot3=="Partial Aliquot")%>%group_by(tr) %>%summarize(sample1=n()) 

cbind(t1_N[c(1,3,4,2),],t2_N[c(1,3,4,2),],t3_N[c(1,3,4,2),])


