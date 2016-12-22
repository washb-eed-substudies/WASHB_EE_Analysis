
#---------------------------------------
# EE-BD-blindedTR-creation.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Adapt the bangladesh blinded treatment script
# to only contain the arms in the EE substudy:
# WSH, Nutrition, WSH+N, and Control
#---------------------------------------


setwd("C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Untouched/")
d<-read.csv("washb-bangladesh-blind-tr.csv")
head(d)
d<-d[,-1]

table(d$tr)

d[d$tr=="Handwashing",3]<-"WSH"
d[d$tr=="Sanitation",3]<-"Nutrition + WSH"
d[d$tr=="Water",3]<-"Nutrition"

table(d$tr)
head(d)


treatment<-d
save(treatment, file="washb-BD-EE-blind-tr.Rdata")


library(foreign)
treatment$tr<-as.character(treatment$tr)
write.dta(treatment, "washb-BD-EE-blind-tr.dta")