
#---------------------------------------
# EE-BD-telo-table1.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The tabulate enrollment variables for 
# telomere manuscript table 1
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)
library(tidyr)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-BD-telo-blind-tr.Rdata")
levels(treatment$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","Nutrition + WSH"))
levels(treatment$tr)

#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)

#Load in telomere datasets to track all children 
#who participated in the telomere substudy
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv")

#Merge treatment information 
dim(telo)
d<-left_join(telo,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)


#Merge in enrollment information
#dim(d)
#dim(enrol)
#d<-left_join(d,enrol, by="dataid")
#dim(d)

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 

#Generate table 1

colnames(d)
#vlist <- c("momage","momedu_prim","patedu","agwork","Nlt18","electricity","cementfloor","ironroof","improvedwater","dminwat","treatwat","odmen","odwom","odch38","odchu3","latown","impr_lat","humfeces","wat_avail" , "soap_avail","HHSmod_sev")

vlist <- c("momage","momeduy","dadeduy","dadagri","Nhh","elec","cement","landacre","tubewell","storewat","treatwat","watmin","odmen","odwom","odch815","odch38","odchu3",
           "latown","latslab","latseal","latfeces","potty","humfeces","humfecesch", "hwlatwat","hwlatsoap","hwkitwat","hwkitsoap","hfiacat")

#[1] "n.1"   

table(vlist %in% colnames(d))

table.dat<-subset(d, select=c("tr", vlist)) %>% subset(tr=="Control" | tr=="Nutrition + WSH")

#Calculate number of compounds

table1_mu<-table.dat%>%
        group_by(tr) %>%
        summarise_each(funs(mean(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

table1_N<-table.dat%>%
        group_by(tr) %>%
        summarise_each(funs(sum(!is.na(.)))) %>%
        ungroup %>% as.data.frame

table1_sd<-table.dat%>%
        group_by(tr) %>%
        summarise_each(funs(sd(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

Ns<-prop.table(table(table.dat$tr))
balance.tab.mu_M<-rbind(Ns,t((table1_mu[,2:ncol(table1_mu)])))
balance.tab.n_M<-rbind(Ns,t((table1_N[,2:ncol(table1_N)])))
balance.tab.sd_M<-rbind(Ns,t((table1_sd[,2:ncol(table1_sd)])))


#save objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(balance.tab.mu_M, balance.tab.n_M, balance.tab.sd_M, 
     file="telo_table1.Rdata")

