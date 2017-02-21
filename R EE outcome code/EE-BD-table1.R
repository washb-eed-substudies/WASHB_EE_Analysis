
#---------------------------------------
# EE-BD-table1.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The tabulate enrollment variables for 
# manuscript table 1
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)
library(tidyr)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-BD-EE-blind-tr.Rdata")
levels(treatment$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
levels(treatment$tr)

#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)

#Load in urine, stool, and medhistory datasets to track all children 
#who participated in the eed substudy
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
urine<-read.csv("BD-EE-urine.csv", stringsAsFactors = TRUE)
stool<-read.csv("BD-EE-stool.csv", stringsAsFactors = TRUE)
medhistory<-read.csv("BD-EE-medhistory.csv", stringsAsFactors = TRUE)

#Select non-duplicate childids:
urine<-urine %>%
  mutate(urine_data1=!is.na(h2aliqout1_t1) & h2aliqout1_t1>1 | !is.na(h5aliqout7_t1) & h5aliqout7_t1>1 | !is.na(preLMaliqout13_t1) & preLMaliqout13_t1>1) %>%
  mutate(urine_data2=!is.na(h2aliqout1_t2) & h2aliqout1_t2>1 | !is.na(h5aliqout7_t2) & h5aliqout7_t2>1 | !is.na(preLMaliqout13_t2) & preLMaliqout13_t2>1) %>%
  mutate(urine_data3=!is.na(h2aliqout1_t3) & h2aliqout1_t3>1 | !is.na(h5aliqout7_t3) & h5aliqout7_t3>1 | !is.na(preLMaliqout13_t3) & preLMaliqout13_t3>1) %>%
  mutate(urine_data= urine_data1 | urine_data2 | urine_data3) %>%
  subset(urine_data==T) %>%
  select(dataid, clusterid) %>%
  distinct(dataid, clusterid)

stool<-stool %>% 
  mutate(stool_data=!is.na(aliqout1_t1) & aliqout1_t1>1 | !is.na(aliqout1_t2) & aliqout1_t2>1 | !is.na(aliqout1_t3) & aliqout1_t3>1) %>%
  subset(stool_data==T) %>%
  select(dataid, clusterid) %>%
  distinct(dataid, clusterid)

medhistory<-medhistory %>%
  #subset(!is.na(consent1) | !is.na(consent2) | !is.na(consent3)) %>%
  subset((consent1==1) | (consent2==1) | (consent3==1)) %>%
  select(dataid, clusterid) %>%
  distinct(dataid, clusterid)


#urineform
#stoolform

childid<-union(urine, stool)
childid<-union(childid, medhistory)


#Merge treatment information 
dim(childid)
d<-left_join(childid,treatment, by="clusterid")
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

#Generate table 1

colnames(d)
vlist <- c("momage","momedu_prim","patedu","agwork","Nlt18","electricity","cementfloor","ironroof","improvedwater","dminwat","treatwat","odmen","odwom","odch38","odchu3","latown","impr_lat","humfeces","wat_avail" , "soap_avail","HHSmod_sev")


#Calculate number of compounds

#summarize means (continious vars) or Ns (categorical vars)
colnames(d)
table1<-d %>%
        group_by(tr) %>%
        summarize(
          momage=mean(momage, na.rm=T), 
          momedu=mean(momeduy, na.rm=T),
          dadeduy=mean(dadeduy, na.rm=T),
          dadagri=sum(dadagri, na.rm=T),  #cat
          Nhh=mean(Nhh, na.rm=T),
          elec=sum(elec, na.rm=T),
          cement=sum(cement, na.rm=T),
          landacre=mean(landacre, na.rm=T),
          tubewell=sum(tubewell, na.rm=T),
          storewat=sum(storewat, na.rm=T),
          treatwat=sum(treatwat, na.rm=T),
          watmin=mean(watmin, na.rm=T),
          odmen=sum(odmen, na.rm=T),
          odwom=sum(odwom, na.rm=T),
          odch815=sum(odch815, na.rm=T),
          odch38=sum(odch38, na.rm=T),
          odchu3=sum(odchu3, na.rm=T),
          latown=sum(latown, na.rm=T),
          latslab=sum(latslab, na.rm=T),
          latseal=sum(latseal, na.rm=T),
          latfeces=sum(latfeces, na.rm=T),
          potty=sum(potty, na.rm=T),
          humfeces=sum(humfeces, na.rm=T),
          humfecesch=sum(humfecesch, na.rm=T),
          hwlatwat=sum(hwlatwat, na.rm=T),
          hwlatsoap=sum(hwlatsoap, na.rm=T),
          hwkitwat=sum(hwkitwat, na.rm=T),
          hwkitsoap=sum(hwkitsoap, na.rm=T),
          hfiacat=sum(hfiacat>1, na.rm=T) #Tabulate food insecure==2, 3, or 4
          )

table1_sd<-d %>%
        group_by(tr) %>%
        summarize(
          momage=sd(momage, na.rm=T), 
          momedu=sd(momeduy, na.rm=T),
          dadeduy=sd(dadeduy, na.rm=T),
          dadagri=mean(dadagri, na.rm=T)*100,  #cat
          Nhh=sd(Nhh, na.rm=T),
          elec=mean(elec, na.rm=T)*100,
          cement=mean(cement, na.rm=T)*100,
          landacre=sd(landacre, na.rm=T),
          tubewell=mean(tubewell, na.rm=T)*100,
          storewat=mean(storewat, na.rm=T)*100,
          treatwat=mean(treatwat, na.rm=T)*100,
          watmin=sd(watmin, na.rm=T),
          odmen=mean(odmen, na.rm=T)*100,
          odwom=mean(odwom, na.rm=T)*100,
          odch815=mean(odch815, na.rm=T)*100,
          odch38=mean(odch38, na.rm=T)*100,
          odchu3=mean(odchu3, na.rm=T)*100,
          latown=mean(latown, na.rm=T)*100,
          latslab=mean(latslab, na.rm=T)*100,
          latseal=mean(latseal, na.rm=T)*100,
          latfeces=mean(latfeces, na.rm=T)*100,
          potty=mean(potty, na.rm=T)*100,
          humfeces=mean(humfeces, na.rm=T)*100,
          humfecesch=mean(humfecesch, na.rm=T)*100,
          hwlatwat=mean(hwlatwat, na.rm=T)*100,
          hwlatsoap=mean(hwlatsoap, na.rm=T)*100,
          hwkitwat=mean(hwkitwat, na.rm=T)*100,
          hwkitsoap=mean(hwkitsoap, na.rm=T)*100,
          hfiacat=mean(hfiacat>1, na.rm=T)*100 #Tabulate food insecure==2, 3, or 4
          )



#Transpose
table1<-table1 %>%
  gather(var, val, 2:ncol(table1)) %>%
  spread(tr, val)
print(table1, n=nrow(table1))

table1_sd<-table1_sd %>%
  gather(var, val, 2:ncol(table1_sd)) %>%
  spread(tr, val)
print(table1_sd, n=nrow(table1_sd))



