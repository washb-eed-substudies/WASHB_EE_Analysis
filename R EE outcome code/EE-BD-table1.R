
#---------------------------------------
# EE-BD-table1.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The tabulate enrollment variables for 
# manuscript table 1
#---------------------------------------

###Load in data
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
enrol<-read.csv("washb-bangladesh-enrol.csv",stringsAsFactors = TRUE)

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
  subset(!is.na(consent1) | !is.na(consent2) | !is.na(consent3)) %>%
  select(dataid, clusterid) %>%
  distinct(dataid, clusterid)


#urineform
#stoolform

childid<-union(urine, stool, medhistory)


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


#Transpose
table1<-table1 %>%
  gather(var, val, 2:ncol(table1)) %>%
  spread(tr, val)
print(table1, n=nrow(table1))


#summarize SDs (continious vars) or %s (categorical vars)
