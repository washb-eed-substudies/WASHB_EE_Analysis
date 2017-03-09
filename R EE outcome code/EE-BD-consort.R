

#---------------------------------------
# EE-BD-consort.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Tabulating Consort numbers for the EE 
# substudy of the WASH Benefits Bangladesh
# trial.
#---------------------------------------

###Load in data
rm(list=ls())
library(foreign)
library(dplyr)
library(washb)
options(dplyr.print_max = 1e9)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
consort<-read.csv("BD-EE-consort.csv")

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-BD-EE-blind-tr.Rdata")
levels(treatment$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
levels(treatment$tr)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
stool<-read.csv("BD-EE-stool.csv",stringsAsFactors = TRUE)
urine<-read.csv("BD-EE-urine.csv",stringsAsFactors = TRUE)



#Merge in treatment data
#dim(consort)
#table(is.na(consort$clusterid))
#d<-left_join(consort,treatment, by="clusterid")
d<-consort
dim(d)
head(d)
table(d$tr)

#Tabulate number of compounds
d %>% group_by(tr) %>%
  distinct(dataid) %>%
  summarize(n.compound=n())

#-----------------------------
#Subsample target
#-----------------------------

# Compounds

d %>% group_by(tr, svy) %>%
  distinct(clusterid) %>%
  summarize(n.clusters=n())

# Index children (any child in the enrollment record)
d %>% group_by(tr, svy) %>%
  #distinct(dataid, childno) %>%
  summarize(n.index=n())
  

#-----------------------------
# Follow-up
#-----------------------------

#Arrange missingness reason
table(d$miss1reason_ee)
d$miss1reason_ee<-factor(d$miss1reason_ee, levels=c("Moved away","Absent","Withdrew","No live birth", "Child death", "Not lost"))

#Drop out non-missing
NotLost<-subset(d, miss1reason_ee=="Not lost")
followup<-subset(d, miss1reason_ee!="Not lost")


#Subset by round
followup.m3<-subset(followup, svy==1)
followup1<-subset(followup, svy==2)
followup2<-subset(followup, svy==3)

head(followup.m3)
miss.m3<-followup.m3 %>% group_by(tr, miss1reason_ee)  %>%
      summarize(N=n())
miss1<-followup1 %>% group_by(tr, miss1reason_ee)  %>%
      summarize(N=n())
miss2<-followup2 %>% group_by(tr, miss1reason_ee)  %>%
      summarize(N=n())

print.data.frame(miss.m3)
print.data.frame(miss1)
print.data.frame(miss2)


# Number new children added for each round

#Subset by round
dm3<-subset(d, svy==1)
d1<-subset(d, svy==2)
d2<-subset(d, svy==3)


anti_join(d1, dm3, by=c("dataid", "childno")) %>%
  group_by(tr) %>%
  summarize(new.children1=n())


anti_join(d2, d1, by=c("dataid", "childno")) %>%
  group_by(tr) %>%
  summarize(new.children2=n())

#-----------------------------
#Subsample enrollment
#-----------------------------

#clusters
NotLost %>% group_by(svy, tr) %>% distinct(clusterid) %>%summarize(N=n())

#Children
NotLost %>% group_by(svy, tr) %>% distinct(dataid, childno) %>%summarize(N=n())






stool.m3<-stool %>% 
  subset(aliqout1_t1>1)
stool1<-stool %>% 
  subset(aliqout1_t2>1)
stool2<-stool %>% 
  subset(aliqout1_t3>1)

urine.m3<-urine %>% 
  subset(h2aliqout1_t1>1)
urine1<-urine %>% 
  subset(h2aliqout1_t2>1)
urine2<-urine %>% 
  subset(h2aliqout1_t3>1)


# Compounds

  full_join(stool.m3, urine.m3, by=c("dataid", "childNo", "clusterid")) %>%
  left_join(.,treatment, by="clusterid") %>%
  group_by(tr) %>% distinct(clusterid) %>%
  summarize(compounds.m3.N=n())

  full_join(stool1, urine1, by=c("dataid", "childNo", "clusterid")) %>%
  left_join(.,treatment, by="clusterid") %>%
  group_by(tr) %>% distinct(clusterid) %>%
  summarize(compounds1.N=n())
  
  full_join(stool2, urine2, by=c("dataid", "childNo", "clusterid")) %>%
  left_join(.,treatment, by="clusterid") %>%
  group_by(tr) %>% distinct(clusterid) %>%
  summarize(compounds2.N=n())  
  

# Index children (any child with stool or urine)


  full_join(stool.m3, urine.m3, by=c("dataid", "childNo", "clusterid")) %>%
  left_join(.,treatment, by="clusterid") %>%
  group_by(tr) %>%
  summarize(index.m3.N=n())

  full_join(stool1, urine1, by=c("dataid", "childNo", "clusterid")) %>%
  left_join(.,treatment, by="clusterid") %>%
  group_by(tr) %>%
  summarize(index1.N=n())
  
  full_join(stool2, urine2, by=c("dataid", "childNo", "clusterid")) %>%
  left_join(.,treatment, by="clusterid") %>%
  group_by(tr) %>%
  summarize(index2.N=n())  
  