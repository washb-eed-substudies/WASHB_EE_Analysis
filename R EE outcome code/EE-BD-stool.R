
#---------------------------------------
# EE-BD-stool.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The stool-based biomarker outcomes for 
# EED Bangladesh sub-study
#---------------------------------------

###Load in data
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
stool<-read.csv("BD-EE-stool.csv")


#Merge treatment information 
dim(stool)
d<-left_join(stool,treatment, by="clusterid")
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

#aliqout time 1
t1s1<-d%>% subset(aliqout1_t1>1)%>%group_by(tr) %>%summarize(sample1=n()) 
t1s2<-d%>% subset(aliqout2_t1>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t1s3<-d%>% subset(aliqout3_t1>1)%>%group_by(tr) %>%summarize(sample3=n()) 
t1s4<-d%>% subset(aliqout4_t1>1)%>%group_by(tr) %>%summarize(sample4=n()) 
t1s5<-d%>% subset(aliqout5_t1>1)%>%group_by(tr) %>%summarize(sample5=n()) 
 
aliquotN_t1<-cbind(t1s1,t1s2[,2],t1s3[,2],t1s4[,2],t1s5[,2])


#aliqout time 2
t2s1<-d%>% subset(aliqout1_t2>1)%>%group_by(tr) %>%summarize(sample1=n()) 
t2s2<-d%>% subset(aliqout2_t2>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t2s3<-d%>% subset(aliqout3_t2>1)%>%group_by(tr) %>%summarize(sample3=n()) 
t2s4<-d%>% subset(aliqout4_t2>1)%>%group_by(tr) %>%summarize(sample4=n()) 
t2s5<-d%>% subset(aliqout5_t2>1)%>%group_by(tr) %>%summarize(sample5=n()) 
 
aliquotN_t2<-cbind(t2s1,t2s2[,2],t2s3[,2],t2s4[,2],t2s5[,2])
aliquotN_t2

#aliqout time 3
t3s1<-d%>% subset(aliqout1_t3>1)%>%group_by(tr) %>%summarize(sample1=n()) 
t3s2<-d%>% subset(aliqout2_t3>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t3s3<-d%>% subset(aliqout3_t3>1)%>%group_by(tr) %>%summarize(sample3=n()) 
t3s4<-d%>% subset(aliqout4_t3>1)%>%group_by(tr) %>%summarize(sample4=n()) 
t3s5<-d%>% subset(aliqout5_t3>1)%>%group_by(tr) %>%summarize(sample5=n()) 

aliquotN_t3<-cbind(t3s1,t3s2[,2],t3s3[,2],t3s4[,2],t3s5[,2])


aliquotN_t1
aliquotN_t2
aliquotN_t3


#Calculate average age across arms at followup time 1, 2, and 3
#Survey 1
#Tabulate overall N, gender, and age 
overallN1<-d%>% subset(!is.na(aliqout1_t1) & aliqout1_t1>1) %>% summarize(N=n(),Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t1<-d%>% subset(!is.na(aliqout1_t1) & aliqout1_t1>1) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d%>% subset(!is.na(aliqout1_t2) & aliqout1_t2>1) %>% summarize(N=n(),Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>% subset(!is.na(aliqout1_t2) & aliqout1_t2>1) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% subset(!is.na(aliqout1_t3) & aliqout1_t3>1) %>% summarize(N=n(),Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d%>% subset(!is.na(aliqout1_t3) & aliqout1_t3>1) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 


rbind(overallN1, t1[c(1,3,4,2),])
rbind(overallN2, t2[c(1,3,4,2),])
rbind(overallN3, t3[c(1,3,4,2),])


#Temporarily generate fake outcome data for AAT (0.5, sd=0.12), NPO (10000, sd=2500), and NEO (2000, sd=250)
set.seed(12345)
d$AAT1<-rnorm(n=nrow(d), mean=0.4, sd=0.12)
d$NPO1<-rnorm(n=nrow(d), mean=11000, sd=0.12)
d$NEO1<-rnorm(n=nrow(d), mean=2000, sd=0.12)

d$AAT2<-rnorm(n=nrow(d), mean=0.6, sd=0.12)
d$NPO2<-rnorm(n=nrow(d), mean=9000, sd=0.12)
d$NEO2<-rnorm(n=nrow(d), mean=2100, sd=0.12)

d$AAT3<-rnorm(n=nrow(d), mean=0.5, sd=0.12)
d$NPO3<-rnorm(n=nrow(d), mean=10000, sd=0.12)
d$NEO3<-rnorm(n=nrow(d), mean=1900, sd=0.12)

#Create and save dataset for Audrie:
stool_simulated_outcomes<-d %>%
    mutate(childid=as.character(dataid*10+childNo)) %>%
    select(childid, AAT1, NPO1, NEO1, AAT2, NPO2, NEO2, AAT3, NPO3, NEO3)
library(stringr)
stool_simulated_outcomes$childid<-str_pad(stool_simulated_outcomes$childid, 6, pad = "0")
head(stool_simulated_outcomes)    

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
save(stool_simulated_outcomes, file="washb-BD-EE-sim-stool-outcomes.Rdata")
write.dta(stool_simulated_outcomes, "washb-BD-EE-sim-stool-outcomes.dta")

