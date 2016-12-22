
#---------------------------------------
# EE-BD-urine.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The urine-based biomarker outcomes for 
# EED Bangladesh sub-study
#---------------------------------------

###Load in data
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)



setwd("C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Untouched/")
load("washb-BD-EE-blind-tr.Rdata")
levels(treatment$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","WSH","Nutrition","Nutrition + WSH"))
levels(treatment$tr)
#Load in enrollment data for adjusted analysis
enrol<-read.csv("washb-bangladesh-enrol.csv",stringsAsFactors = TRUE)

setwd("C:/Users/andre/Dropbox/WBB-EE-analysis/Data/Cleaned/Andrew")
urine<-read.csv("BD-EE-urine.csv")




#Merge treatment information 
dim(urine)
d<-left_join(urine,treatment, by="clusterid")
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
t1s1<-d%>% subset(h2aliqout1_t1==3)%>%group_by(tr) %>%summarize(h2sample1=n()) 
t1s2<-d%>% subset(h2aliqout2_t1==3)%>%group_by(tr) %>%summarize(h2sample2=n()) 
t1s3<-d%>% subset(h2aliqout3_t1==3)%>%group_by(tr) %>%summarize(h2sample3=n()) 
t1s4<-d%>% subset(h2aliqout4_t1==3)%>%group_by(tr) %>%summarize(h2sample4=n()) 
t1s5<-d%>% subset(h2aliqout5_t1==3)%>%group_by(tr) %>%summarize(h2sample5=n()) 
t1s6<-d%>% subset(h2aliqout6_t1==3)%>%group_by(tr) %>%summarize(h2sample6=n()) 
t1s7<-d%>% subset(h5aliqout7_t1==3)%>%group_by(tr) %>%summarize(h5sample7=n()) 
t1s8<-d%>% subset(h5aliqout8_t1==3)%>%group_by(tr) %>%summarize(h5sample8=n()) 
t1s9<-d%>% subset(h5aliqout9_t1==3)%>%group_by(tr) %>%summarize(h5sample9=n()) 
t1s10<-d%>% subset(h5aliqout10_t1==3)%>%group_by(tr) %>%summarize(h5sample10=n()) 
t1s11<-d%>% subset(h5aliqout11_t1==3)%>%group_by(tr) %>%summarize(h5sample11=n()) 
t1s12<-d%>% subset(h5aliqout12_t1==3)%>%group_by(tr) %>%summarize(h5sample12=n()) 

 
aliquotN_t1<-cbind(t1s1,t1s2[,2],t1s3[,2],t1s4[,2],t1s5[,2],t1s6[,2],t1s7[,2],t1s8[,2],t1s9[,2],t1s10[,2],t1s11[,2],t1s12[,2])


#aliqout time 2
t2s1<-d%>% subset(h2aliqout1_t2==3)%>%group_by(tr) %>%summarize(h2sample1=n()) 
t2s2<-d%>% subset(h2aliqout2_t2==3)%>%group_by(tr) %>%summarize(h2sample2=n()) 
t2s3<-d%>% subset(h2aliqout3_t2==3)%>%group_by(tr) %>%summarize(h2sample3=n()) 
t2s4<-d%>% subset(h2aliqout4_t2==3)%>%group_by(tr) %>%summarize(h2sample4=n()) 
t2s5<-d%>% subset(h2aliqout5_t2==3)%>%group_by(tr) %>%summarize(h2sample5=n()) 
t2s6<-d%>% subset(h2aliqout6_t2==3)%>%group_by(tr) %>%summarize(h2sample6=n()) 
t2s7<-d%>% subset(h5aliqout7_t2==3)%>%group_by(tr) %>%summarize(h5sample7=n()) 
t2s8<-d%>% subset(h5aliqout8_t2==3)%>%group_by(tr) %>%summarize(h5sample8=n()) 
t2s9<-d%>% subset(h5aliqout9_t2==3)%>%group_by(tr) %>%summarize(h5sample9=n()) 
t2s10<-d%>% subset(h5aliqout10_t2==3)%>%group_by(tr) %>%summarize(h5sample10=n()) 
t2s11<-d%>% subset(h5aliqout11_t2==3)%>%group_by(tr) %>%summarize(h5sample11=n()) 
t2s12<-d%>% subset(h5aliqout12_t2==3)%>%group_by(tr) %>%summarize(h5sample12=n()) 

 
aliquotN_t2<-cbind(t2s1,t2s2[,2],t2s3[,2],t2s4[,2],t2s5[,2],t2s6[,2],t2s7[,2],t2s8[,2],t2s9[,2],t2s10[,2],t2s11[,2],t2s12[,2])


#aliqout time 3
t3s1<-d%>% subset(h2aliqout1_t3==3)%>%group_by(tr) %>%summarize(h2sample1=n()) 
t3s2<-d%>% subset(h2aliqout2_t3==3)%>%group_by(tr) %>%summarize(h2sample2=n()) 
t3s3<-d%>% subset(h2aliqout3_t3==3)%>%group_by(tr) %>%summarize(h2sample3=n()) 
t3s4<-d%>% subset(h2aliqout4_t3==3)%>%group_by(tr) %>%summarize(h2sample4=n()) 
t3s5<-d%>% subset(h2aliqout5_t3==3)%>%group_by(tr) %>%summarize(h2sample5=n()) 
t3s6<-d%>% subset(h2aliqout6_t3==3)%>%group_by(tr) %>%summarize(h2sample6=n()) 
t3s7<-d%>% subset(h5aliqout7_t3==3)%>%group_by(tr) %>%summarize(h5sample7=n()) 
t3s8<-d%>% subset(h5aliqout8_t3==3)%>%group_by(tr) %>%summarize(h5sample8=n()) 
t3s9<-d%>% subset(h5aliqout9_t3==3)%>%group_by(tr) %>%summarize(h5sample9=n()) 
t3s10<-d%>% subset(h5aliqout10_t3==3)%>%group_by(tr) %>%summarize(h5sample10=n()) 
t3s11<-d%>% subset(h5aliqout11_t3==3)%>%group_by(tr) %>%summarize(h5sample11=n()) 
t3s12<-d%>% subset(h5aliqout12_t3==3)%>%group_by(tr) %>%summarize(h5sample12=n()) 

 
aliquotN_t3<-cbind(t3s1,t3s2[,2],t3s3[,2],t3s4[,2],t3s5[,2],t3s6[,2],t3s7[,2],t3s8[,2],t3s9[,2],t3s10[,2],t3s11[,2],t3s12[,2])


aliquotN_t1
aliquotN_t2
aliquotN_t3



