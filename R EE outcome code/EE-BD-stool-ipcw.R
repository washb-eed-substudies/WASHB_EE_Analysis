
#---------------------------------------
# EE-BD-stool-ipcw.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The analysis script for the WASH Benefits
# EED substudy -IPCW analysis for missing 
# outcomes of stool-based biomarkers
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
stool<-read.csv("BD-EE-stool.csv")
ipcw<-read.csv("BD-EE-ipcw.csv", stringsAsFactors = T) %>% select(-c(tr,block))



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
outcomes<-read.dta("washb-BD-EE-sim-stool-outcomes-stata12.dta")
outcomes$childid<-as.numeric(outcomes$childid)


dim(stool)
dim(outcomes)
stool<-left_join(stool,outcomes, by="childid")
dim(stool)




#Merge in stool outcomes
stool_outcomes<-subset(stool, select=c(dataid,childNo, 
                                       staffid1, staffid2, staffid3, 
                                       month1, month2, month3, 
                                       aged1, aged2,aged3,
                                       aat1,aat2,aat3,
                                       mpo1,mpo2,mpo3,
                                       neo1,neo2,neo3,reg1b2))
dim(stool_outcomes)
d<-merge(ipcw, stool_outcomes, by=c("dataid", "childNo"), all.x=T, all.y=T)
dim(d)


#Merge treatment information 
dim(d)
d<-left_join(d,treatment, by="clusterid")
dim(d)
table(d$tr)



#Subset to Control and WSH+N
d<-subset(d, tr=="Control" | tr=="Nutrition + WSH")



#Clean covariates for adjusted analysis
#Set birthorder to 1, >=2, or missing
class(d$birthord)
d$birthord[d$birthord>1]<-"2+"
d$birthord[is.na(d$birthord)]<-"missing"
d$birthord<-factor(d$birthord)

#Make vectors of adjustment variable names
Wvars<-c('sex', 'birthord',
         'momage', 'momheight','momedu','hfiacat',
         'Nlt18','Ncomp','watmin',
          'walls', 'floor',
         'elec', 'asset_wardrobe', 'asset_table', 'asset_chair', 'asset_clock', 
         'asset_khat', 'asset_chouki', 'asset_radio', 
         'asset_tv', 'asset_refrig', 'asset_bike',
         'asset_moto', 'asset_sewmach', 'asset_mobile',
         'n_cows', 'n_goats', 'n_chickens')





#subset time-constant W adjustment set
W<- subset(d, select=Wvars)

#Clean adjustment variables 
#Check missingness
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(table(is.na(W[,i])))
}

#Replace missingness for factors with new level
#in main dataset 
d$sex<-as.factor(d$sex)
#d$birthord[is.na(d$birthord)]<-"99"
d$birthord<-factor(d$birthord)

d$asset_clock[is.na(d$asset_clock)]<-"99"
d$asset_clock<-factor(d$asset_clock)

#Order data to replicate SL
d <- d[order(d$dataid,d$childNo, d$svy),]

#Re-subset W so new missing categories are included
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(class(W[,i])  )
}


#Truncate unrealistic levels of n_chickens to 60
table(d$n_chickens)
d$n_chickens[d$n_chickens>60]<-60
table(d$n_chickens)




#Relevel all factors
table(d$sex)
d$sex<-addNA(d$sex)
  levels(d$sex)[3]<-"missing"
table(d$sex)
d$momedu=relevel(d$momedu,ref="No education")
d$hfiacat=relevel(d$hfiacat,ref="Food Secure")
    d$hfiacat<-addNA(d$hfiacat)
d$wall<-factor(d$wall)
    d$wall<-addNA(d$wall)
    levels(d$wall)<-c("No improved wall","Improved wall","Missing")
    d$wall=relevel(d$wall,ref="No improved wall")
d$floor<-factor(d$floor)
    d$floor<-addNA(d$floor)
    levels(d$floor)<-c("No improved floor","Improved floor","Missing")
    d$floor=relevel(d$floor,ref="No improved floor")
d$elec<-factor(d$elec)
    d$elec<-addNA(d$elec)
    levels(d$elec)<-c("No electricity","Electricity","Missing")
    d$elec=relevel(d$elec,ref="No electricity")
d$asset_wardrobe<-factor(d$asset_wardrobe)
    d$asset_wardrobe<-addNA(d$asset_wardrobe)
    levels(d$asset_wardrobe)<-c("No wardrobe","Wardrobe","Missing")
    d$asset_wardrobe=relevel(d$asset_wardrobe,ref="No wardrobe")
d$asset_table<-factor(d$asset_table)
    d$asset_table<-addNA(d$asset_table)
    levels(d$asset_table)<-c("No table","Improved table","Missing")
    d$asset_table=relevel(d$asset_table,ref="No table")
d$asset_chair<-factor(d$asset_chair)
    d$asset_chair<-addNA(d$asset_chair)
    levels(d$asset_chair)<-c("No chair","Chair","Missing")
    d$asset_chair=relevel(d$asset_chair,ref="No chair")
d$asset_clock[is.na(d$asset_clock)]<-99
    d$asset_clock<-factor(d$asset_clock)
    d$asset_clock<-addNA(d$asset_clock)
    levels(d$asset_clock)<-c("No clock","Clock","Missing", "Missing")
    d$asset_clock=relevel(d$asset_clock,ref="No clock")
d$asset_khat<-factor(d$asset_khat)
    d$asset_khat<-addNA(d$asset_khat)
    levels(d$asset_khat)<-c("No khat","Khat","Missing")
    d$asset_khat=relevel(d$asset_khat,ref="No khat")
d$asset_chouki<-factor(d$asset_chouki)
    d$asset_chouki<-addNA(d$asset_chouki)
    levels(d$asset_chouki)<-c("No chouki","Chouki","Missing")
    d$asset_chouki=relevel(d$asset_chouki,ref="No chouki")
d$asset_tv<-factor(d$asset_tv)
    d$asset_tv<-addNA(d$asset_tv)
    levels(d$asset_tv)<-c("No TV","Improved TV","Missing")
    d$asset_tv=relevel(d$asset_tv,ref="No TV")
d$asset_refrig<-factor(d$asset_refrig)
    d$asset_refrig<-addNA(d$asset_refrig)
    levels(d$asset_refrig)<-c("No refrigerator","Refrigerator","Missing")
    d$asset_refrig=relevel(d$asset_refrig,ref="No refrigerator")
d$asset_bike<-factor(d$asset_bike)
    d$asset_bike<-addNA(d$asset_bike)
    levels(d$asset_bike)<-c("No bicycle","Bicycle","Missing")
    d$asset_bike=relevel(d$asset_bike,ref="No bicycle")
d$asset_moto<-factor(d$asset_moto)
    d$asset_moto<-addNA(d$asset_moto)
    levels(d$asset_moto)<-c("No motorcycle","Motorcycle","Missing")
    d$asset_moto=relevel(d$asset_moto,ref="No motorcycle")
d$asset_sewmach<-factor(d$asset_sewmach)
    d$asset_sewmach<-addNA(d$asset_sewmach)
    levels(d$asset_sewmach)<-c("No sewing machine","Sewing machine","Missing")
    d$asset_sewmach=relevel(d$asset_sewmach,ref="No sewing machine")
d$asset_mobile<-factor(d$asset_mobile)
    d$asset_mobile<-addNA(d$asset_mobile)
    levels(d$asset_mobile)<-c("No mobile phone","Mobile phone","Missing")
    d$asset_mobile=relevel(d$asset_mobile,ref="No mobile phone")    

#Re-subset W so new re-leveled factors are included
W<- subset(d, select=Wvars)





#Create indicators for missingness
d$aat1.miss<-ifelse(is.na(d$aat1),0,1)
d$aat2.miss<-ifelse(is.na(d$aat2),0,1)
d$aat3.miss<-ifelse(is.na(d$aat3),0,1)

d$mpo1.miss<-ifelse(is.na(d$mpo1),0,1)
d$mpo2.miss<-ifelse(is.na(d$mpo2),0,1)
d$mpo3.miss<-ifelse(is.na(d$mpo3),0,1)

d$neo1.miss<-ifelse(is.na(d$neo1),0,1)
d$neo2.miss<-ifelse(is.na(d$neo2),0,1)
d$neo3.miss<-ifelse(is.na(d$neo3),0,1)

d$reg1b2.miss<-ifelse(is.na(d$reg1b2),0,1)


table(d$aat1.miss)
table(d$aat2.miss)
table(d$aat3.miss)

table(d$mpo1.miss)
table(d$mpo2.miss)
table(d$mpo3.miss)

table(d$neo1.miss)
table(d$neo2.miss)
table(d$neo3.miss)

table(d$reg1b2.miss)




# set missing outcomes to an arbitrary, non-missing value. In this case use 9
d$aat1Delta <- d$aat1
d$aat1Delta[d$aat1.miss==0] <- 9

d$aat2Delta <- d$aat2
d$aat2Delta[d$aat2.miss==0] <- 9

d$aat3Delta <- d$aat3
d$aat3Delta[d$aat3.miss==0] <- 9

d$mpo1Delta <- d$mpo1
d$mpo1Delta[d$mpo1.miss==0] <- 9

d$mpo2Delta <- d$mpo2
d$mpo2Delta[d$mpo2.miss==0] <- 9

d$mpo3Delta <- d$mpo3
d$mpo3Delta[d$mpo3.miss==0] <- 9

d$neo1Delta <- d$neo1
d$neo1Delta[d$neo1.miss==0] <- 9

d$neo2Delta <- d$neo2
d$neo2Delta[d$neo2.miss==0] <- 9

d$neo3Delta <- d$neo3
d$neo3Delta[d$neo3.miss==0] <- 9

d$reg1b2Delta <- d$reg1b2
d$reg1b2Delta[d$reg1b2.miss==0] <- 9




#Order for replication:
d<-d[order(d$block,d$clusterid,d$dataid),]
  
#Run the unadjusted ipcw analysis
aat_t1_unadj_ipcw_M<-washb_tmle(Y=d$aat1Delta, Delta=d$aat1.miss, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
aat_t2_unadj_ipcw_M<-washb_tmle(Y=d$aat2Delta, Delta=d$aat2.miss, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
aat_t3_unadj_ipcw_M<-washb_tmle(Y=d$aat3Delta, Delta=d$aat3.miss, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)

mpo_t1_unadj_ipcw_M<-washb_tmle(Y=d$mpo1Delta, Delta=d$mpo1.miss, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
mpo_t2_unadj_ipcw_M<-washb_tmle(Y=d$mpo2Delta, Delta=d$mpo2.miss, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
mpo_t3_unadj_ipcw_M<-washb_tmle(Y=d$mpo3Delta, Delta=d$mpo3.miss, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)

neo_t1_unadj_ipcw_M<-washb_tmle(Y=d$neo1Delta, Delta=d$neo1.miss, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
neo_t2_unadj_ipcw_M<-washb_tmle(Y=d$neo2Delta, Delta=d$neo2.miss, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
neo_t3_unadj_ipcw_M<-washb_tmle(Y=d$neo3Delta, Delta=d$neo3.miss, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)

reg1b_t2_unadj_ipcw_M<-washb_tmle(Y=d$reg1b2Delta, Delta=d$reg1b2.miss, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)











#Extract estimates
aat_t1_unadj_ipcw_M<-as.data.frame(unlist(aat_t1_unadj_ipcw_M$estimates$ATE))
aat_t2_unadj_ipcw_M<-as.data.frame(unlist(aat_t2_unadj_ipcw_M$estimates$ATE))
aat_t3_unadj_ipcw_M<-as.data.frame(unlist(aat_t3_unadj_ipcw_M$estimates$ATE))

mpo_t1_unadj_ipcw_M<-as.data.frame(unlist(mpo_t1_unadj_ipcw_M$estimates$ATE))
mpo_t2_unadj_ipcw_M<-as.data.frame(unlist(mpo_t2_unadj_ipcw_M$estimates$ATE))
mpo_t3_unadj_ipcw_M<-as.data.frame(unlist(mpo_t3_unadj_ipcw_M$estimates$ATE))

neo_t1_unadj_ipcw_M<-as.data.frame(unlist(neo_t1_unadj_ipcw_M$estimates$ATE))
neo_t2_unadj_ipcw_M<-as.data.frame(unlist(neo_t2_unadj_ipcw_M$estimates$ATE))
neo_t3_unadj_ipcw_M<-as.data.frame(unlist(neo_t3_unadj_ipcw_M$estimates$ATE))

reg1b_t2_unadj_ipcw_M<-as.data.frame(unlist(reg1b_t2_unadj_ipcw_M$estimates$ATE))



#Run the adjusted ipcw analysis
aat_t1_adj_ipcw_M<-washb_tmle(Y=d$aat1Delta, Delta=d$aat1.miss, tr=d$tr, W=W, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
aat_t2_adj_ipcw_M<-washb_tmle(Y=d$aat2Delta, Delta=d$aat2.miss, tr=d$tr, W=W, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
aat_t3_adj_ipcw_M<-washb_tmle(Y=d$aat3Delta, Delta=d$aat3.miss, tr=d$tr, W=W, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)

mpo_t1_adj_ipcw_M<-washb_tmle(Y=d$mpo1Delta, Delta=d$mpo1.miss, tr=d$tr, W=W, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
mpo_t2_adj_ipcw_M<-washb_tmle(Y=d$mpo2Delta, Delta=d$mpo2.miss, tr=d$tr, W=W, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
mpo_t3_adj_ipcw_M<-washb_tmle(Y=d$mpo3Delta, Delta=d$mpo3.miss, tr=d$tr, W=W, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)

neo_t1_adj_ipcw_M<-washb_tmle(Y=d$neo1Delta, Delta=d$neo1.miss, tr=d$tr, W=W, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
neo_t2_adj_ipcw_M<-washb_tmle(Y=d$neo2Delta, Delta=d$neo2.miss, tr=d$tr, W=W, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)
neo_t3_adj_ipcw_M<-washb_tmle(Y=d$neo3Delta, Delta=d$neo3.miss, tr=d$tr, W=W, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)

reg1b_t2_adj_ipcw_M<-washb_tmle(Y=d$reg1b2Delta, Delta=d$reg1b2.miss, tr=d$tr, W=W, id=d$block, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=T)


#Extract estimates
aat_t1_adj_ipcw_M<-as.data.frame(unlist(aat_t1_adj_ipcw_M$estimates$ATE))
aat_t2_adj_ipcw_M<-as.data.frame(unlist(aat_t2_adj_ipcw_M$estimates$ATE))
aat_t3_adj_ipcw_M<-as.data.frame(unlist(aat_t3_adj_ipcw_M$estimates$ATE))

mpo_t1_adj_ipcw_M<-as.data.frame(unlist(mpo_t1_adj_ipcw_M$estimates$ATE))
mpo_t2_adj_ipcw_M<-as.data.frame(unlist(mpo_t2_adj_ipcw_M$estimates$ATE))
mpo_t3_adj_ipcw_M<-as.data.frame(unlist(mpo_t3_adj_ipcw_M$estimates$ATE))

neo_t1_adj_ipcw_M<-as.data.frame(unlist(neo_t1_adj_ipcw_M$estimates$ATE))
neo_t2_adj_ipcw_M<-as.data.frame(unlist(neo_t2_adj_ipcw_M$estimates$ATE))
neo_t3_adj_ipcw_M<-as.data.frame(unlist(neo_t3_adj_ipcw_M$estimates$ATE))

reg1b_t2_adj_ipcw_M<-as.data.frame(unlist(reg1b_t2_adj_ipcw_M$estimates$ATE))









setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(aat_t1_unadj_ipcw_M,
aat_t2_unadj_ipcw_M,
aat_t3_unadj_ipcw_M,
mpo_t1_unadj_ipcw_M,
mpo_t2_unadj_ipcw_M,
mpo_t3_unadj_ipcw_M,
neo_t1_unadj_ipcw_M,
neo_t2_unadj_ipcw_M,
neo_t3_unadj_ipcw_M,
reg1b_t2_unadj_ipcw_M,
aat_t1_adj_ipcw_M,
aat_t2_adj_ipcw_M,
aat_t3_adj_ipcw_M,
mpo_t1_adj_ipcw_M,
mpo_t2_adj_ipcw_M,
mpo_t3_adj_ipcw_M,
neo_t1_adj_ipcw_M,
neo_t2_adj_ipcw_M,
neo_t3_adj_ipcw_M,
reg1b_t2_adj_ipcw_M, 
file="stool_ipcw_res.Rdata")




