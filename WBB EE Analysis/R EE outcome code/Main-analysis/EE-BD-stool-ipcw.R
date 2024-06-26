

#---------------------------------------
# EE-BD-stool-ipcw.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The analysis script for the WASH Benefits
# EED substudy -IPCW analysis for missing 
# outcomes of stool-based biomarkers
#---------------------------------------

#--------------------------------------------------------------------
### Prelude
#--------------------------------------------------------------------

rm(list=ls())
library(foreign)
library(tidyverse)
library(washb)

#--------------------------------------------------------------------
### Load in and merge datasets
#--------------------------------------------------------------------

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-bangladesh-tr (real).Rdata")
d$clusterid<-as.numeric(d$clusterid)
treatment<-d


#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
stool<-read.csv("BD-EE-stool.csv")
ipcw<-read.csv("BD-EE-ipcw.csv", stringsAsFactors = T) %>% select(-c(tr,block))



#Load in lab outcomes
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
outcomes<-read.dta("BD-EE-stool-outcomes-Stata12.dta")

#divide the reg value by 1000 to convert it to ug/ml 
outcomes$t2_reg<-outcomes$t2_reg/1000

#divide all the aat values by 1000000 to convert it to mg/g
outcomes$t1_aat<-outcomes$t1_aat/1000000
outcomes$t2_aat<-outcomes$t2_aat/1000000
outcomes$t3_aat<-outcomes$t3_aat/1000000

#Rename outcomes:
outcomes <- outcomes %>%
  rename(aat1=t1_aat,
         aat2=t2_aat,
         aat3=t3_aat,
         mpo1=t1_mpo,
         mpo2=t2_mpo,
         mpo3=t3_mpo,
         neo1=t1_neo,
         neo2=t2_neo,
         neo3=t3_neo,
         reg1b2=t2_reg)


dim(stool)
dim(outcomes)
outcomes$childid<-as.numeric(outcomes$childid)
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
d<-merge(ipcw, stool_outcomes, by=c("dataid", "childNo"), all.x=T, all.y=F)
dim(d)


#Merge treatment information 
dim(d)
d<-left_join(d,treatment, by="clusterid")
dim(d)
table(d$tr)




#Subset to EED arms
d<-subset(d, tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH")

#check on discrepencies for Jess
d[d$dataid==6103,]
treatment[treatment$dataid==6103,]
ipcw[ipcw$dataid==6103,]
stool_outcomes[stool_outcomes$dataid==6103,]
stool[stool$dataid==6103,]

#--------------------------------------------------------------------
#Impute time varying covariates
#--------------------------------------------------------------------

#set staffid and month to missing if missing stool samples
no_outcome <- is.na(d$aat1) & is.na(d$aat2) & is.na(d$aat3) & is.na(d$reg1b2) & 
                is.na(d$mpo1) & is.na(d$mpo2) & is.na(d$mpo3) & 
                is.na(d$neo1) & is.na(d$neo2) & is.na(d$neo3)
d$staffid1[no_outcome & !is.na(d$staffid1)] <- NA
d$staffid2[no_outcome & !is.na(d$staffid2)] <- NA 
d$staffid3[no_outcome & !is.na(d$staffid3)] <- NA 
d$month1[no_outcome & !is.na(d$month1)] <- NA
d$month2[no_outcome & !is.na(d$month2)] <- NA 
d$month3[no_outcome & !is.na(d$month3)] <- NA 
d$aged1[no_outcome & !is.na(d$aged1)] <- NA
d$aged2[no_outcome & !is.na(d$aged2)] <- NA 
d$aged3[no_outcome & !is.na(d$aged3)] <- NA 


#calculate overall median:
month1_median <-    median(d$month1, na.rm = T)
month2_median <-    median(d$month2, na.rm = T)
month3_median <-    median(d$month3, na.rm = T)

#use clusterid to impute median month where possible
table(d$month1)
table(is.na(d$month1))
d$month1[is.na(d$month1)] <-  ave(d$month1, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month1)] 
d$month1 <- ceiling(d$month1)
table(d$month1)
table(d$month1[d$tr=="Control"])


d$month2[is.na(d$month2)] <-  ave(d$month2, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month2)] 
d$month2 <- ceiling(d$month2)

d$month3[is.na(d$month3)] <-  ave(d$month3, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month3)] 
d$month3 <- ceiling(d$month3)


#impute month with overall median for those observations not in a cluster measured in the EED subsample
d$month1[is.na(d$month1)] <-  7
d$month2[is.na(d$month2)] <-  8
d$month3[is.na(d$month3)] <-  6


d <- d %>% mutate(monsoon1 = ifelse(month1 > 4 & month1 < 11, "1", "0"),
                  monsoon2 = ifelse(month2 > 4 & month2 < 11, "1", "0"),
                  monsoon3 = ifelse(month3 > 4 & month3 < 11, "1", "0"),
                  monsoon1 = ifelse(is.na(month1),"missing", monsoon1),
                  monsoon2 = ifelse(is.na(month2),"missing", monsoon2),
                  monsoon3 = ifelse(is.na(month3),"missing", monsoon3),
                  monsoon1 = factor(monsoon1),
                  monsoon2 = factor(monsoon2),
                  monsoon3 = factor(monsoon3))


#impute child age with overall median
d$aged1[is.na(d$aged1)] <- 84
d$aged2[is.na(d$aged2)] <- 428
d$aged3[is.na(d$aged3)] <- 857


#--------------------------------------------------------------------
#Clean covariates for adjusted analysis
#--------------------------------------------------------------------

#Order data for easier replication
d <- d[order(d$dataid,d$childNo, d$svy),]


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
d$birthord<-factor(d$birthord)
table(d$birthord)
table(W$birthord)

d$asset_clock[is.na(d$asset_clock)]<-"99"
d$asset_clock<-factor(d$asset_clock)


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


#Add in time-varying covariates
Wvars1<-c("aged1", "monsoon1") 
Wvars2<-c("aged2", "monsoon2") 
Wvars3<-c("aged3", "monsoon3") 
W1<- cbind(W, subset(d, select=Wvars1))
W2<- cbind(W, subset(d, select=Wvars2))
W3<- cbind(W, subset(d, select=Wvars3))

#Replace missingness in time varying covariates as a new level
W1$monsoon1[is.na(W1$monsoon1)]<-"missing"
W2$monsoon2[is.na(W2$monsoon2)]<-"missing"
W3$monsoon3[is.na(W3$monsoon3)]<-"missing"




#--------------------------------------------------------------------
# Set up ipcw analysis
#--------------------------------------------------------------------

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

table(d$aat1.miss)
table(d$aat2.miss)
table(d$aat3.miss)


d$aat3Delta.test <- log(d$aat3)
d$aat3Delta.test[d$aat3.miss==0] <- 99


# set missing outcomes to an arbitrary, non-missing value. In this case use 9
d$aat1Delta <- d$aat1
d$aat1Delta[d$aat1.miss==0] <- exp(9)

d$aat2Delta <- d$aat2
d$aat2Delta[d$aat2.miss==0] <- exp(9)

d$aat3Delta <- d$aat3
d$aat3Delta[d$aat3.miss==0] <- exp(9)

d$mpo1Delta <- d$mpo1
d$mpo1Delta[d$mpo1.miss==0] <- exp(9)

d$mpo2Delta <- d$mpo2
d$mpo2Delta[d$mpo2.miss==0] <- exp(9)

d$mpo3Delta <- d$mpo3
d$mpo3Delta[d$mpo3.miss==0] <- exp(9)

d$neo1Delta <- d$neo1
d$neo1Delta[d$neo1.miss==0] <- exp(9)

d$neo2Delta <- d$neo2
d$neo2Delta[d$neo2.miss==0] <- exp(9)

d$neo3Delta <- d$neo3
d$neo3Delta[d$neo3.miss==0] <- exp(9)

d$reg1b2Delta <- d$reg1b2
d$reg1b2Delta[d$reg1b2.miss==0] <- exp(9)




#--------------------------------------------------------------------
#Run the adjusted ipcw analysis
#--------------------------------------------------------------------


#dataframe of stool biomarkers:
Y<-d %>% select(neo1Delta,mpo1Delta,aat1Delta,neo2Delta,mpo2Delta,aat2Delta,reg1b2Delta,neo3Delta,mpo3Delta,aat3Delta)

#dataframe of stool missingness:
miss<-d %>% select(neo1.miss,mpo1.miss,aat1.miss,neo2.miss,mpo2.miss,aat2.miss,reg1b2.miss,neo3.miss,mpo3.miss,aat3.miss)


#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))


#Create empty matrix to hold the glm results:
res_adj<-list(neo_t1_adj=matrix(0,5,5), mpo_t1_adj=matrix(0,5,5), aat_t1_adj=matrix(0,5,5), 
                neo_t2_adj=matrix(0,5,5), mpo_t2_adj=matrix(0,5,5), aat_t2_adj=matrix(0,5,5),  reg1b_t2_adj=matrix(0,5,5),
                neo_t3_adj=matrix(0,5,5), mpo_t3_adj=matrix(0,5,5), aat_t3_adj=matrix(0,5,5))

Wlist <- list(W1,W1,W1,W2,W2,W2,W2,W3,W3,W3)


for(i in 1:10){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_tmle(Y=log(Y[,i]), Delta=miss[,i], tr=d$tr, W=Wlist[[i]], id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], Q.SL.library = c("SL.glm"), seed=12345, print=T)
    cat(i," : ",j, "\n")
    res_adj[[i]][j,]<-(t(unlist(temp$estimates$ATE)))
    colnames(res_adj[[i]])<-c("psi","var.psi","ci.l","ci.u", "Pval")
    rownames(res_adj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}



#Extract estimates
neo_t1_adj_ipcw_M<-res_adj[[1]]
neo_t2_adj_ipcw_M<-res_adj[[4]]
neo_t3_adj_ipcw_M<-res_adj[[8]]

mpo_t1_adj_ipcw_M<-res_adj[[2]]
mpo_t2_adj_ipcw_M<-res_adj[[5]]
mpo_t3_adj_ipcw_M<-res_adj[[9]]

aat_t1_adj_ipcw_M<-res_adj[[3]]
aat_t2_adj_ipcw_M<-res_adj[[6]]
aat_t3_adj_ipcw_M<-res_adj[[10]]

reg_t2_adj_ipcw_M<-res_adj[[7]]




setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(
aat_t1_adj_ipcw_M,
aat_t2_adj_ipcw_M,
aat_t3_adj_ipcw_M,
mpo_t1_adj_ipcw_M,
mpo_t2_adj_ipcw_M,
mpo_t3_adj_ipcw_M,
neo_t1_adj_ipcw_M,
neo_t2_adj_ipcw_M,

neo_t3_adj_ipcw_M,
reg_t2_adj_ipcw_M, 
file="stool_ipcw_res.Rdata")




