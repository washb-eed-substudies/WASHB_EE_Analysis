

#---------------------------------------
# EE-BD-stool.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The stool-based biomarker outcomes for 
# EED Kenya sub-study - ipcw analysis
#---------------------------------------


rm(list=ls())
library(tidyverse)
library(foreign)
library(washb)
library(lubridate)
set.seed(12345)
source("C:/Users/andre/Documents/EE/WASHB_EE_Analysis/WBK EE Analysis/R EE outcome code/washb_tmle_ipcw.R")


#Load in blinded treatment information
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew")
#tr <- read.csv("raw CSV/washk_blindTR.csv")
tr <- read.csv("raw CSV/washk_TR.csv")
tr$tr <- factor(tr$tr, levels = c("Control",  "WSH", "Nutrition", "Nutrition + WSH"))
head(tr)

#Child dates of birth
dob <- readRDS("WBK-EE-childDOB.rds")
#use main trial DOB
dob <- dob %>% subset(., select = -c(sex,DOB))

#Stool outcomes
outcomes<-read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_ee_stool.csv")
head(outcomes)



#Stool collection dates and staffid
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/washk_ee_stool_survey.Rdata")


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
         neo3=t3_neo)

#Baseline covariates from main trial
#enrol <- read.dta("C:/Users/andre/Dropbox/washb_Kenya_primary_outcomes_Andrew/Data-selected/clean/washb-kenya-enrol.dta")
enrol<-read.csv("C:/Users/andre/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/washb-kenya-enrol.csv")
head(enrol)


d <- left_join(outcomes, dob, by="childid")

d <- left_join(d, stsurv, by="childid")

dim(d)
d <- left_join(enrol, d, by="childid")
head(d)
dim(d)
#Subset to EED arms
d<-subset(d, tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH")
dim(d)

#----------------------------------------
# Drop childids with data problems
#----------------------------------------


# Load in id issues from Charles
idprobs <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Untouched/Missing DOB or sex CA_AL.csv")
idprobs
idprobs <- idprobs %>% 
           rename(sex2 = sex, DOB2 = DOB) %>% 
           subset(., select = c(childid, sex2, DOB2, Action)) %>% 
           mutate(sex2 = ifelse(sex2 == 1, 1, 0))


#Merge into main data.frame
d <- left_join(d, idprobs, by = c("childid")) 

#Drop children with data issues
d <- d %>% filter(Action=="keep" | is.na(Action))

#Fill in sex and dob for children missing it in stool dataset
d$sex[is.na(d$sex)] <- d$sex2[is.na(d$sex)] 
d$DOB[is.na(d$DOB)] <- d$DOB2[is.na(d$DOB)]

d <- d %>% subset(., select = -c(sex2, DOB2, Action))

# #Drop rows with no outcomes
# dim(d)
# d <- d %>% filter(!is.na(aat1) | !is.na(aat2) | !is.na(aat3) | 
#                   !is.na(mpo1) | !is.na(mpo2) | !is.na(mpo3) | 
#                   !is.na(neo1) | !is.na(neo2) | !is.na(neo3))
# dim(d)

#Calculate child age and month of the year at each measurement
d <- d %>% 
        mutate(DOB=dmy(DOB),
               aged1= stool_bl_date-DOB,
               aged2= stool_ml_date-DOB,
               aged3= stool_el_date-DOB,
               agem1= as.numeric(aged1/30.25), 
               agem2= as.numeric(aged2/30.25), 
               agem3= as.numeric(aged3/30.25),
               month1= month(d$stool_bl_date),
               month2= month(d$stool_ml_date),
               month3= month(d$stool_el_date))
dim(d)



############################
#Set outcomes:
############################

#dataframe of stool biomarkers:
Y<-d %>% select(neo1,mpo1,aat1,neo2,mpo2,aat2,neo3,mpo3,aat3)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))



#------------------
# Rename covariates
#------------------

d <- d %>%
   rename(elec = electricity,
          hfiacat = HHS,
          asset_radio = radio, 
          asset_tv = television, 
          asset_mobile = mobile, 
          asset_clock = clock, 
          asset_bike = bicycle, 
          asset_moto = motorcycle, 
          asset_stove = stove,  
          n_cows = cow, 
          n_goats = goat,
          n_chickens = chicken, 
          n_dogs = dog, 
          watmin = dminwat)



#------------------
# Clean covariates
#------------------

#Categorize maternal height
summary(d$momheight)
d$mht_cat <- as.character(ntile(d$momheight,4))
d$mht_cat[is.na(d$mht_cat)] <- "missing"
d$mht_cat <- factor(d$mht_cat, levels=c("1","2","3","4","missing"))
table(d$mht_cat)


d$asset_tv <- factor(d$asset_tv)
d$elec <- factor(d$elec)
d$momedu <- factor(d$momedu)

d$asset_tv <-relevel(d$asset_tv, ref = "0")
d$elec <-relevel(d$elec, ref = "0")
d$momedu <-relevel(d$momedu, ref = "IncompletePrimary")

d$birthord[is.na(d$birthord)] <- ""

#Collapse rare categories
d$staffid1 <- fct_lump_min(d$staffid1, 50, other_level = "inexp")
d$staffid2 <- fct_lump_min(d$staffid2, 50, other_level = "inexp")
d$staffid3 <- fct_lump_min(d$staffid3, 50, other_level = "inexp")
d$staffid1[is.na(d$staffid1)] <- "inexp"
d$staffid2[is.na(d$staffid2)] <- "inexp"
d$staffid3[is.na(d$staffid3)] <- "inexp"

d$month1[is.na(d$month1)] <- "missing"
d$month2[is.na(d$month2)] <- "missing"
d$month3[is.na(d$month3)] <- "missing"

d$month1 <- factor(d$month1)
d$month2 <- factor(d$month2)
d$month3 <- factor(d$month3)
d$staffid1 <- factor(d$staffid1)
d$staffid2 <- factor(d$staffid2)
d$staffid3 <- factor(d$staffid3)




#Make vectors of adjustment variable names
Wvars<-c("sex", "birthord",  "momage", "momedu",  "Ncomp", "Nlt18", "elec","roof",
         "mht_cat",
         "asset_radio", "asset_tv", "asset_mobile", "asset_clock", "asset_bike", "asset_moto", "asset_stove",  
         "n_cows", "n_goats","n_chickens", "n_dogs", "watmin", "hfiacat")

# #Add in time varying covariates:
# Wvars1<-c("aged1", "month1", "staffid1") 
# Wvars2<-c("aged2", "month2", "staffid2") 
# Wvars3<-c("aged3", "month3", "staffid3") 



#subset time-constant W adjustment set
W<- subset(d, select=Wvars)
W3 <- W2 <- W1 <- W

#Add in time-varying covariates
# W1<- cbind(W, subset(d, select=Wvars1))
# W2<- cbind(W, subset(d, select=Wvars2))
# W3<- cbind(W, subset(d, select=Wvars3))


# #Set time-varying covariates as factors
# W1$month1<-as.factor(W1$month1)
# W2$month2<-as.factor(W2$month2)
# W3$month3<-as.factor(W3$month3)
# W1$staffid1<-factor(W1$staffid1)
# W2$staffid2<-factor(W2$staffid2)
# W3$staffid3<-factor(W3$staffid3)



#Tabulate missingness
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(table(is.na(W[,i])))
}


#Print means for continious, Ns for factors
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  if(class(W[,i])=="factor"){
    print(table(W[,i]))
  }else{print(mean(W[,i], na.rm=T))}
}



for(i in 1:ncol(W3)){
  print(colnames(W3)[i])
  if(class(W3[,i])=="factor"){
    print(table(W3[,i]))
  }else{print(mean(W3[,i], na.rm=T))}
}




##############################################
#Run GLMs for the adjusted parameter estimates
##############################################



#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))

summary(d$neo1)
summary(d$mht_cat)

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


table(d$aat1.miss)
table(d$aat2.miss)
table(d$aat3.miss)

table(d$mpo1.miss)
table(d$mpo2.miss)
table(d$mpo3.miss)

table(d$neo1.miss)
table(d$neo2.miss)
table(d$neo3.miss)

table(d$aat1.miss)
table(d$aat2.miss)
table(d$aat3.miss)



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



#Order for replication:
d<-d[order(d$block,d$clusterid,d$childid),]
  

#dataframes of urine biomarkers and missingness:
Y<-d %>% select(aat1Delta,mpo1Delta,neo1Delta,
                aat2Delta,mpo2Delta,neo2Delta,
                aat3Delta,mpo3Delta,neo3Delta)
miss<-d %>% select(aat1.miss,mpo1.miss,neo1.miss,
                   aat2.miss,mpo2.miss,neo2.miss,
                   aat3.miss,mpo3.miss,neo3.miss)

#Create empty matrix to hold the ipcw results:
res_adj<-list(neo_t1_adj=matrix(0,5,5), mpo_t1_adj=matrix(0,5,5), aat_t1_adj=matrix(0,5,5), 
                neo_t2_adj=matrix(0,5,5), mpo_t2_adj=matrix(0,5,5), aat_t2_adj=matrix(0,5,5),  
                neo_t3_adj=matrix(0,5,5), mpo_t3_adj=matrix(0,5,5), aat_t3_adj=matrix(0,5,5))

Wlist <- list(W1,W1,W1,W2,W2,W2,W3,W3,W3)



for(i in 1:9){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_tmle_ipcw(Y=log(Y[,i]), Delta=miss[,i], tr=d$tr, W=Wlist[[i]], id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], Q.SL.library = c("SL.glm"), seed=12345, print=T)
    cat(i," : ",j, "\n")
    res_adj[[i]][j,]<-(t(unlist(temp$estimates$ATE)))
    colnames(res_adj[[i]])<-c("psi","var.psi","ci.l","ci.u", "Pval")
    rownames(res_adj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}



#Extract estimates
neo_t1_adj_ipcw_M<-res_adj[[1]]
neo_t2_adj_ipcw_M<-res_adj[[4]]
neo_t3_adj_ipcw_M<-res_adj[[7]]

mpo_t1_adj_ipcw_M<-res_adj[[2]]
mpo_t2_adj_ipcw_M<-res_adj[[5]]
mpo_t3_adj_ipcw_M<-res_adj[[8]]

aat_t1_adj_ipcw_M<-res_adj[[3]]
aat_t2_adj_ipcw_M<-res_adj[[6]]
aat_t3_adj_ipcw_M<-res_adj[[9]]





setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Results/Andrew/")
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
file="stool_ipcw_res.Rdata")




