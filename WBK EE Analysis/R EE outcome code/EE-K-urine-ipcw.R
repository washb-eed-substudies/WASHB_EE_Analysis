
#---------------------------------------
# EE-BD-urine-ipcw.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The analysis script for the WASH Benefits
# EED substudy -IPCW analysis for missing 
# outcomes of urine-based biomarkers
#---------------------------------------

###Load in data
rm(list=ls())
library(foreign)
library(tidyverse)
library(washb)
library(lubridate)
source("C:/Users/andre/Documents/EE/WASHB_EE_Analysis/WBK EE Analysis/R EE outcome code/washb_tmle_ipcw.R")


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew")
tr <- read.csv("raw CSV/washk_TR.csv")
tr$tr <- factor(tr$tr, levels = c("Control",  "WSH", "Nutrition", "Nutrition + WSH"))
head(tr)

dob <- readRDS("WBK-EE-childDOB.rds")
#use main trial DOB and sex
dob <- dob %>% subset(., select = -c(sex, DOB))

lm <- readRDS("WBK-EE-LM-outcomes.rds")
head(lm)

#urine collection dates and staffid
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/washk_ee_urine_survey.Rdata")


#Load in main trial enrollment data for ipcw analysis
#enrol <- read.dta("C:/Users/andre/Dropbox/washb_Kenya_primary_outcomes_Andrew/Data-selected/clean/washb-kenya-enrol.dta")
enrol <- read.csv("C:/Users/andre/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/washb-kenya-enrol.csv")
head(enrol)


d <- left_join(lm, dob, by=c("childid","hhid"))

# d <- left_join(d, ursurv, by="childid")

dim(d)
d <- left_join(enrol, d,  by="childid")
dim(d)

class(d$DOB)
class(d$urine_bl_date)

#Calculate child age and month of the year at each measurement
d <- d %>% 
        mutate(DOB=dmy(DOB),
               aged1= urine_bl_date-DOB,
               aged2= urine_ml_date-DOB,
               aged3= urine_el_date-DOB,
               agem1= as.numeric(aged1/30.25), 
               agem2= as.numeric(aged2/30.25), 
               agem3= as.numeric(aged3/30.25),
               month1= month(d$urine_bl_date),
               month2= month(d$urine_ml_date),
               month3= month(d$urine_el_date))



#Subset to EED arms
d<-subset(d, tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH")
dim(d)



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
# Covariate cleaning
#------------------

#Categorize maternal height
summary(d$momheight)
d$mht_cat <- as.character(ntile(d$momheight,4))
d$mht_cat[is.na(d$mht_cat)] <- "missing"
d$mht_cat <- factor(d$mht_cat, levels=c("1","2","3","4","missing"))
table(d$mht_cat)

# Set factor variables
d$asset_tv <- factor(d$asset_tv)
d$elec <- factor(d$elec)
d$momedu <- factor(d$momedu)

d$asset_tv <-relevel(d$asset_tv, ref = "0")
d$elec <-relevel(d$elec, ref = "0")
d$momedu <-relevel(d$momedu, ref = "IncompletePrimary")

d$birthord[is.na(d$birthord)] <- ""

d$staffid1 <- fct_lump_min(d$staffid1, 50, other_level = "inexp")
d$staffid2 <- fct_lump_min(d$staffid2, 50, other_level = "inexp")
d$staffid3 <- fct_lump_min(d$staffid3, 50, other_level = "inexp")

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

#Add in time varying covariates:
# Wvars1<-c("aged1", "month1", "staffid1") 
# Wvars2<-c("aged2", "month2", "staffid2") 
# Wvars3<-c("aged3", "month3", "staffid3") 



#subset time-constant W adjustment set
W<- subset(d, select=Wvars)
W3 <- W2 <- W1 <- W


# #Add in time-varying covariates
# W1<- cbind(W, subset(d, select=Wvars1))
# W2<- cbind(W, subset(d, select=Wvars2))
# W3<- cbind(W, subset(d, select=Wvars3))
# 
# 
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



############################
#Set up ipcw analysis
############################


#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))




#Create indicators for missingness
d$Lact1.miss<-ifelse(is.na(d$Lact1),0,1)
d$Lact2.miss<-ifelse(is.na(d$Lact2),0,1)
d$Lact3.miss<-ifelse(is.na(d$Lact3),0,1)

d$Mann1.miss<-ifelse(is.na(d$Mann1),0,1)
d$Mann2.miss<-ifelse(is.na(d$Mann2),0,1)
d$Mann3.miss<-ifelse(is.na(d$Mann3),0,1)

d$LM1.miss<-ifelse(is.na(d$LM1),0,1)
d$LM2.miss<-ifelse(is.na(d$LM2),0,1)
d$LM3.miss<-ifelse(is.na(d$LM3),0,1)



table(d$Lact1.miss)
table(d$Lact2.miss)
table(d$Lact3.miss)

table(d$Mann1.miss)
table(d$Mann2.miss)
table(d$Mann3.miss)

table(d$LM1.miss)
table(d$LM2.miss)
table(d$LM3.miss)

table(d$Lact1.miss)
table(d$Lact2.miss)
table(d$Lact3.miss)


# set missing outcomes to an arbitrary, non-missing value. In this case use 9
d$Lact1Delta <- d$Lact1
d$Lact1Delta[d$Lact1.miss==0] <- exp(9)

d$Lact2Delta <- d$Lact2
d$Lact2Delta[d$Lact2.miss==0] <- exp(9)

d$Lact3Delta <- d$Lact3
d$Lact3Delta[d$Lact3.miss==0] <- exp(9)

d$Mann1Delta <- d$Mann1
d$Mann1Delta[d$Mann1.miss==0] <- exp(9)

d$Mann2Delta <- d$Mann2
d$Mann2Delta[d$Mann2.miss==0] <- exp(9)

d$Mann3Delta <- d$Mann3
d$Mann3Delta[d$Mann3.miss==0] <- exp(9)

d$LM1Delta <- d$LM1
d$LM1Delta[d$LM1.miss==0] <- exp(9)

d$LM2Delta <- d$LM2
d$LM2Delta[d$LM2.miss==0] <- exp(9)

d$LM3Delta <- d$LM3
d$LM3Delta[d$LM3.miss==0] <- exp(9)




#Order for replication:
d<-d[order(d$block,d$clusterid,d$childid),]
  
#Run the unadjusted ipcw analysis

#dataframes of urine biomarkers and missingness:
Y<-d %>% select(Lact1Delta,Mann1Delta,LM1Delta,
                Lact2Delta,Mann2Delta,LM2Delta,
                Lact3Delta,Mann3Delta,LM3Delta)
miss<-d %>% select(Lact1.miss,Mann1.miss,LM1.miss,
                Lact2.miss,Mann2.miss,LM2.miss,
                Lact3.miss,Mann3.miss,LM3.miss)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))




#Run the adjusted ipcw analysis
res_adj<-list(lact_t1_adj=matrix(0,5,5), mann_t1_adj=matrix(0,5,5), lm_t1_adj=matrix(0,5,5), 
                lact_t2_adj=matrix(0,5,5), mann_t2_adj=matrix(0,5,5), lm_t2_adj=matrix(0,5,5),  
                lact_t3_adj=matrix(0,5,5), mann_t3_adj=matrix(0,5,5), lm_t3_adj=matrix(0,5,5))

 Wlist <- list(W1,W1,W1,W2,W2,W2,W3,W3,W3)


 
for(i in 1:ncol(Y)){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_tmle_ipcw(Y=log(Y[,i]), Delta=miss[,i], tr=d$tr, W=Wlist[[i]], id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], Q.SL.library = c("SL.glm"), seed=12345, print=T)
    cat(i," : ",j, "\n")
    res_adj[[i]][j,]<-(t(unlist(temp$estimates$ATE)))
    colnames(res_adj[[i]])<-c("psi","var.psi","ci.l","ci.u", "Pval")
    rownames(res_adj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}

mean(Y$Lact1Delta)
dim(Y)
table(is.na(Y$Lact1Delta))

#Extract estimates
l1_adj_ipcw_M<-res_adj[[1]]
l2_adj_ipcw_M<-res_adj[[4]]
l3_adj_ipcw_M<-res_adj[[7]]

m1_adj_ipcw_M<-res_adj[[2]]
m2_adj_ipcw_M<-res_adj[[5]]
m3_adj_ipcw_M<-res_adj[[8]]

lmr1_adj_ipcw_M<-res_adj[[3]]
lmr2_adj_ipcw_M<-res_adj[[6]]
lmr3_adj_ipcw_M<-res_adj[[9]]

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Results/Andrew/")
save(
l1_adj_ipcw_M,
l2_adj_ipcw_M,
l3_adj_ipcw_M,
m1_adj_ipcw_M,
m2_adj_ipcw_M,
m3_adj_ipcw_M,
lmr1_adj_ipcw_M,
lmr2_adj_ipcw_M,
lmr3_adj_ipcw_M,
file="urine_ipcw_res.Rdata")




#--------------------------------
# Percent L and M recovery
# (for supplimentary table)
#--------------------------------

#Create indicators for missingness
d$perl1.miss<-ifelse(is.na(d$per.lact.rec_t1),0,1)
d$perl2.miss<-ifelse(is.na(d$per.lact.rec_t2),0,1)
d$perl3.miss<-ifelse(is.na(d$per.lact.rec_t3),0,1)

d$perm1.miss<-ifelse(is.na(d$per.mann.rec_t1),0,1)
d$perm2.miss<-ifelse(is.na(d$per.mann.rec_t2),0,1)
d$perm3.miss<-ifelse(is.na(d$per.mann.rec_t3),0,1)

# set missing outcomes to an arbitrary, non-missing value. In this case use 9
d$perl1Delta <- d$per.lact.rec_t1
d$perl1Delta[d$perl1.miss==0] <- (9)

d$perl2Delta <- d$per.lact.rec_t2
d$perl2Delta[d$perl2.miss==0] <- (9)

d$perl3Delta <- d$per.lact.rec_t3
d$perl3Delta[d$perl3.miss==0] <- (9)

d$perm1Delta <- d$per.mann.rec_t1
d$perm1Delta[d$perm1.miss==0] <- (9)

d$perm2Delta <- d$per.mann.rec_t2
d$perm2Delta[d$perm2.miss==0] <- (9)

d$perm3Delta <- d$per.mann.rec_t3
d$perm3Delta[d$perm3.miss==0] <- (9)

perY<-d %>% select(perl1Delta,perm1Delta,
                perl2Delta,perm2Delta,
                perl3Delta,perm3Delta)
per.miss<-d %>% select(perl1.miss,perm1.miss,
                perl2.miss,perm2.miss,
                perl3.miss,perm3.miss)

res_per<-list(perl_t1_adj=matrix(0,5,5), perm_t1_adj=matrix(0,5,5), 
                perl_t2_adj=matrix(0,5,5), perm_t2_adj=matrix(0,5,5), 
                perl_t3_adj=matrix(0,5,5), perm_t3_adj=matrix(0,5,5))


perWlist <- list(W1,W1,W2,W2,W3,W3)


for(i in 1:ncol(perY)){
  for(j in 1:5){
    temp<-washb_tmle_ipcw(Y=(perY[,i]), Delta=per.miss[,i], tr=d$tr, W=perWlist[[i]], id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], Q.SL.library = c("SL.glm"), seed=12345, print=T)
    cat(i," : ",j, "\n")
    res_per[[i]][j,]<-(t(unlist(temp$estimates$ATE)))
    colnames(res_per[[i]])<-c("psi","var.psi","ci.l","ci.u", "Pval")
    rownames(res_per[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}



#Extract estimates
perl1_adj_ipcw_M<-res_per[[1]]
perl2_adj_ipcw_M<-res_per[[3]]
perl3_adj_ipcw_M<-res_per[[5]]

perm1_adj_ipcw_M<-res_per[[2]]
perm2_adj_ipcw_M<-res_per[[4]]
perm3_adj_ipcw_M<-res_per[[6]]


save(perl1_adj_ipcw_M,
     perl2_adj_ipcw_M,
     perl3_adj_ipcw_M,
     perm1_adj_ipcw_M,
     perm2_adj_ipcw_M,
     perm3_adj_ipcw_M,
     file="pre_recovery_ipcw_res_M.Rdata"
     )





