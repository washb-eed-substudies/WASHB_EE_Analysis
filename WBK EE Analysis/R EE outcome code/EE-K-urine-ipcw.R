
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
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew")
tr <- read.csv("raw CSV/washk_TR.csv")
tr$tr <- factor(tr$tr, levels = c("Control",  "WSH", "Nutrition", "Nutrition + WSH"))
head(tr)

dob <- readRDS("WBK-EE-childDOB.rds")
#use main trial DOB and sex
dob <- dob %>% subset(., select = -c(sex, DOB))

lm <- readRDS("WBK-EE-LM-outcomes.rds")
head(lm)

#Load in main trial enrollment data for ipcw analysis
#enrol<-read.csv("C:/Users/andre/Dropbox/washb_Kenya_primary_outcomes_Andrew/Data-selected/clean/washb-kenya-enrol.csv",stringsAsFactors = TRUE)
enrol<-read.dta("C:/Users/andre/Dropbox/washb_Kenya_primary_outcomes_Andrew/Data-selected/clean/washb-kenya-enrol.dta")
head(enrol)

d <- left_join(lm, dob, by=c("childid","hhid"))

dim(d)
d <- left_join(enrol, d, by="childid")
dim(d)


#Calculate child age and month of the year at each measurement
d <- d %>% 
        mutate(aged1= urine_bl_date-DOB,
               aged2= urine_ml_date-DOB,
               aged3= urine_el_date-DOB,
               agem1= as.numeric(aged1/30.25), 
               agem2= as.numeric(aged2/30.25), 
               agem3= as.numeric(aged3/30.25),
               month1= month(d$urine_bl_date),
               month2= month(d$urine_ml_date),
               month3= month(d$urine_el_date))




#Single urine outcome not matched to main trial data
#anti_join(urine_outcomes, ipcw, by=c("dataid", "childNo")) %>% subset(select=c(dataid, childNo))



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

# Set factor variables
d$asset_tv <- factor(d$asset_tv)
d$elec <- factor(d$elec)
d$momedu <- factor(d$momedu)

d$asset_tv <-relevel(d$asset_tv, ref = "0")
d$elec <-relevel(d$elec, ref = "0")
d$momedu <-relevel(d$momedu, ref = "Incomplete Primary")

d$month1 <- factor(d$month1)
d$month2 <- factor(d$month2)
d$month3 <- factor(d$month3)
d$staffid1 <- factor(d$staffid1)
d$staffid2 <- factor(d$staffid2)
d$staffid3 <- factor(d$staffid3)


#Make vectors of adjustment variable names
Wvars<-c("sex", "birthord",  "momage", "momedu",  "Ncomp", "Nlt18", "elec","roof",
         "momheight",
         "asset_radio", "asset_tv", "asset_mobile", "asset_clock", "asset_bike", "asset_moto", "asset_stove",  
         "n_cows", "n_goats","n_chickens", "n_dogs", "watmin", "hfiacat")

#Add in time varying covariates:
Wvars1<-c("aged1", "month1", "staffid1") 
Wvars2<-c("aged2", "month2", "staffid2") 
Wvars3<-c("aged3", "month3", "staffid3") 



#subset time-constant W adjustment set
W<- subset(d, select=Wvars)


#Add in time-varying covariates
W1<- cbind(W, subset(d, select=Wvars1))
W2<- cbind(W, subset(d, select=Wvars2))
W3<- cbind(W, subset(d, select=Wvars3))


#Set time-varying covariates as factors
W1$month1<-as.factor(W1$month1)
W2$month2<-as.factor(W2$month2)
W3$month3<-as.factor(W3$month3)
W1$staffid1<-factor(W1$staffid1)
W2$staffid2<-factor(W2$staffid2)
W3$staffid3<-factor(W3$staffid3)



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


mean(d$Lact1.miss, na.rm=T)


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
    temp<-washb_tmle(Y=log(Y[,i]), Delta=miss[,i], tr=d$tr, W=Wlist[[i]], id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], Q.SL.library = c("SL.glm"), seed=12345, print=T)
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
    temp<-washb_tmle(Y=(perY[,i]), Delta=per.miss[,i], tr=d$tr, W=perWlist[[i]], id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], Q.SL.library = c("SL.glm"), seed=12345, print=T)
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


















































#Impute time varying covariates

#calculate overall median:
month1_median1 <-    median(d$month1, na.rm = T)
month1_median2 <-    median(d$month2, na.rm = T)
month1_median3 <-    median(d$month3, na.rm = T)

#use clusterid to impute median month where possible
table(d$month1)
table(is.na(d$month1))
d$month1[is.na(d$month1)] <-  ave(d$month1, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month1)] 
d$month1 <- ceiling(d$month1)
table(d$month1)
table(is.na(d$month1))

d$month2[is.na(d$month2)] <-  ave(d$month2, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month2)] 
d$month2 <- ceiling(d$month2)

d$month3[is.na(d$month3)] <-  ave(d$month3, d$clusterid, FUN=function(x) median(x, na.rm = T))[is.na(d$month3)] 
d$month3 <- ceiling(d$month3)


#impute month with overall median for those observations not in a cluster measured in the EED subsample
d$month1[is.na(d$month1)] <-  month1_median1
d$month2[is.na(d$month2)] <-  month1_median2
d$month3[is.na(d$month3)] <-  month1_median3
table(d$month1)


#impute child age with overall median
# d$aged1[is.na(d$aged1)] <- median(d$aged1, na.rm = T)
# d$aged2[is.na(d$aged2)] <- median(d$aged2, na.rm = T)
# d$aged3[is.na(d$aged3)] <- median(d$aged3, na.rm = T)

d$aged1[is.na(d$aged1)] <- 84
d$aged2[is.na(d$aged2)] <- 428
d$aged3[is.na(d$aged3)] <- 857

summary(d$aged1)
table(d$aged1)
summary(d$aged1[d$tr=="Control"])
summary(d$aged1[d$tr=="WSH"])


#Mark missing staffid
d$staffid1[is.na(d$staffid1)] <- "missing"
d$staffid2[is.na(d$staffid2)] <- "missing"
d$staffid3[is.na(d$staffid3)] <- "missing"

#Truncate staffid at <100
table(rbind(d$staffid1,d$staffid2,d$staffid3))
names(table(rbind(d$staffid1,d$staffid2,d$staffid3)))

#Which staff ids had <100 samples collected
inexp_staff_id<-names(which(table(rbind(d$staffid1,d$staffid2,d$staffid3))<100))
inexp_staff_id
#Assign new category to inexperienced IDs across the 3 staffid-round variables
d$staffid1[d$staffid1 %in% inexp_staff_id]<-"inexp"
d$staffid2[d$staffid2 %in% inexp_staff_id]<-"inexp"
d$staffid3[d$staffid3 %in% inexp_staff_id]<-"inexp"



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
d$walls<-factor(d$walls)

#Re-subset W so new re-leveled factors are included
W<- subset(d, select=Wvars)

#Add in time-varying covariates
Wvars1<-c("aged1", "month1", "staffid1") 
Wvars2<-c("aged2", "month2", "staffid2") 
Wvars3<-c("aged3", "month3", "staffid3") 
W1<- cbind(W, subset(d, select=Wvars1))
W2<- cbind(W, subset(d, select=Wvars2))
W3<- cbind(W, subset(d, select=Wvars3))

#Replace missingness in time varying covariates as a new level
W1$month1[is.na(W1$month1)]<-"missing"
W2$month2[is.na(W2$month2)]<-"missing"
W3$month3[is.na(W3$month3)]<-"missing"
W1$staffid1[is.na(W1$staffid1)]<-"missing"
W2$staffid2[is.na(W2$staffid2)]<-"missing"
W3$staffid3[is.na(W3$staffid3)]<-"missing"


#Set time-varying covariates as factors
W1$month1<-as.factor(W1$month1)
W2$month2<-as.factor(W2$month2)
W3$month3<-as.factor(W3$month3)
W1$staffid1<-factor(W1$staffid1)
W2$staffid2<-factor(W2$staffid2)
W3$staffid3<-factor(W3$staffid3)

W2$month2 <- relevel(W2$month2, ref="1")
W2$staffid2 <- relevel(W2$staffid2, ref="missing")
W2 <- droplevels(W2)

W3$month3 <- relevel(W3$month3, ref="1")
W3$staffid3 <- relevel(W3$staffid3, ref="missing")
W3 <- droplevels(W3)

W1$month1 <- relevel(W1$month1, ref="1")
W1$staffid1 <- relevel(W1$staffid1, ref="missing")
W1 <- droplevels(W1)

