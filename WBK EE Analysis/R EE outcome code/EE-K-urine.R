
#---------------------------------------
# EE-K-urine.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The urine-based biomarker outcomes for 
# EED Kenya sub-study
#---------------------------------------



###Load in data
rm(list=ls())
library(tidyverse)
library(washb)
library(lubridate)


#Load in blinded treatment information
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew")
#tr <- read.csv("raw CSV/washk_blindTR.csv")
tr <- read.csv("raw CSV/washk_TR.csv")
tr$tr <- factor(tr$tr, levels = c("Control",  "WSH", "Nutrition", "Nutrition + WSH"))
head(tr)

dob <- readRDS("WBK-EE-childDOB.rds")

lm <- readRDS("WBK-EE-LM-outcomes.rds")
head(lm)

enrol <- readRDS("WBK-EE-covariates.rds")
head(enrol)

d <- left_join(lm, dob, by=c("childid","hhid"))

d <- left_join(d, enrol, by="hhid")

d <- left_join(d, tr, by="clusterid")


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
               


#--------------------------------
# Urine outcome analysis
#--------------------------------


#Calculate average age across arms at followup time 1, 2, and 3
#Survey 1
#Tabulate overall N, gender, and age 
overallN1<-d%>% subset(!(is.na(d$ln_lact1)|is.na(d$ln_mann1))) %>% summarize(N=n(),Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"
#subset(!is.na(h2aliqout1_t1) & h2aliqout1_t1>1 | !is.na(h5aliqout7_t1) & h5aliqout7_t1>1) 

#Tabulate N, gender, and age across survey rounds
t1<-d %>% subset(!(is.na(d$ln_lact1)|is.na(d$ln_mann1))) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d %>% subset(!(is.na(d$ln_lact2)|is.na(d$ln_mann2))) %>% summarize(N=n(),Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>% subset(!(is.na(d$ln_lact2)|is.na(d$ln_mann2))) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% subset(!(is.na(d$ln_lact3)|is.na(d$ln_mann3))) %>% summarize(N=n(),Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d %>% subset(!(is.na(d$ln_lact3)|is.na(d$ln_mann3))) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 


age_t1_urine_M<-rbind(overallN1, t1)
age_t2_urine_M<-rbind(overallN2, t2)
age_t3_urine_M<-rbind(overallN3, t3)


#Reorder to match Audrie
age_t1_urine_M<-age_t1_urine_M[,c(1,2,4,3,5,7,6)]
age_t2_urine_M<-age_t2_urine_M[,c(1,2,4,3,5,7,6)]
age_t3_urine_M<-age_t3_urine_M[,c(1,2,4,3,5,7,6)]

table(d$vlgid)



#------------------
#N's and geometric means
#------------------


#Create empty  matrices to hold the Ns and geometric means:
lac_t1_N_M<-man_t1_N_M<-lm_t1_N_M<-lac_t2_N_M<-man_t2_N_M<-lm_t2_N_M<-lac_t3_N_M<-man_t3_N_M<-lm_t3_N<-matrix(0,4,2)


  #N's and geometric means
lac_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(Lact1)) %>% summarize(N=n(), mean= mean(log(Lact1), na.rm=T))   
man_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(Mann1)) %>% summarize(N=n(), mean= mean(log(Mann1), na.rm=T))   
lm_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(LM1)) %>% summarize(N=n(), mean= mean(log(LM1), na.rm=T))   
lac_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(Lact2)) %>% summarize(N=n(), mean= mean(log(Lact2), na.rm=T))   
man_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(Mann2)) %>% summarize(N=n(), mean= mean(log(Mann2), na.rm=T))   
lm_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(LM2)) %>% summarize(N=n(), mean= mean(log(LM2), na.rm=T))
lac_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(Lact3)) %>% summarize(N=n(), mean= mean(log(Lact3), na.rm=T))   
man_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(Mann3)) %>% summarize(N=n(), mean= mean(log(Mann3), na.rm=T))   
lm_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(LM3)) %>% summarize(N=n(), mean= mean(log(LM3), na.rm=T))   

lac_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Lact1, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lac_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Lact2, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lac_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Lact3, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Mann1, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Mann2, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$Mann3, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$LM1, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$LM2, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$LM3, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

#------------------
#N Log-transformed means
#------------------

#transform outcomes
d$ln_lact1 <- log(d$Lact1)
d$ln_lact2 <- log(d$Lact2)
d$ln_lact3 <- log(d$Lact3)
d$ln_mann1 <- log(d$Mann1)
d$ln_mann2 <- log(d$Mann2) 
d$ln_mann3 <- log(d$Mann3) 
d$ln_LM1 <- log(d$LM1)
d$ln_LM2 <- log(d$LM2)
d$ln_LM3 <- log(d$LM3) 

#Means and 95% CI's for mean by arm plots
lac_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ln_lact1, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lac_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ln_lact2, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lac_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ln_lact3, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ln_mann1, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ln_mann2, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
man_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ln_mann3, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ln_LM1, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ln_LM2, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
lm_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=.$ln_LM3, id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 



#Means and 95% CI's not stratified by arm
overall_mn_by_round<- 
  d %>% subset(., select=c(childid, block, Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3,per.mann.rec_t1,per.mann.rec_t2,per.mann.rec_t3, per.lact.rec_t1, per.lact.rec_t2, per.lact.rec_t3)) %>%
  rename(pm1=per.mann.rec_t1, pm2=per.mann.rec_t2, pm3=per.mann.rec_t3, pl1=per.lact.rec_t1, pl2=per.lact.rec_t2,  pl3=per.lact.rec_t3) %>%
  gather(key, value, -childid, -block) %>%
  mutate(biomarker = substr(key, 1,2)) %>% 
  group_by(biomarker) %>% 
  do(as.data.frame(washb_mean(Y=.$value, id=.$block, print = F))) %>% 
  ungroup %>% as.data.frame

overall_mn<- 
  d %>% subset(., select=c(childid, block, Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3,per.mann.rec_t1,per.mann.rec_t2,per.mann.rec_t3, per.lact.rec_t1, per.lact.rec_t2, per.lact.rec_t3)) %>%
  gather(key, value, -childid, -block) %>%
  group_by(key) %>% 
  do(as.data.frame(washb_mean(Y=.$value, id=.$block, print = F))) %>% 
  ungroup %>% as.data.frame
colnames(overall_mn)[1]<-"biomarker"
urine_overall_mn <- rbind(overall_mn_by_round, overall_mn)
urine_overall_mn






#------------------
#Unadjusted GLM
#------------------

#dataframe of urine biomarkers:
Y<-d %>% select(ln_lact1,ln_mann1,ln_LM1,ln_lact2,ln_mann2,ln_LM2,ln_lact3,ln_mann3,ln_LM3)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))


#Create empty matrix to hold the glm results:
lact_t1_unadj<-mann_t1_unadj<-lm_t1_unadj<-matrix(0, nrow=5, ncol=6)
lact_t2_unadj<-mann_t2_unadj<-lm_t2_unadj<-matrix(0, nrow=5, ncol=6)
lact_t3_unadj<-mann_t3_unadj<-lm_t3_unadj<-matrix(0, nrow=5, ncol=6)

res_unadj<-list(lact_t1_unadj=lact_t1_unadj, mann_t1_unadj=mann_t1_unadj, lm_t1_unadj=lm_t1_unadj, 
                lact_t2_unadj=lact_t2_unadj, mann_t2_unadj=mann_t2_unadj, lm_t2_unadj=lm_t2_unadj, 
                lact_t3_unadj=lact_t3_unadj, mann_t3_unadj=mann_t3_unadj, lm_t3_unadj=lm_t3_unadj)




#Unadjusted glm models
for(i in 1:ncol(Y)){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=Y[,i], tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_unadj[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_unadj[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_unadj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}


#------------------
#Age and sex adjusted GLMs
#------------------
d$sex<-as.factor(d$sex)
  d$sex=relevel(d$sex,ref="0")

#Create empty matrix to hold the glm results:
lact_t1_sex<-mann_t1_sex<-lm_t1_sex<-matrix(0, nrow=5, ncol=6)
lact_t2_sex<-mann_t2_sex<-lm_t2_sex<-matrix(0, nrow=5, ncol=6)
lact_t3_sex<-mann_t3_sex<-lm_t3_sex<-matrix(0, nrow=5, ncol=6)

res_diff <- res_month <- res_sex <- list(lact_t1_sex=lact_t1_sex, mann_t1_sex=mann_t1_sex, lm_t1_sex=lm_t1_sex, 
                lact_t2_sex=lact_t2_sex, mann_t2_sex=mann_t2_sex, lm_t2_sex=lm_t2_sex, 
                lact_t3_sex=lact_t3_sex, mann_t3_sex=mann_t3_sex, lm_t3_sex=lm_t3_sex)


#Age and sex adjusted glm models
for(i in 1:3){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=Y[,i], tr=d$tr, W=cbind(d$sex, d$aged1), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
for(i in 4:6){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=Y[,i], tr=d$tr, W=cbind(d$sex, d$aged2), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
for(i in 7:9){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=Y[,i], tr=d$tr, W=cbind(d$sex, d$aged3), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}


#------------------
#Adjusted GLM
#------------------

#Categorize maternal height
summary(d$momheight)
d$mht_cat <- as.character(ntile(d$momheight,4))
d$mht_cat[is.na(d$mht_cat)] <- "missing"
d$mht_cat <- factor(d$mht_cat, levels=c("1","2","3","4","missing"))
table(d$mht_cat)


# Set factor variables

d$asset_tv <-relevel(d$asset_tv, ref = "Missing/DK")
d$elec <-relevel(d$elec, ref = "Has electricity")
d$momedu <-relevel(d$momedu, ref = "")

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
d$staffid1 <- fct_lump_min(d$staffid1, 50, other_level = "inexp")
d$staffid2 <- fct_lump_min(d$staffid2, 50, other_level = "inexp")
d$staffid3 <- fct_lump_min(d$staffid3, 50, other_level = "inexp")

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




##############################################
#Run GLMs for the adjusted parameter estimates
##############################################


#Create empty matrix to hold the tmle results:
res_adj<-list(lact_t1_adj=matrix(0,5,6), mann_t1_adj=matrix(0,5,6), lm_t1_adj=matrix(0,5,6), 
                lact_t2_adj=matrix(0,5,6), mann_t2_adj=matrix(0,5,6), lm_t2_adj=matrix(0,5,6),  
                lact_t3_adj=matrix(0,5,6), mann_t3_adj=matrix(0,5,6), lm_t3_adj=matrix(0,5,6))

for(i in 1:3){
  for(j in 1:5){
  temp<-washb_glm(Y=Y[,i], tr=d$tr, W=W1, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}
for(i in 4:6){
  for(j in 1:5){
  temp<-washb_glm(Y=Y[,i], tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}
for(i in 7:9){
  for(j in 1:5){
  temp<-washb_glm(Y=Y[,i], tr=d$tr, W=W3, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}


washb_glm(Y=Y[,i], tr=d$tr, W=W1 %>% subset(., select = -c(staffid1, month1, aged1)), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)$TR
washb_glm(Y=Y[,i], tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)$TR


#------------------
#Save objects
#------------------
lac_t1_unadj_M=res_unadj[[1]]
man_t1_unadj_M=res_unadj[[2]]
lm_t1_unadj_M=res_unadj[[3]] 
lac_t2_unadj_M=res_unadj[[4]] 
man_t2_unadj_M=res_unadj[[5]]
lm_t2_unadj_M=res_unadj[[6]] 
lac_t3_unadj_M=res_unadj[[7]]
man_t3_unadj_M=res_unadj[[8]]
lm_t3_unadj_M=res_unadj[[9]]

lac_t1_adj_sex_age_M=res_sex[[1]]
man_t1_adj_sex_age_M=res_sex[[2]]
lm_t1_adj_sex_age_M=res_sex[[3]] 
lac_t2_adj_sex_age_M=res_sex[[4]] 
man_t2_adj_sex_age_M=res_sex[[5]]
lm_t2_adj_sex_age_M=res_sex[[6]]
lac_t3_adj_sex_age_M=res_sex[[7]]
man_t3_adj_sex_age_M=res_sex[[8]]
lm_t3_adj_sex_age_M=res_sex[[9]]

lac_t1_adj_M=res_adj[[1]]
man_t1_adj_M=res_adj[[2]]
lm_t1_adj_M=res_adj[[3]] 
lac_t2_adj_M=res_adj[[4]] 
man_t2_adj_M=res_adj[[5]]
lm_t2_adj_M=res_adj[[6]]
lac_t3_adj_M=res_adj[[7]]
man_t3_adj_M=res_adj[[8]]
lm_t3_adj_M=res_adj[[9]]


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Results/Andrew/")
save(lac_t1_N_M, man_t1_N_M, lm_t1_N_M,
     lac_t2_N_M, man_t2_N_M, lm_t2_N_M,
     lac_t3_N_M, man_t3_N_M,lm_t3_N_M,
     file="urine_res_N_M.Rdata")

save(lm_t1_mn, lm_t2_mn, lm_t3_mn,
     lac_t1_mn, lac_t2_mn, lac_t3_mn, 
     man_t1_mn, man_t2_mn, man_t3_mn, 
     lm_t1_absmn, lm_t2_absmn, lm_t3_absmn,
     lac_t1_absmn, lac_t2_absmn, lac_t3_absmn, 
     man_t1_absmn, man_t2_absmn, man_t3_absmn, 
     file="urine_res_means.Rdata")

save(urine_overall_mn, file="urine_overall_means.Rdata")

save(lac_t1_unadj_M, man_t1_unadj_M, lm_t1_unadj_M,
     lac_t2_unadj_M, man_t2_unadj_M, lm_t2_unadj_M, 
     lac_t3_unadj_M,man_t3_unadj_M, lm_t3_unadj_M,
     file="urine_res_unadj_M.Rdata")


save(lac_t1_adj_sex_age_M, man_t1_adj_sex_age_M, lm_t1_adj_sex_age_M,
     lac_t2_adj_sex_age_M, man_t2_adj_sex_age_M, lm_t2_adj_sex_age_M, 
     lac_t3_adj_sex_age_M,man_t3_adj_sex_age_M, lm_t3_adj_sex_age_M,
     file="urine_res_adj_sex_age_M.Rdata")

save(lac_t1_adj_M, man_t1_adj_M, lm_t1_adj_M,
     lac_t2_adj_M, man_t2_adj_M, lm_t2_adj_M, 
     lac_t3_adj_M,man_t3_adj_M, lm_t3_adj_M,
     file="urine_res_adj_M.Rdata")


#save data for figures
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Temp/")
save(d, file="urine_figure_data.Rdata")

#save cleaned dataset
write.csv(d, file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/washb-kenya-eed-urine.csv")

#--------------------------------
# Percent L and M recovery
# (for supplimentary table)
#--------------------------------

#N's and geometric means
perl1_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.mann.rec_t1)) %>% summarize(N=n(), mean= exp(mean(log(per.mann.rec_t1), na.rm=T)))   
perl2_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.mann.rec_t2)) %>% summarize(N=n(), mean= exp(mean(log(per.mann.rec_t2), na.rm=T)))  
perl3_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.mann.rec_t3)) %>% summarize(N=n(), mean= exp(mean(log(per.mann.rec_t3), na.rm=T)))   

perm1_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.lact.rec_t1)) %>% summarize(N=n(), mean= exp(mean(log(per.lact.rec_t1), na.rm=T)))  
perm2_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.lact.rec_t2)) %>% summarize(N=n(), mean= exp(mean(log(per.lact.rec_t2), na.rm=T))) 
perm3_N_M<-d %>% group_by(tr) %>% subset(!is.na(per.lact.rec_t3)) %>% summarize(N=n(), mean= exp(mean(log(per.lact.rec_t3), na.rm=T)))



#Means and 95% CI's for mean by arm plots
perl1_mn<-perl2_mn<-perl3_mn<-perm1_mn<-perm2_mn<-perm3_mn<-NULL
perl1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.lact.rec_t1), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
perl2_mn<-d  %>% subset(!is.infinite(per.lact.rec_t2) & !is.na(per.lact.rec_t2)) %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.lact.rec_t2), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
perl3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.lact.rec_t3), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

perm1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.mann.rec_t1), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
perm2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.mann.rec_t2), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
perm3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$per.mann.rec_t3), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 

#Convert from log to raw scale to get the geometric mean
perl1_mn<-perl1_mn[,-c(3:4)]
perl2_mn<-perl2_mn[,-c(3:4)]
perl3_mn<-perl3_mn[,-c(3:4)]
perm1_mn<-perm1_mn[,-c(3:4)]
perm2_mn<-perm2_mn[,-c(3:4)]
perm3_mn<-perm3_mn[,-c(3:4)]

perl1_mn<-exp(perl1_mn[,c(2:4)])
perl2_mn<-exp(perl2_mn[,c(2:4)])
perl3_mn<-exp(perl3_mn[,c(2:4)])
perm1_mn<-exp(perm1_mn[,c(2:4)])
perm2_mn<-exp(perm2_mn[,c(2:4)])
perm3_mn<-exp(perm3_mn[,c(2:4)])


perl1_unadj_M<-perl2_unadj_M<-perl3_unadj_M<-matrix(0, nrow=5, ncol=6)
perm1_unadj_M<-perm2_unadj_M<-perm3_unadj_M<-matrix(0, nrow=5, ncol=6)
perl1_adj_sex_age_M<-perl2_adj_sex_age_M<-perl3_adj_sex_age_M<-matrix(0, nrow=5, ncol=6)
perm1_adj_sex_age_M<-perm2_adj_sex_age_M<-perm3_adj_sex_age_M<-matrix(0, nrow=5, ncol=6)
perl1_adj_M<-perl2_adj_M<-perl3_adj_M<-matrix(0, nrow=5, ncol=6)
perm1_adj_M<-perm2_adj_M<-perm3_adj_M<-matrix(0, nrow=5, ncol=6)

colnames(perl1_unadj_M)<-colnames(perl2_unadj_M)<-colnames(perl3_unadj_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
colnames(perm1_unadj_M)<-colnames(perm2_unadj_M)<-colnames(perm3_unadj_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
colnames(perl1_adj_sex_age_M)<-colnames(perl2_adj_sex_age_M)<-colnames(perl3_adj_sex_age_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
colnames(perm1_adj_sex_age_M)<-colnames(perm2_adj_sex_age_M)<-colnames(perm3_adj_sex_age_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
colnames(perl1_adj_M)<-colnames(perl2_adj_M)<-colnames(perl3_adj_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
colnames(perm1_adj_M)<-colnames(perm2_adj_M)<-colnames(perm3_adj_M)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")

rownames(perl1_unadj_M)<-rownames(perl2_unadj_M)<-rownames(perl3_unadj_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")
rownames(perm1_unadj_M)<-rownames(perm2_unadj_M)<-rownames(perm3_unadj_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")
rownames(perl1_adj_sex_age_M)<-rownames(perl2_adj_sex_age_M)<-rownames(perl3_adj_sex_age_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")
rownames(perm1_adj_sex_age_M)<-rownames(perm2_adj_sex_age_M)<-rownames(perm3_adj_sex_age_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")
rownames(perl1_adj_M)<-rownames(perl2_adj_M)<-rownames(perl3_adj_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")
rownames(perm1_adj_M)<-rownames(perm2_adj_M)<-rownames(perm3_adj_M)<-c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH")

  for(j in 1:5){
    perl1_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t1, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl2_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t2, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl3_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t3, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm1_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t1, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm2_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t2, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm3_unadj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t3, tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
 
    perl1_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t1, tr=d$tr, W=cbind(d$sex, d$aged1), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl2_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t2, tr=d$tr, W=cbind(d$sex, d$aged2), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl3_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t3, tr=d$tr, W=cbind(d$sex, d$aged3), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm1_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t1, tr=d$tr, W=cbind(d$sex, d$aged1), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm2_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t2, tr=d$tr, W=cbind(d$sex, d$aged2), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm3_adj_sex_age_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t3, tr=d$tr, W=cbind(d$sex, d$aged3), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
 
    perl1_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t1, tr=d$tr, W=W1, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl2_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t2, tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perl3_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.lact.rec_t3, tr=d$tr, W=W3, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm1_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t1, tr=d$tr, W=W1, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm2_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t2, tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
    perm3_adj_M[j,]<-as.numeric(washb_glm(Y=d$per.mann.rec_t3, tr=d$tr, W=W3, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)$TR)
  }





setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Results/Andrew/")

save(
perl1_N_M,
perl2_N_M, 
perl3_N_M, 
perm1_N_M,  
perm2_N_M,
perm3_N_M,  
perl1_mn,
perl2_mn,
perl3_mn,
perm1_mn,
perm2_mn,
perm3_mn,
perl1_unadj_M,perl2_unadj_M,perl3_unadj_M,
perm1_unadj_M,perm2_unadj_M,perm3_unadj_M,
perl1_adj_sex_age_M,perl2_adj_sex_age_M,perl3_adj_sex_age_M,
perm1_adj_sex_age_M,perm2_adj_sex_age_M,perm3_adj_sex_age_M,
perl1_adj_M,perl2_adj_M,perl3_adj_M,
perm1_adj_M,perm2_adj_M,perm3_adj_M,
     file="pre_recovery_res_M.Rdata")

