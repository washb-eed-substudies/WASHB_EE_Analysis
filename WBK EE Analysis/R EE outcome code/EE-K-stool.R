

#---------------------------------------
# EE-BD-stool.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The stool-based biomarker outcomes for 
# EED Bangladesh sub-study
#---------------------------------------


rm(list=ls())
library(tidyverse)
library(foreign)
library(washb)

#Load in blinded treatment information
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew")
tr <- read.csv("raw CSV/washk_blindTR.csv")
#tr <- read.csv("raw CSV/washk_TR.csv")
tr$tr <- factor(tr$tr, levels = c("Control",  "WSH", "Nutrition", "Nutrition + WSH"))
head(tr)

#Child dates of birth
dob <- readRDS("WBK-EE-childDOB.rds")

#Stool outcomes
outcomes<-read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_ee_stool.csv")
head(outcomes)

# #divide the reg value by 1000 to convert it to ug/ml 
# outcomes$t2_reg<-outcomes$t2_reg/1000
# 
# #divide all the aat values by 1000000 to convert it to mg/g
# outcomes$t1_aat<-outcomes$t1_aat/1000000
# outcomes$t2_aat<-outcomes$t2_aat/1000000
# outcomes$t3_aat<-outcomes$t3_aat/1000000

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

#Baseline covariates
enrol <- readRDS("WBK-EE-covariates.rds")
head(enrol)

d <- left_join(outcomes, dob, by="childid")

d <- left_join(d, enrol, by="hhid")

d <- left_join(d, tr, by="clusterid")


#Calculate child age and month of the year at each measurement
d <- d %>% 
        mutate(aged1= stool_bl_date-DOB,
               aged2= stool_ml_date-DOB,
               aged3= stool_el_date-DOB,
               agem1= as.numeric(aged1/30.25), 
               agem2= as.numeric(aged2/30.25), 
               agem3= as.numeric(aged3/30.25),
               month1= month(d$stool_bl_date),
               month2= month(d$stool_ml_date),
               month3= month(d$stool_el_date))
               



############################
# TEMP fixes to diagnose
############################

#drop outcomes not merged to treatment arm

table(is.na(d$tr))
d$childid[is.na(d$tr)]

d <- d[!is.na(d$tr),]
#XXXXXXXXXXXXXXXXXXXXXXXXXXX



# 
# #table number of fully collected aliqouts by arm and year (May not match with the actually available
# #outcomes due to mismarked aliquots)
# head(d)
# table(d$tr)
# 
# #aliqout time 1
# t1s1<-d%>% subset(aliqout1_t1>1)%>%group_by(tr) %>%summarize(sample1=n()) 
# t1s2<-d%>% subset(aliqout2_t1>1)%>%group_by(tr) %>%summarize(sample2=n()) 
# t1s3<-d%>% subset(aliqout3_t1>1)%>%group_by(tr) %>%summarize(sample3=n()) 
# t1s4<-d%>% subset(aliqout4_t1>1)%>%group_by(tr) %>%summarize(sample4=n()) 
# t1s5<-d%>% subset(aliqout5_t1>1)%>%group_by(tr) %>%summarize(sample5=n()) 
#  
# aliquotN_t1<-cbind(t1s1,t1s2[,2],t1s3[,2],t1s4[,2],t1s5[,2])
# 
# 
# #aliqout time 2
# t2s1<-d%>% subset(aliqout1_t2>1)%>%group_by(tr) %>%summarize(sample1=n()) 
# t2s2<-d%>% subset(aliqout2_t2>1)%>%group_by(tr) %>%summarize(sample2=n()) 
# t2s3<-d%>% subset(aliqout3_t2>1)%>%group_by(tr) %>%summarize(sample3=n()) 
# t2s4<-d%>% subset(aliqout4_t2>1)%>%group_by(tr) %>%summarize(sample4=n()) 
# t2s5<-d%>% subset(aliqout5_t2>1)%>%group_by(tr) %>%summarize(sample5=n()) 
#  
# aliquotN_t2<-cbind(t2s1,t2s2[,2],t2s3[,2],t2s4[,2],t2s5[,2])
# aliquotN_t2
# 
# #aliqout time 3
# t3s1<-d%>% subset(aliqout1_t3>1)%>%group_by(tr) %>%summarize(sample1=n()) 
# t3s2<-d%>% subset(aliqout2_t3>1)%>%group_by(tr) %>%summarize(sample2=n()) 
# t3s3<-d%>% subset(aliqout3_t3>1)%>%group_by(tr) %>%summarize(sample3=n()) 
# t3s4<-d%>% subset(aliqout4_t3>1)%>%group_by(tr) %>%summarize(sample4=n()) 
# t3s5<-d%>% subset(aliqout5_t3>1)%>%group_by(tr) %>%summarize(sample5=n()) 
# 
# aliquotN_t3<-cbind(t3s1,t3s2[,2],t3s3[,2],t3s4[,2],t3s5[,2])
# 
# 
# aliquotN_t1
# aliquotN_t2
# aliquotN_t3




#Survey 1
#Tabulate overall N, gender, and age 
overallN1<-d%>% summarize(N=n(), Mean_agem=mean(agem1, na.rm=T) ,Median_agem=median(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), numfemales=n()-sum(sex), nummales=sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t1<-d%>% group_by(tr) %>%summarize(N=n(), Mean_agem=mean(agem1, na.rm=T), Median_agem=median(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), numfemales=n()-sum(sex), nummales=sum(sex)) 
#subset((!is.na(aat1)|!is.na(mpo1)|!is.na(neo1))&!is.na(agem1)) %>%

#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d%>% summarize(N=n(), Mean_agem=mean(agem2, na.rm=T), Median_agem=median(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), numfemales=n()-sum(sex), nummales=sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>%  group_by(tr) %>%summarize(N=n(), Mean_agem=mean(agem2, na.rm=T), Median_agem=median(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), numfemales=n()-sum(sex), nummales=sum(sex)) 


#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% summarize(N=n(), Mean_agem=mean(agem3, na.rm=T), Median_agem=median(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), numfemales=n()-sum(sex, na.rm=T), nummales=sum(sex, na.rm=T)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d%>% group_by(tr) %>%summarize(N=n(), Mean_agem=mean(agem3, na.rm=T), Median_agem=median(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), numfemales=n()-sum(sex, na.rm=T), nummales=sum(sex, na.rm=T)) 


age_t1_stool_M<-rbind(overallN1, t1)
age_t2_stool_M<-rbind(overallN2, t2)
age_t3_stool_M<-rbind(overallN3, t3)



############################
#Calculate outcomes:
############################

#dataframe of stool biomarkers:
Y<-d %>% select(neo1,mpo1,aat1,neo2,mpo2,aat2,neo3,mpo3,aat3)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))


#Create empty  matrices to hold the Ns and geometric means:
neo_t1_N_M<-mpo_t1_N_M<-aat_t1_N_M<-neo_t2_N_M<-mpo_t2_N_M<-aat_t2_N_M<-neo_t3_N_M<-mpo_t3_N_M<-aat_t3_N<-matrix(0,4,2)


  #N's and geometric means
aat_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(aat1)) %>% summarize(N=n(), mean= mean(log(aat1), na.rm=T))   
mpo_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(mpo1)) %>% summarize(N=n(), mean= mean(log(mpo1), na.rm=T))   
neo_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(neo1)) %>% summarize(N=n(), mean= mean(log(neo1), na.rm=T))   
aat_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(aat2)) %>% summarize(N=n(), mean= mean(log(aat2), na.rm=T))   
mpo_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(mpo2)) %>% summarize(N=n(), mean= mean(log(mpo2), na.rm=T))   
neo_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(neo2)) %>% summarize(N=n(), mean= mean(log(neo2), na.rm=T))
aat_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(aat3)) %>% summarize(N=n(), mean= mean(log(aat3), na.rm=T))   
mpo_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(mpo3)) %>% summarize(N=n(), mean= mean(log(mpo3), na.rm=T))   
neo_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(neo3)) %>% summarize(N=n(), mean= mean(log(neo3), na.rm=T))   


#Means and 95% CI's for mean by arm plots
aat_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$aat1), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
aat_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$aat2), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
aat_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$aat3), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$mpo1), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$mpo2), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$mpo3), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t1_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$neo1), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t2_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$neo2), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t3_mn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=log(.$neo3), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 


aat_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$aat1), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
aat_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$aat2), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
aat_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$aat3), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$mpo1), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$mpo2), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
mpo_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$mpo3), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t1_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$neo1), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t2_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$neo2), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 
neo_t3_absmn<-d %>% group_by(tr) %>% do(as.data.frame(washb_mean(Y=(.$neo3), id=.$block, print = F))) %>% ungroup %>% as.data.frame %>% `rownames<-`(.[,1]) %>% .[,-1] 


# #Means and 95% CI's not stratified by arm
overall_mn_by_round<-
  d %>% subset(., select=c(dataid, childNo, block, neo1,mpo1,aat1,neo2,mpo2,aat2,neo3,mpo3,aat3)) %>%
  gather(key, value, -dataid, -childNo, -block) %>%
  mutate(biomarker = substr(key, 1,3)) %>%
  group_by(biomarker) %>%
  do(as.data.frame(washb_mean(Y=log(.$value), id=.$block, print = F))) %>%
  ungroup %>% as.data.frame

overall_mn<-
  d %>% subset(., select=c(dataid, childNo, block, neo1,mpo1,aat1,neo2,mpo2,aat2,neo3,mpo3,aat3)) %>%
  gather(key, value, -dataid, -childNo, -block) %>%
  group_by(key) %>%
  do(as.data.frame(washb_mean(Y=log(.$value), id=.$block, print = F))) %>%
  ungroup %>% as.data.frame
colnames(overall_mn)[1]<-"biomarker"
stool_overall_mn <- rbind(overall_mn_by_round, overall_mn)
save(stool_overall_mn, file="stool_overall_means.Rdata")



#Create empty matrix to hold the glm results:
neo_t1_unadj<-mpo_t1_unadj<-aat_t1_unadj<-matrix(0, nrow=5, ncol=6)
neo_t2_unadj<-mpo_t2_unadj<-aat_t2_unadj<-matrix(0, nrow=5, ncol=6)
neo_t3_unadj<-mpo_t3_unadj<-aat_t3_unadj<-matrix(0, nrow=5, ncol=6)

res_unadj<-list(neo_t1_unadj=neo_t1_unadj, mpo_t1_unadj=mpo_t1_unadj, aat_t1_unadj=aat_t1_unadj, 
                neo_t2_unadj=neo_t2_unadj, mpo_t2_unadj=mpo_t2_unadj, aat_t2_unadj=aat_t2_unadj, 
                neo_t3_unadj=neo_t3_unadj, mpo_t3_unadj=mpo_t3_unadj, aat_t3_unadj=aat_t3_unadj)




#Unadjusted glm models
for(i in 1:9){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=NULL, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_unadj[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_unadj[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_unadj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}

############################
#Age and sex adjusted GLMs
############################
d$sex<-as.factor(d$sex)
  d$sex=relevel(d$sex,ref="0")

#Create empty matrix to hold the glm results:
neo_t1_sex<-mpo_t1_sex<-aat_t1_sex<-matrix(0, nrow=5, ncol=6)
neo_t2_sex<-mpo_t2_sex<-aat_t2_sex<-matrix(0, nrow=5, ncol=6)
neo_t3_sex<-mpo_t3_sex<-aat_t3_sex<-matrix(0, nrow=5, ncol=6)

res_sex<-list(neo_t1_sex=neo_t1_sex, mpo_t1_sex=mpo_t1_sex, aat_t1_sex=aat_t1_sex, 
                neo_t2_sex=neo_t2_sex, mpo_t2_sex=mpo_t2_sex, aat_t2_sex=aat_t2_sex, 
                neo_t3_sex=neo_t3_sex, mpo_t3_sex=mpo_t3_sex, aat_t3_sex=aat_t3_sex)




#Age and sex adjusted glm models
for(i in 1:3){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged1), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
for(i in 4:6){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged2), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
for(i in 7:9){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=cbind(d$sex, d$aged3), id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_sex[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_sex[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_sex[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "WSH v Nutrition + WSH", "Nutrition v Nutrition + WSH"))
  }
}
            


#------------------
#Adjusted GLM
#------------------

# Set factor variables

# d$n_chickens[is.na(d$n_chickens)] <- 99
# d$n_dogs[is.na(d$n_dogs)] <- 99
# d$n_cows[is.na(d$n_cows)] <- 99
# d$n_goats[is.na(d$n_goats)] <- 99
# 
# 
# d$Nlt18[is.na(d$Nlt18)] <- 99
# d$Ncomp[is.na(d$Ncomp)] <- 99
# d$watmin[is.na(d$watmin)] <- 99

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






##############################################
#Run GLMs for the adjusted parameter estimates
##############################################

#Create empty matrix to hold the tmle results:
res_adj<-list(neo_t1_adj=matrix(0,5,6), mpo_t1_adj=matrix(0,5,6), aat_t1_adj=matrix(0,5,6), 
                neo_t2_adj=matrix(0,5,6), mpo_t2_adj=matrix(0,5,6), aat_t2_adj=matrix(0,5,6),  
                neo_t3_adj=matrix(0,5,6), mpo_t3_adj=matrix(0,5,6), aat_t3_adj=matrix(0,5,6))

for(i in 1:3){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W1, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}
for(i in 4:6){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W2, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}
for(i in 7:9){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W3, id=d$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=T)
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}




##########################################
#Save objects for replication
##########################################
neo_t1_unadj_M=res_unadj[[1]]
mpo_t1_unadj_M=res_unadj[[2]]
aat_t1_unadj_M=res_unadj[[3]] 
neo_t2_unadj_M=res_unadj[[4]] 
mpo_t2_unadj_M=res_unadj[[5]]
aat_t2_unadj_M=res_unadj[[6]] 
neo_t3_unadj_M=res_unadj[[7]]
mpo_t3_unadj_M=res_unadj[[8]]
aat_t3_unadj_M=res_unadj[[9]]


neo_t1_adj_sex_age_M=res_sex[[1]]
mpo_t1_adj_sex_age_M=res_sex[[2]]
aat_t1_adj_sex_age_M=res_sex[[3]] 
neo_t2_adj_sex_age_M=res_sex[[4]] 
mpo_t2_adj_sex_age_M=res_sex[[5]]
aat_t2_adj_sex_age_M=res_sex[[6]] 
neo_t3_adj_sex_age_M=res_sex[[7]]
mpo_t3_adj_sex_age_M=res_sex[[8]]
aat_t3_adj_sex_age_M=res_sex[[9]]



neo_t1_adj_M=res_adj[[1]]
mpo_t1_adj_M=res_adj[[2]]
aat_t1_adj_M=res_adj[[3]] 
neo_t2_adj_M=res_adj[[4]] 
mpo_t2_adj_M=res_adj[[5]]
aat_t2_adj_M=res_adj[[6]]
neo_t3_adj_M=res_adj[[7]]
mpo_t3_adj_M=res_adj[[8]]
aat_t3_adj_M=res_adj[[9]]


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Results/Andrew/")
save(neo_t1_N_M, mpo_t1_N_M, aat_t1_N_M,
     neo_t2_N_M, mpo_t2_N_M, aat_t2_N_M,
     neo_t3_N_M, mpo_t3_N_M, aat_t3_N_M, 
     file="stool_res_N_M.Rdata")

save(aat_t1_mn, aat_t2_mn, aat_t3_mn,
     mpo_t1_mn, mpo_t2_mn, mpo_t3_mn,
     neo_t1_mn, neo_t2_mn, neo_t3_mn, 
     aat_t1_absmn, aat_t2_absmn, aat_t3_absmn,
     mpo_t1_absmn, mpo_t2_absmn, mpo_t3_absmn,
     neo_t1_absmn, neo_t2_absmn, neo_t3_absmn, 
     file="stool_res_means.Rdata")

save(stool_overall_mn, file="stool_overall_means.Rdata")

save(neo_t1_unadj_M, mpo_t1_unadj_M, aat_t1_unadj_M,
     neo_t2_unadj_M, mpo_t2_unadj_M, aat_t2_unadj_M, 
     neo_t3_unadj_M, mpo_t3_unadj_M, aat_t3_unadj_M, 
     file="stool_res_unadj_M.Rdata")

save(neo_t1_adj_sex_age_M, mpo_t1_adj_sex_age_M, aat_t1_adj_sex_age_M,
     neo_t2_adj_sex_age_M, mpo_t2_adj_sex_age_M, aat_t2_adj_sex_age_M, 
     neo_t3_adj_sex_age_M, mpo_t3_adj_sex_age_M, aat_t3_adj_sex_age_M, 
     file="stool_res_adj_sex_age_M.Rdata")

save(neo_t1_adj_M, mpo_t1_adj_M, aat_t1_adj_M,
     neo_t2_adj_M, mpo_t2_adj_M, aat_t2_adj_M, 
     neo_t3_adj_M, mpo_t3_adj_M, aat_t3_adj_M, 
     file="stool_res_adj_M.Rdata")

#save data for figures
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Temp/")
save(d, file="stool_figure_data.Rdata")

