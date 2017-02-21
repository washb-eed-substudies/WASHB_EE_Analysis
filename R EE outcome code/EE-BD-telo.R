
#---------------------------------------
# EE-BD-telo.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The analysis script for the WASH Benefits
# Telomere substudy
#---------------------------------------

###Load in data
rm(list=ls())
try(detach(package:plyr))
library(foreign)
library(dplyr)
library(washb)



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
load("washb-BD-telo-blind-tr.Rdata")
levels(treatment$tr)
treatment$tr <- factor(treatment$tr,levels=c("Control","Nutrition + WSH"))
levels(treatment$tr)
#Load in enrollment data for adjusted analysis
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("washb-bangladesh-enrol+animals.csv",stringsAsFactors = TRUE)
#enrol<-read.csv("washb-bangladesh-enrol.csv",stringsAsFactors = TRUE)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
telo<-read.csv("BD-EE-telo.csv")

#Merge treatment information 
dim(telo)
d<-left_join(telo,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)
 table(is.na(d$tr))
 

#Merge in enrollment information
#dim(d)
#dim(enrol)
#d<-left_join(d,enrol, by=c("dataid"))
#dim(d)


#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 



#table number of fully collected aliqouts by arm and year
head(d)

t1_N<-d%>% subset(aliquot1>1)%>%group_by(tr) %>%summarize(sample1=n()) 
t2_N<-d%>% subset(aliquot2>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t3_N<-d%>% subset(aliquot3>1)%>%group_by(tr) %>%summarize(sample3=n()) 


cbind(t1_N,t2_N[,2],t3_N[,2])

#Note: Why are there time 1 aliquots? For telo, should only be midline and endline




#Calculate average age across arms at followup time 1, 2, and 3
#Survey 1
#Tabulate overall N, gender, and age 
overallN1<-d%>% subset(!is.na(aliquot1) &  aliquot1>1) %>% summarize(N=n(),Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t1<-d%>% subset(!is.na( aliquot1) &  aliquot1>1) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d%>% subset(!is.na(TS2)) %>% summarize(N=n(),Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>% subset(!is.na(TS2)) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% subset(!is.na( TS3)) %>% summarize(N=n(),Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d%>% subset(!is.na(TS3)) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 


rbind(overallN1, t1)
age_t2_blood_M<-rbind(overallN2, t2)
age_t3_blood_M<-rbind(overallN3, t3)


#d%>% subset(!is.na(TS2)& tr=="Control")  %>%summarize(N=n(), Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex), Missing=sum(is.na(agem2))) 


############################
#Calculate unadjusted outcomes:
############################

#N's and geometric means
    ts_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(TS2)) %>% summarize(N=n(), mean= mean(TS2, na.rm=T))   
    ts_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(TS3)) %>% summarize(N=n(), mean= mean(TS3, na.rm=T))   

#Unadjusted glm models
    ts_t2_unadj_M<-washb_glm(Y=d$TS2, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
    ts_t3_unadj_M<-washb_glm(Y=d$TS3, tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR


############################
#Adjusted GLMs-sex and age
############################
d$sex<-as.factor(d$sex)

#Run GLMs for the sex/age adjusted parameter estimates
    ts_t2_adj_sex_age_M<-washb_glm(Y=d$TS2, tr=d$tr, W=cbind(d$sex, d$aged2), id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR
    ts_t3_adj_sex_age_M<-washb_glm(Y=d$TS3, tr=d$tr, W=cbind(d$sex, d$aged3), id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=F)$TR


    
############################
#Adjusted GLMs-full
############################
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


#Add in time varying covariates:
Wvars1<-c("aged1", "month1", "staffid1") 
Wvars2<-c("aged2", "month2", "staffid2") 
Wvars3<-c("aged3", "month3", "staffid3") 



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

d$birthord[is.na(d$birthord)]<-"99"
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
#Relevel all factors
W$sex<-as.factor(W$sex)
  d$sex=relevel(d$sex,ref="female")
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

#Check missingness:

cbind(d$TS2,W2) %>% subset(!is.na(d$TS2)) %>% apply(., 2, function(x) print(table(is.na(x))[2]))



#Run GLMs for the adjusted parameter estimates
    ts_t2_adj_M<-washb_glm(Y=d$TS2, tr=d$tr, W=W2, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR
    ts_t3_adj_M<-washb_glm(Y=d$TS3, tr=d$tr, W=W3, id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$TR



for(i in 1:ncol(W2)){
  print(colnames(W2)[i])
  print(table(is.na(W2[,i])))
}

    
#Save combined telomere dataset for comparison with Audrie
library(stringr)
d$childid<-str_pad(d$childid, 6, pad = "0")
head(d)    

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
write.dta(d, "washb-BD-EE-telo_Andrew.dta")

    

############################
#Subgroup analysis
############################
    ts_t2_subgroup_M<-washb_glm(Y=d$TS2, tr=d$tr, W=subset(d, select=sex), V="sex", id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$lincom
    ts_t3_subgroup_M<-washb_glm(Y=d$TS3, tr=d$tr, W=subset(d, select=sex), V="sex", id=d$block.x, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), print=T)$lincom




############################
#SL analysis of Telomere Quartiles
############################
#Each specific association that we measure between an exposure (e.g., telomere length at 12 months after 
#intervention) and an outcome (e.g., length-for-age Z-scores (LAZ) measured at 24 months after intervention) 
#will require its own, unique analysis, which could be complicated by non-linear or other complex relationships 
#between the exposure and outcome. We will adopt the following general approach in each case, recognizing that 
#it will be tailored to each specific analysis. First, we will conduct exploratory data analyses that plot the 
#relationship between telomere exposures and length outcomes and summarize the patterns between them using 
#non-parametric smoothed fits with an ensemble approach called Ssuper learner18 that uses cross-validation 
#to optimally combine different models in a library, and we will include the following library in the ensemble: 
#the simple mean, linear models, locally weighted regression (lowess), and natural smoothing splines (generalized 
#additive models). In concert with this visual examination of each exposure/outcome relationship, we will test for 
#the bivariate association between the exposure and outcome using a non-parametric Spearman's rank correlation test 
#with a permutation-based test to determine if it differs from zero. Since the relationship between telomere length 
#and subsequent growth could be nonlinear, we will summarize unadjusted and adjusted mean LAZ by quartiles of telomere 
#length.  We will estimated adjusted means in each quartile and their difference using targeted maximum likelihood 
#estimation (TMLE), which will allow us to flexibly adjust for potential confounding covariates in section 9.2 using 
#the super learner ensemble, described above19. We will first stratify this analysis by intervention arm, and then 
#combine the data across arms if the relationships are similar. 

#Merge in anthro data

#Plot using Ben's antibody package

#Divide TS into quartiles

#By arm, permutation test of each quartile to lowest

############################
#-missing data imputation
# with IPW 
############################

#Create indicator for missingness
d$TS2.miss<-ifelse(is.na(d$TS2),0,1)
d$TS3.miss<-ifelse(is.na(d$TS3),0,1)

ts_t2_ipw_unadj_M<-washb_tmle(Y=d$TS2, tr=d$tr, W=W2, id=d$block.x, Delta=d$TS2.miss, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)
ts_t3_ipw_unadj_M<-washb_tmle(Y=d$TS3, tr=d$tr, W=W3, id=d$block.x, Delta=d$TS3.miss, pair=NULL, family="gaussian", contrast= c("Control","Nutrition + WSH"), Q.SL.library = c("SL.glm"), seed=12345, print=F)

#Extract estimates
ts_t2_ipw_unadj_M<-cbind(ts_t2_ipw_unadj_M$estimates$ATE$psi,t(ts_t2_ipw_unadj_M$estimates$ATE$CI),ts_t2_ipw_unadj_M$estimates$ATE$pval)
ts_t3_ipw_unadj_M<-cbind(ts_t3_ipw_unadj_M$estimates$ATE$psi,t(ts_t3_ipw_unadj_M$estimates$ATE$CI),ts_t3_ipw_unadj_M$estimates$ATE$pval)

 
##########################################
#Save objects for replication
##########################################


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(age_t2_blood_M, age_t3_blood_M, ts_t2_N_M, ts_t3_N_M,ts_t2_unadj_M, ts_t3_unadj_M, ts_t2_adj_sex_age_M, ts_t3_adj_sex_age_M, ts_t2_adj_M, ts_t3_adj_M,
     ts_t2_ipw_unadj_M, ts_t3_ipw_unadj_M, ts_t2_subgroup_M, ts_t3_subgroup_M,
     file="telo_res.Rdata")

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
save(d, file="telo_figure_data.Rdata")










