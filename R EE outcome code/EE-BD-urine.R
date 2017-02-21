
#---------------------------------------
# EE-BD-urine.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The urine-based biomarker outcomes for 
# EED Bangladesh sub-study
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
urine<-read.csv("BD-EE-urine.csv",stringsAsFactors = TRUE)




#Merge treatment information 
dim(urine)
d<-left_join(urine,treatment, by="clusterid")
dim(d)
head(d)
table(d$tr)

#Check single row not merging to treatment
d[is.na(d$tr),]
#Seems to be blank row. Drop
d<-d[!is.na(d$tr),]


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
t1s1<-d%>% subset(h2aliqout1_t1>1)%>%group_by(tr) %>%summarize(h2sample1=n()) 
t1s2<-d%>% subset(h2aliqout2_t1>1)%>%group_by(tr) %>%summarize(h2sample2=n()) 
t1s3<-d%>% subset(h2aliqout3_t1>1)%>%group_by(tr) %>%summarize(h2sample3=n()) 
t1s4<-d%>% subset(h2aliqout4_t1>1)%>%group_by(tr) %>%summarize(h2sample4=n()) 
t1s5<-d%>% subset(h2aliqout5_t1>1)%>%group_by(tr) %>%summarize(h2sample5=n()) 
t1s6<-d%>% subset(h2aliqout6_t1>1)%>%group_by(tr) %>%summarize(h2sample6=n()) 
t1s7<-d%>% subset(h5aliqout7_t1>1)%>%group_by(tr) %>%summarize(h5sample7=n()) 
t1s8<-d%>% subset(h5aliqout8_t1>1)%>%group_by(tr) %>%summarize(h5sample8=n()) 
t1s9<-d%>% subset(h5aliqout9_t1>1)%>%group_by(tr) %>%summarize(h5sample9=n()) 
t1s10<-d%>% subset(h5aliqout10_t1>1)%>%group_by(tr) %>%summarize(h5sample10=n()) 
t1s11<-d%>% subset(h5aliqout11_t1>1)%>%group_by(tr) %>%summarize(h5sample11=n()) 
t1s12<-d%>% subset(h5aliqout12_t1>1)%>%group_by(tr) %>%summarize(h5sample12=n()) 

 
aliquotN_t1<-cbind(t1s1,t1s2[,2],t1s3[,2],t1s4[,2],t1s5[,2],t1s6[,2],t1s7[,2],t1s8[,2],t1s9[,2],t1s10[,2],t1s11[,2],t1s12[,2])


#aliqout time 2
t2s1<-d%>% subset(h2aliqout1_t2>1)%>%group_by(tr) %>%summarize(h2sample1=n()) 
t2s2<-d%>% subset(h2aliqout2_t2>1)%>%group_by(tr) %>%summarize(h2sample2=n()) 
t2s3<-d%>% subset(h2aliqout3_t2>1)%>%group_by(tr) %>%summarize(h2sample3=n()) 
t2s4<-d%>% subset(h2aliqout4_t2>1)%>%group_by(tr) %>%summarize(h2sample4=n()) 
t2s5<-d%>% subset(h2aliqout5_t2>1)%>%group_by(tr) %>%summarize(h2sample5=n()) 
t2s6<-d%>% subset(h2aliqout6_t2>1)%>%group_by(tr) %>%summarize(h2sample6=n()) 
t2s7<-d%>% subset(h5aliqout7_t2>1)%>%group_by(tr) %>%summarize(h5sample7=n()) 
t2s8<-d%>% subset(h5aliqout8_t2>1)%>%group_by(tr) %>%summarize(h5sample8=n()) 
t2s9<-d%>% subset(h5aliqout9_t2>1)%>%group_by(tr) %>%summarize(h5sample9=n()) 
t2s10<-d%>% subset(h5aliqout10_t2>1)%>%group_by(tr) %>%summarize(h5sample10=n()) 
t2s11<-d%>% subset(h5aliqout11_t2>1)%>%group_by(tr) %>%summarize(h5sample11=n()) 
t2s12<-d%>% subset(h5aliqout12_t2>1)%>%group_by(tr) %>%summarize(h5sample12=n()) 
t3s13<-d%>% subset(preLMaliqout13_t2>1)%>%group_by(tr) %>%summarize(preLMsample13_t2=n()) 

 
aliquotN_t2<-cbind(t2s1,t2s2[,2],t2s3[,2],t2s4[,2],t2s5[,2],t2s6[,2],t2s7[,2],t2s8[,2],t2s9[,2],t2s10[,2],t2s11[,2],t2s12[,2],t3s13[,2])


#aliqout time 3
t3s1<-d%>% subset(h2aliqout1_t3>1)%>%group_by(tr) %>%summarize(h2sample1=n()) 
t3s2<-d%>% subset(h2aliqout2_t3>1)%>%group_by(tr) %>%summarize(h2sample2=n()) 
t3s3<-d%>% subset(h2aliqout3_t3>1)%>%group_by(tr) %>%summarize(h2sample3=n()) 
t3s4<-d%>% subset(h2aliqout4_t3>1)%>%group_by(tr) %>%summarize(h2sample4=n()) 
t3s5<-d%>% subset(h2aliqout5_t3>1)%>%group_by(tr) %>%summarize(h2sample5=n()) 
t3s6<-d%>% subset(h2aliqout6_t3>1)%>%group_by(tr) %>%summarize(h2sample6=n()) 
t3s7<-d%>% subset(h5aliqout7_t3>1)%>%group_by(tr) %>%summarize(h5sample7=n()) 
t3s8<-d%>% subset(h5aliqout8_t3>1)%>%group_by(tr) %>%summarize(h5sample8=n()) 
t3s9<-d%>% subset(h5aliqout9_t3>1)%>%group_by(tr) %>%summarize(h5sample9=n()) 
t3s10<-d%>% subset(h5aliqout10_t3>1)%>%group_by(tr) %>%summarize(h5sample10=n()) 
t3s11<-d%>% subset(h5aliqout11_t3>1)%>%group_by(tr) %>%summarize(h5sample11=n()) 
t3s12<-d%>% subset(h5aliqout12_t3>1)%>%group_by(tr) %>%summarize(h5sample12=n()) 
t3s13<-d%>% subset(preLMaliqout13_t3>1)%>%group_by(tr) %>%summarize(preLMsample13_t3=n()) 

 
aliquotN_t3<-cbind(t3s1,t3s2[,2],t3s3[,2],t3s4[,2],t3s5[,2],t3s6[,2],t3s7[,2],t3s8[,2],t3s9[,2],t3s10[,2],t3s11[,2],t3s12[,2],t3s13[,2])


aliquotN_t1[c(1,3,4,2),c(1:2,8)]
aliquotN_t2[c(1,3,4,2),c(1:2,8,14)]
aliquotN_t3[c(1,3,4,2),c(1:2,8,14)]



#Calculate average age across arms at followup time 1, 2, and 3
#Survey 1
#Tabulate overall N, gender, and age 
overallN1<-d%>% subset(!is.na(h2aliqout1_t1) & h2aliqout1_t1>1 | !is.na(h5aliqout7_t1) & h5aliqout7_t1>1) %>% summarize(N=n(),Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t1<-d%>% subset(!is.na(h2aliqout1_t1) & h2aliqout1_t1>1 | !is.na(h5aliqout7_t1) & h5aliqout7_t1>1) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d%>% subset(!is.na(h2aliqout1_t2) & h2aliqout1_t2>1 | !is.na(h5aliqout7_t2) & h5aliqout7_t2>1) %>% summarize(N=n(),Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>% subset(!is.na(h2aliqout1_t2) & h2aliqout1_t2>1 | !is.na(h5aliqout7_t2) & h5aliqout7_t2>1) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% subset(!is.na(h2aliqout1_t3) & h2aliqout1_t3>1 | !is.na(h5aliqout7_t3) & h5aliqout7_t3>1) %>% summarize(N=n(),Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d%>% subset(!is.na(h2aliqout1_t3) & h2aliqout1_t3>1 | !is.na(h5aliqout7_t3) & h5aliqout7_t3>1) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 


age_t1_urine_M<-rbind(overallN1, t1[c(1,3,4,2),])
age_t2_urine_M<-rbind(overallN2, t2[c(1,3,4,2),])
age_t3_urine_M<-rbind(overallN3, t3[c(1,3,4,2),])



#Temporarily generate fake outcome data for L and M
#Mannitol: 1228.8 (1230.07) ug/ml and Lactulose: 245.3 (265.08) ug/ml 
set.seed(12345)
d$Lact1<-round(abs(rnorm(n=nrow(d), mean=1228.8, sd=1230.07)),4)
d$Mann1<-round(abs(rnorm(n=nrow(d), mean=245.3, sd=265.08)),4)

d$Lact2<-round(abs(rnorm(n=nrow(d), mean=1300, sd=1200)),4)
d$Mann2<-round(abs(rnorm(n=nrow(d), mean=300, sd=250)),4)

d$Lact3<-round(abs(rnorm(n=nrow(d), mean=1200, sd=1100)),4)
d$Mann3<-round(abs(rnorm(n=nrow(d), mean=200, sd=200)),4)

#Create and save dataset for Audrie:
urine_simulated_outcomes<-d %>%
    mutate(childid=as.character(dataid*10+childNo)) %>%
    select(childid, Lact1, Mann1, Lact2, Mann2, Lact3, Mann3)
library(stringr)
urine_simulated_outcomes$childid<-str_pad(urine_simulated_outcomes$childid, 6, pad = "0")
head(urine_simulated_outcomes)    

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
save(urine_simulated_outcomes, file="washb-BD-EE-sim-urine-outcomes.Rdata")
write.dta(urine_simulated_outcomes, "washb-BD-EE-sim-urine-outcomes.dta")


#------------------
#Generate LM ratio
#------------------
#To calculate total lactulose dosed (mg) or total mannitol dosed (mg):
 #The children ingest a solution of 250 mg/ml lactulose and 50 mg/ml of mannitol in a dose of 2 ml/kg of weight up to 20 ml maximum.
 #Q9 of the EE urine form is the total volume of LM solution ingested (in ml). For example, a child who ingested 20 ml of LM solution (the maximum dose), would have ingested 1000 mg of mannitol and 5000 mg of lactulose. The 1000 mg and 5000 mg would then be used in the above formula as the "total mannitol dosed (mg) or total lactulose dosed (mg)".
 mean(d$LMvol_t1, na.rm=T)
d$lact.dose_t1<-d$LMvol_t1*250
d$lact.dose_t2<-d$LMvol_t2*250
d$lact.dose_t3<-d$LMvol_t3*250
d$mann.dose_t1<-d$LMvol_t1*50
d$mann.dose_t2<-d$LMvol_t2*50
d$mann.dose_t3<-d$LMvol_t3*50

#% lactulose recovery = (urine concentration lactulose (mg/L) * urine volume (L) * 100 / total lactulose dosed (mg))
per.lact.rec_t1<-d$Lact1*100/d$lact.dose_t1
per.lact.rec_t2<-d$Lact1*100/d$lact.dose_t2
per.lact.rec_t3<-d$Lact1*100/d$lact.dose_t3

#% mannitol recovery = (urine concentration mannitol (mg/L) * urine volume (L) * 100 / total mannitol dosed (mg))
per.mann.rec_t1<-d$Mann1*100/d$mann.dose_t1
per.mann.rec_t2<-d$Mann1*100/d$mann.dose_t2
per.mann.rec_t3<-d$Mann1*100/d$mann.dose_t3

#LM ratio
d$LM1<-per.lact.rec_t1/per.mann.rec_t1
d$LM2<-per.lact.rec_t2/per.mann.rec_t2
d$LM3<-per.lact.rec_t3/per.mann.rec_t3

#We also need to report Lactulose recovery and Mannitol recovery in mmol/L (as indicated on our table shells).
    #mmol/L of Lactulose = ??g/ml * 1000 ml/L * 1 mg/1000??g * 1g/1000mg * 1mol/342.296g * 1000mmol/1 mol
#The above simplifies to (??g/ml) * (1 / 342.296) = mmol/L
    #mmol/L of Mannitol = ??g/ml * 1000 ml/L * 1 mg/1000??g * 1g/1000mg * 1mol/182.172g * 1000mmol/1 mol
#The above simplifies to (??g/ml) * (1 / 182.172) = mmol/L
mean(d$Lact1, na.rm=T)
mean(d$LMvol_t1, na.rm=T)
mean(d$Mann1, na.rm=T)

d$lact.rec.MMOL_t1<-(d$lact.dose_t1/1000)*(1/182.172)
d$lact.rec.MMOL_t2<-(d$lact.dose_t2/1000)*(1/182.172)
d$lact.rec.MMOL_t3<-(d$lact.dose_t3/1000)*(1/182.172)
d$mann.rec.MMOL_t1<-(d$mann.dose_t1/1000)*(1/182.172)
d$mann.rec.MMOL_t2<-(d$mann.dose_t2/1000)*(1/182.172)
d$mann.rec.MMOL_t3<-(d$mann.dose_t3/1000)*(1/182.172)



############################
#Calculate outcomes:
############################

#dataframe of urine biomarkers:
Y<-d %>% select(Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"), c("WSH","Nutrition + WSH"))

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



#------------------
#Unadjusted GLM
#------------------



#Create empty matrix to hold the glm results:
lact_t1_unadj<-mann_t1_unadj<-lm_t1_unadj<-matrix(0, nrow=5, ncol=6)
lact_t2_unadj<-mann_t2_unadj<-lm_t2_unadj<-reg1b_t2_unadj<-matrix(0, nrow=5, ncol=6)
lact_t3_unadj<-mann_t3_unadj<-lm_t3_unadj<-matrix(0, nrow=5, ncol=6)

res_unadj<-list(lact_t1_unadj=lact_t1_unadj, mann_t1_unadj=mann_t1_unadj, lm_t1_unadj=lm_t1_unadj, 
                lact_t2_unadj=lact_t2_unadj, mann_t2_unadj=mann_t2_unadj, lm_t2_unadj=lm_t2_unadj, 
                lact_t3_unadj=lact_t3_unadj, mann_t3_unadj=mann_t3_unadj, lm_t3_unadj=lm_t3_unadj)



#Unadjusted glm models
for(i in 1:ncol(Y)){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_unadj[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_unadj[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_unadj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "Nutrition v Nutrition + WSH", "WSH v Nutrition + WSH"))
  }
}


#------------------
#Adjusted GLM
#------------------

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


d$asset_clock[is.na(d$asset_clock)]<-99
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




#Check that prevalence >5% for all binary variables
for(i in 1:ncol(W)){
  if(class(W[,i])=="factor"){
    for(j in 1:dim(table(W[,i]))){
      flag<-0
      if(sum(W[,i]==levels(W[,i])[j], na.rm=T)/nrow(W)*100<5){
        perc<-sum(W[,i]==levels(W[,i])[j], na.rm=T)/nrow(W)*100
        cat("\n>95% missing: ",colnames(W)[i]," level:",levels(W[,i])[j],"perc:",perc,"\n")
        flag<-1
      }
    }
      if(flag==1){
        print(table(W[,i]))
      }
  }else{
    if(sum(is.na(W[,i]))/nrow(W)*100>95){
      cat("\n>95% missing: ",colnames(W)[i],"\n")
    }
  }
}



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
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W1, id=d$clusterid.x, pair=NULL, family="gaussian", contrast= contrasts[[j]])
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}
for(i in 4:6){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W2, id=d$clusterid.x, pair=NULL, family="gaussian", contrast= contrasts[[j]])
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}
for(i in 7:9){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W3, id=d$clusterid.x, pair=NULL, family="gaussian", contrast= contrasts[[j]])
  res_adj[[i]][j,]<-as.numeric(temp$TR)
  }
}








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

lac_t1_adj_M=res_adj[[1]]
man_t1_adj_M=res_adj[[2]]
lm_t1_adj_M=res_adj[[3]] 
lac_t2_adj_M=res_adj[[4]] 
man_t2_adj_M=res_adj[[5]]
lm_t2_adj_M=res_adj[[6]]
lac_t3_adj_M=res_adj[[7]]
man_t3_adj_M=res_adj[[8]]
lm_t3_adj_M=res_adj[[9]]


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(age_t1_urine_M,age_t2_urine_M,age_t3_urine_M,
     lac_t1_N_M, man_t1_N_M, lm_t1_N_M,
     lac_t2_N_M, man_t2_N_M, lm_t2_N_M,
     lac_t3_N_M, man_t3_N_M,lm_t3_N_M,
     file="urine_res_N_M.Rdata")


save(lac_t1_unadj_M, man_t1_unadj_M, lm_t1_unadj_M,
     lac_t2_unadj_M, man_t2_unadj_M, lm_t2_unadj_M, 
     lac_t3_unadj_M,man_t3_unadj_M, lm_t3_unadj_M,
     file="urine_res_unadj_M.Rdata")


save(lac_t1_adj_M, man_t1_adj_M, lm_t1_adj_M,
     lac_t2_adj_M, man_t2_adj_M, lm_t2_adj_M, 
     lac_t3_adj_M,man_t3_adj_M, lm_t3_adj_M,
     file="urine_res_adj_M.Rdata")