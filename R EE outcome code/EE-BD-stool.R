
#---------------------------------------
# EE-BD-stool.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The stool-based biomarker outcomes for 
# EED Bangladesh sub-study
#---------------------------------------

###Load in data
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
enrol<-read.csv("washb-bangladesh-enrol.csv",stringsAsFactors = TRUE)

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
stool<-read.csv("BD-EE-stool.csv")


#Merge treatment information 
dim(stool)
d<-left_join(stool,treatment, by="clusterid")
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
t1s1<-d%>% subset(aliqout1_t1>1)%>%group_by(tr) %>%summarize(sample1=n()) 
t1s2<-d%>% subset(aliqout2_t1>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t1s3<-d%>% subset(aliqout3_t1>1)%>%group_by(tr) %>%summarize(sample3=n()) 
t1s4<-d%>% subset(aliqout4_t1>1)%>%group_by(tr) %>%summarize(sample4=n()) 
t1s5<-d%>% subset(aliqout5_t1>1)%>%group_by(tr) %>%summarize(sample5=n()) 
 
aliquotN_t1<-cbind(t1s1,t1s2[,2],t1s3[,2],t1s4[,2],t1s5[,2])


#aliqout time 2
t2s1<-d%>% subset(aliqout1_t2>1)%>%group_by(tr) %>%summarize(sample1=n()) 
t2s2<-d%>% subset(aliqout2_t2>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t2s3<-d%>% subset(aliqout3_t2>1)%>%group_by(tr) %>%summarize(sample3=n()) 
t2s4<-d%>% subset(aliqout4_t2>1)%>%group_by(tr) %>%summarize(sample4=n()) 
t2s5<-d%>% subset(aliqout5_t2>1)%>%group_by(tr) %>%summarize(sample5=n()) 
 
aliquotN_t2<-cbind(t2s1,t2s2[,2],t2s3[,2],t2s4[,2],t2s5[,2])
aliquotN_t2

#aliqout time 3
t3s1<-d%>% subset(aliqout1_t3>1)%>%group_by(tr) %>%summarize(sample1=n()) 
t3s2<-d%>% subset(aliqout2_t3>1)%>%group_by(tr) %>%summarize(sample2=n()) 
t3s3<-d%>% subset(aliqout3_t3>1)%>%group_by(tr) %>%summarize(sample3=n()) 
t3s4<-d%>% subset(aliqout4_t3>1)%>%group_by(tr) %>%summarize(sample4=n()) 
t3s5<-d%>% subset(aliqout5_t3>1)%>%group_by(tr) %>%summarize(sample5=n()) 

aliquotN_t3<-cbind(t3s1,t3s2[,2],t3s3[,2],t3s4[,2],t3s5[,2])


aliquotN_t1
aliquotN_t2
aliquotN_t3


#Calculate average age across arms at followup time 1, 2, and 3
#Survey 1
#Tabulate overall N, gender, and age 
overallN1<-d%>% subset(!is.na(aliqout1_t1) & aliqout1_t1>1) %>% summarize(N=n(),Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t1<-d%>% subset(!is.na(aliqout1_t1) & aliqout1_t1>1) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 2
#Tabulate overall N, gender, and age 
overallN2<-d%>% subset(!is.na(aliqout1_t2) & aliqout1_t2>1) %>% summarize(N=n(),Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN2<-cbind("Overall", overallN2)
colnames(overallN2)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t2<-d%>% subset(!is.na(aliqout1_t2) & aliqout1_t2>1) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem2, na.rm=T), Mean_agem=mean(agem2, na.rm=T), Sd_agem=sd(agem2, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 


#Survey 3
#Tabulate overall N, gender, and age 
overallN3<-d%>% subset(!is.na(aliqout1_t3) & aliqout1_t3>1) %>% summarize(N=n(),Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 
overallN3<-cbind("Overall", overallN3)
colnames(overallN3)[1]<-"tr"

#Tabulate N, gender, and age across survey rounds
t3<-d%>% subset(!is.na(aliqout1_t3) & aliqout1_t3>1) %>% group_by(tr) %>%summarize(N=n(), Median_agem=median(agem3, na.rm=T), Mean_agem=mean(agem3, na.rm=T), Sd_agem=sd(agem3, na.rm=T), nummales=sum(sex, na.rm=T), numfemales=n()-sum(sex, na.rm=T)) 


rbind(overallN1, t1[c(1,3,4,2),])
rbind(overallN2, t2[c(1,3,4,2),])
rbind(overallN3, t3[c(1,3,4,2),])


#Temporarily generate fake outcome data for aat (0.5, sd=0.12), mpo (10000, sd=2500), and neo (2000, sd=250)
set.seed(12345)
d$aat1<-rnorm(n=nrow(d), mean=13, sd=1)
d$mpo1<-rnorm(n=nrow(d), mean=11000, sd=0.12)
d$neo1<-rnorm(n=nrow(d), mean=2000, sd=0.12)

d$aat2<-rnorm(n=nrow(d), mean=12, sd=1.5)
d$mpo2<-rnorm(n=nrow(d), mean=9000, sd=0.12)
d$neo2<-rnorm(n=nrow(d), mean=2100, sd=0.12)
d$reg1b2<-rnorm(n=nrow(d), mean=30, sd=5)

d$aat3<-rnorm(n=nrow(d), mean=13.5, sd=1.3)
d$mpo3<-rnorm(n=nrow(d), mean=10000, sd=0.12)
d$neo3<-rnorm(n=nrow(d), mean=1900, sd=0.12)

#Create and save dataset for Audrie:
stool_simulated_outcomes<-d %>%
    mutate(childid=as.character(dataid*10+childNo)) %>%
    select(childid, aat1, mpo1, neo1, aat2, mpo2, neo2, reg1b2, aat3, mpo3, neo3)
library(stringr)
stool_simulated_outcomes$childid<-str_pad(stool_simulated_outcomes$childid, 6, pad = "0")
head(stool_simulated_outcomes)    

#setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
#save(stool_simulated_outcomes, file="washb-BD-EE-sim-stool-outcomes.Rdata")
#write.dta(stool_simulated_outcomes, "washb-BD-EE-sim-stool-outcomes.dta")



############################
#Calculate outcomes:
############################

#dataframe of stool biomarkers:
Y<-d %>% select(aat1,mpo1,neo1,aat2,mpo2,neo2,aat3,mpo3,neo3)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"), c("WSH","Nutrition + WSH"))


#Create empty  matrices to hold the Ns and geometric means:
neo_t1_N_M<-mpo_t1_N_M<-aat_t1_N_M<-neo_t2_N_M<-mpo_t2_N_M<-aat_t2_N_M<-neo_t3_N_M<-mpo_t3_N_M<-aat_t3_N<-matrix(0,4,2)


  #N's and geometric means
aat_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(aat1)) %>% summarize(N=n(), geo_mean= exp(mean(log(aat1), na.rm=T)))   
mpo_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(mpo1)) %>% summarize(N=n(), geo_mean= exp(mean(log(mpo1), na.rm=T)))   
neo_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(neo1)) %>% summarize(N=n(), geo_mean= exp(mean(log(neo1), na.rm=T)))   
aat_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(aat2)) %>% summarize(N=n(), geo_mean= exp(mean(log(aat2), na.rm=T)))   
mpo_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(mpo2)) %>% summarize(N=n(), geo_mean= exp(mean(log(mpo2), na.rm=T)))   
neo_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(neo2)) %>% summarize(N=n(), geo_mean= exp(mean(log(neo2), na.rm=T)))   
aat_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(aat3)) %>% summarize(N=n(), geo_mean= exp(mean(log(aat3), na.rm=T)))   
mpo_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(mpo3)) %>% summarize(N=n(), geo_mean= exp(mean(log(mpo3), na.rm=T)))   
neo_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(neo3)) %>% summarize(N=n(), geo_mean= exp(mean(log(neo3), na.rm=T)))   



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
    temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_unadj[[i]][j,]<-as.numeric(temp$TR)
    colnames(res_unadj[[i]])<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(res_unadj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "Nutrition v Nutrition + WSH", "WSH v Nutrition + WSH"))
  }
}



############################
#Adjusted GLMs
############################

#Make vectors of adjustment variable names

Wvars<-c('sex', 'birthord',
         'momage', 'momheight','momedu','hfiacat',
         'Nlt18','Ncomp','watmin',
         'elec', 'asset_wardrobe', 'asset_table', 'asset_chair', 'asset_clock', 
         'n_asset_khat', 'n_asset_chouki', 'asset_radio', 
         'asset_tvcol', 'asset_refrig', 'asset_bike',
         'asset_moto', 'asset_sewmach', 'asset_mobile')
#Add in time varying covariates:
Wvars1<-c("aged1", "month1") 
Wvars2<-c("aged2", "month2") 
Wvars3<-c("aged3", "month3") 



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

d$birthord[is.na(d$birthord)]<-99
d$birthord<-factor(d$birthord)

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
#set covariates as factors
W$sex<-as.factor(W$sex)
W$momedu<-as.factor(W$momedu)
W$elec<-as.factor(W$elec)
W$asset_wardrobe<-as.factor(W$asset_wardrobe)
W$asset_table<-as.factor(W$asset_table)
W$asset_chair<-as.factor(W$asset_chair)
W$asset_clock<-as.factor(W$asset_clock)
W$asset_radio<-as.factor(W$asset_radio)
W$asset_tvcol<-as.factor(W$asset_tvcol)
W$asset_refrig<-as.factor(W$asset_refrig)
W$asset_bike<-as.factor(W$asset_bike)
W$asset_moto<-as.factor(W$asset_moto)
W$asset_sewmach<-as.factor(W$asset_sewmach)
W$asset_mobile<-as.factor(W$asset_mobile)


#Add in time-varying covariates
W1<- cbind(W, subset(d, select=Wvars1))
W2<- cbind(W, subset(d, select=Wvars2))
W3<- cbind(W, subset(d, select=Wvars3))

#Set time-varying covariates as factors
W1$month<-as.factor(W1$month)
W2$month<-as.factor(W2$month)
W3$month<-as.factor(W3$month)


#Run GLMs for the adjusted parameter estimates



#Create empty matrix to hold the tmle results:
res_adj<-list(neo_t1_adj=matrix(0,5,3), mpo_t1_adj=matrix(0,5,3), aat_t1_adj=matrix(0,5,3), 
                neo_t2_adj=matrix(0,5,3), mpo_t2_adj=matrix(0,5,3), aat_t2_adj=matrix(0,5,3), 
                neo_t3_adj=matrix(0,5,3), mpo_t3_adj=matrix(0,5,3), aat_t3_adj=matrix(0,5,3))

for(i in 1:3){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W1, id=d$clusterid.x, pair=NULL, family="binomial", contrast= contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=12345, print=T)
  res_adj<-temp$TR
  }
}
for(i in 1:3){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W2, id=d$clusterid.x, pair=NULL, family="binomial", contrast= contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=12345, print=T)
  res_adj<-temp$TR
  }
}
for(i in 1:3){
  for(j in 1:5){
  temp<-washb_glm(Y=log(Y[,i]), tr=d$tr, W=W3, id=d$clusterid.x, pair=NULL, family="binomial", contrast= contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=12345, print=T)
  res_adj<-temp$TR
  }
}

#colnames(diar_h1_pr_adj)<-c("PR","95CI lb","95CI ub","logPR","logSE","Zval","Pval")
#colnames(diar_h1_rd_adj)<-c("Risk Diff","95CI lb","95CI ub","SE","Zval","Pval")
colnames(diar_h1_pr_adj)<-c("PR","95CI lb","95CI ub")
colnames(diar_h1_rd_adj)<-c("RD","95CI lb","95CI ub")
rownames(diar_h1_rd_adj) <- rownames(diar_h1_pr_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")



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

neo_t1_adj_M=res_adj[[1]]
mpo_t1_adj_M=res_adj[[2]]
aat_t1_adj_M=res_adj[[3]] 
neo_t2_adj_M=res_adj[[4]] 
mpo_t2_adj_M=res_adj[[5]]
aat_t2_adj_M=res_adj[[6]] 
neo_t3_adj_M=res_adj[[7]]
mpo_t3_adj_M=res_adj[[8]]
aat_t3_adj_M=res_adj[[9]]


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(neo_t1_N_M, mpo_t1_N_M, aat_t1_N_M,
     neo_t2_N_M, mpo_t2_N_M, aat_t2_N_M, 
     neo_t3_N_M, mpo_t3_N_M, aat_t3_N_M, 
     file="stool_res_N_M.Rdata")


save(neo_t1_unadj_M, mpo_t1_unadj_M, aat_t1_unadj_M,
     neo_t2_unadj_M, mpo_t2_unadj_M, aat_t2_unadj_M, 
     neo_t3_unadj_M, mpo_t3_unadj_M, aat_t3_unadj_M, 
     file="stool_res_unadj_M.Rdata")

save(neo_t1_adj_M, mpo_t1_adj_M, aat_t1_adj_M,
     neo_t2_adj_M, mpo_t2_adj_M, aat_t2_adj_M, 
     neo_t3_adj_M, mpo_t3_adj_M, aat_t3_adj_M, 
     file="stool_res_adj_M.Rdata")
