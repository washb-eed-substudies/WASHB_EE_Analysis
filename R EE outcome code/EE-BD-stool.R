
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


#Temporarily generate fake outcome data for AAT (0.5, sd=0.12), NPO (10000, sd=2500), and NEO (2000, sd=250)
set.seed(12345)
d$AAT1<-rnorm(n=nrow(d), mean=0.4, sd=0.12)
d$NPO1<-rnorm(n=nrow(d), mean=11000, sd=0.12)
d$NEO1<-rnorm(n=nrow(d), mean=2000, sd=0.12)

d$AAT2<-rnorm(n=nrow(d), mean=0.6, sd=0.12)
d$NPO2<-rnorm(n=nrow(d), mean=9000, sd=0.12)
d$NEO2<-rnorm(n=nrow(d), mean=2100, sd=0.12)

d$AAT3<-rnorm(n=nrow(d), mean=0.5, sd=0.12)
d$NPO3<-rnorm(n=nrow(d), mean=10000, sd=0.12)
d$NEO3<-rnorm(n=nrow(d), mean=1900, sd=0.12)

#Create and save dataset for Audrie:
stool_simulated_outcomes<-d %>%
    mutate(childid=as.character(dataid*10+childNo)) %>%
    select(childid, AAT1, NPO1, NEO1, AAT2, NPO2, NEO2, AAT3, NPO3, NEO3)
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
Y<-d %>% select(AAT1,NPO1,NEO1,AAT2,NPO2,NEO2,AAT3,NPO3,NEO3)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"), c("WSH","Nutrition + WSH"))


#Create empty  matrices to hold the Ns and geometric means:
NEO_t1_N_M<-NPO_t1_N_M<-AAT_t1_N_M<-NEO_t2_N_M<-NPO_t2_N_M<-AAT_t2_N_M<-NEO_t3_N_M<-NPO_t3_N_M<-AAT_t3_N<-matrix(0,4,2)


  #N's and geometric means
AAT_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(AAT1)) %>% summarize(N=n(), geo_mean= exp(mean(log(AAT1), na.rm=T)))   
NPO_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(NPO1)) %>% summarize(N=n(), geo_mean= exp(mean(log(NPO1), na.rm=T)))   
NEO_t1_N_M<-d %>% group_by(tr) %>% subset(!is.na(NEO1)) %>% summarize(N=n(), geo_mean= exp(mean(log(NEO1), na.rm=T)))   
AAT_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(AAT2)) %>% summarize(N=n(), geo_mean= exp(mean(log(AAT2), na.rm=T)))   
NPO_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(NPO2)) %>% summarize(N=n(), geo_mean= exp(mean(log(NPO2), na.rm=T)))   
NEO_t2_N_M<-d %>% group_by(tr) %>% subset(!is.na(NEO2)) %>% summarize(N=n(), geo_mean= exp(mean(log(NEO2), na.rm=T)))   
AAT_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(AAT3)) %>% summarize(N=n(), geo_mean= exp(mean(log(AAT3), na.rm=T)))   
NPO_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(NPO3)) %>% summarize(N=n(), geo_mean= exp(mean(log(NPO3), na.rm=T)))   
NEO_t3_N_M<-d %>% group_by(tr) %>% subset(!is.na(NEO3)) %>% summarize(N=n(), geo_mean= exp(mean(log(NEO3), na.rm=T)))   



#Create empty matrix to hold the glm results:
NEO_t1_unadj<-NPO_t1_unadj<-AAT_t1_unadj<-matrix(0, nrow=5, ncol=3)
NEO_t2_unadj<-NPO_t2_unadj<-AAT_t2_unadj<-matrix(0, nrow=5, ncol=3)
NEO_t3_unadj<-NPO_t3_unadj<-AAT_t3_unadj<-matrix(0, nrow=5, ncol=3)

res_unadj<-list(NEO_t1_unadj=NEO_t1_unadj, NPO_t1_unadj=NPO_t1_unadj, AAT_t1_unadj=AAT_t1_unadj, 
                NEO_t2_unadj=NEO_t2_unadj, NPO_t2_unadj=NPO_t2_unadj, AAT_t2_unadj=AAT_t2_unadj, 
                NEO_t3_unadj=NEO_t3_unadj, NPO_t3_unadj=NPO_t3_unadj, AAT_t3_unadj=AAT_t3_unadj)


#Unadjusted glm models
for(i in 1:9){
  for(j in 1:5){
    temp<-washb_glm(Y=Y[,i], tr=d$tr, W=NULL, id=d$block.x, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    res_unadj[[i]][j,]<-as.numeric(temp$TR[1:3])
    colnames(res_unadj[[i]])<-c("RD","ci.l","ci.u")
    rownames(res_unadj[[i]])<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH", "Nutrition v Nutrition + WSH", "WSH v Nutrition + WSH"))
  }
}




#adjusted



#Create empty matrix to hold the tmle results:
res_adj<-list(NEO_t1_adj=matrix(0,5,3), NPO_t1_adj=matrix(0,5,3), AAT_t1_adj=matrix(0,5,3), 
                NEO_t2_adj=matrix(0,5,3), NPO_t2_adj=matrix(0,5,3), AAT_t2_adj=matrix(0,5,3), 
                NEO_t3_adj=matrix(0,5,3), NPO_t3_adj=matrix(0,5,3), AAT_t3_adj=matrix(0,5,3))

###
Make sure to select the right W
XXXXXXXX
####

Wvars<-c('sex','aged','month','momage','momedu','momheight','Ncomp ','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','roof','floor','cow','goat','dog','chicken','dminwat','hfiacat','fracode')


#Create the fracode staff experience factor
ad$staffid[ad$staffid==327] = 0
ad$staffid[ad$staffid==460] = 0
ad$staffid[ad$staffid==1213] = 0
ad$staffid[ad$staffid==1400] = 0
ad$staffid[ad$staffid==1405] = 0
ad$staffid[ad$staffid==1723] = 0
ad$staffid[ad$staffid==1727] = 0
ad$staffid[ad$staffid==1728] = 0
ad$staffid[ad$staffid==1830] = 0
ad$staffid[ad$staffid==2105] = 0
ad$staffid[ad$staffid==2112] = 0
ad$staffid[ad$staffid==2174] = 0
ad$staffid[ad$staffid==2217] = 0
ad$staffid[ad$staffid==2242] = 0
ad$staffid[ad$staffid==2311] = 0
ad$staffid[ad$staffid==2321] = 0
ad$staffid[ad$staffid==2328] = 0
ad$staffid[ad$staffid==2674] = 0
ad$staffid[ad$staffid==2847] = 0
ad$staffid[ad$staffid==3102] = 0
ad$staffid[ad$staffid==3322] = 0
ad$staffid[ad$staffid==3323] = 0
ad$staffid[ad$staffid==3352] = 0
ad$staffid[ad$staffid==3357] = 0
ad$staffid[ad$staffid==3408] = 0
ad$staffid[ad$staffid==3410] = 0
ad$staffid[ad$staffid==3418] = 0
ad$staffid[ad$staffid==3420] = 0
ad$staffid[ad$staffid==3421] = 0
ad$staffid[ad$staffid==3424] = 0
ad$staffid[ad$staffid==3425] = 0
ad$staffid[ad$staffid==3435] = 0
ad$staffid[ad$staffid==3436] = 0
ad$staffid[ad$staffid==3437] = 0
ad$staffid[ad$staffid==3783] = 0
ad$staffid[ad$staffid==4187] = 0
ad$staffid[ad$staffid==4328] = 0
ad$staffid[ad$staffid==4345] = 0
ad$staffid[ad$staffid==4347] = 0
ad$staffid[ad$staffid==4348] = 0
ad$staffid[ad$staffid==4382] = 0
ad$staffid[ad$staffid==4433] = 0
ad$staffid[ad$staffid==4438] = 0
ad$staffid[ad$staffid==4471] = 0
ad$staffid[ad$staffid==4515] = 0
ad$staffid[ad$staffid==4518] = 0
ad$staffid[ad$staffid==4522] = 0
ad$staffid[ad$staffid==4531] = 0
ad$staffid[ad$staffid==4548] = 0
ad$staffid[ad$staffid==4645] = 0
ad$staffid[ad$staffid==5422] = 0
ad$staffid[ad$staffid==7383] = 0
ad$staffid[ad$staffid==8152] = 0
ad$staffid[ad$staffid==8274] = 0
ad$staffid[ad$staffid==8604] = 0
ad$staffid[ad$staffid==8787] = 0
ad$staffid[ad$staffid==8883] = 0
ad$staffid[ad$staffid==8884] = 0
ad$staffid[ad$staffid==9999] = 0

ad$fracode <- factor(ad$staffid)



#Order data to replicate SL
ad <- ad[order(ad$block,ad$clusterid,ad$hhid,ad$childid,ad$studyyear),]



#subset W adjustment set
W<- subset(d, select=Wvars)

#check that all the factor variables are set
for(i in 1:ncol(W)){
  print(colnames(W)[i])
  print(class(W[,i])  )
}

#set covariates as factors
diarW$month<-as.factor(diarW$month)
diarW$HHS<-as.factor(diarW$HHS)
diarW$electricity<-as.factor(diarW$electricity)
diarW$radio<-as.factor(diarW$radio)
diarW$television<-as.factor(diarW$television)
diarW$mobile<-as.factor(diarW$mobile)
diarW$clock<-as.factor(diarW$clock)
diarW$bicycle<-as.factor(diarW$bicycle)
diarW$motorcycle<-as.factor(diarW$motorcycle)
diarW$stove<-as.factor(diarW$stove)
diarW$roof<-as.factor(diarW$roof)
diarW$floor<-as.factor(diarW$floor)
ad$block<-as.factor(ad$block)




#Run TMLE for the adjusted parameter estimates
 #Create empty matrices to hold results
diar_h1_pr_adj<-matrix(0, nrow=7, ncol=3)
diar_h1_rd_adj<-matrix(0, nrow=7, ncol=3)





for(i in 1:7){
  temp<-washb_tmle(Y=ad$diar7d, tr=ad$tr, W=diarW, id=ad$block,pair=ad$block,family="binomial", contrast= h1.contrasts[[i]],Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=T)
  diar_h1_pr_adj[i,1]<-temp$estimates$RR$psi
  diar_h1_pr_adj[i,2:3]<-temp$estimates$RR$CI
  #diar_h1_pr_adj[i,4]<-temp$estimates$RR$pvalue
  diar_h1_rd_adj[i,1]<-temp$estimates$ATE$psi
  diar_h1_rd_adj[i,2:3]<-temp$estimates$ATE$CI
  #diar_h1_rd_adj[i,4]<-temp$estimates$ATE$pvalue
  
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h1.contrasts[[i]], id=ad$clusterid, family=poisson(link="log"), print=F, verbose=F)
  #diar_h1_pr_adj[i,]<-as.matrix(temp$TR)
  #temp<-washb_glm(Y=ad$diar7d, tr=ad$tr, pair=ad$block, W=diarW, contrast = h1.contrasts[[i]], id=ad$clusterid, family="gaussian", print=F, verbose=F)
  #diar_h1_rd_adj[i,]<-as.matrix(temp$TR)
}


#colnames(diar_h1_pr_adj)<-c("PR","95CI lb","95CI ub","logPR","logSE","Zval","Pval")
#colnames(diar_h1_rd_adj)<-c("Risk Diff","95CI lb","95CI ub","SE","Zval","Pval")
colnames(diar_h1_pr_adj)<-c("PR","95CI lb","95CI ub")
colnames(diar_h1_rd_adj)<-c("RD","95CI lb","95CI ub")
rownames(diar_h1_rd_adj) <- rownames(diar_h1_pr_adj) <- c("Passive C v C","Water v C","Sanitation v C","Handwashing v C","WSH v C","Nutrition v C","Nutrition + WSH v C")



##########################################
#Save objects for replication
##########################################
NEO_t1_unadj_M=res_unadj[[1]]
NPO_t1_unadj_M=res_unadj[[2]]
AAT_t1_unadj_M=res_unadj[[3]] 
NEO_t2_unadj_M=res_unadj[[4]] 
NPO_t2_unadj_M=res_unadj[[5]]
AAT_t2_unadj_M=res_unadj[[6]] 
NEO_t3_unadj_M=res_unadj[[7]]
NPO_t3_unadj_M=res_unadj[[8]]
AAT_t3_unadj_M=res_unadj[[9]]


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(NEO_t1_N_M, NPO_t1_N_M, AAT_t1_N_M,
     NEO_t2_N_M, NPO_t2_N_M, AAT_t2_N_M, 
     NEO_t3_N_M, NPO_t3_N_M, AAT_t3_N_M, 
     file="stool_res_N_M.Rdata")


save(NEO_t1_unadj_M, NPO_t1_unadj_M, AAT_t1_unadj_M,
     NEO_t2_unadj_M, NPO_t2_unadj_M, AAT_t2_unadj_M, 
     NEO_t3_unadj_M, NPO_t3_unadj_M, AAT_t3_unadj_M, 
     file="stool_res_unadj_M.Rdata")

save(NEO_t1_adj_M, NPO_t1_adj_M, AAT_t1_adj_M,
     NEO_t2_adj_M, NPO_t2_adj_M, AAT_t2_adj_M, 
     NEO_t3_adj_M, NPO_t3_adj_M, AAT_t3_adj_M, 
     file="stool_res_adj_M.Rdata")
