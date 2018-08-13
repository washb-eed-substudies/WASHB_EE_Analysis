


rm(list=ls())
library(tidyverse)
library(xlsx)
library(stringr)
library(washb)
library(knitr)
library(lubridate)


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/Endline/")

#Saliva lab data
child <- read.xlsx("EE_Endline_Child_Saliva_FU2.xlsx", 1)
mother <- read.xlsx("EE_Endline_Mother_Saliva_FU2.xlsx", 1)


#Saliva survey
c_survey<- read.csv("Endline_child_Salimetrics_raw_data_28Dec15.csv")
m_survey<- read.csv("EE_Endline_Mother Salimetrics_raw_data_08Dec15.csv")


head(child)
head(mother)
head(c_survey)
head(m_survey)

#Load primary EED dataset
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/")
d <- read.csv("EE-BD_fulldata.csv")

#Load anthropometry outcomes and only keep ID and outcome variables
anthro<-read.csv("BD-EE-anthro.csv")
anthro <- anthro %>% subset(select=c(dataid, childNo,
                                     laz1,	waz1,	whz1,	hcz1,
                                     laz2,	waz2,	whz2,	hcz2,
                                     laz3,	waz3,	whz3,	hcz3))

#Merge in anthro measures
 d<-left_join(d, anthro, by=c("dataid","childNo"))

 #Create binary anthropometry outcomes
 d$laz1[d$laz1 < -5 | d$laz1 > 5] <- NA
 d$laz2[d$laz2 < -5 | d$laz2 > 5] <- NA
 d$laz3[d$laz3 < -5 | d$laz3 > 5] <- NA

 d$whz1[d$whz1 < -5 | d$whz1 > 5] <- NA
 d$whz2[d$whz2 < -5 | d$whz2 > 5] <- NA
 d$whz3[d$whz3 < -5 | d$whz3 > 5] <- NA
 
 d$waz1[d$waz1 < -5 | d$waz1 > 5] <- NA
 d$waz2[d$waz2 < -5 | d$waz2 > 5] <- NA
 d$waz3[d$waz3 < -5 | d$waz3 > 5] <- NA
 
 d$hcz1[d$hcz1 < -5 | d$hcz1 > 5] <- NA
 d$hcz2[d$hcz2 < -5 | d$hcz2 > 5] <- NA
 d$hcz3[d$hcz3 < -5 | d$hcz3 > 5] <- NA

 d$stunt1 <- ifelse(d$laz1 < -2, 1, 0)
     d$stunt1[is.na(d$laz1)] <- NA 
 d$stunt2 <- ifelse(d$laz2 < -2, 1, 0)
     d$stunt2[is.na(d$laz2)] <- NA
 d$stunt3 <- ifelse(d$laz3 < -2, 1, 0)
     d$stunt3[is.na(d$laz3)] <- NA

 d$wast1 <- ifelse(d$whz1 < -2, 1, 0)
     d$wast1[is.na(d$whz1)] <- NA
 d$wast2 <- ifelse(d$whz2 < -2, 1, 0)
     d$wast2[is.na(d$whz2)] <- NA
 d$wast3 <- ifelse(d$whz3 < -2, 1, 0)
     d$wast3[is.na(d$whz3)] <- NA      

 d$sstunt1 <- ifelse(d$laz1 < -3, 1, 0)
     d$sstunt1[is.na(d$laz1)] <- NA 
 d$sstunt2 <- ifelse(d$laz2 < -3, 1, 0)
     d$sstunt2[is.na(d$laz2)] <- NA
 d$sstunt3 <- ifelse(d$laz3 < -3, 1, 0)
     d$sstunt3[is.na(d$laz3)] <- NA

 d$swast1 <- ifelse(d$whz1 < -3, 1, 0)
     d$swast1[is.na(d$whz1)] <- NA
 d$swast2 <- ifelse(d$whz2 < -3, 1, 0)
     d$swast2[is.na(d$whz2)] <- NA
 d$swast3 <- ifelse(d$whz3 < -3, 1, 0)
     d$swast3[is.na(d$whz3)] <- NA     

 d$underwt1 <- ifelse(d$waz1 < -2, 1, 0)
     d$underwt1[is.na(d$waz1)] <- NA
 d$underwt2 <- ifelse(d$waz2 < -2, 1, 0)
     d$underwt2[is.na(d$waz2)] <- NA
 d$underwt3 <- ifelse(d$waz3 < -2, 1, 0)
     d$underwt3[is.na(d$waz3)] <- NA      

 d$sunderwt1 <- ifelse(d$waz1 < -3, 1, 0)
     d$sunderwt1[is.na(d$waz1)] <- NA
 d$sunderwt2 <- ifelse(d$waz2 < -3, 1, 0)
     d$sunderwt2[is.na(d$waz2)] <- NA
 d$sunderwt3 <- ifelse(d$waz3 < -3, 1, 0)
     d$sunderwt3[is.na(d$waz3)] <- NA       
     
#Grab IDs from survey datasets
# c_survey <- c_survey %>% 
#   rename(childno=memid, sample_id=sampleid) %>% 
#   subset(., select=c(dataid, childno, sample_id))
# 
# m_survey <- m_survey %>% 
#   rename(childno=q6, sample_id=sampleid) %>% 
#   subset(., select=c(dataid, childno, sample_id))


#Process child lab data

      #   There were 2 typo's in the sample IDs of the child FU2 lab results:
      # 26703E1Z01 instead of 25703E1Z01
      # 65303E1Z01 instead of 65603E1Z01
  child$sample_id <- as.character(child$sample_id)
  child$sample_id[child$sample_id=="25703E1Z01"] <- "26703E1Z01"
  child$sample_id[child$sample_id=="65603E1Z01"] <- "65303E1Z01"

    
  #Grab dataid from the sample ID
  child$dataid <- as.numeric(sapply(strsplit(child$sample_id, "E", fixed=T), `[`, 1))
  child$childNo <- as.numeric(substr(sapply(strsplit(child$sample_id, "E", fixed=T), `[`, 2),1,1))
  
  #mark FU2 status
  child$cFU2 <- NA
  child$cFU2[child$lea_result=="Positive" & child$leb_result=="Positive"] <- "Positive"
  child$cFU2[child$lea_result=="Negative" & child$leb_result=="Positive"] <- "Positive"
  child$cFU2[child$lea_result=="Positive" & child$leb_result=="Negative"] <- "Negative"
  child$cFU2[child$lea_result=="Negative" & child$leb_result=="Negative"] <- "Inconclusive"
  table(child$cFU2)
  
  child <- child %>% subset(., select=c(dataid, childNo, cFU2))
  


#Process mother lab data

  #Grab dataid from the sample ID
  mother$dataid <- as.numeric(sapply(strsplit(as.character(mother$sample_id), "E", fixed=T), `[`, 1))

  #mark FU2 status
  mother$mFU2 <- NA
  mother$mFU2[mother$lea_result=="Positive" & mother$leb_result=="Positive"] <- "Positive"
  mother$mFU2[mother$lea_result=="Negative" & mother$leb_result=="Positive"] <- "Positive"
  mother$mFU2[mother$lea_result=="Positive" & mother$leb_result=="Negative"] <- "Negative"
  mother$mFU2[mother$lea_result=="Negative" & mother$leb_result=="Negative"] <- "Inconclusive"
  table(mother$mFU2)
  
  mother <- mother %>% subset(., select=c(dataid, mFU2))
  
  
#Merge saliva results with the primary EED dataset
d <- left_join(d, child, by=c("dataid","childNo"))
d <- left_join(d, mother, by=c("dataid"))


#Summary statistics on FU2

overall_c <- d %>% group_by(cFU2) %>% summarize(N_child=n())
overall_m <- d %>% group_by(mFU2) %>% filter(childNo==1) %>% summarize(N_mother=n()) # filter to childNo==1 to remove duplicated
overall_c <- data.frame(tr="Overall", overall_c)
overall_m <- data.frame(tr="Overall", overall_m)

tr_c <- d %>% group_by(tr, cFU2) %>% summarize(N_child=n())
tr_m <- d %>% group_by(tr, mFU2) %>% filter(childNo==1) %>% summarize(N_mother=n()) # filter to childNo==1 to remove duplicated

summary_c <- bind_rows(overall_c, tr_c) %>% rename(FU2_status = cFU2)
summary_m <- bind_rows(overall_m, tr_m) %>% rename(FU2_status = mFU2)
summary_res <- merge(summary_c, summary_m, by=c("tr","FU2_status"))
summary_res$tr <- factor(summary_res$tr, levels=c("Overall", "Control", "WSH", "Nutrition", "Nutrition + WSH"))
summary_res <- summary_res %>% arrange(tr)
summary_res

# 
# #compare to audrie
# da <- read.csv("C:/Users/andre/Downloads/refu2statusns/bangladesh-dm-ee-anthro-diar-ee-med-stool-fu2-blind-tr-enrol-covariates-lab.csv")
# table(d$cFU2)
# table(da$status)
# 
# d <- d %>% rename(childno=childNo)
# df <- left_join(d, da, by=c("dataid","childno"))
# table(df$cFU2)
# table(df$status)
# df$FU2<-NA
# df$FU2[df$cFU2=="Inconclusive"] <- 2
# df$FU2[df$cFU2=="Negative"] <- 0
# df$FU2[df$cFU2=="Positive"] <- 1
# table(df$status==df$FU2)
# 
# n_occur <- data.frame(table(paste0(d$dataid, d$childno)))
# n_occur[n_occur$Freq > 1,]



#Create mother and child FU2 analysis datasets
dc <- d %>% filter(!is.na(cFU2) & cFU2!="Inconclusive")
dm <- d %>% filter(!is.na(mFU2) & mFU2!="Inconclusive")


#dataframe of EED biomarkers:
Yc <- dc %>% subset(., select=c(Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3, neo1,mpo1,aat1,neo2,mpo2,aat2,reg1b2,neo3,mpo3,aat3))
Ym <- dm %>% subset(., select=c(Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3, neo1,mpo1,aat1,neo2,mpo2,aat2,reg1b2,neo3,mpo3,aat3))

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"), c("WSH","Nutrition + WSH"), c("Nutrition","Nutrition + WSH"))


#-------------------------------------------------------
# FU2 as an effect modifier
#-------------------------------------------------------

#Create empty matrix to hold the glm results:
res_childFU2_EM_unadj <- list()
res_motherFU2_EM_unadj <- list()

#Unadjusted glm models -FU2 as an EM
for(i in 1:ncol(Yc)){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp <- washb_glm(Y=log(Yc[,i]), tr=dc$tr, W=data.frame(cFU2=dc$cFU2), V="cFU2", id=dc$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    #Get interaction p-val
    pval<-temp$fit$`Pr(>|z|)`[nrow(temp$fit)]
    temp <- data.frame(contrast= paste0(contrasts[[j]][1]," v ",contrasts[[j]][2]), temp$lincom, int_pval=pval)
    colnames(temp)<-c("Contrast","Subgroup","RD","Std. Error","ci.l","ci.u", "z value", "Pval", "InteractionPval")
    if(j==1){
      res_childFU2_EM_unadj[[i]] <- temp
    }else{
      res_childFU2_EM_unadj[[i]] <- rbind(res_childFU2_EM_unadj[[i]], temp) 
    }
  }
}

names(res_childFU2_EM_unadj) <- colnames(Yc)
  

for(i in 1:ncol(Ym)){
  for(j in 1:5){
    #note the log transformation of the outcome prior to running GLM model:
    temp <- washb_glm(Y=log(Ym[,i]), tr=dm$tr, W=data.frame(mFU2=dm$mFU2), V="mFU2", id=dm$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    #Get interaction p-val
    pval<-temp$fit$`Pr(>|z|)`[nrow(temp$fit)]
    temp <- data.frame(contrast= paste0(contrasts[[j]][1]," v ",contrasts[[j]][2]), temp$lincom, int_pval=pval)
    colnames(temp)<-c("Contrast","Subgroup","RD","Std. Error","ci.l","ci.u", "z value", "Pval", "InteractionPval")
    if(j==1){
      res_motherFU2_EM_unadj[[i]] <- temp
    }else{
      res_motherFU2_EM_unadj[[i]] <- rbind(res_motherFU2_EM_unadj[[i]], temp) 
    }
  }
}

names(res_motherFU2_EM_unadj) <- colnames(Ym)



#-------------------------------------------------------
# FU2 as a risk factor
#-------------------------------------------------------



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

#Clean adjustment variables 

#Set all factors as characters to allow level replacement
for(i in 1:ncol(d)){
  if(class(d[,i])=="factor"){
    d[,i] <- as.character(d[,i])
  }
}

#Order data to replicate SL
d <- d[order(d$dataid,d$childNo, d$svy),]

#Clean covariates
d$sex<-as.factor(d$sex)
d$sex=relevel(d$sex,ref="0")


#Set birthorder to 1, >=2, or missing
class(d$birthord)
d$birthord[d$birthord>1]<-"2+"
d$birthord[is.na(d$birthord)]<-"missing"
d$birthord<-factor(d$birthord)



#Truncate unrealistic levels of n_chickens to 60
table(d$n_chickens)
d$n_chickens[d$n_chickens>60]<-60
table(d$n_chickens)


#Relevel all factors
d$momedu=relevel(factor(d$momedu),ref="No education")
d$hfiacat=relevel(factor(d$hfiacat),ref="Food Secure")
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

#Re-subset d so new re-leveled factors are included
dc <- d %>% filter(!is.na(cFU2) & cFU2!="Inconclusive")
dm <- d %>% filter(!is.na(mFU2) & mFU2!="Inconclusive")

Wc<- subset(dc, select=Wvars)
Wm<- subset(dm, select=Wvars)



#Add in time-varying covariates
dc <- dc %>% mutate(monsoon1 = ifelse(st_month1 > 4 & st_month1 < 11, "1", "0"),
                  monsoon2 = ifelse(st_month2 > 4 & st_month2 < 11, "1", "0"),
                  monsoon3 = ifelse(st_month3 > 4 & st_month3 < 11, "1", "0"),
                  monsoon1 = ifelse(is.na(st_month1),"missing", monsoon1),
                  monsoon2 = ifelse(is.na(st_month2),"missing", monsoon2),
                  monsoon3 = ifelse(is.na(st_month3),"missing", monsoon3),
                  monsoon1 = factor(monsoon1),
                  monsoon2 = factor(monsoon2),
                  monsoon3 = factor(monsoon3))
dm <- dm %>% mutate(monsoon1 = ifelse(st_month1 > 4 & st_month1 < 11, "1", "0"),
                  monsoon2 = ifelse(st_month2 > 4 & st_month2 < 11, "1", "0"),
                  monsoon3 = ifelse(st_month3 > 4 & st_month3 < 11, "1", "0"),
                  monsoon1 = ifelse(is.na(st_month1),"missing", monsoon1),
                  monsoon2 = ifelse(is.na(st_month2),"missing", monsoon2),
                  monsoon3 = ifelse(is.na(st_month3),"missing", monsoon3),
                  monsoon1 = factor(monsoon1),
                  monsoon2 = factor(monsoon2),
                  monsoon3 = factor(monsoon3))


Wvars1<-c("st_aged1", "monsoon1") 
Wvars2<-c("st_aged2", "monsoon2") 
Wvars3<-c("st_aged3", "monsoon3") 

cW1<- cbind(Wc, subset(dc, select=Wvars1))
cW2<- cbind(Wc, subset(dc, select=Wvars2))
cW3<- cbind(Wc, subset(dc, select=Wvars3))

mW1<- cbind(Wm, subset(dm, select=Wvars1))
mW2<- cbind(Wm, subset(dm, select=Wvars2))
mW3<- cbind(Wm, subset(dm, select=Wvars3))


#Create lists of adjustment covariates
cWlist <- list(cW1,cW1,cW1,cW2,cW2,cW2,cW3,cW3,cW3,
               cW1,cW1,cW1,cW2,cW2,cW2,cW2,cW3,cW3,cW3)
mWlist <- list(mW1,mW1,mW1,mW2,mW2,mW2,mW3,mW3,mW3,
               mW1,mW1,mW1,mW2,mW2,mW2,mW2,mW3,mW3,mW3)




#-------------------------------------------------------
# Associations with EED outcomes
#-------------------------------------------------------

library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
#library=c("SL.glm")

#Set up SL library

#Create empty matrix to hold the glm results:
res_childFU2_RF_adj <- NULL
res_motherFU2_RF_adj <- NULL

#TMLE models -FU2 as a RF
for(i in  1:ncol(Yc)){
    #note the log transformation of the outcome prior to running TMLE model:
    temp<-washb_tmle(Y=log(Yc[,i]), tr=dc$cFU2, W=cWlist[[i]], id=dc$block, pair=NULL, family="gaussian", contrast=c("Negative", "Positive"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$ATE)))
    colnames(temp)<-c("ATE","variance","ci.l","ci.u", "Pvalue")
    res_childFU2_RF_adj <- rbind(res_childFU2_RF_adj , temp) 
} 
rownames(res_childFU2_RF_adj)<-colnames(Yc)




for(i in  1:ncol(Ym)){
    #note the log transformation of the outcome prior to running TMLE model:
    temp<-washb_tmle(Y=log(Ym[,i]), tr=dm$mFU2, W=mWlist[[i]], id=dm$block, pair=NULL, family="gaussian", contrast=c("Negative", "Positive"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$ATE)))
    colnames(temp)<-c("ATE","variance","ci.l","ci.u", "Pvalue")
    res_motherFU2_RF_adj <- rbind(res_motherFU2_RF_adj , temp) 
} 
rownames(res_motherFU2_RF_adj) <- colnames(Ym)
  


#-------------------------------------------------------
# Associations with growth outcomes
#-------------------------------------------------------


#dataframe of EED biomarkers:
Yc_cont <- dc %>% subset(., select=c(laz1, laz2, laz3, whz1, whz2, whz3, waz1, waz2, waz3, hcz1, hcz2, hcz3))
Ym_cont <- dm %>% subset(., select=c(laz1, laz2, laz3, whz1, whz2, whz3, waz1, waz2, waz3, hcz1, hcz2, hcz3))

Yc_bin <- dc %>% subset(., select=c( stunt1, stunt2, stunt3, wast1, wast2, wast3, 
                                     sstunt1, sstunt2, sstunt3, swast1, swast2, swast3,  
                                     underwt1, underwt2, underwt3,
                                     sunderwt1, sunderwt2, sunderwt3))
Ym_bin <- dm %>% subset(., select=c( stunt1, stunt2, stunt3, wast1, wast2, wast3, 
                                     sstunt1, sstunt2, sstunt3, swast1, swast2, swast3,  
                                     underwt1, underwt2, underwt3,
                                     sunderwt1, sunderwt2, sunderwt3))



    
     


#Create empty matrix to hold the glm results:
res_childFU2_growthRF_cont <- NULL
res_motherFU2_growthRF_cont <- NULL
res_childFU2_growthRF_bin <- NULL
res_motherFU2_growthRF_bin <- NULL

#TMLE models -continious outcomes 
for(i in  1:ncol(Yc_cont)){
    #note the log transformation of the outcome prior to running TMLE model:
    temp<-washb_tmle(Y=Yc_cont[,i], tr=dc$cFU2, W=cWlist[[i]], id=dc$block, pair=NULL, family="gaussian", contrast=c("Negative", "Positive"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$ATE)))
    colnames(temp)<-c("ATE","variance","ci.l","ci.u", "Pvalue")
    res_childFU2_growthRF_cont <- rbind(res_childFU2_growthRF_cont , temp) 
} 
rownames(res_childFU2_growthRF_cont)<-colnames(Yc_cont)


for(i in  1:ncol(Ym_cont)){
    #note the log transformation of the outcome prior to running TMLE model:
    temp<-washb_tmle(Y=Ym_cont[,i], tr=dm$mFU2, W=mWlist[[i]], id=dm$block, pair=NULL, family="gaussian", contrast=c("Negative", "Positive"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$ATE)))
    colnames(temp)<-c("ATE","variance","ci.l","ci.u", "Pvalue")
    res_motherFU2_growthRF_cont <- rbind(res_motherFU2_growthRF_cont , temp) 
} 
rownames(res_motherFU2_growthRF_cont) <- colnames(Ym_cont)
  
#TMLE models -binary outcomes 
for(i in  1:ncol(Yc_bin)){
    #note the log transformation of the outcome prior to running TMLE model:
    temp<-washb_tmle(Y=Yc_bin[,i], tr=dc$cFU2, W=cWlist[[i]], id=dc$block, pair=NULL, family="binomial", contrast=c("Negative", "Positive"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$RR)))
    colnames(temp)<-c("RR","ci.l","ci.u", "Pvalue", "logRR","logRR_var")
    res_childFU2_growthRF_bin <- rbind(res_childFU2_growthRF_bin , temp) 
} 
rownames(res_childFU2_growthRF_bin)<-colnames(Yc_bin)


for(i in  1:ncol(Ym_bin)){
    #note the log transformation of the outcome prior to running TMLE model:
    temp<-washb_tmle(Y=Ym_bin[,i], tr=dm$mFU2, W=mWlist[[i]], id=dm$block, pair=NULL, family="binomial", contrast=c("Negative", "Positive"), print=F, Q.SL.library=library)
    temp<-(t(unlist(temp$estimates$RR)))
    colnames(temp)<-c("RR","ci.l","ci.u", "Pvalue", "logRR","logRR_var")
    res_motherFU2_growthRF_bin <- rbind(res_motherFU2_growthRF_bin , temp) 
} 
rownames(res_motherFU2_growthRF_bin) <- colnames(Ym_bin)
  

#-------------------------------------------------------
# Plotting dataframe data management
#-------------------------------------------------------




#Create plotting dataframes for EM results
resC<- do.call("rbind", res_childFU2_EM_unadj)
resM<- do.call("rbind", res_motherFU2_EM_unadj)

resC$Outcome <- sapply(strsplit(rownames(resC), ".", fixed=T), `[`, 1)
resM$Outcome <- sapply(strsplit(rownames(resM), ".", fixed=T), `[`, 1)


#Drop the non-control contrasts
resC<- resC %>% filter(Contrast!="WSH v Nutrition + WSH" & Contrast!="Nutrition v Nutrition + WSH")
resM<- resM %>% filter(Contrast!="WSH v Nutrition + WSH" & Contrast!="Nutrition v Nutrition + WSH")

#Grab time of outcome
resC$Round <- str_sub(resC$Outcome,-1,-1)
resM$Round <- str_sub(resM$Outcome,-1,-1)

#Grab biomarker
resC$biomarker = substr(resC$Outcome,1,nchar(resC$Outcome)-1)
resM$biomarker = substr(resM$Outcome,1,nchar(resM$Outcome)-1)

resC$biomarker <- factor(resC$biomarker, levels=c("Lact","Mann","LM","neo","mpo","aat","reg1b"))
resM$biomarker <- factor(resM$biomarker, levels=c("Lact","Mann","LM","neo","mpo","aat","reg1b"))

#Create combination of intervention and EM variable
resC$X <- paste0(gsub("Nutrition","N",sapply(strsplit(as.character(resC$Contrast), "v ", fixed=T), `[`, 2)), " FU2", resC$Subgroup)
resM$X <- paste0(gsub("Nutrition","N",sapply(strsplit(as.character(resM$Contrast), "v ", fixed=T), `[`, 2)), " FU2", resM$Subgroup)

resC$X <- gsub("Positive", "+", resC$X)
resC$X <- gsub("Negative", "-", resC$X)
resM$X <- gsub("Positive", "+", resM$X)
resM$X <- gsub("Negative", "-", resM$X)






#-------------------------------------------------------
# Results plots
#-------------------------------------------------------


#Plot parameters
scaleFUN <- function(x) sprintf("%.2f", x)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")





C_plot_EM<- ggplot(resC, aes(x=X)) + 
        geom_point(aes(y=RD, fill=Contrast, color=Contrast), size = 4) +
        geom_linerange(aes(ymin=ci.l, ymax=ci.u, color=Contrast),
                       alpha=0.5, size = 3) +
        labs(x = "Treatment contrast and FU2 status", y = "Average treatment effect") +
        geom_text(aes(x=X, y=ci.u+0.02, label=ifelse(resC$InteractionPval<0.05,"*",""))) + 
        geom_hline(yintercept = 0) +
        #coord_cartesian(ylim=range(yticks)) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        scale_size_continuous(range = c(0.5, 1))+
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        facet_grid(biomarker~Round,) +
        ggtitle("Child FU2 status")


M_plot_EM<- ggplot(resM, aes(x=X)) + 
        geom_point(aes(y=RD, fill=Contrast, color=Contrast), size = 4) +
        geom_linerange(aes(ymin=ci.l, ymax=ci.u, color=Contrast),
                       alpha=0.5, size = 3) +
        labs(x = "Treatment contrast and FU2 status", y = "Average treatment effect") +
        geom_text(aes(x=X, y=ci.u+0.02, label=ifelse(resM$InteractionPval<0.05,"*",""))) + 
        geom_hline(yintercept = 0) +
        #coord_cartesian(ylim=range(yticks)) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        scale_size_continuous(range = c(0.5, 1))+
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        facet_grid(biomarker~Round) +
        ggtitle("Mother FU2 status")

plotdf <-data.frame(X= rownames(plotdf), res_childFU2_growthRF_cont)
plotdf$outcome <- substr(plotdf$X, 1,3)
C_plot_cont <- ggplot(plotdf, aes(x=X)) + 
        geom_point(aes(y=ATE, fill=outcome, color=outcome), size = 4) +
        geom_linerange(aes(ymin=ci.l, ymax=ci.u, color=outcome),
                       alpha=0.5, size = 3) +
        labs(x = "Continious growth outcome", y = "Average treatment effect") +
        geom_hline(yintercept = 0) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        ggtitle("Child FU2 status effect on continious growth outcomes")




# Save results
save(summary_res,
     res_childFU2_EM_unadj, res_motherFU2_EM_unadj,
     res_childFU2_RF_adj, res_motherFU2_RF_adj,
     res_childFU2_growthRF_cont, res_motherFU2_growthRF_cont,
     res_childFU2_growthRF_bin,res_motherFU2_growthRF_bin,
     resC, resM, C_plot_EM, M_plot_EM, 
     file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FU2_results.Rdata")






