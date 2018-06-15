



rm(list=ls())
library(tidyverse)
library(foreign)
library(washb)
library(lubridate)


#Load in treatment information
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew")
tr <- read.csv("raw CSV/washk_TR.csv")
tr$tr <- factor(tr$tr, levels = c("Control",  "WSH", "Nutrition", "Nutrition + WSH"))
head(tr)

#Child dates of birth
dob <- readRDS("WBK-EE-childDOB.rds")

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

#Baseline covariates
enrol <- readRDS("WBK-EE-covariates.rds")
head(enrol)

d <- left_join(outcomes, dob, by="childid")

d <- left_join(d, stsurv, by="childid")

d <- left_join(d, enrol, by="hhid")

d <- left_join(d, tr, by="clusterid")



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

#Drop rows with no outcomes
d <- d %>% filter(!is.na(aat1) | !is.na(aat2) | !is.na(aat3) | 
                    !is.na(mpo1) | !is.na(mpo2) | !is.na(mpo3) | 
                    !is.na(neo1) | !is.na(neo2) | !is.na(neo3))


#drop outcomes not merged to treatment arm
#Note- this child only does not merge to the blinded treatments
# Child has a true treatment assignment
no_tr <- which(is.na(d$tr))
d$childid[no_tr]
d <- d[!is.na(d$tr),]


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
               




#-------------------------------------------
# stratify below and above 6 
# months of age 
#-------------------------------------------

summary(d$agem1)

d <- d %>% filter(!is.na(agem1))

d$above6mo <- ifelse(d$agem1>=6, 1,0)

d %>% group_by(above6mo) %>% summarize(mn = mean(aat1, na.rm=T), age=mean(agem1))

d %>% group_by(tr,above6mo) %>% summarize(mn = mean(aat1, na.rm=T))

d1 <- d %>% filter(above6mo==0)
d2 <- d %>% filter(above6mo==1)


#-------------------------------------------
#Calculate treatment difference:
#-------------------------------------------

mpo_t1_under6<-mpo_t1_above6<-neo_t1_under6<-neo_t1_above6<-aat_t1_under6<-aat_t1_above6<-matrix(0, nrow=3, ncol=6)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))


#Unadjusted glm models
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$aat1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    aat_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(aat_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(aat_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d2$aat1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    aat_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(aat_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(aat_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }

  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$neo1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    neo_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(neo_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(neo_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d2$neo1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    neo_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(neo_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(neo_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }

  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$mpo1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    mpo_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(mpo_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(mpo_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    mpo<-washb_glm(Y=log(d2$mpo1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    mpo_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(mpo_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(mpo_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }




#-------------------------------------------
# Create plot data.frame
#-------------------------------------------

aat_t1_under6
aat_t1_above6
res <- data.frame(rbind(aat_t1_under6, aat_t1_above6,
                        neo_t1_under6 ,neo_t1_above6,
                        mpo_t1_under6, mpo_t1_above6))


df <- data.frame(outcome= c(rep("AAT",6),rep("NEO",6),rep("MPO",6)),
                 above6mo=rep(c(rep("Under 6 months old",3), rep("6 months or older",3)),3),
                 contrast=rep(c("C v WSH","C v N","C v N+WSH"),6),
                 RD=res$RD,
                 ci.lb=res$ci.l,
                 ci.ub=res$ci.u)
df$above6mo <- factor(df$above6mo)
df$above6mo <- relevel(df$above6mo, ref="Under 6 months old")


#-------------------------------------------
# Customize plot layout
#-------------------------------------------

# main study colors
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cols <- c("gray30",cbPalette[c(2:4,6:8)])
# brighter color blind palette:  https://personal.sron.nl/~pault/ 
cblack <- "#000004FF"
cblue <- "#3366AA"
cteal <- "#11AA99"
cgreen <- "#66AA55"
cchartr <- "#CCCC55"
cmagent <- "#992288"
cred <- "#EE3333"
corange <- "#EEA722"
cyellow <- "#FFEE33"
cgrey <- "#777777"

#tr.cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange,N=cred,"WSH+N"=cmagent)
tr.cols=c(C=cblack,WSH=cblue,N=cred,"WSH+N"=cgreen)

#cols=c("C v WSH"=corange,"C v N"=cred,"C v N+WSH"=cmagent)
cols=c("C v WSH"=cblue,"C v N"=cred,"C v N+WSH"=cgreen)

theme_set(theme_bw())

p<-ggplot(df, aes(x=contrast)) + 
    geom_point(aes(y=RD, fill=contrast, color=contrast), size = 4) +
    geom_linerange(aes( ymin=ci.lb, ymax=ci.ub, color=contrast),
                   alpha=0.5, size = 3) +
    labs(x = "Contrast", y = "Log Difference") +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values=cols) +
    scale_colour_manual(values=cols) +
    theme(strip.background = element_blank(),
      legend.position="none",
      strip.text.x = element_text(size=12),
      axis.text.x = element_text(size=12)) +
    facet_wrap(outcome~above6mo, ncol=2, scales = "fixed") +
    ggtitle("WBK Intervention Effects on stool biomarkers, stratified by age at measurement round 1")
p


ggsave(p, filename = "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Figures/EE-Stool-6mo-diff-plot.pdf",width=10,height=8.5)




