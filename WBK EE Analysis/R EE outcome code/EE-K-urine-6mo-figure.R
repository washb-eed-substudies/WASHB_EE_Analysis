


###Load in data
#rm(list=ls())
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
               


#-------------------------------------------
# stratify below and above 6 
# months of age 
#-------------------------------------------

summary(d$agem1)
summary(d$agem2)

d <- d %>% filter(!is.na(agem1))

d$above6mo <- ifelse(d$agem1>=6, 1,0)


d1 <- d %>% filter(above6mo==0)
d2 <- d %>% filter(above6mo==1)


#-------------------------------------------
#Calculate treatment difference:
#-------------------------------------------

#Create empty matrix to hold the glm results:
Lact_t1_under6<-Mann_t1_under6<-LM_t1_under6<-matrix(0, nrow=3, ncol=6)
Lact_t1_above6<-Mann_t1_above6<-LM_t1_above6<-matrix(0, nrow=3, ncol=6)

#Set contrasts:
contrasts <- list(c("Control","WSH"), c("Control","Nutrition"), c("Control","Nutrition + WSH"))


#Unadjusted glm models
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$Lact1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    Lact_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(Lact_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(Lact_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d2$Lact1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    Lact_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(Lact_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(Lact_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }

  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$Mann1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    Mann_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(Mann_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(Mann_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d2$Mann1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    Mann_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(Mann_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(Mann_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }

  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    temp<-washb_glm(Y=log(d1$LM1), tr=d1$tr, W=NULL, id=d1$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    LM_t1_under6[j,]<-as.numeric(temp$TR)
    colnames(LM_t1_under6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(LM_t1_under6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }
  for(j in 1:3){
    #note the log transformation of the outcome prior to running GLM model:
    LM<-washb_glm(Y=log(d2$LM1), tr=d2$tr, W=NULL, id=d2$block, pair=NULL, family="gaussian", contrast= contrasts[[j]], print=F)
    LM_t1_above6[j,]<-as.numeric(temp$TR)
    colnames(LM_t1_above6)<-c("RD","ci.l","ci.u", "Std. Error", "z value", "Pval")
    rownames(LM_t1_above6)<-c(c("Control v WSH", "Control v Nutrition", "Control v Nutrition + WSH"))
  }




#-------------------------------------------
# Create plot data.frame
#-------------------------------------------

Lact_t1_under6
Lact_t1_above6
res <- data.frame(rbind(Lact_t1_under6, Lact_t1_above6,
                        Mann_t1_under6 ,Mann_t1_above6,
                        LM_t1_under6, LM_t1_above6))


df <- data.frame(outcome= c(rep("Lact",6),rep("Mann",6),rep("LM",6)),
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
    ggtitle("WBK Intervention Effects on urine biomarkers, stratified by age at measurement round 1")
p


ggsave(p, filename = "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Figures/EE-Urine-6mo-diff-plot.pdf",width=10,height=8.5)
ggsave(p, filename = "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Figures/EE-Urine-6mo-diff-plot.png",width=10,height=8.5)




