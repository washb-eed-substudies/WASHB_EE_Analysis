
#---------------------------------------
# EE-K-tables.R
#
# andrew mertens (amertens@berkeley.edu)
# and sophia tan
# 
# Create R-objects for xtable printing
# for the WASH Benefits Bangladesh EED 
# substudy
#---------------------------------------




rm(list=ls())
library(foreign)
library(dplyr)
library(washb)
library(tidyr)
library(reshape2)
library(xtable)


# Table functions
cleantable <- function(x,digits) {
 print( xtable(x,digits=digits),
        sanitize.text.function=function(y){y},
        floating=FALSE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        only.contents=TRUE,
        hline.after=NULL
 )
}

pt.est.n.f=function(obj,decimals,scale){
  pts=obj[1]*scale
  pt=sprintf(paste("%0.0",decimals,"f",sep=""),pts)
  a=paste(pt, " (",obj[4],")",sep="")
  return(a)
}

pt.est.f=function(obj,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),obj[1]*scale)
  return(a)
}

per.f=function(obj,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),obj[1]*scale)
  a=paste(as.numeric(a),"\\%",sep="")
  return(a)
}


ci.f=function(obj,decimals,scale){
  b=sprintf(paste("%0.0",decimals,"f",sep=""),obj[2]*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),obj[3]*scale)
  return(paste("(",b,", ",c,")",sep=""))
}

pt.est.ci.f=function(obj,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),obj[1]*scale)
  b=sprintf(paste("%0.0",decimals,"f",sep=""),obj[2]*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),obj[3]*scale)
  return(paste(a," (",b,", ",c,")",sep=""))
}




#Function to round 0.5 away from zero (differs from R's default behavior)
round2 <- function(x, n=3) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

rnd<- function(x, n=2) {
  x<-round2(x,3)
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

#--------------------------------
#Table 2
#--------------------------------

#load objects
setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Results/Andrew/")
#setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Results/Andrew/")
load("stool_res_unadj_M.Rdata")
load("stool_res_adj_M.Rdata")
load("stool_res_N_M.Rdata")
load("stool_res_means.Rdata")
load("urine_res_unadj_M.Rdata")
load("urine_res_adj_M.Rdata")
load("urine_res_N_M.Rdata")
load("urine_res_means.Rdata")

aat_t1_N_M
aat_t2_N_M
aat_t3_N_M
aat_t1_mn
aat_t2_mn
aat_t3_mn
aat_t1_unadj_M
aat_t2_unadj_M
aat_t3_unadj_M
mpo_t1_unadj_M
mpo_t2_unadj_M
mpo_t3_unadj_M
neo_t1_unadj_M
neo_t2_unadj_M
neo_t3_unadj_M



 
glm_print<-function(n, mean, glm, t=F){
# n<-aat_t1_N_M
# mean<-aat_t1_mn
# glm<-aat_t1_unadj_M

rownames(glm)<-NULL
glmh1<-rbind(rep(NA, 3), glm[1:3,1:3])
#adjh1<-rbind(rep(NA, 3), adj[1:3,1:3])
glmh2a<-rbind(rep(NA, 3),rep(NA, 3),rep(NA, 3), glm[5,1:3])
glmh2b<-rbind(rep(NA, 3),rep(NA, 3),rep(NA, 3), glm[4,1:3])


obj<-cbind(n[,1:2], mean[,2:3], glmh1, glmh2a, glmh2b)

  if(t==F){
        obj<-as.data.frame((obj))
  }else{
        obj<-as.data.frame(t(obj))
  }

obj[,3]<-sprintf("%1.2f",obj[,3])
obj[,4]<-sprintf("%1.2f",obj[,4])
obj[2:4,5]<-sprintf("%1.2f",obj[2:4,5])
obj[2:4,6]<-sprintf("%1.2f",obj[2:4,6])
obj[2:4,7]<-sprintf("%1.2f",obj[2:4,7])
obj[4,8]<-sprintf("%1.2f",obj[4,8])
obj[4,9]<-sprintf("%1.2f",obj[4,9])
obj[4,10]<-sprintf("%1.2f",obj[4,10])
obj[4,11]<-sprintf("%1.2f",obj[4,11])
obj[4,12]<-sprintf("%1.2f",obj[4,12])
obj[4,13]<-sprintf("%1.2f",obj[4,13])
  #Add later if p-val is needed:
  #if(as.numeric(obj[6])<0.05){obj[6]<-paste0(sprintf("%1.3f",obj[c(6)]),"*")
  #                            flag=T}

  rownames(obj)<-colnames(obj)<-NULL
  
  obj[,14]<-paste(obj[,5]," (",obj[,6],",",obj[,7],")", sep="")
  obj[,15]<-paste(obj[,8]," (",obj[,9],",",obj[,10],")", sep="")
  obj[,16]<-paste(obj[,11]," (",obj[,12],",",obj[,13],")", sep="")

  obj<-obj[,-c(5:13)]
  obj[1,5]<-""
  obj[1:3,6:7]<-""
  #out<-paste(obj[1],"(",obj[2],",",obj[3],") P=",obj[4], sep="")
  return(obj)
}

tab2<-rbind(
  glm_print(mpo_t1_N_M, mpo_t1_mn, mpo_t1_unadj_M),
  glm_print(aat_t1_N_M, aat_t1_mn, aat_t1_unadj_M),
  glm_print(neo_t1_N_M, neo_t1_mn, neo_t1_unadj_M),
  glm_print(lac_t1_N_M, lac_t1_mn, lac_t1_unadj_M),
  glm_print(man_t1_N_M, man_t1_mn, man_t1_unadj_M),
  glm_print(lm_t1_N_M, lm_t1_mn, lm_t1_unadj_M)
)




# tab2[,1]<-as.character(tab2[,1])
# tab2[c(1,3,5),1]<-rep("~~~Control",3)
# tab2[c(2,4,6),1]<-rep("~~~N+WSH",3)

blank=rep("",6)
tab2<-as.matrix(tab2)
tab2<-rbind(t(c("Ln myeloperoxidase (ng/ml)",blank)),
            tab2[1:4,],
            t(c("Ln alpha-1 antitrypsin (mg/g)",blank)),
            tab2[5:8,],
            t(c("Ln neopterin (nmol/L)",blank)),
            tab2[9:12,],
            t(c("Ln lactulose (mmol/L)",blank)),
            tab2[13:16,],
            t(c("Ln mannitol (mmol/L)",blank)),
            tab2[17:20,],
            t(c("Ln L:M ratio",blank)),
            tab2[21:24,]
            )

setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Results/Tables/")
col_names <- c("Outcome, Arm", "N", "Mean", "SD", 
               "Difference from Control (95% CI)", "Difference from Nutrition", 
               "Difference from WSH")
save(tab2, file="EE-K-table2.RData")

tab2 <- as.data.frame(tab2)
names(tab2) <- col_names 
library(flextable)
tab2_flex <- flextable(tab2) %>% 
  bold(i=c(1, 6, 11, 16, 21, 26)) %>% 
  align(j=2:7, align="center", part="all")
tab2_flex






#--------------------------------
#Table 3
#--------------------------------

tab3<-rbind(
  glm_print(mpo_t2_N_M, mpo_t2_mn, mpo_t2_unadj_M),
  glm_print(aat_t2_N_M, aat_t2_mn, aat_t2_unadj_M),
  glm_print(neo_t2_N_M, neo_t2_mn, neo_t2_unadj_M),
  glm_print(lac_t2_N_M, lac_t2_mn, lac_t2_unadj_M),
  glm_print(man_t2_N_M, man_t2_mn, man_t2_unadj_M),
  glm_print(lm_t2_N_M, lm_t2_mn, lm_t2_unadj_M)
)



blank=rep("",6)
tab3<-as.matrix(tab3)
tab3<-rbind(t(c("Ln myeloperoxidase (ng/ml)",blank)),
            tab3[1:4,],
            t(c("Ln alpha-1 antitrypsin (mg/g)",blank)),
            tab3[5:8,],
            t(c("Ln neopterin (nmol/L)",blank)),
            tab3[9:12,],
            t(c("Ln lactulose (mmol/L)",blank)),
            tab3[13:16,],
            t(c("Ln mannitol (mmol/L)",blank)),
            tab3[17:20,],
            t(c("Ln L:M ratio",blank)),
            tab3[21:24,]
            )

setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Results/Tables/")
save(tab3, file="EE-K-table3.RData")

tab3 <- as.data.frame(tab3)
names(tab3) <- col_names 
tab3_flex <- flextable(tab3) %>% 
  bold(i=c(1, 6, 11, 16, 21, 26)) %>% 
  align(j=2:7, align="center", part="all")
tab3_flex




#--------------------------------
#Table 4
#--------------------------------

tab4<-rbind(
  glm_print(mpo_t3_N_M, mpo_t3_mn, mpo_t3_unadj_M),
  glm_print(aat_t3_N_M, aat_t3_mn, aat_t3_unadj_M),
  glm_print(neo_t3_N_M, neo_t3_mn, neo_t3_unadj_M),
  glm_print(lac_t3_N_M, lac_t3_mn, lac_t3_unadj_M),
  glm_print(man_t3_N_M, man_t3_mn, man_t3_unadj_M),
  glm_print(lm_t3_N_M, lm_t3_mn, lm_t3_unadj_M)
)



blank=rep("",6)
tab4<-as.matrix(tab4)
tab4<-rbind(t(c("Ln myeloperoxidase (ng/ml)",blank)),
            tab4[1:4,],
            t(c("Ln alpha-1 antitrypsin (mg/g)",blank)),
            tab4[5:8,],
            t(c("Ln neopterin (nmol/L)",blank)),
            tab4[9:12,],
            t(c("Ln lactulose (mmol/L)",blank)),
            tab4[13:16,],
            t(c("Ln mannitol (mmol/L)",blank)),
            tab4[17:20,],
            t(c("Ln L:M ratio}",blank)),
            tab4[21:24,]
            )

setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Results/Tables/")
save(tab4, file="EE-K-table4.RData")

tab4 <- as.data.frame(tab4)
names(tab4) <- col_names 
tab4_flex <- flextable(tab4) %>% 
  bold(i=c(1, 6, 11, 16, 21, 26)) %>% 
  align(j=2:7, align="center", part="all")
tab4_flex


save_as_docx("Table 2. Effect of intervention on environmental enteric dysfunction measurements at 6 months" = tab2_flex,
             "Table 3. Effect of intervention on environmental enteric dysfunction measurements at 17 months" = tab3_flex,
             "Table 4. Effect of intervention on environmental enteric dysfunction measurements at 22 months" = tab4_flex,
             path = "EE_kenya_tables_2_4.docx")


#--------------------------------
#Supplementary Table 4: 
#   Effect of intervention on environmental enteric dysfunction measurements 
# at follow-up 1 
#--------------------------------

setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Results/Andrew/")
load("stool_ipcw_res.Rdata")
load("urine_ipcw_res.Rdata")
load("stool_res_adj_sex_age_M.Rdata")
load("urine_res_adj_sex_age_M.Rdata")



###NOTE! Need to fix to print the 


s.glm_print<-function(n, mean, glm, age, adj,ipcw, t=F){
 # n<-aat_t1_N_M
 # mean<-aat_t1_mn
 # glm<-aat_t1_unadj_M
 # adj<-aat_t1_adj_M
 # ipcw<-aat_t1_adj_ipcw_M
 
rownames(glm)<-NULL
glmh1<-rbind(rep(NA, 3), glm[1:3,1:3])
adjh1<-rbind(rep(NA, 3), adj[1:3,1:3])
ageh1<-rbind(rep(NA, 3), age[1:3,1:3])
ipcwh1<-rbind(rep(NA, 3), ipcw[1:3,c(1,3,4)])


obj<-cbind(n[,1:2], mean[,2:3], glmh1 ,ageh1, adjh1, ipcwh1)

  if(t==F){
        obj<-as.data.frame((obj))
  }else{
        obj<-as.data.frame(t(obj))
  }

obj[,3]<-sprintf("%1.2f",obj[,3])
obj[,4]<-sprintf("%1.2f",obj[,4])
obj[2:4,5]<-sprintf("%1.2f",obj[2:4,5])
obj[2:4,6]<-sprintf("%1.2f",obj[2:4,6])
obj[2:4,7]<-sprintf("%1.2f",obj[2:4,7])
obj[2:4,8]<-sprintf("%1.2f",obj[2:4,8])
obj[2:4,9]<-sprintf("%1.2f",obj[2:4,9])
obj[2:4,10]<-sprintf("%1.2f",obj[2:4,10])
obj[2:4,11]<-sprintf("%1.2f",obj[2:4,11])
obj[2:4,12]<-sprintf("%1.2f",obj[2:4,12])
obj[2:4,13]<-sprintf("%1.2f",obj[2:4,13])
obj[2:4,14]<-sprintf("%1.2f",obj[2:4,14])
obj[2:4,15]<-sprintf("%1.2f",obj[2:4,15])
obj[2:4,16]<-sprintf("%1.2f",obj[2:4,16])
  #Add later if p-val is needed:
  #if(as.numeric(obj[6])<0.05){obj[6]<-paste0(sprintf("%1.3f",obj[c(6)]),"*")
  #                            flag=T}

  rownames(obj)<-colnames(obj)<-NULL
  
  obj[,17]<-paste(obj[,5]," (",obj[,6],",",obj[,7],")", sep="")
  obj[,18]<-paste(obj[,8]," (",obj[,9],",",obj[,10],")", sep="")
  obj[,19]<-paste(obj[,11]," (",obj[,12],",",obj[,13],")", sep="")
  obj[,20]<-paste(obj[,14]," (",obj[,15],",",obj[,16],")", sep="")

  obj<-obj[,-c(5:16)]
  obj[1,5]<-""
  obj[1,6]<-""
  obj[1,7]<-""
  obj[1,8]<-""

  return(obj)
}

s.tab2<-rbind(
  s.glm_print(mpo_t1_N_M, mpo_t1_mn, mpo_t1_unadj_M, mpo_t1_adj_sex_age_M, mpo_t1_adj_M, mpo_t1_adj_ipcw_M),
  s.glm_print(aat_t1_N_M, aat_t1_mn, aat_t1_unadj_M, aat_t1_adj_sex_age_M, aat_t1_adj_M, aat_t1_adj_ipcw_M),
  s.glm_print(neo_t1_N_M, neo_t1_mn, neo_t1_unadj_M, neo_t1_adj_sex_age_M, neo_t1_adj_M, neo_t1_adj_ipcw_M),
  s.glm_print(lac_t1_N_M, lac_t1_mn, lac_t1_unadj_M, lac_t1_adj_sex_age_M, lac_t1_adj_M, l1_adj_ipcw_M),
  s.glm_print(man_t1_N_M, man_t1_mn, man_t1_unadj_M, man_t1_adj_sex_age_M, man_t1_adj_M, m1_adj_ipcw_M),
  s.glm_print(lm_t1_N_M, lm_t1_mn, lm_t1_unadj_M, lm_t1_adj_sex_age_M, lm_t1_adj_M, lmr1_adj_ipcw_M)
)




blank=rep("",7)
s.tab2<-as.matrix(s.tab2)
s.tab2<-rbind(t(c("Ln myeloperoxidase (ng/ml)",blank)),
            s.tab2[1:4,],
            t(c("Ln alpha-1 antitrypsin (mg/g)",blank)),
            s.tab2[5:8,],
            t(c("Ln neopterin (nmol/L)",blank)),
            s.tab2[9:12,],
            t(c("Ln lactulose (mmol/L)",blank)),
            s.tab2[13:16,],
            t(c("Ln mannitol (mmol/L)",blank)),
            s.tab2[17:20,],
            t(c("Ln L:M ratio",blank)),
            s.tab2[21:24,]
            )

col_names <- c("Outcome, Arm", "N", "Mean", "SD", 
               "Unadjusted Difference from Control (95% CI)", "Age and Sex-Adjusted Difference from Control (95% CI)", 
               "Adjusted Difference from Control (95% CI)", "IPCW-adjusted Difference from Control (95% CI)")
s.tab2 <- as.data.frame(s.tab2)
names(s.tab2) <- col_names
save(s.tab2, file="EE-K-s.table4.RData")

cleantable(s.tab2, 2)





#--------------------------------
# Supplementary Table 3:
#   Effect of intervention on environmental enteric dysfunction measurements 
# after 1 year of intervention 
#--------------------------------
s.tab3<-rbind(
    s.glm_print(mpo_t2_N_M, mpo_t2_mn, mpo_t2_unadj_M, mpo_t2_adj_sex_age_M, mpo_t2_adj_M, mpo_t2_adj_ipcw_M),
  s.glm_print(aat_t2_N_M, aat_t2_mn, aat_t2_unadj_M, aat_t2_adj_sex_age_M, aat_t2_adj_M, aat_t2_adj_ipcw_M),
  s.glm_print(neo_t2_N_M, neo_t2_mn, neo_t2_unadj_M, neo_t2_adj_sex_age_M, neo_t2_adj_M, neo_t2_adj_ipcw_M),
  s.glm_print(lac_t2_N_M, lac_t2_mn, lac_t2_unadj_M, lac_t2_adj_sex_age_M, lac_t2_adj_M, l2_adj_ipcw_M),
  s.glm_print(man_t2_N_M, man_t2_mn, man_t2_unadj_M, man_t2_adj_sex_age_M, man_t2_adj_M, m2_adj_ipcw_M),
  s.glm_print(lm_t2_N_M, lm_t2_mn, lm_t2_unadj_M, lm_t2_adj_sex_age_M, lm_t2_adj_M, lmr2_adj_ipcw_M)
)

  



blank=rep("",7)
s.tab3<-as.matrix(s.tab3)
s.tab3<-rbind(t(c("Ln myeloperoxidase (ng/ml)",blank)),
            s.tab3[1:4,],
            t(c("Ln alpha-1 antitrypsin (mg/g)",blank)),
            s.tab3[5:8,],
            t(c("Ln neopterin (nmol/L)",blank)),
            s.tab3[9:12,],
            t(c("Ln lactulose (mmol/L)",blank)),
            s.tab3[13:16,],
            t(c("Ln mannitol (mmol/L)",blank)),
            s.tab3[17:20,],
            t(c("Ln L:M ratio",blank)),
            s.tab3[21:24,]
            )

s.tab3 <- as.data.frame(s.tab3)
names(s.tab3) <- col_names
save(s.tab3, file="EE-K-s.table5.RData")

cleantable(s.tab3, 2)

#--------------------------------
# Supplementary Table 4: 
#   Effect of intervention on environmental enteric dysfunction measurements 
# after 2 years of intervention 
#--------------------------------
s.tab4<-rbind(
  s.glm_print(mpo_t3_N_M, mpo_t3_mn, mpo_t3_unadj_M, mpo_t3_adj_sex_age_M, mpo_t3_adj_M, mpo_t3_adj_ipcw_M),
  s.glm_print(aat_t3_N_M, aat_t3_mn, aat_t3_unadj_M, aat_t3_adj_sex_age_M, aat_t3_adj_M, aat_t3_adj_ipcw_M),
  s.glm_print(neo_t3_N_M, neo_t3_mn, neo_t3_unadj_M, neo_t3_adj_sex_age_M, neo_t3_adj_M, neo_t3_adj_ipcw_M),
  s.glm_print(lac_t3_N_M, lac_t3_mn, lac_t3_unadj_M, lac_t3_adj_sex_age_M, lac_t3_adj_M, l3_adj_ipcw_M),
  s.glm_print(man_t3_N_M, man_t3_mn, man_t3_unadj_M, man_t3_adj_sex_age_M, man_t3_adj_M, m3_adj_ipcw_M),
  s.glm_print(lm_t3_N_M, lm_t3_mn, lm_t3_unadj_M, lm_t3_adj_sex_age_M, lm_t3_adj_M, lm3_adj_ipcw_M)
)

blank=rep("",7)
s.tab4<-as.matrix(s.tab4)
s.tab4<-rbind(t(c("Ln myeloperoxidase (ng/ml)",blank)),
            s.tab4[1:4,],
            t(c("Ln alpha-1 antitrypsin (mg/g)",blank)),
            s.tab4[5:8,],
            t(c("Ln neopterin (nmol/L)",blank)),
            s.tab4[9:12,],
            t(c("Ln lactulose (mmol/L)",blank)),
            s.tab4[13:16,],
            t(c("Ln mannitol (mmol/L)",blank)),
            s.tab4[17:20,],
            t(c("Ln L:M ratio",blank)),
            s.tab4[21:24,]
            )

s.tab4 <- as.data.frame(s.tab4)
names(s.tab4) <- col_names
save(s.tab4, file="EE-K-s.table6.RData")

cleantable(s.tab4, 2)

tabs4_flex <- flextable(s.tab2) %>% 
  bold(i=c(1, 6, 11, 16, 21, 26)) %>% 
  align(j=2:8, align="center", part="all")
tabs5_flex <- flextable(s.tab3) %>% 
  bold(i=c(1, 6, 11, 16, 21, 26)) %>% 
  align(j=2:8, align="center", part="all")
tabs6_flex <- flextable(s.tab4) %>% 
  bold(i=c(1, 6, 11, 16, 21, 26)) %>% 
  align(j=2:8, align="center", part="all")


save_as_docx("Table S4. Effect of intervention on environmental enteric dysfunction measurements at 6 months" = tabs4_flex,
             "Table S5. Effect of intervention on environmental enteric dysfunction measurements at 17 months" = tabs5_flex,
             "Table S6. Effect of intervention on environmental enteric dysfunction measurements at 22 months" = tabs6_flex,
             path = "EE_kenya_tables_s4-6.docx")



#--------------------------------
# Supplementary Table 7: 
#   Unadjusted and adjusted p-values 
# across all time points
#--------------------------------


glm=mpo_t1_unadj_M
age=mpo_t1_adj_sex_age_M
adj=mpo_t1_adj_M
ipcw=mpo_t1_adj_ipcw_M

s.pval_print<-function(t, sample ,glm, age, adj,ipcw){
 
contrasts<-c("C v WSH", "C v N", "C v N+WSH", "WSH v N+WSH", "N v N+WSH")
  
rownames(glm)<-NULL
glmh1<-rbind(rep(NA, 3), glm[1:3,1:3])
adjh1<-rbind(rep(NA, 3), adj[1:3,1:3])
ageh1<-rbind(rep(NA, 3), age[1:3,1:3])
ipcwh1<-rbind(rep(NA, 3), ipcw[1:3,c(1,3,4)])

glm<-data.frame(glm[,c(1,6)])
age<-data.frame(age[,c(1,6)])
adj<-data.frame(adj[,c(1,6)])
ipcw<-data.frame(ipcw[,c(1,5)])

colnames(glm)<-colnames(age)<-colnames(adj)<-colnames(ipcw)<-c("diff","pval")

glm$adjp<-glm[,2]*3
age$adjp<-age[,2]*3
adj$adjp<-adj[,2]*3
ipcw$adjp<-ipcw[,2]*3

glm$adjp<-ifelse(glm$adjp>0.99,0.99,glm$adjp)
age$adjp<-ifelse(age$adjp>0.99,0.99,age$adjp)
adj$adjp<-ifelse(adj$adjp>0.99,0.99,adj$adjp)
ipcw$adjp<-ifelse(ipcw$adjp>0.99,0.99,ipcw$adjp)

time<-ifelse(t>6,"Year 1", "3 month")
time<-ifelse(t>13,"Year 2", time)

obj<-data.frame((rep(paste0(sample[t]),5)), (rep(time,5)), contrasts, glm ,age, adj, ipcw)
for(i in 4:ncol(obj)){
  sig<-rep("", nrow(obj))
  obj[,i]<-sprintf("%1.2f",obj[,i])
    obj[,i]<-as.character(obj[,i])
    
  if(i==5|i==6|i==8|i==9|i==11|i==12|i==14|i==15){
  sig<-ifelse(obj[,i]<0.05, paste0(sig, "*"), sig)
  sig<-ifelse(obj[,i]<0.01, paste0(sig, "*"), sig)
  sig<-ifelse(obj[,i]<0.001, paste0(sig, "*"), sig)
  obj[,i]<-paste0(obj[,i],sig)
    }
}

  return(obj)
}

sample<-c("Ln myeloperoxidase (ng/ml)",
"Ln alpha-1 antitrypsin (mg/g)",
"Ln neopterin (nmol/L)",
"Ln lactulose (mmol/L)",
"Ln mannitol (mmol/L)",
"Ln L:M ratio",
"Ln myeloperoxidase (ng/ml)",
"Ln alpha-1 antitrypsin (mg/g)",
"Ln neopterin (nmol/L)",
"Ln lactulose (mmol/L)",
"Ln regenerating gene 1$\\beta$ ($\\mu$g/ml)",
"Ln mannitol (mmol/L)",
"Ln L:M ratio",
"Ln myeloperoxidase (ng/ml)",
"Ln alpha-1 antitrypsin (mg/g)",
"Ln neopterin (nmol/L)",
"Ln lactulose (mmol/L)",
"Ln mannitol (mmol/L)",
"Ln L:M ratio"
)

s.tab7<-rbind(
  s.pval_print(1, sample, mpo_t1_unadj_M, mpo_t1_adj_sex_age_M, mpo_t1_adj_M, mpo_t1_adj_ipcw_M),
  s.pval_print(2, sample,aat_t1_unadj_M, aat_t1_adj_sex_age_M, aat_t1_adj_M, aat_t1_adj_ipcw_M),
  s.pval_print(3, sample,neo_t1_unadj_M, neo_t1_adj_sex_age_M, neo_t1_adj_M, neo_t1_adj_ipcw_M),
  s.pval_print(4, sample,lac_t1_unadj_M, lac_t1_adj_sex_age_M, lac_t1_adj_M, l1_adj_ipcw_M),
  s.pval_print(5, sample,man_t1_unadj_M, man_t1_adj_sex_age_M, man_t1_adj_M, m1_adj_ipcw_M),
  s.pval_print(6, sample,lm_t1_unadj_M, lm_t1_adj_sex_age_M, lm_t1_adj_M, lmr1_adj_ipcw_M),
  s.pval_print(7, sample,mpo_t2_unadj_M, mpo_t2_adj_sex_age_M, mpo_t2_adj_M, mpo_t2_adj_ipcw_M),
  s.pval_print(8, sample,aat_t2_unadj_M, aat_t2_adj_sex_age_M, aat_t2_adj_M, aat_t2_adj_ipcw_M),
  s.pval_print(9, sample,neo_t2_unadj_M, neo_t2_adj_sex_age_M, neo_t2_adj_M, neo_t2_adj_ipcw_M),
  s.pval_print(10, sample,reg1b_t2_unadj_M, reg1b_t2_adj_sex_age_M, reg1b_t2_adj_M, reg_t2_adj_ipcw_M),
  s.pval_print(11, sample,lac_t2_unadj_M, lac_t2_adj_sex_age_M, lac_t2_adj_M, l2_adj_ipcw_M),
  s.pval_print(12, sample,man_t2_unadj_M, man_t2_adj_sex_age_M, man_t2_adj_M, m2_adj_ipcw_M),
  s.pval_print(13, sample,lm_t2_unadj_M, lm_t2_adj_sex_age_M, lm_t2_adj_M, lmr2_adj_ipcw_M),
  s.pval_print(14, sample,mpo_t3_unadj_M, mpo_t3_adj_sex_age_M, mpo_t3_adj_M, mpo_t3_adj_ipcw_M),
  s.pval_print(15, sample,aat_t3_unadj_M, aat_t3_adj_sex_age_M, aat_t3_adj_M, aat_t3_adj_ipcw_M),
  s.pval_print(16, sample,neo_t3_unadj_M, neo_t3_adj_sex_age_M, neo_t3_adj_M, neo_t3_adj_ipcw_M),
  s.pval_print(17, sample,lac_t3_unadj_M, lac_t3_adj_sex_age_M, lac_t3_adj_M, l3_adj_ipcw_M),
  s.pval_print(18, sample,man_t3_unadj_M, man_t3_adj_sex_age_M, man_t3_adj_M, m3_adj_ipcw_M),
  s.pval_print(19, sample,lm_t3_unadj_M, lm_t3_adj_sex_age_M, lm_t3_adj_M, lmr3_adj_ipcw_M)
)

#Drop out age_sex and ipcw

s.tab7<-s.tab7[,c(1,2,3,4,5,6,10,11,12)]

  cleantable(s.tab7,2)
  
  #Split by time
  s.tab7.m3<-s.tab7[s.tab7[,2]=="3 month",c(1,3:9)]
  s.tab7.1<-s.tab7[s.tab7[,2]=="Year 1",c(1,3:9)]
  s.tab7.2<-s.tab7[s.tab7[,2]=="Year 2",c(1,3:9)]
  
      cleantable(s.tab7.m3,2)
      cleantable(s.tab7.1,2)
      cleantable(s.tab7.2,2)


      





