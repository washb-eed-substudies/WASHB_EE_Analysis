
#---------------------------------------
# EE-BD-telo-tables.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Create R-objects for xtable printing
#---------------------------------------




rm(list=ls())
try(detach(package:plyr))
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


table1_create<-function(mu, n, sd, mean.ind, vargroup, vargroup.row, Rownames, round){
  dat<-NULL
  for(j in 1:nrow(n)){
      if(mean.ind[j]==1){
      temp<-cbind(mu[j,1],sd[j,1],mu[j,2],sd[j,2])
      temp<-round(temp, 1)
      dat<-rbind(dat, temp)
      }else{
      temp<-cbind(n[j,1],n[j,1]/sum(n[j,])*100,n[j,2],n[j,2]/sum(n[j,])*100)
      temp<-round(temp, 2)
      dat<-rbind(dat, temp)
      }
  }
  rownames(dat)<-rownames(mu)
  #dat<-round(dat, round)
  #dat<-cbind(Rownames,dat)
  col1<-NULL
  col2<-NULL
  for(j in 1:nrow(n)){
      if(mean.ind[j]==1){
      temp1<-cbind(paste(dat[j,1],"(",dat[j,2],")",sep=""))
      temp2<-cbind(paste(dat[j,3],"(",dat[j,4],")",sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)
      }else{
        if(j==1){
      temp1<-cbind(paste(dat[j,1],sep=""))
      temp2<-cbind(paste(dat[j,3],sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)           
        }else{
      temp1<-cbind(paste(dat[j,1],"(",dat[j,2],"\\%)",sep=""))
      temp2<-cbind(paste(dat[j,3],"(",dat[j,4],"\\%)",sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2) 
        }
      }
  } 
  dat<-cbind(Rownames, col1, col2)
  colnames(dat)=c(" ","Control","Nutrition + WSH")
  return(dat)
}


#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("telo_table1.Rdata")


balance.tab.mu_M 
balance.tab.n_M 
balance.tab.sd_M

mean.ind<-c(0,1,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#vargroup<-c(0)
#vargroup.row<-c(2,5,8,13,20,21,27,32,33,36,37,40,43)
Rownames<-c("No. of compounds",
            "Age (years)",
  "Years of education",
  "Years of education",
  "Works in agriculture",
  "Number of persons"
  ,"Has electricity"
  ,"Has a cement floor"
  ,"Acres of agricultural land owned",
      "Shallow tubewell primary water source",
    "Stored water observed at home",
    "Reported treating water yesterday",
    "Distance (mins) to primary water source",
        "Adult men",
        "Adult women",
        "Children: 8-\\textless 15 years",
        "Children: 3-\\textless 8 years",
        "Children: 0-\\textless 3 years",
        "Owned",
        "Concrete slab",
        "Functional water seal",
        "Visible stool on slab or floor",
        "Owned a potty",
        "House",
        "Child's play area",
        "Has water",
        "Has soap",
        "Has water",
        "Has soap",
    "Food insecure"
  )

tab<-table1_create(mu=balance.tab.mu_M, 
                   n=balance.tab.n_M, 
                   sd=balance.tab.sd_M, 
                   mean.ind=mean.ind,
                   Rownames=Rownames,
                   round=1)
tab



#Add in variable group labels
blank=rep("",8)

n.comp.f<-tab[1,]
tab<-tab[-1,]

table1_f=   rbind(
               c("\\textbf{Maternal}",blank,blank),
               tab[c(1:2),],
               c( "\\textbf{Paternal}",blank,blank),
               tab[c(3:4),],
               c("\\textbf{Household}",blank,blank),
               tab[c(5:8),],
               c("\\textbf{Drinking Water}",blank,blank),
               tab[c(9:12),],
               c("\\textbf{Sanitation}",blank,blank),
               c("Reported daily open defecation",blank,blank),
               tab[c(13:17),],
               c("Latrine",blank,blank),
               tab[c(18:22),],
               c("Human feces observed in the",blank,blank),
               tab[c(23:24),],
               c("\\textbf{Handwashing}",blank,blank),
               c("Within 6 steps of latrine",blank,blank),
               tab[c(25:26),],
               c("Within 6 steps of kitchen",blank,blank),
               tab[c(27:28),],
               c("\\textbf{Nutrition}",blank,blank),
               tab[c(29),])

rownames(table1_f)=NULL

table1_f
n.comp.f

n.comp.f[2]=paste("(N=",n.comp.f[2],")",sep="")
n.comp.f[3]=paste("(N=",n.comp.f[3],")",sep="")
n.comp.f<-n.comp.f[2:3]
colnames(n.comp.f)<-NULL


#FIX:
for(i in c(2:3,5:6,8:11,13:16,18,24,29:30,34,37,41)){
  table1_f[i,1]=paste("~~~",table1_f[i,1],sep="")
}
for(i in c(19:23,25:28,26:27,31,32,35:36,38:39)){
  table1_f[i,1]=paste("~~~~~",table1_f[i,1],sep="")
}



#n.comp.f<-c("No. of compounds:")


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(n.comp.f, table1_f, file="table1_f.RData")


#cleantable(n.comp.f,digits=0)
cleantable(table1_f,digits=0)


#--------------------------------
#Table 2
#--------------------------------

#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("telo_res.Rdata")
ts_t2_N_M
ts_t3_N_M
ts_t2_unadj_M
ts_t3_unadj_M
ts_t2_adj_sex_age_M
ts_t3_adj_sex_age_M
ts_t2_adj_M
ts_t3_adj_M
delta_ts_N_M
delta_ts_unadj_M
delta_ts_adj_sex_age_M
delta_ts_adj_M

glm_print<-function(obj){
  #obj<-ts_t2_unadj_M
  obj<-t(as.matrix(round(obj[1:3],2)))
  #rownames(obj)<-colnames(obj)<-NULL
  out<-paste(obj[1],"(",obj[2],",",obj[3],")", sep="")
  return(out)
}

tab2<-data.frame(rbind(ts_t2_N_M,ts_t3_N_M,delta_ts_N_M))
tab2[,3]<-round(tab2[,3],2)
tab2[,4:6]<-matrix("",6,3)
tab2[2,4]<-glm_print(ts_t2_unadj_M)
tab2[4,4]<-glm_print(ts_t3_unadj_M)
tab2[6,4]<-glm_print(delta_ts_unadj_M)
tab2[2,5]<-glm_print(ts_t2_adj_sex_age_M)
tab2[4,5]<-glm_print(ts_t3_adj_sex_age_M)
tab2[6,5]<-glm_print(delta_ts_adj_sex_age_M)
tab2[2,6]<-glm_print(ts_t2_adj_M)
tab2[4,6]<-glm_print(ts_t3_adj_M)
tab2[6,6]<-glm_print(delta_ts_adj_M)
colnames(tab2)<-NULL

tab2[,1]<-as.character(tab2[,1])
tab2[c(2,4,6),1]<-rep("Nutrition + Water + Sanitation + Handwashing",3)

blank=rep("",5)
tab2<-as.matrix(tab2)
tab2<-rbind(t(c("\\textbf{After 1 year of intervention (age x months)}",blank)),
            tab2[1:2,],
            t(c("\\textbf{After 2 years of intervention (age x months)}",blank)),
            tab2[3:4,],
            t(c("\\textbf{$\\Delta$ Telomere length between 1 to 2-Year follow-up}",blank)),
            tab2[5:6,])

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(tab2, file="table2.RData")

cleantable(tab2, 2)



