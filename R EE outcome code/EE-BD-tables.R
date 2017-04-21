
#---------------------------------------
# EE-BD-tables.R
#
# andrew mertens (amertens@berkeley.edu)
#
# Create R-objects for xtable printing
# for the WASH Benefits Bangladesh EED 
# substudy
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
      temp<-cbind(mu[j,1],sd[j,1],mu[j,2],sd[j,2],mu[j,3],sd[j,3],mu[j,4],sd[j,4])
      temp<-rnd(temp, 0)
      dat<-rbind(dat, temp)
      }else{
      temp<-cbind(n[j,1],n[j,1]/(n[1,1])*100,
                  n[j,2],n[j,2]/(n[1,2])*100,
                  n[j,3],n[j,3]/(n[1,3])*100,
                  n[j,4],n[j,4]/(n[1,4])*100)
      temp<-rnd(temp, 0)
      dat<-rbind(dat, temp)
      }
  }
  rownames(dat)<-rownames(mu)
  #dat<-rnd(dat, round)
  #dat<-cbind(Rownames,dat)
  col1<-NULL
  col2<-NULL
  col3<-NULL
  col4<-NULL
  for(j in 1:nrow(n)){
      if(mean.ind[j]==1){
      temp1<-cbind(paste(dat[j,1],"(",dat[j,2],")",sep=""))
      temp2<-cbind(paste(dat[j,3],"(",dat[j,4],")",sep=""))
      temp3<-cbind(paste(dat[j,5],"(",dat[j,6],")",sep=""))
      temp4<-cbind(paste(dat[j,7],"(",dat[j,8],")",sep=""))

      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)
      col3<-rbind(col3, temp3)
      col4<-rbind(col4, temp4)

      }else{
        if(j==1){
      temp1<-cbind(paste(dat[j,1],sep=""))
      temp2<-cbind(paste(dat[j,3],sep=""))
      temp3<-cbind(paste(dat[j,5],sep=""))
      temp4<-cbind(paste(dat[j,7],sep=""))

      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2)  
      col3<-rbind(col3, temp3)           
      col4<-rbind(col4, temp4)           
        }else{
      temp1<-cbind(paste(dat[j,1],"(",dat[j,2],"\\%)",sep=""))
      temp2<-cbind(paste(dat[j,3],"(",dat[j,4],"\\%)",sep=""))
      temp3<-cbind(paste(dat[j,5],"(",dat[j,6],"\\%)",sep=""))
      temp4<-cbind(paste(dat[j,7],"(",dat[j,8],"\\%)",sep=""))
      col1<-rbind(col1, temp1)
      col2<-rbind(col2, temp2) 
      col3<-rbind(col3, temp3) 
      col4<-rbind(col4, temp4) 
          }
      }
  } 
  dat<-cbind(Rownames, col1, col2, col4, col4)
  colnames(dat)=c(" ","Control","WSH","Nutrition","Nutrition + WSH")
  return(dat)
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


#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("EE-BD-table1.Rdata")


balance.tab.mu_M 
balance.tab.n_M 
balance.tab.sd_M

mean.ind<-c(0,1,1,1,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

rownames(balance.tab.mu_M)[which(mean.ind==1)]
rownames(balance.tab.mu_M)[which(mean.ind!=1)]
balance.tab.n_M[which(mean.ind!=1),]
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
blank=rep("",12)

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
n.comp.f[4]=paste("(N=",n.comp.f[4],")",sep="")
n.comp.f[5]=paste("(N=",n.comp.f[5],")",sep="")
n.comp.f<-n.comp.f[2:5]
colnames(n.comp.f)<-NULL


for(i in c(2:3,5:6,8:11,13:16,18,24,29:30,34,37,41)){
  table1_f[i,1]=paste("~~~",table1_f[i,1],sep="")
}
for(i in c(19:23,25:28,26:27,31,32,35:36,38:39)){
  table1_f[i,1]=paste("~~~~~",table1_f[i,1],sep="")
}

table1_f

#n.comp.f<-c("No. of compounds:")


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(n.comp.f, table1_f, file="EE-BD-table1_f.RData")


#cleantable(n.comp.f,digits=0)
cleantable(table1_f,digits=0)


#--------------------------------
#Table 2
#--------------------------------

#load objects
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
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
reg1b_t2_unadj_M


 
glm_print<-function(n, mean, glm, adj, t=F){
# n<-aat_t1_N_M
# mean<-aat_t1_mn
# glm<-aat_t1_unadj_M

rownames(glm)<-NULL
glmh1<-rbind(rep(NA, 3), glm[1:3,1:3])
adjh1<-rbind(rep(NA, 3), adj[1:3,1:3])

obj<-cbind(n[,1:2], mean[,2:3], glmh1,adjh1)

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
  #Add later if p-val is needed:
  #if(as.numeric(obj[6])<0.05){obj[6]<-paste0(sprintf("%1.3f",obj[c(6)]),"*")
  #                            flag=T}

  rownames(obj)<-colnames(obj)<-NULL
  
  obj[,11]<-paste(obj[,5],"(",obj[,6],",",obj[,7],")", sep="")
  obj[,12]<-paste(obj[,8],"(",obj[,9],",",obj[,10],")", sep="")

  obj<-obj[,-c(5:10)]
  obj[1,5]<-""
  obj[1,6]<-""
  #out<-paste(obj[1],"(",obj[2],",",obj[3],") P=",obj[4], sep="")
  return(obj)
}

tab2<-rbind(
  glm_print(aat_t1_N_M, aat_t1_mn, aat_t1_unadj_M,aat_t1_adj_M),
  glm_print(mpo_t1_N_M, mpo_t1_mn, mpo_t1_unadj_M, mpo_t1_adj_M),
  glm_print(neo_t1_N_M, neo_t1_mn, neo_t1_unadj_M, neo_t1_adj_M),
  glm_print(lac_t2_N_M, lac_t1_mn, lac_t1_unadj_M, lac_t1_adj_M),
  glm_print(man_t1_N_M, man_t1_mn, man_t1_unadj_M, man_t1_adj_M),
  glm_print(lm_t1_N_M, lm_t1_mn, lm_t1_unadj_M, lm_t1_adj_M)
)




# tab2[,1]<-as.character(tab2[,1])
# tab2[c(1,3,5),1]<-rep("~~~Control",3)
# tab2[c(2,4,6),1]<-rep("~~~N+WSH",3)

blank=rep("",5)
tab2<-as.matrix(tab2)
tab2<-rbind(t(c("\\textbf{Alpha-1 Antitrypsin}",blank)),
            tab2[1:4,],
            t(c("\\textbf{Myeloperoxidase}",blank)),
            tab2[5:8,],
            t(c("\\textbf{Neopterin}",blank)),
            tab2[9:12,],
            t(c("\\textbf{Lactulose}",blank)),
            tab2[13:16,],
            t(c("\\textbf{Mannitol}",blank)),
            tab2[17:20,],
            t(c("\\textbf{L/M Ratio}",blank)),
            tab2[21:24,]
            )

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(tab2, file="EE-BD-table2.RData")

cleantable(tab2, 2)

