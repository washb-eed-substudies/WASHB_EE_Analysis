

rm(list=ls())
library(tidyverse)
library(lubridate)

#Child age and sex
child<-read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_allchild_covariates.csv")
head(child)
child <- child %>% subset(., select=c(childid, sex, DOB, nulliparous)) %>% 
                   rename(birthord=nulliparous) %>% 
                   mutate(DOB=dmy(DOB),
                          sex=as.numeric(sex)-2)

table(child$birthord)
child$birthord <- relevel(child$birthord, ref="no")
saveRDS(child, file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/WBK-EE-childDOB.rds")



#Household covariates
d<-read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_ee_covariates.csv")
head(d)


colnames(d)

#Primary outcomes vars:
#Wvars<-c('sex','aged','month','momage','momedu','momheight','Ncomp','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','roof','floor','cow','goat','dog','chicken','dminwat','HHS','fracode')


d <- d %>% subset(., select=c(hhid, clusterid,  mother_age,       mother_edu,     father_edu,       father_agri,     motherht,  HHS, water_time,
                              roof,             cow, goat, poultry, dog,          
floor,            elec,             radio,            tv,               mobilephone,      clock,           
bicycle,          motorcycle,       stove,            cooker,           car,             Nhh,             
Ncomp,            Nunder18  )) %>%
  rename(
    momage=mother_age, 
    momheight=motherht,
    momedu=mother_edu,
    hfiacat=HHS,
    Nlt18=Nunder18,
    watmin=water_time,
    asset_stove=stove,
    asset_cooker=cooker, 
    asset_clock=clock, 
    asset_radio=radio, 
    asset_tv=tv, 
    asset_car=car, 
    asset_bike=bicycle,
    asset_moto=motorcycle, 
    asset_car=car, 
    asset_mobile=mobilephone,
    n_cows=cow, 
    n_goats=goat, 
    n_chicken=poultry,
    n_dogs=dog
  )


       

saveRDS(d, file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/WBK-EE-covariates.rds")








#subset time-constant d adjustment set
d<- subset(d, select=Wvars)

#Clean adjustment variables 
#Check missingness
for(i in 1:ncol(d)){
  print(colnames(d)[i])
  print(table(is.na(d[,i])))
}

#Replace missingness for factors with new level
#in main dataset 


d$asset_clock[is.na(d$asset_clock)]<-99
d$asset_clock<-factor(d$asset_clock)

#Order data to replicate SL
d <- d[order(d$dataid,d$childNo, d$svy),]



#check that all the factor variables are set
for(i in 1:ncol(d)){
  print(colnames(d)[i])
  print(class(d[,i])  )
}

#Truncate unrealistic levels of n_chickens to 60
table(d$n_chickens)
d$n_chickens[d$n_chickens>60]<-60
table(d$n_chickens)

#Relevel all factors
d$sex<-as.factor(d$sex)
  d$sex=relevel(d$sex,ref="0")
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



#Check that prevalence >5% for all binary variables
for(i in 1:ncol(d)){
  if(class(d[,i])=="factor"){
    for(j in 1:dim(table(d[,i]))){
      flag<-0
      if(sum(d[,i]==levels(d[,i])[j], na.rm=T)/nrow(d)*100<5){
        perc<-sum(d[,i]==levels(d[,i])[j], na.rm=T)/nrow(d)*100
        cat("\n>95% missing: ",colnames(d)[i]," level:",levels(d[,i])[j],"perc:",perc,"\n")
        flag<-1
      }
    }
      if(flag==1){
        print(table(d[,i]))
      }
  }else{
    if(sum(is.na(W[,i]))/nrow(W)*100>95){
      cat("\n>95% missing: ",colnames(W)[i],"\n")
    }
  }
}


