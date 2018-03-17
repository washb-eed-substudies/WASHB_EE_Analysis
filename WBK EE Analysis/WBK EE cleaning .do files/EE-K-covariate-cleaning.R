

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
    n_chickens=poultry,
    n_dogs=dog
  )





#Clean adjustment variables 
#Check missingness
for(i in 1:ncol(d)){
  print(colnames(d)[i])
  print(table(is.na(d[,i])))
}



#Impute missing for momage (74 missing) and momheight (1498 missing) and watmin (73 missing)?







#check that all the factor variables are set
for(i in 1:ncol(d)){
  cat(colnames(d)[i]," ", class(d[,i]),"\n"  )
}


#Do this?
#Truncate unrealistic levels of n_chickens to 60
# table(d$n_chickens)
# d$n_chickens[d$n_chickens>60]<-60
# table(d$n_chickens)

#Relevel all factors
d$momedu=relevel(d$momedu,ref="Incomplete primary")
d$father_edu=relevel(d$father_edu,ref="incomplete primary")
d$father_agri=relevel(d$father_agri,ref="does not work in ag")
d$hfiacat=relevel(d$hfiacat,ref="Little to none")
d$floor=relevel(d$floor,ref="Earth/dung") #Not used due to sparsity
d$roof=relevel(d$roof,ref="Iron/other") #Not used due to sparsity
d$elec=relevel(d$elec,ref="No electricity")
d$asset_tv=relevel(d$asset_tv,ref="No TV")
d$asset_cooker=relevel(d$asset_cooker,ref="No gas cooker")
d$asset_car=relevel(d$asset_car,ref="No car")
                                            
            
                              


#Check that prevalence >5% for all binary variables
for(i in 3:ncol(d)){
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
    if(sum(is.na(d[,i]))/nrow(d)*100>95){
      cat("\n>95% missing: ",colnames(d)[i],"\n")
    }
  }
}



#Order data to replicate SL
d <- d[order(d$hhid),]  

saveRDS(d, file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/WBK-EE-covariates.rds")


