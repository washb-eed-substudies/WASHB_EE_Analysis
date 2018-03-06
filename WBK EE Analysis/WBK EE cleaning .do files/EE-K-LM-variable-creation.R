




rm(list=ls())
library(tidyverse)
library(lubridate)

bl<-read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_urine.csv")

ml<-read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_urine.csv")

el<-read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_urine.csv")

# aliq_num_2hr aliq_num_5hr aliq_num_prelm

#Calculate urine volumes
vol_bl <- data.frame(childid=bl$childid, urine_bl_date=dmy(bl$ee_bl_urine_date), staffid1=bl$cm_20_502, LMvol_t1=bl$cm_20_509, urineVol_t1= bl %>% select(contains("cm_20_516")) %>% rowSums(., na.rm=T))
vol_ml <- data.frame(childid=ml$childid, urine_ml_date=dmy(ml$ee_ml_urine_date), staffid2=ml$cm_20_502, LMvol_t2=ml$cm_20_509, urineVol_t2= ml %>% select(contains("cm_20_516")) %>% rowSums(., na.rm=T))
vol_el <- data.frame(childid=el$childid, urine_el_date=dmy(el$ee_el_urine_date), staffid3=el$cm_20_502, LMvol_t3=el$cm_20_509, urineVol_t3= el %>% select(contains("cm_20_516")) %>% rowSums(., na.rm=T))

urinevol <- merge(vol_bl, vol_ml, by="childid")
urinevol <- merge(urinevol, vol_el, by="childid")


#extract L and M concentrations
lm.full<-read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_ee_LM.csv")

lm <- lm.full %>% subset(., select=c(childid, hhid,
                                     mannitol_postLM_bl, lactulose_postLM_bl,
                                     mannitol_postLM_ml, lactulose_postLM_ml,
                                     mannitol_postLM_el, lactulose_postLM_el)) %>% 
                  rename(Lact1= lactulose_postLM_bl,
                         Mann1= mannitol_postLM_bl,
                         Lact2= lactulose_postLM_ml,
                         Mann2= mannitol_postLM_ml,
                         Lact3= lactulose_postLM_el,
                         Mann3= mannitol_postLM_el)

d <- merge(lm, urinevol, by="childid")




#Truncate staffid at <100
table(rbind(d$staffid1,d$staffid2,d$staffid3))
names(table(rbind(d$staffid1,d$staffid2,d$staffid3)))

#Which staff ids had <100 samples collected
inexp_staff_id<-names(which(table(rbind(d$staffid1,d$staffid2,d$staffid3))<100))
inexp_staff_id
#Assign new category to inexperienced IDs across the 3 staffid-round variables
d$staffid1[d$staffid1 %in% inexp_staff_id]<-"inexp"
d$staffid2[d$staffid2 %in% inexp_staff_id]<-"inexp"
d$staffid3[d$staffid3 %in% inexp_staff_id]<-"inexp"


#------------------
#Generate LM ratio
#------------------



#To calculate total lactulose dosed (mg) or total mannitol dosed (mg):
 #The children ingest a solution of 250 mg/ml lactulose and 50 mg/ml of mannitol in a dose of 2 ml/kg of weight up to 20 ml maximum.
 #Q9 of the EE urine form is the total volume of LM solution ingested (in ml). For example, a child who ingested 20 ml of LM solution
#(the maximum dose), would have ingested 1000 mg of mannitol and 5000 mg of lactulose. 
#The 1000 mg and 5000 mg would then be used in the above formula as the "total mannitol dosed (mg) or total lactulose dosed (mg)".
 mean(d$LMvol_t1, na.rm=T)
 mean(d$urineVol_t1, na.rm=T)/1000

d$lact.dose_t1<-d$LMvol_t1*250
d$lact.dose_t2<-d$LMvol_t2*250
d$lact.dose_t3<-d$LMvol_t3*250
d$mann.dose_t1<-d$LMvol_t1*50
d$mann.dose_t2<-d$LMvol_t2*50
d$mann.dose_t3<-d$LMvol_t3*50

mean(d$lact.dose_t1, na.rm=T)
mean(d$mann.dose_t1, na.rm=T)


#% lactulose recovery = (urine concentration lactulose (mg/L) * urine volume (L) * 100 / total lactulose dosed (mg))
d$per.lact.rec_t1<-d$Lact1*(d$urineVol_t1/1000)*100/d$lact.dose_t1
d$per.lact.rec_t2<-d$Lact2*(d$urineVol_t2/1000)*100/d$lact.dose_t2
d$per.lact.rec_t3<-d$Lact3*(d$urineVol_t3/1000)*100/d$lact.dose_t3
mean(d$per.lact.rec_t1, na.rm=T)
mean(d$per.lact.rec_t2, na.rm=T)
mean(d$per.lact.rec_t3, na.rm=T)

table(d$lact.dose_t1==0)
table(d$lact.dose_t2==0)
table(d$lact.dose_t3==0)

#% mannitol recovery = (urine concentration mannitol (mg/L) * urine volume (L) * 100 / total mannitol dosed (mg))
d$per.mann.rec_t1<-d$Mann1*(d$urineVol_t1/1000)*100/d$mann.dose_t1
d$per.mann.rec_t2<-d$Mann2*(d$urineVol_t2/1000)*100/d$mann.dose_t2
d$per.mann.rec_t3<-d$Mann3*(d$urineVol_t3/1000)*100/d$mann.dose_t3
mean(d$per.mann.rec_t1, na.rm=T)
mean(d$per.mann.rec_t2, na.rm=T)
mean(d$per.mann.rec_t3, na.rm=T)



table(d$lact.dose_t1==0)
table(d$lact.dose_t2==0)
table(d$lact.dose_t3==0)


#LM ratio
d$LM1<-d$per.lact.rec_t1/d$per.mann.rec_t1
d$LM2<-d$per.lact.rec_t2/d$per.mann.rec_t2
d$LM3<-d$per.lact.rec_t3/d$per.mann.rec_t3
mean(d$LM1, na.rm=T)


#Data check: are there less LM than lact or mann?
table(d$per.mann.rec_t1==0)
table(d$per.mann.rec_t2==0)
table(d$per.mann.rec_t3==0)

table(d$urineVol_t1==0)
table(d$urineVol_t2==0)
table(d$urineVol_t3==0)


#We also need to report Lactulose recovery and Mannitol recovery in mmol/L (as indicated on our table shells).
    #mmol/L of Lactulose = ??g/ml * 1000 ml/L * 1 mg/1000??g * 1g/1000mg * 1mol/342.296g * 1000mmol/1 mol
#The above simplifies to (??g/ml) * (1 / 342.296) = mmol/L
    #mmol/L of Mannitol = ??g/ml * 1000 ml/L * 1 mg/1000??g * 1g/1000mg * 1mol/182.172g * 1000mmol/1 mol
#The above simplifies to (??g/ml) * (1 / 182.172) = mmol/L
mean(d$Lact1, na.rm=T)
mean(d$LMvol_t1, na.rm=T)
mean(d$Mann1, na.rm=T)

d$lact.rec.MMOL_t1<-(d$Lact1/1000)*(1/342.296)
d$lact.rec.MMOL_t2<-(d$Lact2/1000)*(1/342.296)
d$lact.rec.MMOL_t3<-(d$Lact3/1000)*(1/342.296)
d$mann.rec.MMOL_t1<-(d$Mann1/1000)*(1/182.172)
d$mann.rec.MMOL_t2<-(d$Mann2/1000)*(1/182.172)
d$mann.rec.MMOL_t3<-(d$Mann3/1000)*(1/182.172)
mean(d$lact.rec.MMOL_t1, na.rm=T)


############################
#Calculate outcomes:
############################

d$Lact1<-d$Lact1*(1/342.296)
d$Lact2<-d$Lact2*(1/342.296)
d$Lact3<-d$Lact3*(1/342.296)

d$Mann1<-d$Mann1*(1/182.172)
d$Mann2<-d$Mann2*(1/182.172)
d$Mann3<-d$Mann3*(1/182.172)



############################
#Subset to relevant variables 
#and save data
############################

d <- d %>% subset(., select=c(childid:Lact3,LM1,LM2,LM3,urine_bl_date,urine_ml_date,urine_el_date,staffid1,staffid2,staffid3))

saveRDS(d, file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/WBK-EE-LM-outcomes.rds")
