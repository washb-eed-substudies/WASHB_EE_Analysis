


rm(list=ls())
library(tidyverse)
library(lubridate)



bl <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_urine.csv")
ml <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_urine.csv")
el <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_urine.csv")

bl <- bl %>% 
        rename(urine_bl_date = ee_bl_urine_date,
               staffid1= cm_20_502) %>%
       subset(., select=c(childid, urine_bl_date, staffid1))
ml <- ml %>% 
        rename(urine_ml_date = ee_ml_urine_date,
               staffid2= cm_20_502) %>%
       subset(., select=c(childid, urine_ml_date, staffid2))
el <- el %>% 
        rename(urine_el_date = ee_el_urine_date,
               staffid3= cm_20_502) %>%
       subset(., select=c(childid, urine_el_date, staffid3))


ursurv <- merge(bl, ml, by="childid", all.x = T, all.y = T)
ursurv <- merge(ursurv, el, by="childid", all.x = T, all.y = T)

              



#Convert dates to date classes
ursurv <- ursurv %>% 
          mutate(urine_bl_date = dmy(urine_bl_date),
                 urine_ml_date = dmy(urine_ml_date),
                 urine_el_date = dmy(urine_el_date))



#Truncate staffid at <100
table(rbind(ursurv$staffid1,ursurv$staffid2,ursurv$staffid3))
names(table(rbind(ursurv$staffid1,ursurv$staffid2,ursurv$staffid3)))

#Which staff ids had <100 samples collected
inexp_staff_id<-names(which(table(rbind(ursurv$staffid1,ursurv$staffid2,ursurv$staffid3))<100))
inexp_staff_id
#Assign new category to inexperienced IDs across the 3 staffid-round variables
ursurv$staffid1[ursurv$staffid1 %in% inexp_staff_id]<-"inexp"
ursurv$staffid2[ursurv$staffid2 %in% inexp_staff_id]<-"inexp"
ursurv$staffid3[ursurv$staffid3 %in% inexp_staff_id]<-"inexp"
table(rbind(ursurv$staffid1,ursurv$staffid2,ursurv$staffid3))
                  


#Save survey data
save(ursurv, file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/washk_ee_urine_survey.Rdata")




