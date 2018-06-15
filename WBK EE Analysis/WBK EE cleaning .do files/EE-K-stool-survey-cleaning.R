


rm(list=ls())
library(tidyverse)
library(lubridate)



bl <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eebl_stool.csv")
ml <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeml_stool.csv")
el <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/raw CSV/washk_eeel_stool.csv")

bl <- bl %>% 
        rename(stool_bl_date = ee_bl_stool_date,
               staffid1= cm_20_402) %>%
       subset(., select=c(childid, stool_bl_date, staffid1))
ml <- ml %>% 
        rename(stool_ml_date = ee_ml_stool_date,
               staffid2= cm_20_402) %>%
       subset(., select=c(childid, stool_ml_date, staffid2))
el <- el %>% 
        rename(stool_el_date = ee_el_stool_date,
               staffid3= cm_20_402) %>%
       subset(., select=c(childid, stool_el_date, staffid3))


stsurv <- merge(bl, ml, by="childid", all.x = T, all.y = T)
stsurv <- merge(stsurv, el, by="childid", all.x = T, all.y = T)

              



#Convert dates to date classes
stsurv <- stsurv %>% 
          mutate(stool_bl_date = dmy(stool_bl_date),
                 stool_ml_date = dmy(stool_ml_date),
                 stool_el_date = dmy(stool_el_date))



#Truncate staffid at <100
table(rbind(stsurv$staffid1,stsurv$staffid2,stsurv$staffid3))
names(table(rbind(stsurv$staffid1,stsurv$staffid2,stsurv$staffid3)))

#Which staff ids had <100 samples collected
inexp_staff_id<-names(which(table(rbind(stsurv$staffid1,stsurv$staffid2,stsurv$staffid3))<100))
inexp_staff_id
#Assign new category to inexperienced IDs across the 3 staffid-round variables
stsurv$staffid1[stsurv$staffid1 %in% inexp_staff_id]<-"inexp"
stsurv$staffid2[stsurv$staffid2 %in% inexp_staff_id]<-"inexp"
stsurv$staffid3[stsurv$staffid3 %in% inexp_staff_id]<-"inexp"
table(rbind(stsurv$staffid1,stsurv$staffid2,stsurv$staffid3))
                  


#Save survey data
save(stsurv, file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/washk_ee_stool_survey.Rdata")




