
#---------------------------------------
# consort-diagram-FUT2.R
#
# andrew mertens
#
#---------------------------------------

###Load in data
rm(list=ls())
library(foreign)
library(dplyr)
library(washb)
library(tidyverse)
library(reshape2)

#ADAPT FOR FUT2

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")


d <- readRDS("EE-BD_fut2_analysis_dataset.rds") %>%
  #select(childid, clusterid, tr, agem1,    agem2,    agem3, cFU2, mFU2) %>%
  mutate(has_cFU2=1*!is.na(cFU2),
         has_mFU2=1*!is.na(mFU2),
         has_EED=!is.na(aat) | !is.na(mpo) | !is.na(neo) | !is.na(reg1b) | !is.na(Mann) | !is.na(Lact) )
summary(d$cFU2)

table(d$tr, (d$has_cFU2))
table(d$tr, (d$has_mFU2))
table(d$tr, (d$has_EED))


colnames(d)

# Between May 2012 and July 2013, in the overall trial, 720 clusters of 5551 pregnant women were randomized to an intervention (previously described) or control arm. W
# e assessed FUT2 secretor status in 1499 children and 1379 mothers. Of the children, 1022 (68.2%) were secretors, 323 (21.5%) were non-secretors, and
# 154 (10.3%) were inconclusive (Table and Figure 1). Among the mothers, 893 (64.8%) were secretors, 308 (22.3%) were non-secretors, and 178 (12.9%) were inconclusive (Table 1).

clust1 <- d %>% filter(!is.na(cFU2)|!is.na(mFU2), has_EED==1, round==1) %>% distinct(clusterid, tr) 
table(clust1$tr)
clust1 <- d %>% filter(!is.na(cFU2)|!is.na(mFU2), has_EED==1,round==2) %>% distinct(clusterid, tr) 
table(clust1$tr)

clust1 <- d %>% filter(has_cFU2==1, has_EED==1, round==1) %>% distinct(childid, tr) 
table(clust1$tr)
clust1 <- d %>% filter(has_cFU2==1, has_EED==1, round==2) %>% distinct(childid, tr) 
table(clust1$tr)

cluster <- d %>% filter(!is.na(cFU2)|!is.na(mFU2), has_EED==1) %>%
  distinct(clusterid, round, .keep_all = T) 
table(cluster$round, cluster$tr)

cfut2 <- d %>% filter(!is.na(cFU2), has_EED==1) %>%
  distinct(childid, round, .keep_all = T) 
table(cfut2$round, cfut2$tr)

mfut2 <- d %>% filter(!is.na(mFU2), has_EED==1) %>%
  distinct(childid, round, .keep_all = T) 
table(mfut2$round, mfut2$tr)


# #Check Ronit's manuscript and the actual graph to get what's needed in the consort
# From EED manuscript:
# 
# d %>% group_by(tr) %>% 
#   summarise(t1=sum(has_t1), t2=sum(has_t2), t3=sum(has_t3), 
#             t1_stool=sum(has_t1.y,na.rm=T), t2_stool=sum(has_t2.y,na.rm=T), t3_stool=sum(has_t3.y,na.rm=T),
#             t1_urine=sum(has_t1.x,na.rm=T), t2_urine=sum(has_t2.x,na.rm=T), t3_urine=sum(has_t3.x,na.rm=T),
#   ) %>% select(c(tr, t1, t1_stool, t1_urine, t2, t2_stool, t2_urine, t3, t3_stool, t3_urine)) 
# 
# 
# 
# #Missing samples
# samp_present <- d %>% group_by(tr) %>% 
#   summarise(
#             t1_miss_stool=sum(has_t1.y,na.rm=T), t2_miss_stool=sum(has_t2.y,na.rm=T), t3_miss_stool=sum(has_t3.y,na.rm=T),
#             t1_miss_urine=sum(has_t1.x,na.rm=T), t2_miss_urine=sum(has_t2.x,na.rm=T), t3_miss_urine=sum(has_t3.x,na.rm=T))
# 
# enrolled_kids <- data.frame(tr=c("Control", "WSH", "Nutrition", "Nutrition + WSH"),
#                             t1_kids=c(378, 377, 355, 383),
#                             t2_kids=c(412, 360, 370, 362),
#                             t3_kids=c(407, 344, 356, 337))
# 
# samp_present <- left_join(samp_present, enrolled_kids, by="tr")
# samp_present %>% mutate(t1_miss_stool = t1_kids - t1_miss_stool,
#                         t2_miss_stool = t2_kids - t2_miss_stool, 
#                         t3_miss_stool = t3_kids - t3_miss_stool, 
#                         t1_miss_urine = t1_kids - t1_miss_urine, 
#                         t2_miss_urine = t2_kids - t2_miss_urine, 
#                         t3_miss_urine = t3_kids - t3_miss_urine) %>%
#                  subset(., select = -c(t1_kids, t2_kids, t3_kids))
# 
# #These numbers don't seem to match the subsample attendence
# 
# 
# d %>% filter(has_t1==T) %>% summarise(clusters=length(unique(clusterid)))
# d %>% filter(has_t2==T) %>% summarise(clusters=length(unique(clusterid)))
# d %>% filter(has_t3==T) %>% summarise(clusters=length(unique(clusterid)))
# 
# 
# urine %>% group_by(tr) %>% 
#   summarise(t1=sum(has_t1), t2=sum(has_t2), t3=sum(has_t3))
# 
# stool %>% group_by(tr) %>% 
#   summarise(t1=sum(has_t1), t2=sum(has_t2), t3=sum(has_t3))
# 
# head(urine)
# head(stool)
# 
# dfull <- merge(urine, stool, by=c("childid","tr"), all.x=T, all.y = T)
# head(dfull)
# 
# dim(urine)
# dim(stool)
# dim(dfull)
# 
# dfull$clusterid = dfull$clusterid.x
# dfull$clusterid[is.na(dfull$clusterid.x)] = dfull$clusterid.y[is.na(dfull$clusterid.x)] 
#   
# 
# dfull %>% group_by(tr) %>% 
#   summarise(t1.clust=length(unique(clusterid)), t1=sum(has_t1.x|has_t1.y, na.rm=T), t2.clust=length(unique(clusterid)), t2=sum(has_t2.x|has_t2.y, na.rm=T), t3.clust=length(unique(clusterid)), t3=sum(has_t3.x|has_t3.y, na.rm=T))
# 
# 
# # get number of children and clusters at each age
# dfull %>% group_by(tr) %>% 
#   summarise(t1.clust=length(unique(clusterid)), t1=sum(has_t1.x|has_t1.y, na.rm=T), t2.clust=length(unique(clusterid)), t2=sum(has_t2.x|has_t2.y, na.rm=T), t3.clust=length(unique(clusterid)), t3=sum(has_t3.x|has_t3.y, na.rm=T))
# 
# #
# #These 2 sentences from the text need to be completed with numbers: 
# #The mean (± SD) ages of children at the time of EED measurement was 5.4 (± 1.8) months, 16.7 (± 2.1) months, and 22.3 (± 1.8) months.
# head(dfull)
# round(mean(dfull$agem1.x, na.rm=T),1)
# round(mean(dfull$agem1.y, na.rm=T),1)
# round(sd(dfull$agem1.x, na.rm=T),1)
# round(sd(dfull$agem1.y, na.rm=T),1)
# 
# round(mean(dfull$agem2.x, na.rm=T),1)
# round(mean(dfull$agem2.y, na.rm=T),1)
# round(sd(dfull$agem2.x, na.rm=T),2)
# round(sd(dfull$agem2.y, na.rm=T),2)
# 
# round(mean(dfull$agem3.x, na.rm=T),1)
# round(mean(dfull$agem3.y, na.rm=T),1)
# round(sd(dfull$agem3.x, na.rm=T),1)
# round(sd(dfull$agem3.y, na.rm=T),1)
# 
# 
# #The mean concentrations of the pooled EED biomarkers across the three time points were: 
# #neopterin (1284.298 nmol/L), myeloperoxidase (3706.581 ng/ml), alpha-1-antitrypsin (0.5119887 mg/g), lactulose (0.2994443 mmol/L), and mannitol (2.64883 mmol/L). 
# #[These are absolute values, not logged values]
# 
# mean(c(dfull$Lact1, dfull$Lact2, dfull$Lact3), na.rm=T)
# mean(c(dfull$Mann1, dfull$Mann2, dfull$Mann3), na.rm=T)
# mean(c(dfull$mpo1, dfull$mpo2, dfull$mpo3), na.rm=T)
# mean(c(dfull$neo1, dfull$neo2, dfull$neo3), na.rm=T)
# mean(c(dfull$aat1, dfull$aat2, dfull$aat3), na.rm=T)
# 
# 
# 
# 
# 
