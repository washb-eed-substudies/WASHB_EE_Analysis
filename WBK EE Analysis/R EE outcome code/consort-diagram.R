
#---------------------------------------
# EE-BD-consort diagram.R
#
# sophia tan 
#
#---------------------------------------

###Load in data
rm(list=ls())
library(foreign)
library(dplyr)
library(washb)
library(tidyverse)
library(reshape2)

setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Data/Cleaned/Andrew")

#Load in urine, stool datasets 
urine<-read.csv("washb-kenya-eed-urine.csv", stringsAsFactors = TRUE) %>% 
  select(childid, clusterid, tr, Lact1, Lact2, Lact3, Mann1, Mann2, Mann3, LM1, LM2, LM3)
stool<-read.csv("washb-kenya-eed-stool.csv", stringsAsFactors = TRUE) %>%
  select(childid, clusterid, tr, aat1, aat2, aat3, mpo1, mpo2, mpo3, neo1, neo2, neo3)

urine<-urine %>%
  mutate(has_t1 = !is.na(Lact1)|!is.na(Mann1)) %>% 
  mutate(has_t2 = !is.na(Lact2)|!is.na(Mann2)) %>% 
  mutate(has_t3 = !is.na(Lact3)|!is.na(Mann3)) %>% 
  distinct(childid, .keep_all = T) %>% group_by(tr)

stool<-stool %>%
  mutate(has_t1 = !is.na(neo1)|!is.na(mpo1)|!is.na(aat1)) %>% 
  mutate(has_t2 = !is.na(neo2)|!is.na(mpo2)|!is.na(aat2)) %>% 
  mutate(has_t3 = !is.na(neo3)|!is.na(mpo3)|!is.na(aat3)) %>% 
  distinct(childid, .keep_all = T) %>% group_by(tr)

d <- urine %>% full_join(stool, by=c("childid", "clusterid", "tr"))
d <- d %>% mutate(has_t1=has_t1.x|has_t1.y,
                  has_t2=has_t2.x|has_t2.y,
                  has_t3=has_t3.x|has_t3.y) %>% 
  replace_na(list(has_t1=F, has_t2=F, has_t3=F))

d %>% group_by(tr) %>% 
  summarise(t1=sum(has_t1), t2=sum(has_t2), t3=sum(has_t3), 
            t1_stool=sum(has_t1.y,na.rm=T), t2_stool=sum(has_t2.y,na.rm=T), t3_stool=sum(has_t3.y,na.rm=T),
            t1_urine=sum(has_t1.x,na.rm=T), t2_urine=sum(has_t2.x,na.rm=T), t3_urine=sum(has_t3.x,na.rm=T),
  ) %>% select(c(tr, t1, t1_stool, t1_urine, t2, t2_stool, t2_urine, t3, t3_stool, t3_urine))

d %>% filter(has_t1==T) %>% summarise(clusters=length(unique(clusterid)))
d %>% filter(has_t2==T) %>% summarise(clusters=length(unique(clusterid)))
d %>% filter(has_t3==T) %>% summarise(clusters=length(unique(clusterid)))


urine %>% group_by(tr) %>% 
  summarise(t1=sum(has_t1), t2=sum(has_t2), t3=sum(has_t3))

stool %>% group_by(tr) %>% 
  summarise(t1=sum(has_t1), t2=sum(has_t2), t3=sum(has_t3))
