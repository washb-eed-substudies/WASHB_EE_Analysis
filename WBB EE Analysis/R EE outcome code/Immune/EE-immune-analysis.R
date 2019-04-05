
rm(list=ls())
library(tidyverse)
library(haven)
library(washb)
library(data.table)

#load immune
imm<-read_dta("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/immune/washb-bangladesh-immun-lab-t2-t3.dta")
imm <- as.data.frame(imm)
imm$childid <- as.numeric(imm$childid)
head(imm)

#log transform outcomes
imm[,-1] <- log(imm[,-1])

fulld <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew/EE-BD_fulldata.csv")
colnames(fulld)

#Subset to needed variables

#Merge in immune outcomes
dim(imm)
dim(fulld)
d <- left_join(fulld, imm, by="childid")
dim(d)

#Drop real treatment arm
d <- subset(d, select = -c(tr))
#Merge in blinded treatment
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/washb-bangladesh-blind-tr.RData")
blind_tr$clusterid <- as.numeric(blind_tr$clusterid)
d <- left_join(d, blind_tr, by=c("block", "clusterid"))
dim(d)
table(d$tr)

#load in names of Audrie's objects
nm <- list.files(path="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Audrie/")
nm


#N's and means
outcomes <- c("igf_t2",          "igf_t3",          "crp_t2",         
  "agp_t2",          "gmcsf_t2",        "ifng_t2",         "il10_t2",         "il12_t2",        
  "il13_t2",         "il17_t2",         "il1_t2",          "il2_t2",          "il21_t2",        
  "il4_t2",          "il5_t2",          "il6_t2",          "tnfa_t2",         "gmcsf_t3",       
  "ifng_t3",         "il10_t3",         "il12_t3",         "il13_t3",         "il17_t3",        
  "il1_t3",          "il2_t3",          "il21_t3",         "il4_t3",          "il5_t3",         
  "il6_t3",          "tnfa_t3")


mean_sd <- d %>% select(outcomes) %>% summarise_all(funs(mean, sd), na.rm=T) %>% gather()
n <-nrow(mean_sd)/2
#split mean and SD into different columns
mean_sd <- data.frame(Y=gsub("_mean","",mean_sd[1:n,1]), mean=mean_sd[1:n,2], sd=mean_sd[(n+1):(2*n),2]) 

#Compare to Audrie's

load("Results/Audrie/immune_N_means.RData")
ls()

aud_N <- as.data.frame(rbindlist(lapply(ls(pattern="_N_L"), get)))
aud_N$Y = gsub("_N_L","",ls(pattern="_N_L"))

#merge and compare
N_comp <- merge(aud_N, mean_sd, by="Y")
dim(N_comp)
N_comp$mean.diff <- N_comp$mean.x - N_comp$mean.y
N_comp$sd.diff <- N_comp$sd.x - N_comp$sd.y
max(N_comp$mean.diff)
max(N_comp$sd.diff)

#unadjusted analysis







