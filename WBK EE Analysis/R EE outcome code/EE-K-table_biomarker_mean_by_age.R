
#---------------------------------------
# EE-BD-tables3.R
#
# sophia tan 
#
# Summary measures of absolute concentration 
# of EED biomarkers by child age
#---------------------------------------

###Load in data
rm(list=ls())
library(foreign)
library(dplyr)
library(washb)
library(tidyr)
library(reshape2)

#setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Data/Cleaned/Andrew")
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/Andrew/")

#Load in urine, stool datasets 
urine<-read.csv("washb-kenya-eed-urine.csv", stringsAsFactors = TRUE) %>% 
  select(childid, Lact1, Lact2, Lact3, Mann1, Mann2, Mann3, LM1, LM2, LM3,
         per.lact.rec_t1, per.lact.rec_t2, per.lact.rec_t3,
         per.mann.rec_t1, per.mann.rec_t2, per.mann.rec_t3)
stool<-read.csv("washb-kenya-eed-stool.csv", stringsAsFactors = TRUE) %>%
  select(childid, aat1, aat2, aat3, mpo1, mpo2, mpo3, neo1, neo2, neo3)

d <- full_join(urine, stool, "childid")

library(data.table)
results <- d %>% select(!childid) %>% 
  apply(2, quantile, c(0.25, .5, .75), na.rm=T) %>% 
  round(2) %>% as.data.frame() %>% transpose(keep.names = "biomarker")

results$`Median (IQR)` <- paste0(results$V2, " (", results$V1, ", ", results$V3, ")")

results <- results %>% select(!c(V1, V2, V3))
t1 <- grep("1", results$biomarker, value=T)
t2 <- grep("2", results$biomarker, value=T)
t3 <- grep("3", results$biomarker, value=T)

results <- results %>% filter(biomarker %in% t1) %>%
  cbind(results %>% filter(biomarker %in% t2) %>% select(!biomarker)) %>% 
  cbind(results %>% filter(biomarker %in% t3) %>% select(!biomarker))

results <-results[c(7,6,8,1,2,3),]
results$biomarker <- c("Myeloperoxidase (ng/ml)", "Alpha-1 antitrypsin (mg/g)", 
                       "Neopterin (nmol/L)", "Lactulose (mmol/L)", "Mannitol (mmol/L)",
                       "L:M ratio")
results <- rbind(results, c("Outcome", rep("Median (IQR)", 3)))
results <- results[c(7, 1:6),]
names(results) <- c(" ", "Child age 6 months", "Child age 17 months", "Child age 22 months")

#save objects
#setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Results/Tables")
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Results/Tables/")

library(flextable)
flextable(results) %>% hline(i = 1) %>% 
  align(j=2:4, align="center", part="all") %>% save_as_docx(path="EE-Kenya-tableSX.docx")
