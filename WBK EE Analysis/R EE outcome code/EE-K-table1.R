
#---------------------------------------
# EE-BD-table1.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The tabulate enrollment variables for 
# manuscript table 1
#---------------------------------------

###Load in data
rm(list=ls())
library(foreign)
library(dplyr)
library(washb)
library(tidyr)
library(reshape2)

setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Data/Cleaned/Andrew")
#setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Untouched/")
tr <- read.csv("raw CSV/washk_TR.csv")
tr$tr <- factor(tr$tr, levels = c("Control", "WSH", "Nutrition", "Nutrition + WSH"))

# #Load in enrollment data for adjusted analysis
# setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
enrol<-read.csv("raw CSV/washk_ee_covariates.csv")

#Load in urine, stool, and medhistory datasets to track all children 
#who participated in the eed substudy
#setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
urine<-read.csv("washb-kenya-eed-urine.csv", stringsAsFactors = TRUE)
stool<-read.csv("washb-kenya-eed-stool.csv", stringsAsFactors = TRUE)
# medhistory<-read.csv("BD-EE-medhistory.csv", stringsAsFactors = TRUE)

#Select non-duplicate childids:
urine<-urine %>%
  mutate(urine_data = Lact1|Lact2|Lact3|Mann1|Mann2|Mann3) %>% 
  subset(urine_data==T) %>%
  select(childid, hhid, clusterid) %>%
  distinct(childid, hhid, clusterid)

stool<-stool %>% 
  mutate(stool_data=aat1|aat2|aat3|mpo1|mpo2|mpo3|neo1|neo2|neo3) %>%
  subset(stool_data==T) %>%
  select(childid, hhid, clusterid) %>%
  distinct(childid, hhid, clusterid)

childid<-union(urine, stool)


#Merge treatment information 
dim(childid)
d<-left_join(childid,tr, by="clusterid")
dim(d)
head(d)
table(d$tr)


#Merge in enrollment information
dim(d)
dim(enrol)
d<-left_join(d,enrol, by=c("hhid", "clusterid"))
dim(d)

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 

#Generate table 1
colnames(d)

#Drop twins so HH characteristics aren't duplicates
d<-d%>%group_by(hhid)%>%summarise_all(first)

#test that all rows are matched to enrollment data
table(is.na(d$svydate)) 

#Generate table 1
colnames(d)
d$foodinsecure<-ifelse(d$HHS_bi=="little none", 0,ifelse(d$HHS_bi=="", NA, 1))
d$mom_primary<-ifelse(d$mother_edu=="Incomplete primary", 0, ifelse(d$mother_edu=="", NA, 1))
d$dad_primary<-ifelse(d$father_edu=="incomplete primary", 0, ifelse(d$father_edu=="", NA, 1))
d$dad_agri <- ifelse(d$father_agri=="works in ag", 1, ifelse(d$father_agri=="", NA, 0))
d$elec <- ifelse(d$elec=="Has electricity", 1, ifelse(d$elec=="Missing/DK", NA, 0))
d$cement <- ifelse(d$floor=="Concrete", 1, ifelse(d$floor=="Missing/DK", NA, 0))
d$roof <- ifelse(d$roof=="Iron/other", 1, 0)

d$prim_drink_ws_bl <-ifelse(d$prim_drink_ws_bl=="yes", 1, ifelse(d$prim_drink_ws_bl=="", NA, 0))
d$tr_storedwt_bl <-ifelse(d$tr_storedwt_bl=="yes", 1, ifelse(d$tr_storedwt_bl=="", NA, 0))
d$od_child03_bl <-ifelse(d$od_child03_bl=="yes", 1, ifelse(d$od_child03_bl=="", NA, 0))
d$ownlat_bl <-ifelse(d$ownlat_bl=="yes", 1, ifelse(d$ownlat_bl=="", NA, 0))
d$imp_lat_bl <-ifelse(d$imp_lat_bl=="yes", 1, ifelse(d$imp_lat_bl=="", NA, 0))
d$feces_bl <-ifelse(d$feces_bl=="yes", 1, ifelse(d$feces_bl=="", NA, 0))
d$water_bl <-ifelse(d$water_bl=="yes", 1, ifelse(d$water_bl=="", NA, 0))
d$soap_bl <-ifelse(d$soap_bl=="yes", 1, ifelse(d$soap_bl=="", NA, 0))

vlist <- c("mother_age","motherht", "mom_primary","dad_primary","dad_agri","Nhh","elec","cement","roof","water_time","prim_drink_ws_bl","tr_storedwt_bl",
           "od_child03_bl","ownlat_bl", "imp_lat_bl", "feces_bl","water_bl","soap_bl", "foodinsecure")


table(vlist %in% colnames(d))

table.dat<-subset(d, select=c("tr", vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH")


#Change factors to indicators
for(i in 1:ncol(table.dat)){
  cat(colnames(table.dat)[i]," : ",class((table.dat[,i])),"\n")
}


#Calculate number of compounds

table1_mu<-table.dat%>%
        group_by(tr) %>%
        summarise_all(funs(mean(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

table1_N<-table.dat%>%
        group_by(tr) %>%
        summarise_each(funs(sum(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

table1_sd<-table.dat%>%
        group_by(tr) %>%
        summarise_each(funs(sd(., na.rm = TRUE))) %>%
        ungroup %>% as.data.frame

Ns<-table(table.dat$tr)
balance.tab.mu_M<-rbind(Ns,t((table1_mu[,2:ncol(table1_mu)])))
balance.tab.n_M<-rbind(Ns,t((table1_N[,2:ncol(table1_N)])))
balance.tab.sd_M<-rbind(Ns,t((table1_sd[,2:ncol(table1_sd)])))


#save objects
setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Results/Andrew")
#setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
save(balance.tab.mu_M, balance.tab.n_M, balance.tab.sd_M, 
     file="EE-Kenya-table1.Rdata")

results <- data.frame()
for (i in 1:length(vlist)) {
  mu <- balance.tab.mu_M[i+1,]
  n <- round(balance.tab.n_M[i+1,])
  sd <- round(balance.tab.sd_M[i+1,])
  
  if(vlist[i] %in% c("mother_age","motherht","Nhh","water_time")){
    results <- rbind(results, paste0(round(mu), " (", sd, ")"))
  }else{
    results <- rbind(results, paste0(n, " (", round(mu*100), "%)"))
  }
}

Ns <- Ns %>% as.data.frame()
N <- Ns$Freq
names(results) <- paste0(c("Control", "Water, Sanitation, and Handwashing", "Nutrition", "Water, Sanitation, Handwashing, and Nutrition"),
                         " (N=", N, ")")
vlist_names <- c("Age (years)","Height (cm)", "Completed at least primary education","Completed at least primary education",
                 "Works in agriculture","Number of people","Has electricity","Has a cement floor","Has an iron roof",
                 "One-way walking time to primary water source (min)",
                 "Primary drinking water source is improved","Reported treating currently stored water",
                 "Daily defecating in the open, children aged 0 to <3 years","Own any latrine", "Access to improved latrine", 
                 "Human feces observed in compound","Has water within 2 m","Has soap within 2 m", "Moderate to severe household hunger")
categories <- c("Maternal", "","", "Paternal", "", "Household", "","","","Drinking water", "","","Sanitation","","","","Handwashing location", "","Food security")
results$` ` <- vlist_names
results$`  ` <- categories
results <- results %>% select(6, 5, 1, 2, 3, 4)

setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Results/tables")
library(flextable)
flextable(results) %>% align(j=3:6, align="center") %>% hline(i=c(3,5,9,12,16,18)) %>% save_as_docx(path="EE-Kenya-table1.docx")

#Create supplimentary table 1
#Merge in outcomes


#Create supplimentary table 1
#Merge in outcomes
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
urine.outcomes<-read.dta("washb-BD-EE-urine-outcomes-stata12.dta")
stool.outcomes<-read.dta("BD-EE-stool-outcomes-Stata12.dta")
urine.outcomes$childid<-as.numeric(urine.outcomes$childid)
stool.outcomes$childid<-as.numeric(stool.outcomes$childid)



d<-left_join(d,urine.outcomes, by="childid")
d<-left_join(d,stool.outcomes, by="childid")


overallN1<-d%>% 
  subset(!is.na(d$Lact1)|!is.na(d$Mann1)|!is.na(d$t1_aat)|!is.na(d$t1_mpo)|!is.na(d$t1_neo)) %>% 
  summarize(N=n(),Median_agem=median(agem1, na.rm=T), Mean_agem=mean(agem1, na.rm=T), Sd_agem=sd(agem1, na.rm=T), nummales=sum(sex), numfemales=n()-sum(sex)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

suppd1<-d %>% 
    subset(is.na(d$Lact2) & is.na(d$Mann2)&is.na(d$t2_aat)&is.na(d$t2_mpo)&is.na(d$t2_neo))
dim(suppd1)


suppd2<-d %>% 
    subset(is.na(d$Lact3) & is.na(d$Mann3) & is.na(d$t3_aat) & is.na(d$t3_mpo) & is.na(d$t3_neo))
dim(suppd2) 
  

dim(suppd1)
dim(suppd2)

d$col<-1
suppd1$col<-2
suppd2$col<-3


table.dat<-subset(d, select=c("tr","col",vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH") %>% select(-tr)
#Telomere substudy enrolled at Year 1
s1.table.dat<-subset(suppd1, select=c("tr","col", vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH") %>% select(-tr)
#Telomere substudy lost to follow-up at Year 2
s2.table.dat<-subset(suppd2, select=c("tr","col", vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH") %>% select(-tr)

#combine:
s.table.dat<-rbind(table.dat,s1.table.dat,s2.table.dat)
head(s.table.dat)

#Change factors to indicators
for(i in 1:ncol(s.table.dat)){
  cat(colnames(s.table.dat)[i]," : ",class((s.table.dat[,i])),"\n")
}

s.table.dat$hfiacat<-ifelse(s.table.dat$hfiacat=="Severely Food Insecure" | s.table.dat$hfiacat=="Moderately Food Insecure", 1,0)

#Supplimentary table 1 
s.table1_mu<-s.table.dat%>%
        group_by(col) %>%
        summarise_each(funs(mean(., na.rm = TRUE))) %>%
        ungroup %>% 
        as.data.frame

s.table1_N<-s.table.dat%>%
        group_by(col) %>%
        summarise_each(funs(sum(., na.rm = TRUE))) %>%
        ungroup %>% 
        as.data.frame

s.table1_sd<-s.table.dat%>%
        group_by(col) %>%
        summarise_each(funs(sd(., na.rm = TRUE))) %>%
        ungroup %>% 
        as.data.frame


s.Ns<-table(s.table.dat$col)
s.Ns<-c(s.Ns[1],s.Ns[2],s.Ns[3])


s.balance.tab.mu_M<-s.table1_mu
s.balance.tab.n_M<-s.table1_N
s.balance.tab.sd_M<-s.table1_sd
s.balance.tab.mu_M[,1]<-s.Ns
s.balance.tab.n_M[,1]<-s.Ns
s.balance.tab.sd_M[,1]<-s.Ns

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Tables/")
save(s.balance.tab.mu_M, s.balance.tab.n_M, s.balance.tab.sd_M, 
     file="EE-BD_s.table1.Rdata")


