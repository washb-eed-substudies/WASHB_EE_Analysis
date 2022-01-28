
#---------------------------------------
# EE-BD-table1.R
#
# sophia tan
#
# The tabulate enrollment variables for 
# manuscript table 1
# Also contains supplementary tables 1 and 2 
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

vlist <- c("mother_age","motherht", "mom_primary","dad_primary","dad_agri","Ncomp","elec","cement","roof","water_time","prim_drink_ws_bl","tr_storedwt_bl",
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
  
  if(vlist[i] %in% c("mother_age","motherht","Ncomp","water_time")){
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
                 "Works in agriculture","Number of people per compound","Has electricity","Has a cement floor","Has an iron roof",
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

ctrl <- c("26 (6)", "160 (6)", "916 (48%)", "1098 (62%)", "749 (41%)", 
          "8 (5)", "122 (6%)", "107 (6%)", "1302 (68%)", 
          "11 (12)", "1446 (76%)", "196 (13%)", 
          "789 (78%)", "1561 (82%)", "309 (17%)", "163 (9%)", 
          "487 (25%)", "164 (9%)", "203 (11%)")
wsh <- c("26 (6)", "160 (6)", "430 (47%)", "521 (61%)", "374 (43%)", 
         "8 (5)", "64 (7%)", "50 (5%)", "574 (63%)", 
         "11 (13)", "624 (69%)", "97 (13%)", 
         "394 (77%)", "754 (83%)", "153 (18%)", "73 (8%)", 
         "251 (28%)", "115 (13%)", "101 (11%)")
nutrition <- c("26 (6)", "160 (6)", "409 (49%)", "491 (64%)", "343 (43%)", 
          "8 (7)", "58 (7%)", "48 (6%)", "581 (69%)", 
          "11 (12)", "603 (72%)", "79 (12%)", 
          "363 (79%)", "701 (83%)", "119 (15%)", "73 (9%)", 
          "228 (27%)", "90 (11%)", "98 (12%)")
n_wsh <- c("26 (6)", "160 (6)", "438 (48%)", "526 (62%)", "372 (43%)", 
           "8 (5)", "67 (7%)", "56 (6%)", "615 (67%)", 
           "11 (12)", "697 (76%)", "106 (14%)", 
           "388 (78%)", "764 (83%)", "143 (16%)", "87 (9%)", 
           "249 (27%)", "87 (9%)", "104 (11%)")

tbls1 <- cbind(ctrl, wsh, nutrition, n_wsh) %>% as.data.frame()
names(tbls1) <- paste0(c("Control", "Water, Sanitation, and Handwashing", "Nutrition", "Water, Sanitation, Handwashing, and Nutrition"),
                         " (N=", c(1919, 912, 843, 921), ")")

tbls1 <- cbind(results, tbls1) %>% select(1, 2, 7:10, 3:6)
tbls1_flex <- flextable(tbls1) %>% align(j=3:10, align="center", part = "all") %>% hline(i=c(3,5,9,12,16,18))
tbls1_flex <- tbls1_flex %>% add_header_row(values = c("", "WASH Benefits Main Trial", "EED Substudy"), colwidths = c(2, 4, 4)) %>% hline_top(part="header") 

tbls1_flex %>% save_as_docx(path="EE-Kenya-tables1.docx")



#Create supplimentary table 2
#Merge in outcomes


#Create supplimentary table 1
#Merge in outcomes
setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Data/Cleaned/Andrew")
#setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Cleaned/Andrew")
urine<-read.csv("washb-kenya-eed-urine.csv", stringsAsFactors = TRUE)
stool<-read.csv("washb-kenya-eed-stool.csv", stringsAsFactors = TRUE)
urine$childid<-as.numeric(urine$childid)
stool$childid<-as.numeric(stool$childid)

urine <- urine%>%select(childid, Lact1, Lact2, Lact3, Mann1, Mann2, Mann3, agem1, agem2, agem3) %>% 
  rename("agem_u1"="agem1", "agem_u2"="agem2", "agem_u3"="agem3")
stool <- stool%>%select(childid, aat1, aat2, aat3, mpo1, mpo2, mpo3, neo1, neo2, neo3, agem1, agem2, agem3) %>% 
  rename("agem_s1"="agem1", "agem_s2"="agem2", "agem_s3"="agem3")


d<-left_join(d,urine, by="childid")
d<-left_join(d,stool, by="childid")
d <- d%>%mutate(agem1_min = min(agem_u1, agem_s1, na.rm=T), agem2_min = min(agem_u2, agem_s2, na.rm=T), agem3_min = min(agem_u3, agem_s3, na.rm=T))

d <- d %>% left_join(read_csv("raw CSV/washk_allchild_covariates.csv") %>% select(childid, sex), 'childid')

overallN1<-d%>% 
  subset(!is.na(d$Lact1)|!is.na(d$Mann1)|!is.na(d$aat1)|!is.na(d$mpo1)|!is.na(d$neo1)|
           !is.na(d$Lact2)|!is.na(d$Mann2)|!is.na(d$aat2)|!is.na(d$mpo2)|!is.na(d$neo2)|
           !is.na(d$Lact3)|!is.na(d$Mann3)|!is.na(d$aat3)|!is.na(d$mpo3)|!is.na(d$neo3)) %>% 
  summarize(N=n(),
            Median_agem=median(agem1_min, na.rm=T), 
            Mean_agem=mean(agem1_min, na.rm=T), 
            Sd_agem=sd(agem1_min, na.rm=T), nummales=sum(sex=="Male", na.rm=T), numfemales=sum(sex=="Female", na.rm=T)) 
overallN1<-cbind("Overall", overallN1)
colnames(overallN1)[1]<-"tr"

suppd1<-d %>% 
    subset(is.na(d$Lact2) & is.na(d$Mann2)&is.na(d$aat2)&is.na(d$mpo2)&is.na(d$neo2))
dim(suppd1)


suppd2<-d %>% 
    subset(is.na(d$Lact3) & is.na(d$Mann3) & is.na(d$aat3) & is.na(d$mpo3) & is.na(d$neo3))
dim(suppd2) 
  

dim(suppd1)
dim(suppd2)

d$col<-1
suppd1$col<-2
suppd2$col<-3


table.dat<-subset(d, select=c("tr","col",vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH") %>% select(-tr)
#Lost to follow-up at t2
s1.table.dat<-subset(suppd1, select=c("tr","col", vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH") %>% select(-tr)
#Lost to follow-up at t3
s2.table.dat<-subset(suppd2, select=c("tr","col", vlist)) %>% subset(tr=="Control" | tr=="WSH" | tr=="Nutrition" | tr=="Nutrition + WSH") %>% select(-tr)

#combine:
s.table.dat<-rbind(table.dat,s1.table.dat,s2.table.dat)
head(s.table.dat)

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

setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Results/tables")
save(s.balance.tab.mu_M, s.balance.tab.n_M, s.balance.tab.sd_M, 
     file="EE-BD_s.table2.Rdata")


library(data.table)
s.mu <- s.balance.tab.mu_M %>%transpose(keep.names = "v")
s.n <- s.balance.tab.n_M %>%transpose(keep.names = "v")
s.sd <- s.balance.tab.sd_M %>%transpose(keep.names = "v")

results <- data.frame()
for (i in 1:length(vlist)) {
  mu <- s.mu[i+1,2:4] %>% as.vector()
  n <- round(s.n[i+1,2:4])%>% as.vector()
  sd <- round(s.sd[i+1,2:4])%>% as.vector()
  
  if(vlist[i] %in% c("mother_age","motherht","Ncomp","water_time")){
    results <- rbind(results, paste0(round(mu), " (", sd, ")"))
  }else{
    results <- rbind(results, paste0(n, " (", round(mu*100), "%)"))
  }
}

N <- s.mu[1,2:4] %>% as.vector()
names(results) <- paste0(c("Included", "Lost to follow-up at 17 months", "Lost to follow-up at 22 months"),
                         " (N=", N, ")")
vlist_names <- c("Age (years)","Height (cm)", "Completed at least primary education","Completed at least primary education",
                 "Works in agriculture","Number of people per compound","Has electricity","Has a cement floor","Has an iron roof",
                 "One-way walking time to primary water source (min)",
                 "Primary drinking water source is improved","Reported treating currently stored water",
                 "Daily defecating in the open, children aged 0 to <3 years","Own any latrine", "Access to improved latrine", 
                 "Human feces observed in compound","Has water within 2 m","Has soap within 2 m", "Moderate to severe household hunger")
categories <- c("Maternal", "","", "Paternal", "", "Household", "","","","Drinking water", "","","Sanitation","","","","Handwashing location", "","Food security")
results$` ` <- vlist_names
results$`  ` <- categories
results <- results %>% select(5, 4, 1, 2, 3)

setwd("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Results/tables")
library(flextable)
flextable(results) %>% align(j=3:5, align="center", part = "all") %>% autofit() %>% hline(i=c(3,5,9,12,16,18)) %>% save_as_docx(path="EE-Kenya-tables2.docx")
