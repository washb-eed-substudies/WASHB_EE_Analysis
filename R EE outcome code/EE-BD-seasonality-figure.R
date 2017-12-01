
##########################################
#Plot EED biormarker over time to examine seasonality
##########################################



rm(list=ls())
library(RColorBrewer)
library(scales)
library(foreign)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes) 
library(gridExtra)
library(lubridate)
library(reshape2)
theme_set(theme_bw())


# --------------------------------------
# load the analysis data and output
# --------------------------------------
setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Data/Temp/")
load("stool_figure_data.Rdata")
stool <- d %>% select(dataid, childNo, tr, date1, date2, date3, neo1,mpo1,aat1,neo2,mpo2,aat2,reg1b2,neo3,mpo3,aat3)
#add empty variables for reg1b at time 1 and 3 to allow reshape to work
stool$reg1b1 <- stool$reg1b3 <- NA

varying <- list(c("date1", "date2", "date3"),c("neo1","neo2","neo3"),c("mpo1","mpo2","mpo3"),c("aat1","aat2","aat3"),c("reg1b1","reg1b2","reg1b3"))
stool<-reshape(stool, idvar=c("dataid", "childNo", "tr"), varying =varying , times=c(1:3), sep = "", direction = 'long')
stool <- stool %>% filter(!is.na(neo1) | !is.na(mpo1) | !is.na(aat1) | !is.na(reg1b1))
colnames(stool)[5:9] <-c("date","neo","mpo","aat","reg1b")                     
stool<-melt(stool, id.vars=c("dataid", "childNo", "tr","time","date"))

load("urine_figure_data.Rdata")
urine <- d %>% select(dataid, childNo, tr, date1, date2, date3, Lact1,Mann1,LM1,Lact2,Mann2,LM2,Lact3,Mann3,LM3)
varying <- list(c("date1", "date2", "date3"),c("Lact1","Lact2","Lact3"),c("Mann1","Mann2","Mann3"),c("LM1","LM2","LM3"))
urine<-reshape(urine, idvar=c("dataid", "childNo", "tr"), varying =varying , times=c(1:3), sep = "", direction = 'long')
urine <- urine %>% filter(!is.na(Lact1) | !is.na(Mann1) | !is.na(LM1))
colnames(urine)[5:8] <-c("date","Lact","Mann","LM")                     
urine<-melt(urine, id.vars=c("dataid", "childNo", "tr","time","date"))

#combine urine and stool data
df<-rbind(stool, urine)

#Set calendar date variable
df$caldate <- as.Date(df$date,format="%d%b%Y")

#log transform outcome
df$logvalue <- log(df$value)


#Set colors:
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

p1<- ggplot(df, aes(x = caldate)) +
    #geom_point(aes(x=caldate, y=logvalue, group=tr, color=tr),  alpha = 0.1, size=0.5) +
    geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
      facet_wrap(~variable, scale="free_y") +
    scale_colour_manual( values=tableau10[1:4]) +
    labs(x = "Date",
         y = "ln-value",
         title = "Ln biomarker concentration over calendar time",
         color="Intervention Arm") +
    theme(#panel.border = element_blank(), 
          strip.background = element_blank() )#,
          #legend.position="none")

p2<- ggplot(df, aes(x = caldate)) +
    geom_point(aes(x=caldate, y=logvalue, group=tr, color=tr),  alpha = 0.1, size=0.5) +
    geom_smooth(aes(y=logvalue, group=tr, color=tr)) +
      facet_wrap(~variable, scale="free_y") +
    scale_colour_manual( values=tableau10[1:4]) +
    labs(x = "Date",
         y = "ln-value",
         title = "Ln biomarker concentration over calendar time",
         color="Intervention Arm") +
    theme(#panel.border = element_blank(), 
          strip.background = element_blank() )#,
          #legend.position="none")

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures")
ggsave(filename="EED-biomarker-seasonality-smooth.pdf",plot = p1,device='pdf',width=9,height=9)
ggsave(filename="EED-biomarker-seasonality-smoothpoints.pdf",plot = p2,device='pdf',width=9,height=9)
