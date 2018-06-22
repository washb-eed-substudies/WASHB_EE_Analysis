#------------------------------
# WASHB-summary-heatmaps.R
#
# create heat maps results 
# to summarize the WASH B
# trials

# version with continuous fill 

# final versions made in adobe
# illustrator
#------------------------------

#------------------------------
# preamble
#------------------------------
rm(list=ls())
library(tidyverse)
library(googlesheets)
library(RColorBrewer)
library(scales)
library(gridExtra)


# custom labels to use for results
# careful, these are re-ordered from the original -2,...3
# since ggplot2 reverses the
reslabs <- c("benefit (p<0.05)","benefit (p<0.10)","null","harm (p<0.10)","harm (p<0.05)","too rare to study","not measured")

# custom grouping order for results type
typelabs <- c("EED at 6m","EED at 17m","EED at 22m")

# custom labels for arms
armlevs <- c("W","S","H","WSH","N","WSH+N")
armlabs <- c("Water","Sanitation","Handwashing","WSH","Nutrition","WSH+Nutrition")


#------------------------------
# Load the data
#------------------------------

d <- read.csv("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Data/Cleaned/WASHBenefits-EED-results-summary.csv")
head(d)

#Subset to kenya and measured arms
d <- d %>% filter(country=="Kenya" & rescat!="not measured")
d <- droplevels(d)

#------------------------------
# bind and then format the 
# results data for plotting
#------------------------------
d <- d %>%
  mutate(country = factor(country,levels=c("Kenya")),
         type = factor(type,levels=typelabs),
         outcome = factor(outcome),
         group = factor(group,levels=rev(armlevs),labels=rev(armlabs)),
         rescat = factor(rescat,levels=reslabs)
  )

d=as.data.frame(d)
d$type=as.character(d$type)


d$type=as.factor(d$type)
d=subset(d,!is.na(d$type))

#------------------------------
# retrieve numerical results
#------------------------------
d$stop = regexpr("\\(", d$result) 
d$ptest=substr(d$result,4,d$stop-2)

d$ptest[d$type=="EED at 6m"|d$type=="EED at 17m"|d$type=="EED at 22m"]=
  substr(d$result[d$type=="EED at 6m"|d$type=="EED at 17m"|d$type=="EED at 22m"],1,
         d$stop[d$type=="EED at 6m"|d$type=="EED at 17m"|d$type=="EED at 22m"]-2)

d$ptest=as.numeric(d$ptest)

d$ptest.s=d$ptest
d$ptest.s[d$rescat=="null"]=NA
d$ptest.s[d$rescat=="too rare to study"]=NA

d$rescat2=as.character(d$rescat)
d$rescat2[d$rescat %in% c("benefit (p<0.05)","benefit (p<0.10)",
                          "harm (p<0.05)","harm (p<0.10)")]="significant"
d$rescat2=as.factor(d$rescat2)

d$signif=as.factor(ifelse(as.character(d$rescat2)=="significant","significant","not significant"))

d=d[!is.na(d$rescat),]

#------------------------------
# create EE data frame for plotting
#------------------------------
ee=d[d$type=="EED at 6m"|d$type=="EED at 17m"|d$type=="EED at 22m",]


ee$outcome=droplevels(ee$outcome)

ee$outcome=as.character(ee$outcome)
ee$outcome[ee$outcome=="myeloperoxidase"]="MPO"
ee$outcome[ee$outcome=="alpha-1-anti."]="AAT"
ee$outcome[ee$outcome=="Neopterin"]="NEO"
ee$outcome[ee$outcome=="L:M ratio"]="L:M"

ee$outcome=factor(ee$outcome,levels=c(
    "NEO",  "MPO","AAT",
  "Lactulose",  "Mannitol",  "L:M","REG1"))

ee$type=droplevels(pr$type)
# pr$type=factor(pr$type,levels=c("Nutr.","Enterics","STH"))

ee$time[ee$type=="EED at 6m"]="6m"
ee$time[ee$type=="EED at 17m"]="17m"
ee$time[ee$type=="EED at 22m"]="22m"

ee$time=factor(ee$time,levels=c("6m","17m","22m"))

ee$outtime[ee$outcome=="AAT" & ee$time=="6m"]="AAT at 6m"
ee$outtime[ee$outcome=="AAT" & ee$time=="17m"]="AAT at 17m"
ee$outtime[ee$outcome=="AAT" & ee$time=="22m"]="AAT at 22m"

ee$outtime[ee$outcome=="NEO" & ee$time=="6m"]="NEO at 6m"
ee$outtime[ee$outcome=="NEO" & ee$time=="17m"]="NEO at 17m"
ee$outtime[ee$outcome=="NEO" & ee$time=="22m"]="NEO at 22m"

ee$outtime[ee$outcome=="L:M" & ee$time=="6m"]="L:M at 6m"
ee$outtime[ee$outcome=="L:M" & ee$time=="17m"]="L:M at 17m"
ee$outtime[ee$outcome=="L:M" & ee$time=="22m"]="L:M at 22m"

ee$outtime[ee$outcome=="Lactulose" & ee$time=="6m"]="Lactulose at 6m"
ee$outtime[ee$outcome=="Lactulose" & ee$time=="17m"]="Lactulose at 17m"
ee$outtime[ee$outcome=="Lactulose" & ee$time=="22m"]="Lactulose at 22m"

ee$outtime[ee$outcome=="Mannitol" & ee$time=="6m"]="Mannitol at 6m"
ee$outtime[ee$outcome=="Mannitol" & ee$time=="17m"]="Mannitol at 17m"
ee$outtime[ee$outcome=="Mannitol" & ee$time=="22m"]="Mannitol at 22m"

ee$outtime[ee$outcome=="MPO" & ee$time=="6m"]="MPO at 6m"
ee$outtime[ee$outcome=="MPO" & ee$time=="17m"]="MPO at 17m"
ee$outtime[ee$outcome=="MPO" & ee$time=="22m"]="MPO at 22m"

ee$outtime=factor(ee$outtime,levels=c(
  "NEO at 6m","NEO at 17m","NEO at 22m",
  "MPO at 6m","MPO at 17m","MPO at 22m",
  "AAT at 6m","AAT at 17m","AAT at 22m",
  "Lactulose at 6m","Lactulose at 17m","Lactulose at 22m",
  "Mannitol at 6m","Mannitol at 17m","Mannitol at 22m",
  "L:M at 6m","L:M at 17m","L:M at 22m"))

ee$eegroup[ee$outcome=="MPO"]="MPO"
ee$eegroup[ee$outcome=="AAT"]="AAT"
ee$eegroup[ee$outcome=="NEO"]="NEO"
ee$eegroup[ee$outcome=="Mannitol"]="LM"
ee$eegroup[ee$outcome=="Lactulose"]="LM"
ee$eegroup[ee$outcome=="L:M"]="L:M ratio"


#------------------------------
# EE diff plots
#------------------------------
# myeloperoxidase 
myelo.plot= ggplot(data=ee[ee$eegroup=="MPO",],aes(x=outtime,y=group,fill=ptest.s)) +
  #facet over village
  facet_grid(~outcome,scales='free',space='free')+
  
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Diff. in\nlog ng/ml",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.3,0.3),na.value = "#ECE8ED")+
  # include text with results
  geom_text(aes(y=group,x=outtime,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom",axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),strip.text.y=element_blank(),
        legend.text=element_text(size=7),legend.title=element_text(size=7),
        plot.margin=unit(c(0.5,-0.1,0.5,-0.1), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(filename="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Figures/heatmap-MPO.png",
       plot = myelo.plot,device='png',width=2,height=6)

myelo.plot.nox= ggplot(data=ee[ee$eegroup=="MPO",],aes(x=outtime,y=group,fill=ptest.s)) +
  #facet over village
  facet_grid(country~outcome,scales='free',space='free')+
  
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Diff. in\nlog ng/ml",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.3,0.3),na.value = "#ECE8ED")+
  # include text with results
  # geom_text(aes(y=group,x=outtime,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom",axis.text.y=element_blank(),
        strip.text = element_text(face="bold", size=15),
        axis.ticks.y=element_blank(),strip.text.y=element_blank(),
        legend.text=element_text(size=9),legend.title=element_text(size=9),
        plot.margin=unit(c(0.5,-0.1,0.5,-0.5), "cm"),
        axis.text.x = element_blank())

# neopterin 
neo.plot= ggplot(data=ee[ee$eegroup=="NEO",],aes(x=outtime,y=group,fill=ptest.s)) +
  #facet over village
  facet_grid(country~outcome,scales='free',space='free')+
  
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Diff. in\nlog ng/ml",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.25,0.25),na.value = "#ECE8ED")+
  # include text with results
  geom_text(aes(y=group,x=outtime,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom",axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),strip.text.y=element_blank(),
        legend.text=element_text(size=7),legend.title=element_text(size=7),
        plot.margin=unit(c(0.5,-0.1,0.5,-0.1), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Figures/heatmap-NEO.png",
       plot = neo.plot,device='png',width=2,height=6)

# neopterin 
neo.plot.nox= ggplot(data=ee[ee$eegroup=="NEO",],aes(x=outtime,y=group,fill=ptest.s)) +
  #facet over village
  facet_grid(country~outcome,scales='free',space='free')+
  
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Diff. in\nlog ng/ml",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.25,0.25),na.value = "#ECE8ED")+
  # include text with results
  # geom_text(aes(y=group,x=outtime,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom",axis.text.y=element_blank(),
        strip.text = element_text(face="bold", size=15),
        axis.ticks.y=element_blank(),strip.text.y=element_blank(),
        legend.text=element_text(size=9),legend.title=element_text(size=9),
        plot.margin=unit(c(0.5,-0.1,0.5,-0.5), "cm"),
        axis.text.x = element_blank())

#alpha-1-anti.
a1a.plot= ggplot(data=ee[ee$eegroup=="AAT",],aes(x=outtime,y=group,fill=ptest.s)) +
  #facet over village
  facet_grid(country~outcome,scales='free',space='free')+
  
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Diff. in\nlog mg/g",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.2,0.2),na.value = "#ECE8ED")+
  # include text with results
  geom_text(aes(y=group,x=outtime,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom",axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),strip.text.y=element_blank(),
        legend.text=element_text(size=7),legend.title=element_text(size=7),
        plot.margin=unit(c(0.5,-0.1,0.5,-0.1), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Figures/heatmap-AAT.png",
       plot = a1a.plot,device='png',width=2,height=6)


#alpha-1-anti.
a1a.plot.nox= ggplot(data=ee[ee$eegroup=="AAT",],aes(x=outtime,y=group,fill=ptest.s)) +
  #facet over village
  facet_grid(country~outcome,scales='free',space='free')+
  
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Diff. in\nlog mg/g",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.2,0.2),na.value = "#ECE8ED")+
  # include text with results
  # geom_text(aes(y=group,x=outtime,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom",axis.text.y=element_blank(),
        strip.text = element_text(face="bold", size=15),
        axis.ticks.y=element_blank(),strip.text.y=element_blank(),
        legend.text=element_text(size=9),legend.title=element_text(size=9),
        plot.margin=unit(c(0.5,-0.1,0.5,-0.5), "cm"),
        axis.text.x = element_blank())

#LM
lm.plot= ggplot(data=ee[ee$eegroup=="LM",],aes(x=outtime,y=group,fill=ptest.s)) +
  #facet over village
  facet_grid(country~outcome,scales='free',space='free')+
  
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Diff. in\nlog mmol/L",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.7,0.7),na.value = "#ECE8ED")+
  # include text with results
  geom_text(aes(y=group,x=outtime,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom",axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),strip.text.y=element_blank(),
        legend.text=element_text(size=7),legend.title=element_text(size=7),
        plot.margin=unit(c(0.5,-0.1,0.5,0.2), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Figures/heatmap-LM.png",
       plot = lm.plot,device='png',width=4.2,height=6)

lm.plot.nox= ggplot(data=ee[ee$eegroup=="LM",],aes(x=outtime,y=group,fill=ptest.s)) +
  #facet over village
  facet_grid(country~outcome,scales='free',space='free')+
  
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Diff. in\nlog mmol/L",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.7,0.7),na.value = "#ECE8ED")+
  # include text with results
  # geom_text(aes(y=group,x=outtime,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom",axis.text.y=element_blank(),
        strip.text = element_text(face="bold", size=15),
        axis.ticks.y=element_blank(),strip.text.y=element_blank(),
        legend.text=element_text(size=9),legend.title=element_text(size=9),
        plot.margin=unit(c(0.5,-0.1,0.5,-0.5), "cm"),
        axis.text.x = element_blank())+
        theme(panel.spacing.x=unit(0.1, "lines"))

# LM ratio and reg 1B plot
lmratio.plot= ggplot(data=ee[ee$eegroup=="L:M ratio",],aes(x=outtime,y=group,fill=ptest.s)) +
  #facet over village
  facet_grid(country~outcome,scales='free',space='free')+
  
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Diff. in\nlog nnmol/L",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.3,0.3),na.value = "#ECE8ED")+
  # include text with results
  geom_text(aes(y=group,x=outtime,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom",axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),strip.text.y=element_blank(),
        legend.text=element_text(size=7),legend.title=element_text(size=7),
        plot.margin=unit(c(0.5,-0.1,0.5,-0.1), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Figures/heatmap-LMr.png",
       plot = lmratio.plot,device='png',width=2.75,height=6)

lmratio.plot.nox= ggplot(data=ee[ee$eegroup=="L:M ratio",],aes(x=outtime,y=group,fill=ptest.s)) +
  #facet over village
  facet_grid(country~outcome,scales='free',space='free')+
  
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Diff. in\nlog nnmol/L",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.3,0.3),na.value = "#ECE8ED")+
  # include text with results
  # geom_text(aes(y=group,x=outtime,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom",axis.text.y=element_blank(),
        strip.text.y=element_text(face="bold"),
        strip.text = element_text(face="bold", size=15),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=9),legend.title=element_text(size=9),
        plot.margin=unit(c(0.5,0.5,0.5,-0.5), "cm"),
        axis.text.x = element_blank(),
          panel.spacing.x=unit(0.1, "lines"))






# All outcomes
EED.plot <- ggplot(data=ee,aes(x=time,y=group,fill=ptest.s)) +
  #facet over village
  facet_wrap(~outcome)+
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove y axis labels, 
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  # set continuous legend color palette for results p<0.1
  scale_fill_gradientn("Log difference",
                       colors=rev(brewer.pal(11,"RdYlGn")),
                       limits=c(-0.5,0.5),na.value = "#ECE8ED")+
  # include text with results
  #geom_text(aes(y=group,x=time,label=ptest.s),size=3)+
  # place legend at bottom and arm labels and axis ticks
  theme(legend.position="bottom", #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),strip.text.y=element_blank(),
        legend.text=element_text(size=7),legend.title=element_text(size=7),
        plot.margin=unit(c(0.5,-0.1,0.5,-0.1), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Figures/heatmap-all-outcomes.png",
       plot = EED.plot, device='png',width=8,height=7)

