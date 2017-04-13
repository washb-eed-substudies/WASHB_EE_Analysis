
rm(list=ls())
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(grid)
library(gridExtra)
library(scales)
library(lattice)

#Useful links
#Facet options:
     #http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
#Coloring X-axis text
     #https://stackoverflow.com/questions/22972478/color-axis-text-by-variable-in-ggplot
#Arranging ggplots:
     #https://github.com/baptiste/gridextra/wiki/arranging-ggplot
#Arranging grobs
     #https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
#Scaling in grid.arrange
     #https://stackoverflow.com/questions/16298599/keep-or-set-the-ratio-between-text-labels-and-size-of-plot-in-grid-arrange


#---------------------------------------
# Load data
#---------------------------------------


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/")
load("stool_res_N_M.Rdata")
load("stool_res_unadj_M.Rdata")
load("urine_res_N_M.Rdata")
load("urine_res_unadj_M.Rdata")

ls()

#-----------------------------------
# Unadjusted difference processing
#-----------------------------------

aat_dif1<-cbind("AAT","T1", rownames(aat_t1_unadj_M), as.data.frame(aat_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_dif1<-cbind("NEO","T1", rownames(neo_t1_unadj_M), as.data.frame(neo_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_dif1<-cbind("MPO","T1", rownames(mpo_t1_unadj_M), as.data.frame(mpo_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
ml_dif1<-cbind("LM","T1", rownames(lm_t1_unadj_M), as.data.frame(lm_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 



#Construct dataframe of prevalence results
dif_wb<-(rbind(
ec_tw_dif,
ec_sw_dif,
ec_p_dif,
ec_h_dif,
ec_f_dif,
ec_s_dif,
ec_y_dif))
rownames(dif_wb)<-NULL
dif_wb<-cbind(dif_wb[,1:2],rep(9999, nrow(dif_wb)),dif_wb[,3:8])
colnames(dif_wb)<-c("Location","TR", "N", "Dif","lower.ci","upper.ci", "SD","Robust SE")

levels(dif_wb$TR)<-c("S","WSH")
dif_wb[,3:ncol(dif_wb)]<-round(dif_wb[,3:ncol(dif_wb)],2)

dif_wb$TR.N<-paste0(dif_wb$TR, "\n(N=\n",dif_wb$N,")")
dif_wb$round<-"World Bank"
#dif_wb$dif.perc<-paste0(dif_wb$difalence*100, "%")
dif_wb$N<-paste0("(N=\n",dif_wb$N,")")

dif_wb$TR<-factor(dif_wb$TR)
dif_wb$TR = factor(dif_wb$TR,c("C","W","S","H","WSH","N","WSH+N"))

#Add in N's from mn objects
#dif_wb.N<-mn_wb[mn_wb$TR!="C",3]
#dif_wb$TR.N<-paste0(dif_wb$TR, "\n", dif_wb.N)

#Remove N's from axis labels
dif_wb$TR.N<-dif_wb$TR



#-----------------------------------
# Midline log difference processing
#-----------------------------------

ec_tw_dif_mid_a<-cbind("Tubewell", rownames(ec_tw_dif_h1_unadj_mid_a), as.data.frame(ec_tw_dif_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_dif_mid_a<-cbind("Stored water", rownames(ec_sw_dif_h1_unadj_mid_a), as.data.frame( ec_sw_dif_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_dif_mid_a<-cbind("Hands", rownames(ec_h_dif_h1_unadj_mid_a), as.data.frame( ec_h_dif_h1_unadj_mid_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_dif_mid_a<-cbind("Toys", rownames(t(ec_t_dif_h1_unadj_mid_a)), as.data.frame( t(ec_t_dif_h1_unadj_mid_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of difalence results
dif_mid<-(rbind(
ec_tw_dif_mid_a,
ec_sw_dif_mid_a,
ec_h_dif_mid_a,
ec_t_dif_mid_a))
rownames(dif_mid)<-NULL
dif_mid<-cbind(dif_mid[,1:2],rep(9999, nrow(dif_mid)),dif_mid[,3:8])
colnames(dif_mid)<-c("Location","TR", "N", "Dif","lower.ci","upper.ci", "SD","Robust SE")
levels(dif_mid$TR)
levels(dif_mid$TR)<-c("W","WSH","H")
dif_mid[,3:ncol(dif_mid)]<-round(dif_mid[,3:ncol(dif_mid)],2)

dif_mid$TR.N<-paste0(dif_mid$TR, "\n(N=\n",dif_mid$N,")")
dif_mid$round<-"Year 1"
#dif_mid$dif.perc<-paste0(dif_mid$difalence*100, "%")
dif_mid$N<-paste0("(N=\n",dif_mid$N,")")

dif_mid$TR<-factor(dif_mid$TR)
dif_mid$TR = factor(dif_mid$TR,c("C","W","S","H","WSH","N","WSH+N"))

#Add in N's from mn objects
#dif_mid.N<-mn_mid[mn_mid$TR!="C",3]
#dif_mid$TR.N<-paste0(dif_mid$TR, "\n", dif_mid.N)

#Remove N's from axis labels
dif_mid$TR.N<-dif_mid$TR

#-----------------------------------
# Endline log difference processing
#-----------------------------------


ec_tw_dif_end_a<-cbind("Tubewell", rownames(ec_tw_dif_h1_unadj_end_a), as.data.frame(ec_tw_dif_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_sw_dif_end_a<-cbind("Stored water", rownames(ec_sw_dif_h1_unadj_end_a), as.data.frame( ec_sw_dif_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_h_dif_end_a<-cbind("Hands", rownames(ec_h_dif_h1_unadj_end_a), as.data.frame( ec_h_dif_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8")) 
ec_t_dif_end_a<-cbind("Toys", rownames(t(ec_t_dif_h1_unadj_end_a)), as.data.frame( t(ec_t_dif_h1_unadj_end_a))) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 
ec_f_dif_end_a<-cbind("Food", rownames(ec_f_dif_h1_unadj_end_a), as.data.frame( ec_f_dif_h1_unadj_end_a)) %>% `rownames<-`(NULL) %>%  setNames(., c("1","2","3","4","5","6","7","8")) 

#Construct dataframe of difalence results
dif_end<-(rbind(
ec_tw_dif_end_a,
ec_sw_dif_end_a,
ec_h_dif_end_a,
ec_t_dif_end_a,
ec_f_dif_end_a))
rownames(dif_end)<-NULL
dif_end<-cbind(dif_end[,1:2],rep(9999, nrow(dif_end)),dif_end[,3:8])
colnames(dif_end)<-c("Location","TR", "N", "Dif","lower.ci","upper.ci", "SD","Robust SE")

levels(dif_end$TR)
levels(dif_end$TR)<-c("W","WSH","H")
dif_end[,3:ncol(dif_end)]<-round(dif_end[,3:ncol(dif_end)],2)

dif_end$TR.N<-paste0(dif_end$TR, "\n(N=\n",dif_end$N,")")
dif_end$round<-"Year 2"
#dif_end$dif.perc<-paste0(dif_end$difalence*100, "%")
dif_end$N<-paste0("(N=\n",dif_end$N,")")

dif_end$TR<-factor(dif_end$TR)
dif_end$TR = factor(dif_end$TR,c("C","W","S","H","WSH","N","WSH+N"))

#Add in N's from mn objects
#dif_end.N<-mn_end[mn_end$TR!="C",3]
#dif_end$TR.N<-paste0(dif_end$TR, "\n", dif_end.N)

#Remove N's from axis labels
dif_end$TR.N<-dif_end$TR


dif.dat<-rbind(dif_wb,dif_mid, dif_end)




#-------------------------------------------
# Customize plot layout
#-------------------------------------------

# main study colors
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cols <- c("gray30",cbPalette[c(2:4,6:8)])
# brighter color blind palette:  https://personal.sron.nl/~pault/ 
cblack <- "#000004FF"
cblue <- "#3366AA"
cteal <- "#11AA99"
cgreen <- "#66AA55"
cchartr <- "#CCCC55"
cmagent <- "#992288"
cred <- "#EE3333"
corange <- "#EEA722"
cyellow <- "#FFEE33"
cgrey <- "#777777"
cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange,N=cred,"WSH+N"=cmagent)

cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange)


arms=c("C", "W", "S", "H", "WSH")

# general label plot
ulabplot <- function(title) {
  plot(1,1,type="n",
       xaxt="n",xlab="",xlim=c(0,1),
       yaxt="n",ylab="",bty="n",ylim=c(0,1)
  )
  text(1,0.5,title,adj=1,cex=1.5)
}


#Order prevalence data
unique(prev.dat$Location)
unique(prev.dat$TR)
unique(prev.dat$round)
prev.dat$Location<-factor(prev.dat$Location)
table(prev.dat$Location)
prev.dat$Location<-factor(prev.dat$Location, c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies"))
table(prev.dat$Location)
prev.dat$TR

#for(i in 1:24){
prevplot<-function(d, i){
  
  if(nrow(d)==0){
       op <- par(mar=c(3,1,2,0)+0.1)
    	ulabplot("")
    	
         mtext(ifelse(i<10,
                      c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)

  }else{
  
  ytics <- seq(0,100,by=10)  #<----------Set the Y-axis range here


if(i==1 | i==10 | i==19){
   op <- par(mar=c(3,0,2,1)+0.1)

	if(i==1){ulabplot("E.coli \nprevalence \nat early \nassessment \n(%)")}
	if(i==10){ulabplot("E.coli \nprevalence \nat one-year \nassessment \n(%)")}
	if(i==19){ulabplot("E.coli \nprevalence \nat two-year \nassessment \n(%)")}

   	#,side=2,line=3,las=1)

	}else{
   #op <- par(mar=c(4,1,3,0.5)+0.1)
   op <- par(mar=c(3,1,2,0)+0.1)

   # set up an empty plot
MidPts <- barplot(1:5,names.arg=NA,border=NA,col=NA,
	ylim=c(range(ytics)[1],range(ytics)[2]+5),ylab="",yaxt="n",
	las=1,bty="n"
	)
	segments(x0=0,x1=max(MidPts+0.5),y0=ytics,lty=2,lwd=1,col="gray80")
	if(i==2 | i==11 | i==20){
	    axis(2,at=ytics,las=1)
	}
	#Vector of arms to plot:
	which.arms<-arms %in% d$TR
	
	# plot estimates
	arrows(x0=MidPts[which.arms], y0=d$lower.ci, y1=d$upper.ci, col=cols[which.arms],lwd=2,length=0.05,angle=90,code=3)
	points(MidPts[which.arms],d$Prevalence,pch=21,cex=1.5,lwd=1,col=cols[which.arms],bg="white")
	points(MidPts[which.arms],d$Prevalence,pch=21,cex=1.5,lwd=0,col=cols[which.arms],bg=alpha(cols[which.arms],alpha=0.5))
	#text(x=MidPts[which.arms]+0.05,y=d$TR.N,pos=4,cex=1,col=cols[which.arms],font=1)
	  # X-axis labels
  mtext(d$TR.N,side=1,line=2,at=MidPts[which.arms],col=cols[which.arms],cex=0.6,las=1)
  #mtext(d$Location,side=3,line=0.25,col="gray20",cex=1)
           mtext(ifelse(i<10,
                      c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)
  }
}

	# print header and footer labels
	# mtext(glab,at=MidPts,side=3,line=6,col=cols,font=1  )
	# hx <- MidPts[1]-0.5
	# prform <- function(pr,lb,ub) {
	# 	paste(sprintf("%1.2f",pr)," (",sprintf("%1.2f",lb),", ",sprintf("%1.2f",ub),")",sep="")
	# }
}


  
setwd("C:/Users/andre/Dropbox/WASHB EML/Results/Figures")
pdf("Env Prevalence Plot_draft.pdf",width=10,height=8.5, paper="USr")
    #op <- par(mar=c(1,9,9,0)+0.1,xpd=TRUE)
lo <- layout(mat=matrix(1:27,ncol=9,nrow=3,byrow=T),widths=c(1,1,1,1,1,1,1,1,1))
op <- par(mar=c(4,1,3,0.5)+0.1)

    i<-c(1,1,1)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(1,1,2)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(2,1,3)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(3,1,4)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(4,1,5)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(5,1,6)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(6,1,7)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(7,1,8)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
 
    i<-c(8,1,9)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(1,2,10)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(1,2,11)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(2,2,12)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(3,2,13)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(4,2,14)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(5,2,15)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(6,2,16)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(7,2,17)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(8,2,18)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(1,3,19)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(1,3,20)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(2,3,21)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(3,3,22)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(4,3,23)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(5,3,24)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(6,3,25)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(7,3,26)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])
    i<-c(8,3,27)
    prevplot(d=prev.dat[prev.dat$Location==levels(prev.dat$Location)[i[1]] & prev.dat$round==unique(prev.dat$round)[i[2]],], i[3])

dev.off()



