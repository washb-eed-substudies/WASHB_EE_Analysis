
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


setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Results/Andrew/")
load("stool_res_N_M.Rdata")
load("stool_res_unadj_M.Rdata")
load("stool_res_adj_M.Rdata")
load("stool_res_means.Rdata")

load("urine_res_N_M.Rdata")
load("urine_res_unadj_M.Rdata")
load("urine_res_adj_M.Rdata")
load("urine_res_means.Rdata")



ls()

#-----------------------------------
# Adjusted results cleaning
#-----------------------------------

rownames(aat_t3_adj_M) <- rownames(aat_t2_adj_M) <- rownames(aat_t1_adj_M) <- rownames(aat_t1_unadj_M)
rownames(mpo_t3_adj_M) <- rownames(mpo_t2_adj_M) <- rownames(mpo_t1_adj_M) <- rownames(mpo_t1_unadj_M)
rownames(neo_t3_adj_M) <- rownames(neo_t2_adj_M) <- rownames(neo_t1_adj_M) <- rownames(neo_t1_unadj_M)
rownames(lac_t3_adj_M) <- rownames(lac_t2_adj_M) <- rownames(lac_t1_adj_M) <- rownames(lac_t1_unadj_M)
rownames(man_t3_adj_M) <- rownames(man_t2_adj_M) <- rownames(man_t1_adj_M) <- rownames(man_t1_unadj_M)
rownames(lm_t3_adj_M) <- rownames(lm_t2_adj_M) <- rownames(lm_t1_adj_M) <- rownames(lm_t1_unadj_M)

#-----------------------------------
# Adjusted difference processing
#-----------------------------------

#Compile N's
aat_N1<-cbind("AAT","T1", rownames(aat_t1_mn), as.data.frame(aat_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_N1<-cbind("NEO","T1", rownames(neo_t1_mn), as.data.frame(neo_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_N1<-cbind("MPO","T1", rownames(mpo_t1_mn), as.data.frame(mpo_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
lm_N1<-cbind("LM","T1", rownames(lm_t1_mn), as.data.frame(lm_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
l_N1<-cbind("Lact","T1", rownames(lac_t1_mn), as.data.frame(lac_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
m_N1<-cbind("Mann","T1", rownames(man_t1_mn), as.data.frame(man_t1_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 

aat_N2<-cbind("AAT","T2", rownames(aat_t2_mn), as.data.frame(aat_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_N2<-cbind("NEO","T2", rownames(neo_t2_mn), as.data.frame(neo_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_N2<-cbind("MPO","T2", rownames(mpo_t2_mn), as.data.frame(mpo_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
lm_N2<-cbind("LM","T2", rownames(lm_t2_mn), as.data.frame(lm_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
l_N2<-cbind("Lact","T2", rownames(lac_t2_mn), as.data.frame(lac_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
m_N2<-cbind("Mann","T2", rownames(man_t2_mn), as.data.frame(man_t2_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 

aat_N3<-cbind("AAT","T3", rownames(aat_t3_mn), as.data.frame(aat_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_N3<-cbind("NEO","T3", rownames(neo_t3_mn), as.data.frame(neo_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_N3<-cbind("MPO","T3", rownames(mpo_t3_mn), as.data.frame(mpo_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
lm_N3<-cbind("LM","T3", rownames(lm_t3_mn), as.data.frame(lm_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
l_N3<-cbind("Lact","T3", rownames(lac_t3_mn), as.data.frame(lac_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
m_N3<-cbind("Mann","T3", rownames(man_t3_mn), as.data.frame(man_t3_mn)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 






#Compile differences
aat_dif1<-cbind("AAT","T1", rownames(aat_t1_unadj_M), as.data.frame(aat_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_dif1<-cbind("NEO","T1", rownames(neo_t1_unadj_M), as.data.frame(neo_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_dif1<-cbind("MPO","T1", rownames(mpo_t1_unadj_M), as.data.frame(mpo_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
lm_dif1<-cbind("LM","T1", rownames(lm_t1_unadj_M), as.data.frame(lm_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
l_dif1<-cbind("Lact","T1", rownames(lac_t1_unadj_M), as.data.frame(lac_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
m_dif1<-cbind("Mann","T1", rownames(man_t1_unadj_M), as.data.frame(man_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 



aat_dif2<-cbind("AAT","T2", rownames(aat_t2_unadj_M), as.data.frame(aat_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_dif2<-cbind("NEO","T2", rownames(neo_t2_unadj_M), as.data.frame(neo_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_dif2<-cbind("MPO","T2", rownames(mpo_t2_unadj_M), as.data.frame(mpo_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
lm_dif2<-cbind("LM","T2", rownames(lm_t2_unadj_M), as.data.frame(lm_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
l_dif2<-cbind("Lact","T2", rownames(lac_t2_unadj_M), as.data.frame(lac_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
m_dif2<-cbind("Mann","T2", rownames(man_t2_unadj_M), as.data.frame(man_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 

aat_dif3<-cbind("AAT","T3", rownames(aat_t3_unadj_M), as.data.frame(aat_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_dif3<-cbind("NEO","T3", rownames(neo_t3_unadj_M), as.data.frame(neo_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_dif3<-cbind("MPO","T3", rownames(mpo_t3_unadj_M), as.data.frame(mpo_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
lm_dif3<-cbind("LM","T3", rownames(lm_t3_unadj_M), as.data.frame(lm_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
l_dif3<-cbind("Lact","T3", rownames(lac_t3_unadj_M), as.data.frame(lac_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
m_dif3<-cbind("Mann","T3", rownames(man_t3_unadj_M), as.data.frame(man_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 





#Construct dataframes of results
dif_df<-(rbind(
aat_dif1,
neo_dif1,
mpo_dif1,
l_dif1,
m_dif1,
lm_dif1,
aat_dif2,
neo_dif2,
mpo_dif2,
l_dif2,
m_dif2,
lm_dif2,
aat_dif3,
neo_dif3,
mpo_dif3,
l_dif3,
m_dif3,
lm_dif3
))
rownames(dif_df)<-NULL
#dif_df<-cbind(dif_df[,1:3],rep(9999, nrow(dif_df)),dif_df[,4:8])
colnames(dif_df)<-c("Location","round","TR", "Dif","lower.ci","upper.ci", "SD","Robust SE", "Pval")
levels(dif_df$TR)
levels(dif_df$TR)<-c("C v N", "C v N+WSH", "C v WSH", "N v N + WSH", "WSH v N+WSH")

dif_df$Location<-factor(dif_df$Location)
dif_df$round<-factor(dif_df$round)
dif_df$TR<-factor(dif_df$TR)
#dif_df$TR = factor(dif_df$TR,c("C","W","S","H","WSH","N","WSH+N"))

#Drop H2 comparisons
dif_df<-subset(dif_df, TR!="WSH v N+WSH" & TR!="N v N + WSH")

#Drop unneeded columns
dif_df<-dif_df[,c(1:6,9)]

#Round numeric columns
# dif_df[,4]<-as.numeric(sprintf("%1.2f",dif_df[,4]))
# dif_df[,5]<-as.numeric(sprintf("%1.2f",dif_df[,5]))
# dif_df[,6]<-as.numeric(sprintf("%1.2f",dif_df[,6]))
 dif_df[,7]<-as.numeric(sprintf("%1.3f",dif_df[,7]))

#Add comparison group for colors
dif_df$comp.TR<-rep(c("WSH","N","N+WSH"),19)


#Formated comparison for x-axis printing
dif_df$TR.format<-paste0("C vs.\n",dif_df$comp.TR)




N_df<-(rbind(
aat_N1,
neo_N1,
mpo_N1,
l_N1,
m_N1,
lm_N1,
aat_N2,
neo_N2,
mpo_N2,
l_N2,
m_N2,
lm_N2,
aat_N3,
neo_N3,
mpo_N3,
l_N3,
m_N3,
lm_N3
))
rownames(N_df)<-NULL
colnames(N_df)<-c("Location","round","TR", "N" ,"Dif","SD","Robust SE","lower.ci","upper.ci")
levels(N_df$TR)
levels(N_df$TR)<-c("C", "N", "N+WSH", "WSH")

N_df$Location<-factor(N_df$Location)
N_df$round<-factor(N_df$round)
N_df$comp.TR<-N_df$TR<-factor(N_df$TR)

#Drop unneeded columns
N_df<-N_df[,c(1:5,8,9)]

#Round the numeric columns that are printed
# N_df[,4]<-as.numeric(sprintf("%1.2f",N_df[,4]))
# N_df[,5]<-as.numeric(sprintf("%1.2f",N_df[,5]))
# N_df[,6]<-as.numeric(sprintf("%1.2f",N_df[,6]))
# N_df[,7]<-as.numeric(sprintf("%1.2f",N_df[,7]))

#Add formatted treatment for x-acis label
#N_df$TR.format<-N_df$TR  
N_df$TR.format<-ifelse(N_df$TR=="N+WSH",
  paste0("N+\nWSH"),
    paste0(N_df$TR,"\n"))
N_df$TR.format<-ifelse(N_df$TR=="WSH",
  paste0("\nWSH"),
    N_df$TR.format)



#Figure out Y-axis range for different sample types
AAT<-N_df[N_df$Location=="AAT",]
NEO<-N_df[N_df$Location=="NEO",]
MPO<-N_df[N_df$Location=="MPO",]
Lact<-N_df[N_df$Location=="Lact",]
Mann<-N_df[N_df$Location=="Mann",]
LM<-N_df[N_df$Location=="LM",]

seq(min(AAT$lower.ci),max(AAT$upper.ci),by=diff(range(min(AAT$lower.ci),max(AAT$upper.ci)))/10)
c(min(AAT$lower.ci),max(AAT$upper.ci))
c(min(NEO$lower.ci),max(NEO$upper.ci))
c(min(MPO$lower.ci),max(MPO$upper.ci))
c(min(Lact$lower.ci),max(Lact$upper.ci))
c(min(Mann$lower.ci),max(Mann$upper.ci))
c(min(LM$lower.ci),max(LM$upper.ci))





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

#tr.cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange,N=cred,"WSH+N"=cmagent)
tr.cols=c(C=cblack,WSH=cblue,N=cred,"WSH+N"=cgreen)

#cols=c("C v WSH"=corange,"C v N"=cred,"C v N+WSH"=cmagent)
cols=c("C v WSH"=cblue,"C v N"=cred,"C v N+WSH"=cgreen)


arms=c("C", "W", "S", "H", "WSH")


# general label plot
ulabplot <- function(title) {
  plot(1,1,type="n",
       xaxt="n",xlab="",xlim=c(0,1),
       yaxt="n",ylab="",bty="n",ylim=c(0,1)
  )
  text(1,0.5,title,adj=1,cex=1.5)
}



long.plot<-function(d, yrange=NULL,  cols=cols, tickspace=0.2){

  op <- par(mar=c(3,3,2,0)+0.1)
  
if(is.null(yrange)){
    if((d$Location=="AAT")[1]){yrange<-c(-2,0)
    ytics <- seq(-2,0,by=0.2)
    title<-"Ln alpha 1-antitrypsin (mg/g)"}
  if((d$Location=="NEO")[1]){yrange<-c(5,9)
    ytics <- seq(5,9,by=.4)
    title<-"Ln neopterin (nmol/L)"}
  if((d$Location=="MPO")[1]){yrange<-c(5,9)
    ytics <- seq(5,9,by=.4)
    title<-"Ln myeloperoxidase (ng/ml)"}
  if((d$Location=="Lact")[1]){yrange<-c(-3,-1)
    ytics <- seq(-3,-1,by=0.2)
    title<-"Ln lactulose (mmol/L)"}
  if((d$Location=="Mann")[1]){yrange<-c(-0.6,1.4)
    ytics <- round(seq(-0.6,1.4,by=.2),1)
    title<-"Ln mannitol (mmol/L)"}
  if((d$Location=="LM")[1]){yrange<-c(-3.8,-2.8)
    ytics <- seq(-4,-2,by=0.1)
    title<-"Ln L:M ratio"}
  if((d$Location=="REG")[1]){yrange<-c(4.2,5.2)
    ytics <- seq(4.8,5.2,by=.05)
    title<-""}
}else{
        seq(yrange[1],yrange[2],by=tickspace) #<----------Set the Y-axis range here
}

  n<-3
   # set up an empty plot
MidPts <- barplot(1:n, names.arg=NA,col=NA,
                  border=NA,
	  	ylim= if(yrange[1]=="auto"){c(min(d$lower.ci),max(d$upper.ci))}else{c(range(yrange)[1],range(yrange)[2])},
	  	ylab="",yaxt="n",
      las=1,bty="n"
	)
	segments(x0=0,x1=max(MidPts+0.5),y0=ytics,lty=2,lwd=1,col="gray80")
	#segments(x0=0,x1=max(MidPts+0.5),y0=0,lty=1,lwd=1,col="black")
	axis(2,at=ytics,las=1)
	mtext(title,side=3,line=0.25,col="gray20",cex=1)

	
	plotpoints<-rep(MidPts, each=4) + rep(c(-0.15,-0.05, 0.05, 0.15), 3)

	# plot estimates
	arrows(x0=plotpoints, y0=d$lower.ci, y1=d$upper.ci, col=tr.cols,lwd=2,length=0.05,angle=90,code=3)
	  segments(x0=plotpoints[1:4],x1=plotpoints[5:8],y0=d$Dif[1:4],y1=d$Dif[5:8],col=alpha(tr.cols,.1), lwd=2)
  segments(x0=plotpoints[5:8],x1=plotpoints[9:12],y0=d$Dif[5:8],y1=d$Dif[9:12],col=alpha(tr.cols,.1), lwd=2)
	points(plotpoints,d$Dif,pch=21,cex=1.5,lwd=1,col=tr.cols,bg="white")
	points(plotpoints,d$Dif,pch=21,cex=1.5,lwd=0,col=tr.cols,bg=alpha(tr.cols,alpha=0.5))
	#text(x=MidPts,y=d$upper.ci, labels=d$Pval,pos=3,cex=1,col=cols,font=1)

	  # X-axis labels
  mtext(c("6 month\nmean",
          "17 month\nmean",
          "22 month\nmean"),
        side=1,line=2,at=MidPts,col="black",cex=0.8,las=1)
  box()
}

#Legend plot
legendplot<-function(legend=T, cex=1, x=0.1, y=0.8){
  
  if(legend==F){
      op <- par(mar=c(3,1,2,0)+0.1)
    	ulabplot("")
  }else{
        op <- par(mar=c(3,1,2,0)+0.1)
    	ulabplot("")
    legend(x=x, y=y, c("Control","Water + Sanitation +\nHandwashing","Nutrition","Nutrition + Water +\nSanitation + Handwashing"), col = tr.cols, pt.bg=alpha(tr.cols,alpha=0.5),
       text.col = tr.cols, lty = c(1, 1, 1), pch = c(21, 21, 21),
       merge = TRUE, bg = "white", cex=cex,  box.col="white",
       xjust=0.5,
       yjust=0.5,
       y.intersp=1.5)
  }
}



setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBK-EE-analysis/Figures")
pdf("EE-mean-longitudinal-plots.pdf",width=10,height=8.5, paper="USr")
lo <- layout(mat=matrix(1:9,ncol=3,nrow=3,byrow=T),widths=c(1,1,0.6))
op <- par(mar=c(4,0,3,0)+0.1)

long.plot(d<-N_df[N_df$Location=="AAT",])
long.plot(d<-N_df[N_df$Location=="Lact",])
legendplot(F,1)
long.plot(d<-N_df[N_df$Location=="NEO",])
long.plot(d<-N_df[N_df$Location=="Mann",])
legendplot(x=0.5, y=.5, T, 1.3)
long.plot(d<-N_df[N_df$Location=="MPO",])
long.plot(d<-N_df[N_df$Location=="LM",])
legendplot(F,1)

 
dev.off()

