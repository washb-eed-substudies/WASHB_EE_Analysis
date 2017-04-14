
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

#Compile N's
aat_N1<-cbind("AAT","T1", rownames(aat_t1_N_M), as.data.frame(aat_t1_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_N1<-cbind("NEO","T1", rownames(neo_t1_N_M), as.data.frame(neo_t1_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_N1<-cbind("MPO","T1", rownames(mpo_t1_N_M), as.data.frame(mpo_t1_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
ml_N1<-cbind("LM","T1", rownames(lm_t1_N_M), as.data.frame(lm_t1_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 

aat_N2<-cbind("AAT","T2", rownames(aat_t2_N_M), as.data.frame(aat_t2_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_N2<-cbind("NEO","T2", rownames(neo_t2_N_M), as.data.frame(neo_t2_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_N2<-cbind("MPO","T2", rownames(mpo_t2_N_M), as.data.frame(mpo_t2_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
ml_N2<-cbind("LM","T2", rownames(lm_t2_N_M), as.data.frame(lm_t2_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
reg_N2<-cbind("REG","T2", rownames(reg1b_t2_N_M), as.data.frame(reg1b_t2_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 

aat_N3<-cbind("AAT","T3", rownames(aat_t3_N_M), as.data.frame(aat_t3_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
neo_N3<-cbind("NEO","T3", rownames(neo_t3_N_M), as.data.frame(neo_t3_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
mpo_N3<-cbind("MPO","T3", rownames(mpo_t3_N_M), as.data.frame(mpo_t3_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 
ml_N3<-cbind("LM","T3", rownames(lm_t3_N_M), as.data.frame(lm_t3_N_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6")) 






#Compile differences
aat_dif1<-cbind("AAT","T1", rownames(aat_t1_unadj_M), as.data.frame(aat_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_dif1<-cbind("NEO","T1", rownames(neo_t1_unadj_M), as.data.frame(neo_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_dif1<-cbind("MPO","T1", rownames(mpo_t1_unadj_M), as.data.frame(mpo_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
ml_dif1<-cbind("LM","T1", rownames(lm_t1_unadj_M), as.data.frame(lm_t1_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 

aat_dif2<-cbind("AAT","T2", rownames(aat_t2_unadj_M), as.data.frame(aat_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_dif2<-cbind("NEO","T2", rownames(neo_t2_unadj_M), as.data.frame(neo_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_dif2<-cbind("MPO","T2", rownames(mpo_t2_unadj_M), as.data.frame(mpo_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
ml_dif2<-cbind("LM","T2", rownames(lm_t2_unadj_M), as.data.frame(lm_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
reg_dif2<-cbind("REG","T2", rownames(reg1b_t2_unadj_M), as.data.frame(reg1b_t2_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 

aat_dif3<-cbind("AAT","T3", rownames(aat_t3_unadj_M), as.data.frame(aat_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
neo_dif3<-cbind("NEO","T3", rownames(neo_t3_unadj_M), as.data.frame(neo_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
mpo_dif3<-cbind("MPO","T3", rownames(mpo_t3_unadj_M), as.data.frame(mpo_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 
ml_dif3<-cbind("LM","T3", rownames(lm_t3_unadj_M), as.data.frame(lm_t3_unadj_M)) %>% `rownames<-`(NULL) %>% setNames(., c("1","2","3","4","5","6","7","8","9")) 




#Construct dataframes of results
dif_df<-(rbind(
aat_dif1,
neo_dif1,
mpo_dif1,
ml_dif1,
aat_dif2,
neo_dif2,
mpo_dif2,
ml_dif2,
reg_dif2,
aat_dif3,
neo_dif3,
mpo_dif3,
ml_dif3
))
rownames(dif_df)<-NULL
#dif_df<-cbind(dif_df[,1:3],rep(9999, nrow(dif_df)),dif_df[,4:8])
colnames(dif_df)<-c("Location","round","TR", "Dif","lower.ci","upper.ci", "SD","Robust SE")
levels(dif_df$TR)
levels(dif_df$TR)<-c("C v N", "C v N+WSH", "C v WSH", "N v N + WSH", "WSH v N+WSH")

dif_df$Location<-factor(dif_df$Location)
dif_df$round<-factor(dif_df$round)
dif_df$TR<-factor(dif_df$TR)
#dif_df$TR = factor(dif_df$TR,c("C","W","S","H","WSH","N","WSH+N"))



dif_df<-dif_df[,1:6]


N_df<-(rbind(
aat_N1,
neo_N1,
mpo_N1,
ml_N1,
aat_N2,
neo_N2,
mpo_N2,
ml_N2,
reg_N2,
aat_N3,
neo_N3,
mpo_N3,
ml_N3
))


#Drop H2 comparisons
dif_df<-subset(dif_df, TR!="WSH v N+WSH" & TR!="N v N + WSH")

#Round numeric columns
dif_df[,4]<-as.numeric(sprintf("%1.2f",dif_df[,4]))
dif_df[,5]<-as.numeric(sprintf("%1.2f",dif_df[,5]))
dif_df[,6]<-as.numeric(sprintf("%1.2f",dif_df[,6]))

#Add comparison group for colors
dif_df$comp.TR<-rep(c("WSH","N","N+WSH"),13)


#Formated comparison for x-axis printing
dif_df$TR.format<-paste0("C vs.\n",dif_df$comp.TR)


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
# cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange,N=cred,"WSH+N"=cmagent)
# cols=c(C=cblack,W=cblue,S=cteal,H=cgreen,WSH=corange)

cols=c("C v WSH"=corange,"C v N"=cred,"C v N+WSH"=cmagent)



arms=c("C", "W", "S", "H", "WSH")


# general label plot
ulabplot <- function(title) {
  plot(1,1,type="n",
       xaxt="n",xlab="",xlim=c(0,1),
       yaxt="n",ylab="",bty="n",ylim=c(0,1)
  )
  text(1,0.5,title,adj=1,cex=1.5)
}


#Order data
# unique(prev.dat$Location)
# unique(prev.dat$TR)
# unique(prev.dat$round)
# prev.dat$Location<-factor(prev.dat$Location)
# table(prev.dat$Location)
# prev.dat$Location<-factor(prev.dat$Location, c("Tubewell", "Stored water", "Hands", "Toys", "Food", "Ponds", "Soil", "Flies"))
# table(prev.dat$Location)
# prev.dat$TR

#for(i in 1:24){
prevplot<-function(d, i, yrange=c(-0.5,0.5)){
  
  if(nrow(d)==0){
       op <- par(mar=c(3,1,2,0)+0.1)
    	ulabplot("")
    	
         mtext(ifelse(i<7,
                      c("AAT", "NEO", "MPO", "LM Ratio", "REG1b")[i-1],
                        ""),
                        side=3,line=0.25,col="gray20",cex=1)

  }else{
  
  ytics <- seq(yrange[1],yrange[2],by=.1)  #<----------Set the Y-axis range here


if(i==1 | i==7 | i==13){
   op <- par(mar=c(3,0,2,1)+0.1)

	if(i==1){ulabplot("3 Month\nDifference")}
	if(i==7){ulabplot("Year 1\nDifference")}
	if(i==13){ulabplot("Year 2\nDifference")}

   	#,side=2,line=3,las=1)

	}else{
   #op <- par(mar=c(4,1,3,0.5)+0.1)
   op <- par(mar=c(3,1,2,0)+0.1)

   # set up an empty plot
MidPts <- barplot(1:3,names.arg=NA,border=NA,col=NA,
	ylim=c(range(yrange)[1],range(yrange)[2]),ylab="",yaxt="n",
	las=1,bty="n"
	)
	segments(x0=0,x1=max(MidPts+0.5),y0=ytics,lty=2,lwd=1,col="gray80")
	if(i==2 | i==8 | i==14){
	    axis(2,at=ytics,las=1)
	}

	
	# plot estimates
	arrows(x0=MidPts, y0=d$lower.ci, y1=d$upper.ci, col=cols,lwd=2,length=0.05,angle=90,code=3)
	points(MidPts,d$Dif,pch=21,cex=1.5,lwd=1,col=cols,bg="white")
	points(MidPts,d$Dif,pch=21,cex=1.5,lwd=0,col=cols,bg=alpha(cols,alpha=0.5))
	#text(x=MidPts+0.05,y=d$TR.N,pos=4,cex=1,col=cols,font=1)
	  # X-axis labels
  mtext(d$TR.format,side=1,line=2,at=MidPts,col=cols,cex=0.8,las=1)
  #mtext(d$Location,side=3,line=0.25,col="gray20",cex=1)
           mtext(ifelse(i<7,
                      c("AAT", "NEO", "MPO", "LM Ratio", "REG1b")[i-1],
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

dif_df

setwd("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Figures")
pdf("EE Difference Plot_draft.pdf",width=10,height=8.5, paper="USr")
lo <- layout(mat=matrix(1:18,ncol=6,nrow=3,byrow=T),widths=c(1,1,1,1,1,1))
op <- par(mar=c(4,1,3,0.5)+0.1)

i<-1
for(j in 1:3){
  for(k in 1:6){
    ifelse(k==1,k<-1,k<-k-1)
    prevplot(d=dif_df[dif_df$Location==levels(dif_df$Location)[k] & dif_df$round==levels(dif_df$round)[j],], i, yrange=c(-0.5,0.5))
    i<-i+1
    }
  }

dev.off()

 




