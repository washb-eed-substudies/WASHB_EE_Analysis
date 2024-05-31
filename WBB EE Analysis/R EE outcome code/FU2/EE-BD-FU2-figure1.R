
rm(list=ls())
library(tidyverse)
library(knitr)
library(here)
library(cowplot)
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FU2_results.RDATA")
load("C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FU2_pooled_results.Rdata")

#Plot parameters
scaleFUN <- function(x) sprintf("%.2f", x)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")



# Plots of associations between FUT2 status and growth outcomes (FUT2 as a risk factor)

### Child FUT2 status effect on continuous growth outcomes

#fig.width=12, fig.height=8}
plotdf <-data.frame(X= rownames(pooled_childFU2_growthRF_cont), pooled_childFU2_growthRF_cont)
plotdf$outcome <- substr(plotdf$X, 1,3)
plotdf$X <- factor(plotdf$X, levels=c("LAZ", "WHZ", "WAZ", "HCZ"))
plotdf <- plotdf %>% arrange(X)
p1 <- ggplot(plotdf, aes(x=X)) + 
        geom_point(aes(y=ATE, fill=X, color=X), size = 4) +
        geom_linerange(aes(ymin=ci.l, ymax=ci.u, color=X),
                       alpha=0.5, size = 3) +
        scale_y_continuous(n.breaks=6) +
        labs(x = "", y = "Difference in Z-scores") +
        geom_hline(yintercept = 0) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 20, hjust = 0.85)) +
  ggtitle("Child FUT2 status effect\non continuous growth outcomes")

### Child FUT2 status effect on binary growth outcomes


plotdf <- data.frame(X= rownames(pooled_childFU2_growthRF_bin), pooled_childFU2_growthRF_bin)
plotdf$X <- gsub("Severe","Severely",plotdf$X)
plotdf$RR[plotdf$X=="Severely wasted - T1"] <- NA #Temp drop extreme estimates
plotdf$ci.l[plotdf$X=="Severely wasted - T1"] <- NA
plotdf$ci.u[plotdf$X=="Severely wasted - T1"] <- NA

plotdf$X <- factor(plotdf$X, levels=c("Stunted", "Wasted", "Underweight", "Severely stunted", "Severely wasted", "Severely underweight"))

plotdf$outcome <- substr(plotdf$X, 1,9)
p2 <- ggplot(plotdf, aes(x=X)) + 
        geom_point(aes(y=RR, fill=X, color=X), size = 4) +
        geom_linerange(aes(ymin=ci.l, ymax=ci.u, color=X),
                       alpha=0.5, size = 3) +
        labs(x = "", y = "Prevalence ratio") +
        scale_y_continuous(trans='log10', n.breaks=6) +
        geom_hline(yintercept = 1) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 20, hjust = 0.85)) +
  ggtitle("Child FUT2 status effect\non categorical growth outcomes")


### Mother FUT2 status effect on continuous growth outcomes

plotdf <-data.frame(X= rownames(pooled_motherFU2_growthRF_cont), pooled_motherFU2_growthRF_cont)
plotdf$outcome <- sapply(strsplit(as.character(plotdf$X), " -", fixed=T), `[`, 1)

plotdf$X <- factor(plotdf$X, levels=c("LAZ", "WHZ", "WAZ", "HCZ"))
plotdf <- plotdf %>% arrange(X)

p3 <- ggplot(plotdf, aes(x=X)) + 
        geom_point(aes(y=ATE, fill=X, color=X), size = 4) +
        geom_linerange(aes(ymin=ci.l, ymax=ci.u, color=X),
                       alpha=0.5, size = 3) +
        labs(x = "", y = "Difference in Z-scores") +
        scale_y_continuous( n.breaks=6) +
        geom_hline(yintercept = 0) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 20, hjust = 0.85)) +
  ggtitle("Maternal FUT2 status effect\non continuous growth outcomes")



### Mother FUT2 status effect on binary growth outcomes

plotdf <-data.frame(X= rownames(pooled_motherFU2_growthRF_bin), pooled_motherFU2_growthRF_bin)
plotdf$X <- gsub("Severe","Severely",plotdf$X)
plotdf$RR[plotdf$X=="Severely wasted - T1"] <- NA #Temp drop extreme estimates
plotdf$ci.l[plotdf$X=="Severely wasted - T1"] <- NA
plotdf$ci.u[plotdf$X=="Severely wasted - T1"] <- NA



plotdf$outcome <- sapply(strsplit(as.character(plotdf$X), " -", fixed=T), `[`, 1)
plotdf$X <- factor(plotdf$X, levels=c("Stunted", "Wasted", "Underweight", "Severely stunted", "Severely wasted", "Severely underweight"))
plotdf <- plotdf %>% arrange(X)

p4 <- ggplot(plotdf, aes(x=X)) + 
        geom_point(aes(y=RR, fill=outcome, color=X), size = 4) +
        geom_linerange(aes(ymin=ci.l, ymax=ci.u, color=X),
                       alpha=0.5, size = 3) +
        labs(x = "", y = "Prevalence ratio") +
        scale_y_continuous(trans='log10', n.breaks=6) +
        geom_hline(yintercept = 1) +       
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 20, hjust = 0.85)) +
  ggtitle("Maternal FUT2 status effect\non categorical growth outcomes")

p <- cowplot::plot_grid(p1, p2, p3, p4, ncol=2, labels="AUTO")
p
ggsave(p, file=paste0(here(),"/WBB EE Analysis/R EE outcome output/FUT2_figure1.png"), width=12, height=8)

#Figure legend:
 #In panels A and C, the Y-axis is the difference in Z-scores for FUT2 Negative (non-secretor) minus FUT2 Positive (secretor) children.
 #In panels B and C, the Y-axis is the prevalence ratio between  FUT2 Negative (non-secretor) over FUT2 Positive (secretor) children.

