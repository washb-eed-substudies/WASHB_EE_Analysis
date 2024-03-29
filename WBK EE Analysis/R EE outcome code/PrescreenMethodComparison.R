
#---------------------------------------
# Anthro Analysis.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The outcome analysis in Wash Benefits Kenya for all anthropometry measures and indices.
#---------------------------------------

#---------------------------------------
# preamble
#---------------------------------------
###Load in data
rm(list=ls())
library(foreign)
try(detach(package:plyr))
library(dplyr)
library(magrittr)
library(rmarkdown)
library(washb)
library(SuperLearner)

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew")
#enrol<-read.csv("washb-kenya-enrol.csv", stringsAsFactors = TRUE)
#anthro<-read.csv("washb-kenya-anthro.csv", stringsAsFactors = TRUE)


enrol<-read.dta("washb-kenya-enrol.dta")
anthro<-read.dta("washb-kenya-anthro.dta")


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")



# source the base functions
# which includes the design matrix function used below
source("C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/R Primary Outcome Code/washb_functions.R")

####set arms
arms<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N","PassiveControl")



#-------------------
#Load and clean data
#-------------------



#merge in adjustment covariates
enroltomerge<-subset(enrol, select=c('childid','Ncomp','momage','momedu','momheight','dminwat','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','cow','goat','dog','chicken','roof', 'floor','HHS' ))


#-------------------
#Load and clean data and create shell function for analysis
#-------------------

#merge in adjustment covariates
dim(anthro)
anthro_enrol<-merge(anthro, enroltomerge, by=c("childid"),all.x=T,all.y=F)
dim(anthro_enrol)




#enroltomerge<-select(enrol, childid, diar_agem_bl, diar_agem_ml, diar_agem_el)
#laz <- merge(enrol,anthro,by=c("childid"),all.x=F,all.y=T)
laz<-subset(anthro_enrol, childtype=="Study Child"|childtype=="Study Child Twin")


laz$tr <- factor(laz$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))
laz$month <- factor(laz$month)
laz$floor <- factor(laz$floor)

# re-order the treatment factor for convenience
laz$tr <- factor(laz$tr,levels=c("Control","Passive Control","Water","Sanitation","Handwashing","WSH","Nutrition","Nutrition + WSH"))

laz <- laz[order(laz$block,laz$clusterid,laz$hhid,laz$childid),]

##Adjusted
#Adjustment variables to include
#month included within diar and anthro datasets.
Wvars<-c('sex','aged','month','momage','momedu','momheight','Ncomp','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','roof','floor','cow','goat','dog','chicken','dminwat','HHS','fracode')

#Create fracode staffid experience factore
laz$staffid[laz$staffid==231] = 0
laz$staffid[laz$staffid==431] = 0
laz$staffid[laz$staffid==542] = 0
laz$staffid[laz$staffid==545] = 0
laz$staffid[laz$staffid==551] = 0
laz$staffid[laz$staffid==860] = 0
laz$staffid[laz$staffid==1727] = 0
laz$staffid[laz$staffid==2312] = 0
laz$staffid[laz$staffid==2687] = 0
laz$staffid[laz$staffid==3334] = 0
laz$staffid[laz$staffid==3433] = 0
laz$staffid[laz$staffid==3436] = 0
laz$staffid[laz$staffid==3438] = 0
laz$staffid[laz$staffid==3446] = 0
laz$staffid[laz$staffid==3450] = 0
laz$staffid[laz$staffid==3508] = 0
laz$staffid[laz$staffid==3683] = 0
laz$staffid[laz$staffid==3783] = 0
laz$staffid[laz$staffid==3785] = 0
laz$staffid[laz$staffid==3787] = 0
laz$staffid[laz$staffid==4218] = 0
laz$staffid[laz$staffid==4313] = 0
laz$staffid[laz$staffid==4314] = 0
laz$staffid[laz$staffid==4347] = 0
laz$staffid[laz$staffid==4345] = 0
laz$staffid[laz$staffid==4348] = 0
laz$staffid[laz$staffid==4405] = 0
laz$staffid[laz$staffid==4515] = 0
laz$staffid[laz$staffid==4534] = 0
laz$staffid[laz$staffid==4538] = 0
laz$staffid[laz$staffid==4548] = 0
laz$staffid[laz$staffid==4617] = 0
laz$staffid[laz$staffid==4630] = 0
laz$staffid[laz$staffid==4714] = 0
laz$staffid[laz$staffid==4715] = 0
laz$staffid[laz$staffid==4717] = 0
laz$staffid[laz$staffid==4785] = 0
laz$staffid[laz$staffid==4812] = 0
laz$staffid[laz$staffid==4835] = 0
laz$staffid[laz$staffid==5324] = 0
laz$staffid[laz$staffid==5421] = 0
laz$staffid[laz$staffid==5422] = 0
laz$staffid[laz$staffid==5818] = 0
laz$staffid[laz$staffid==7540] = 0
laz$staffid[laz$staffid==7640] = 0
laz$staffid[laz$staffid==7848] = 0
laz$staffid[laz$staffid==8246] = 0
laz$staffid[laz$staffid==8274] = 0
laz$staffid[laz$staffid==8303] = 0
laz$staffid[laz$staffid==8604] = 0
laz$staffid[laz$staffid==8681] = 0
laz$staffid[laz$staffid==8787] = 0
laz$staffid[laz$staffid==8803] = 0
laz$staffid[laz$staffid==8884] = 0

laz$fracode <- factor(laz$staffid)





#Drop children missing laz
laz<-subset(laz, !is.na(laz))
dim(laz)

# Drop children with extreme LAZ values
laz <- subset(laz,laz_x!=1)
dim(laz)




##################
#LAZ outcome, nutrition vs. control
##################
  

  #subset to year 2
  laz2 <- subset(laz,studyyear==2)




  #Create adjustment variable dataframe
  Wanthro<-subset(laz2, select=Wvars)
  #ensure factors are set
  Wanthro$fracode<-as.factor(Wanthro$fracode)
  Wanthro$month<-as.factor(Wanthro$month)
  Wanthro$HHS<-as.factor(Wanthro$HHS)
  Wanthro$electricity<-as.factor(Wanthro$electricity)
  Wanthro$radio<-as.factor(Wanthro$radio)
  Wanthro$television<-as.factor(Wanthro$television)
  Wanthro$mobile<-as.factor(Wanthro$mobile)
  Wanthro$clock<-as.factor(Wanthro$clock)
  Wanthro$bicycle<-as.factor(Wanthro$bicycle)
  Wanthro$motorcycle<-as.factor(Wanthro$motorcycle)
  Wanthro$stove<-as.factor(Wanthro$stove)
  Wanthro$roof<-as.factor(Wanthro$roof)
  Wanthro$floor<-as.factor(Wanthro$floor)
  laz2$block<-as.factor(laz2$block)




  #Order to match Jade
  laz2 <- laz2[order(laz2$block,laz2$clusterid,laz2$hhid,laz2$childid,laz2$studyyear),]


    #GLM, no prescreening
  CvN.glm.no.screening<-washbglm(Y=laz2$laz, tr=laz2$tr, W=Wanthro, forcedW=colnames(Wanthro), id=laz2$block, pair=laz2$block, family="gaussian", contrast= c("Control","Nutrition"), print=T)

  
    #GLM, LR test pre-screening 
  CvN.glm.screened<-washbglm(Y=laz2$laz, tr=laz2$tr, W=Wanthro, forcedW=NULL, id=laz2$block, pair=laz2$block, family="gaussian", contrast= c("Control","Nutrition"), print=T)

  
  
      #TMLE, LR test pre-screening outside of the Q(Y|A,W) cross-validated estimation
  CvN.tmle<-washb_tmle(Y=laz2$laz, tr=laz2$tr, W=Wanthro, id=laz2$block,pair=laz2$block,family="gaussian",contrast= c("Control","Nutrition"), Q.SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet"), seed=67890, print=F)
  CvN.tmle
  
  
      #TMLE, LR test pre-screening within the Q(Y|A,W) cross-validated estimation
  
  #Create prescreen function with including variables with a Pearson'g Pvalue=0.2 
  screen.corP2<-function (Y, X, family, obsWeights, id, method = "pearson", minPvalue = 0.2, 
    minscreen = 2, ...) 
{
    listp <- apply(X, 2, function(x, Y, method) {
        ifelse(var(x) <= 0, 1, cor.test(x, y = Y, method = method)$p.value)
    }, Y = Y, method = method)
    whichVariable <- (listp <= minPvalue)
    if (sum(whichVariable) < minscreen) {
        warning("number of variables with p value less than minPvalue is less than minscreen")
        whichVariable[rank(listp) <= minscreen] <- TRUE
    }
    return(whichVariable)
}
  
  c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
  
  #Set library with corrP prescreening
  screen.lib=list(c("SL.mean","All"), # not adjusted, so doesn't matter
                  c("SL.glm","screen.corP2"),c("SL.bayesglm","screen.corP2"),
                  c("SL.gam","screen.corP2"),c("SL.glmnet","screen.corP2"))
  


  CvN.tmle.screenCorP<-washb_tmle(Y=laz2$laz, tr=laz2$tr, W=Wanthro, pval=0.99, id=laz2$block,pair=laz2$block,family="gaussian", contrast= c("Control","Nutrition"), Q.SL.library=screen.lib, seed=67890, print=F)
  CvN.tmle.screenCorP

  #Format tmle results
  TR.tmle.prescreen<-cbind(CvN.tmle$estimates$ATE$psi,CvN.tmle$estimates$ATE$CI[1],CvN.tmle$estimates$ATE$CI[2],CvN.tmle$estimates$ATE$var.psi,CvN.tmle$estimates$ATE$pvalue)
  TR.tmle.corP<-cbind(CvN.tmle.screenCorP$estimates$ATE$psi,CvN.tmle.screenCorP$estimates$ATE$CI[1],CvN.tmle.screenCorP$estimates$ATE$CI[2],CvN.tmle.screenCorP$estimates$ATE$var.psi,CvN.tmle.screenCorP$estimates$ATE$pvalue)
  colnames(TR.tmle.prescreen)<-colnames(TR.tmle.corP)<-c("ATE","ci.lb","ci.ub","var","Pval")
    
  #Compare:
  CvN.glm.no.screening$TR
  CvN.glm.screened$TR
  TR.tmle.prescreen
  TR.tmle.corP
  
  
  
  
  


washbglm<-function (Y, tr, pair = NULL, W = NULL, forcedW = NULL, V = NULL, 
    id, contrast, family = "gaussian", pval = 0.2, print = TRUE, 
    verbose = FALSE) 
{
  require(sandwich)
    require(lmtest)
    options(scipen = 20)
    Subgroups = NULL
    if (!is.null(W)) {
        W <- data.frame(W)
    }
    if (!is.null(pair)) {
        if (!is.null(W)) {
            glmdat <- data.frame(id, Y, tr, pair, W)
        }
        else {
            glmdat <- data.frame(id, Y, tr, pair)
        }
        glmdat$tr <- factor(glmdat$tr, levels = contrast[1:2])
        glmdat$pair <- factor(glmdat$pair)
    }
    else {
        if (!is.null(W)) {
            glmdat <- data.frame(id, Y, tr, W)
        }
        else {
            glmdat <- data.frame(id, Y, tr)
        }
        glmdat$tr <- factor(glmdat$tr, levels = contrast[1:2])
    }
    glmdat <- subset(glmdat, tr == contrast[1] | tr == contrast[2])
    glmdat$tr <- factor(glmdat$tr, levels = contrast[1:2])
    if (!is.null(pair)) {
        n.orig <- dim(glmdat)[1]
        miss <- NULL
        activeOnly <- ((subset(glmdat, tr == contrast[1])))
        nomiss <- sort(unique(activeOnly$pair))
        miss1 <- (unique(pair)[which(!(unique(pair) %in% (nomiss)))])
        activeOnly2 <- ((subset(glmdat, tr == contrast[2])))
        nomiss2 <- sort(unique(activeOnly2$pair))
        miss2 <- (unique(pair)[which(!(unique(pair) %in% (nomiss2)))])
        miss <- append(miss1, miss2)
        glmdat <- subset(glmdat, !(pair %in% miss))
        n.sub <- dim(glmdat)[1]
        if (print == TRUE) 
            if (n.orig > n.sub) 
                cat("\n-----------------------------------------\n", 
                  "Starting N:  ", n.orig, "\nN after block dropping: ", 
                  n.sub)
        if (print == TRUE) 
            if (n.orig > n.sub) 
                cat("\n-----------------------------------------\n", 
                  "Pairs/blocks dropped due to missingness in at least one treatment level:\n", 
                  sort(unique(miss)), "\n\nDropping", n.orig - 
                    n.sub, "observations due to missing pairs.", 
                  "\n-----------------------------------------\n")
    }
    n.orig <- dim(glmdat)[1]
    rowdropped <- rep(1, nrow(glmdat))
    rowdropped[which(complete.cases(glmdat))] <- 0
    glmdat <- glmdat[complete.cases(glmdat), ]
    n.sub <- dim(glmdat)[1]
    if (print == TRUE) 
        if (n.orig > n.sub) 
            cat("\n-----------------------------------------\nDropping", 
                n.orig - n.sub, "observations due to missing values in 1 or more variables\n", 
                "Final sample size:", n.sub, "\n-----------------------------------------\n")
    if (!is.null(W)) {
        colnamesW <- names(W)
    }
    if (!is.null(W)) {
        if (!is.null(V)) {
            forcedW = c(V, forcedW)
        }
        if (!is.null(forcedW)) {
            screenW <- subset(glmdat, select = colnamesW)
            toexclude <- names(screenW) %in% forcedW
            if (length(which(toexclude == TRUE)) != length(forcedW)) 
                stop("A forcedW variable name is not a variable within the W data frame.")
            screenW = screenW[!toexclude]
            if (ncol(screenW) == 0) {
                screenW <- NULL
            }
            if (print == TRUE) {
                cat("\n-----------------------------------------\nInclude the following adjustment covariates without screening:\n-----------------------------------------\n")
                print(forcedW, sep = "\n")
            }
        }
        else {
            screenW <- subset(glmdat, select = colnamesW)
        }
    }
    else {
        screenW <- NULL
    }
    if (!is.null(screenW)) {
        if (print == TRUE) 
            cat("\n-----------------------------------------\nPre-screening the adjustment covariates:\n-----------------------------------------\n")
        suppressWarnings(Wscreen <- washb_prescreen(Y = glmdat$Y, 
            Ws = screenW, family = family, pval = pval, print = print))
    }
    else {
        Wscreen = NULL
    }
    if (!is.null(pair)) {
        if (!is.null(forcedW)) {
            if (!is.null(Wscreen)) {
                dmat <- subset(glmdat, select = c("Y", "tr", 
                  forcedW, Wscreen, "pair"))
            }
            else {
                dmat <- subset(glmdat, select = c("Y", "tr", 
                  forcedW, "pair"))
            }
        }
        else {
            if (!is.null(Wscreen)) {
                dmat <- subset(glmdat, select = c("Y", "tr", 
                  Wscreen, "pair"))
            }
            else {
                dmat <- subset(glmdat, select = c("Y", "tr", 
                  "pair"))
            }
        }
    }
    else {
        if (!is.null(forcedW)) {
            if (!is.null(Wscreen)) {
                dmat <- subset(glmdat, select = c("Y", "tr", 
                  forcedW, Wscreen))
            }
            else {
                dmat <- subset(glmdat, select = c("Y", "tr", 
                  forcedW))
            }
        }
        else {
            if (!is.null(Wscreen)) {
                dmat <- subset(glmdat, select = c("Y", "tr", 
                  Wscreen))
            }
            else {
                dmat <- subset(glmdat, select = c("Y", "tr"))
            }
        }
    }
    
  

            suppressWarnings(fit <- glm(Y ~ ., family = family, 
                data = dmat))

dat=dmat 
fm = fit
cluster =droplevels(glmdat$id)

            
    require(sandwich, quietly = TRUE)
    require(lmtest, quietly = TRUE)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
    uj <- apply(estfun(fm), 2, function(x) tapply(x, cluster, sum))
    vcovCL <- dfc * sandwich(fm, meat = crossprod(uj)/N)           
            
            
rfit <- coeftest(fit, vcovCL)
    
        modelfit <- washb_glmFormat(glmModel = fit, rfit = rfit, 
            dmat = dmat, rowdropped = rowdropped, contrast = contrast, 
            pair = pair, vcovCL = vcovCL, family = family, V = V, 
            Subgroups = Subgroups, print = print, verbose = verbose)
        
        return(modelfit)
}




