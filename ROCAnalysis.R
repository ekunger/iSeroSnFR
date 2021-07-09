library(tidyverse)
library(pROC)
library(psycho)

read.excel <- function(header=TRUE, ...){read.table("clipboard", sep="\t", header = header, ...)}
rawdata=read.excel() #read in data: cols = trials/ROIs, rows = timepoints

longdata <- gather(rawdata, ROI, dFFs, -Baseline.Signal) #tidy data
dffs <- longdata$dFFs
signoise <- longdata$Baseline.Signal #Baseline.Signal = classifier column, 0 = baseline, 1 = signal

plot(x=dffs, y=signoise) #frequency distribution
glm.fit=glm(signoise ~ dffs, family = binomial) #establish classifier
lines(dffs, glm.fit$fitted.values) #check classifier fidelity

par(pty = "s") #plot constrained to square
roc(signoise, glm.fit$fitted.values, 
    plot=TRUE, 
    legacy.axes=TRUE, 
    xlab="False Positive Rate", 
    ylab="True Positive Rate", 
    print.auc=TRUE) #ROC plot
roc.info <- roc(signoise, glm.fit$fitted.values, legacy.axes=TRUE)
roc.df <- data.frame(
    truepos = roc.info$sensitivities*100, 
    falsepos = (1-roc.info$specificities)*100, 
    misses = (1-roc.info$sensitivities)*100, 
    correj = roc.info$specificities*100, 
    thresholds = roc.info$thresholds) #data for d' analysis

indices <- psycho::dprime(
    n_hit = roc.df$truepos, 
    n_fa = roc.df$falsepos, 
    n_miss = roc.df$misses, 
    n_cr = roc.df$correj)
plot(x=roc.df$thresholds, y=indices[[1]], xlab = "Threshold", ylab = "d' score")

par(pty = "m") #plot fills space by default
