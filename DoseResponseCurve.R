library(tidyr)
library(drc)
library(ggplot2)

#copy data to clipboard, concentrations are rows, trials are columns
read.excel <- function(header=TRUE, ...){read.table("clipboard", sep="\t", header = header, ...)}
rawdata=read.excel()

#Tidy data
colnames(rawdata)[1]<- "concs"
longdata <- gather(rawdata, Trial, dFFs, -concs)

#Fit the Hill equation
mycurve <- drm(longdata$dFFs ~ longdata$concs, fct = LL.4())

#Calculate 95% confidence intervals
minconc <- min(longdata$concs, na.rm=TRUE)
maxconc <- max(longdata$concs, na.rm=TRUE)
newdata <- expand.grid(concs=exp(seq(log(minconc/2), log(maxconc*2), length=maxconc*2)))
pm <- predict(mycurve, newdata = newdata, interval = "confidence")
newdata$p <- pm[,1]
newdata$pmin <- pm[,2]
newdata$pmax <- pm[,3]

#plot the data
ggplot(longdata, aes(x=concs, y=dFFs)) + 
  geom_point(color="blue") + 
  geom_line(data=newdata, aes(x=concs, y=p), color="blue") + 
  scale_x_log10() + xlab("uncaging time (ms)") + ylab("dF/F") + 
  geom_ribbon(data=newdata, aes(x=concs, y=p, ymin=pmin, ymax=pmax), alpha=0.2)

#b = (-)Hill coefficient, c = min offset, d = max, e = Kd
summary(mycurve)

#for saving and opening in illustrator, use eps
ggplot(longdata, aes(x=concs, y=dFFs)) + geom_point(color="blue") + 
    geom_line(data=newdata, aes(x=concs, y=p), color="blue") + 
    scale_x_log10() + xlab("[5HT] (uM)") + ylab("dF/F") + 
    geom_line(data=newdata, aes(x=concs, y=pmin)) +
    geom_line(data=newdata, aes(x=concs, y=pmax)) +
    ggtitle("Dose Response Curve") + theme(plot.title = element_text(hjust = 0.5))

ggsave(plot = last_plot(), file="C:\\Users\\EKUnger\\Desktop\\neuroncurves.eps", width = 14, height = 8)
