library(tidyr)
library(ggplot2)
library(tidyverse)

read.excel <- function(header=TRUE, ...){read.table("clipboard", sep="\t", header = header, ...)}
rawdata=read.excel()
colnames(rawdata)[1]<- "timesec"
#longdata<- gather(rawdata, Trial, dFFs, -timesec)
longdata2 <- slice(rawdata, 1:8000) %>% gather(Trial,dFFs, -timesec) #Equalize trial length
longdata3 <- slice(rawdata, 1:51) %>% gather(Trial,dFFs, -timesec) #For graphing only fast phase

# Double exponential equation
# m1 + m2*(1 - exp(-m3*x)) + m4*(1 - exp(-m5*x))
# Where m1 is Y-intercept
# Where m2 is ∆F1 and m3 is k1 (slow phase)
# Where m4 is ∆F2 and m5 is k2 (fast phase)

doublexp <- function(x,m1,m2,m3,m4,m5) m1 + m2*(1-exp(-m3*x)) + m4*(1-exp(-m5*x))
nlsfit <- nls(longdata2$dFFs ~ doublexp(x = longdata2$timesec, m1,m2,m3,m4,m5), data = longdata2, 
              start = list(m1=1.8, m2=1.2, m3=0.05, m4=.1, m5=2000), 
              nls.control(maxiter = 500, minFactor = .0000001), 
              lower = c(0,0,0,0,0), algorithm = "port", trace = TRUE)
summary(nlsfit)

m1=coef(nlsfit)[[1]]
m2=coef(nlsfit)[[2]]
m3=coef(nlsfit)[[3]]
m4=coef(nlsfit)[[4]]
m5=coef(nlsfit)[[5]]

ggplot(longdata3, aes(x=timesec, y=dFFs)) +
  geom_point(size = 0.1, alpha = 0.2) +
  theme_light(base_size = 16) +
  ylab("AU") + xlab("Time (s)") +
  ggtitle("256 uM") +
  scale_y_continuous(limits = c(4.8,5.1)) +
  stat_function(geom = "path", fun = function(x) (m1 + m2*(1 - exp(-m3*x)) + m4*(1 - exp(-m5*x))))


# ggplot(longdata, aes(x=timesec, y=dFFs)) + 
#   geom_line(aes(color=Trial), size=0.1) + 
#   scale_x_log10() +  xlab("Time (s)") + ylab("dF/F")

# df_summary <- data.frame(Time=rawdata$timesec, n=tapply(longdata$dFFs, longdata$timesec, length), mean=tapply(longdata$dFFs, longdata$timesec, mean))
# df_summary$sd <- tapply(longdata$dFFs, longdata$timesec, sd)
# df_summary$sem <- df_summary$sd/sqrt(df_summary$n-1)
# df_summary$CI_lower <- df_summary$mean + qt((1-0.95)/2, df=df_summary$n-1)*df_summary$sem
# df_summary$CI_upper <- df_summary$mean - qt((1-0.95)/2, df=df_summary$n-1)*df_summary$sem

# ggplot(df_summary, aes(x=Time, y=mean)) +
#   geom_line(size=1, alpha=0.8) +
#   geom_point(data=longdata, aes(x=timesec, y=dFFs, group=Trial), color="grey", alpha=0.01, size=0.01) +
#   geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), fill="blue", alpha=0.2) +
#   theme_light(base_size = 16) +
#   ylab("deltaF/F") + xlab("Time (s)") +
#   ggtitle("16 mM")
# 
# ggplot(longdata, aes(x=timesec, y=dFFs)) + 
#   geom_point(size = 0.1, alpha = 0.002) +
#   geom_line(data=df_summary, aes(x=Time, y=mean)) +
#   #geom_ribbon(data=df_summary, aes(ymin=CI_lower, ymax=CI_upper), fill="blue", alpha=0.2) +
#   theme_light(base_size = 16) +
#   ylab("deltaF/F") + xlab("Time (s)") +
#   ggtitle("16 mM")

