##Big Data Analysis

#3week (liner regression)
library(ggplot2)
install.packages('car')
library(car)
tan<-read.csv("C:/Data/tannin.csv")
tan
dim(tan)
str(tan)
attach(tan)
lm(growth~tannin)
ggplot(tan,aes(tannin,growth))+geom_point()+stat_smooth(method = "lm")
lm(tan$tannin~tan$growth)
summary(lm(growth~tannin))
ggplot(tan,aes(growth,tannin))+geom_point()+stat_smooth(method = 'lm')
tan<-data.frame(tan)
vif(tan)
pts<-data.frame(temp=c(1.5,0.9,1.4,0.5,-0.1,2.4,-0.7,1.7,0.5,-0.7,-0.4,-1,1.5,0.7),appli=c(48,80,100,110,63,24,42,52,95,150,116,121,106,112))
pts
lm(pts$appli~pts$temp)
ggplot(pts,aes(temp,appli))+geom_point()+geom_smooth(formula = 'y~x',method = 'loess')