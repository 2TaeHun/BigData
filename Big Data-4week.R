#4week (logistic regression)
library(dplyr)
library(ggplot2)
chall<-read.csv('C:/Data/oring.txt',sep='',header = F,strip.white = T,stringsAsFactors = T)
chall
dim(chall)
colSums(is.na(chall))
names(chall)<-c("o-ring_ct","distress_ct","temperature","pressure","luanch_id")
glimpse(chall)
str(chall)
attach(chall)
chall$temperature<-(chall$temperature-32)*5/9 #온도 변수를 화씨 -> 섭씨
ggplot(chall,aes(temperature,distress_ct,col=distress_ct))+geom_point()
table(chall$distress_ct)
ggplot(chall,aes(factor(distress_ct),temperature,fill=factor(distress_ct)))+geom_boxplot()
chall_glm<-glm(cbind(distress_ct,`o-ring_ct`-distress_ct)~temperature,data=chall,family = binomial)
chall_glm
summary(chall_glm)
predict(chall_glm)
predict(chall_glm,type='response')
predict(chall_glm,data.frame(temperature=10))
predict(chall_glm,data.frame(temperature=10),type='response')
logistic<-function(x){exp(x)/(exp(x)+1)}
a<-(20-32)*5/9
b<-(85-32)*5/9
plot(c(a,b),c(0,1),type="n",xlab="Temperature",ylab="Prob")
tp<-seq(a,b,1)
chall_glm_pred<-predict(chall_glm,data.frame(temperature=tp),se.fit = T)
lines(tp,logistic(chall_glm_pred$fit))
lines(tp,logistic(chall_glm_pred$fit-1.96*chall_glm_pred$se.fit),lty=2,col='red')
lines(tp,logistic(chall_glm_pred$fit+1.96*chall_glm_pred$se.fit),lty=2,col='green')
abline(v=(30-32)*5/9,lty=2,col='blue')