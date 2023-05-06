x1<-c(0,0,1,1)
x2<-c(0,1,0,1)
d<-c(0,1,1,1)
train_data<-data.frame(x1,x2,d)
test_data<-data.frame(x1,x2,d)
train_data
test_data
install.packages("neuralnet")
library(neuralnet)
ann<-neuralnet(d~x1+x2,data = train_data,hidden = 0,linear.output = F) # 은닉층이 0개 인공신경망
plot(ann)
pred<-compute(ann,test_data)
pred$net.result
pred_binary<-ifelse(pred$net.result>=0.5,1,0)
pred_binary
