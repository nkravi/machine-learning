
require(ggplot2)
library (ROCR)

#load_data used to load single vatiable data
load_data <-function(fileName){
  inputData <- read.csv(fileName)
  return(inputData)
}

#calculate the mean for specified col_num and class
cal.mean<-function(data,col_num,class){
  return(mean(data[data$class == class , col_num]))
}

#calculate the sd for specified col_num and class
cal.sd<-function(data,col_num,class){
  return(sd(data[data$class == class , col_num]))
}

#formulae for 1D GDA
GDA.1D.formulae<-function(x,mean,SD,prior){
  return(-log(SD)-((0.5)*((x-mean)^2/SD^2))+log(prior))
}

#compare and return max class
GDA.1D.compare<-function(val1,val2,class){
  if(val1>val2)
    return(toString(class[1]))
  return(toString(class[2]))
}

#X is a vector and retuns a vetor with class label
GDA.1D<-function(X,mean,SD,class=c(1,2),prior=c(1,1)){
  predict.class<-1:length(X)
  for(i in 1:length(X)){
    predict.class[i]<-GDA.1D.compare(GDA.1D.formulae(X[i],mean[1],SD[1],prior[1]),GDA.1D.formulae(X[i],mean[2],SD[2],prior[2]),class)
  }
  return(predict.class)
}

#used to split data in to training and testing set
split.train.test<-function(data,pct=0.75,seed=123){
  sampleSize <- floor(pct * nrow(data))
  set.seed(seed)
  Ind  <- sample(seq_len(nrow(data)), size = sampleSize)
  train.data <- data[Ind, ]
  test.data  <- data[-Ind, ]
  return(list("train.data" = train.data, "test.data" = test.data)) 
}
#used to plot gaussian graph given mean and SD
plot.distribution<-function(mean,SD){
  x   <- seq(min(mean)-4*max(SD),max(mean)+4*max(SD),length=1000)
  y1   <- dnorm(x,mean=mean[1], sd=SD[1])
  y2   <- dnorm(x,mean=mean[2], sd=SD[2])
  df <- data.frame(x,y1,y2)
  ggplot(df, aes(x)) +                    
    geom_line(aes(y=y1), colour="red") +  
    geom_line(aes(y=y2), colour="blue")
}

#calculate accuracy
perfomance.eval.accuracy<-function(actual,predicted){
  diff<-c(actual==predicted)
  return(length(diff[diff==TRUE])/length(actual))
}
#calculate persision
perfomance.eval.persision<-function(actual,predicted){
  
}
#evaluation
perfomance.eval<-function(actual,predicted){
  accuracy<-perfomance.eval.accuracy(actual,predicted)
  sprintf('accuarcy is %.2f',accuracy*100)
}

#get data
one.D.Data<-load_data('iris.data.txt')
#rename coloumns
names(one.D.Data)<-c('sepal.length','sepal.width','petal.length','petal.width','class')
summary(one.D.Data)

#make the data for 1D prediction
one.D.Data<-one.D.Data[,4:5]
one.D.Data<-rbind(subset(one.D.Data, one.D.Data$class=="Iris-setosa"),subset(one.D.Data, one.D.Data$class =="Iris-virginica"))
plot(one.D.Data[,1],rep(0,length(one.D.Data[,1])),col=one.D.Data[,2])

#split to train and test
one.D.train.data<-split.train.test(one.D.Data)$train.data
one.D.test.data<-split.train.test(one.D.Data)$test.data


#calculate parameter
mean<-c(cal.mean(one.D.train.data,1,"Iris-setosa"),cal.mean(one.D.train.data,1,"Iris-virginica"))
SD<-c(cal.sd(one.D.train.data,1,"Iris-setosa"),cal.sd(one.D.train.data,1,"Iris-virginica"))
class<-c("Iris-setosa","Iris-virginica")
mean
SD

#predict using parameter
predict.value<-GDA.1D(one.D.test.data[,1],mean,SD,class)
actual.value<-as.vector(one.D.test.data[,2])
predict.value
actual.value

data(ROCR.simple)
ROCR.simple$predictions
ROCR.simple$labels
pred <- prediction(ROCR.simple$predictions,ROCR.simple$labels)
pred <- prediction(as.matrix(predict.value), as.matrix(predict.value))

#plot graph
plot.distribution(mean,SD)
perfomance.eval(actual.value,predict.value)

