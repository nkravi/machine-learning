#libraries used
library (ROCR)
library(DAAG)
library(caret)

#load data
load_data <-function(fileName){
  inputData <- read.csv(fileName)
  return(inputData)
}

#used to split data in to training and testing set
split.train.test<-function(X,Y,pct=0.75,seed=123){
  sampleSize <- floor(pct * nrow(X))
  set.seed(seed)
  Ind  <- sample(seq_len(nrow(X)), size = sampleSize)
  X.train <- X[Ind, ]
  X.test  <- X[-Ind,]
  Y.train <- Y[Ind]
  Y.test <- Y[-Ind]
  return(list("X.train" = X.train, "X.test" = X.test,"Y.train"=Y.train,"Y.test"=Y.test)) 
}

h_theta<-function(x,theta){
  x<-as.matrix(x)
  theta<-as.matrix(theta)
  if(ncol(x) != 1){
    x<-t(x)#to convert in to coloumn matrix
  }
  return(1/(1+exp(-(t(theta)%*%x))))
}

#cost function
cost_function<-function(X,Y,theta){
  total<-0
  m<-length(Y)
  for(i in c(1:m)){
    x<-as.matrix(X[i,])
    if(ncol(x) != 1){
      x<-t(x)
    }
    total<- total + ( ( Y[i] * log( h_theta(X[i,],theta) ) ) + ( (1-Y[i]) * log( 1 - h_theta(X[i,],theta) )))
    print('total')
    print(total)
  }
  return(-(1/m)*total)
}

#cost function derivative
cost_function.derivative<-function(X,Y,theta){
  m<-length(Y)
  n<-ncol(X)
  total<-as.matrix(rep(0,n))
  for(i in c(1:m)){
    y.hat<-h_theta(X[i,],theta)
    x<-as.matrix(X[i,])
    if(ncol(x) != 1){
      x<-t(x)#to convert in to coloumn matrix
    }
    total<- total + (( as.double(y.hat - Y[i])) * x) #col matrix conversion
  }
  return(total)
}

#gradient decent
gradient_decent<-function(X,Y,learning_rate,iterations){
  n<-ncol(X)
  theta<-as.matrix(rep(0.001,n))
  for(i in c(1:iterations)){
    print(cost_function(X,Y,theta))
    theta<- theta - (learning_rate*cost_function.derivative(X,Y,theta))
  }
  return(theta)
}

#estimate thea for logistic regression
estimate_parameters<-function(X,Y,classes,learning_rate=0.0001,iterations=50){
  #placeholder for  boolean vector
  Y_new<-c(1:length(Y))
  parameters<-list()
  for(class in classes){
    print(class)
    Y_new[Y == class]<-1
    Y_new[Y != class]<-0
    factor(Y_new)
    theta<-gradient_decent(X,Y_new,learning_rate,iterations)
    parameters[[class]]<-theta
  }
  return(parameters)
}

#predict
predict<-function(X,parameters,classes){
  m<-nrow(X)
  prediction<-c(1:m)
  for(i in c(1:m)){
    val<-c(1:length(classes))
    for(j in c(1:length(classes))){
      val[j]<-h_theta(X[i,],parameters[[classes[j]]])
    }
    prediction[i]<-classes[[which.max(val)]]
  }
  return(prediction)
}


#does k fold cross validation 
doKfoldCrossValidation<-function(X,Y,k=5,learning_rate=0.0001,iterations=50){
  
  classes<-as.vector(unique(Y))
  folds<-createFolds(Y,k)
  Xfolds <- lapply(folds, function(ind, dat) dat[ind,], dat = X)
  Yfolds <- lapply(folds, function(ind, dat) dat[ind], dat = Y)
  
  avgPercision<-rep(0,length(classes))
  avgRecall<-rep(0,length(classes))
  avgFmeasure<-rep(0,length(classes))
  avgAccuracy<-0.0
  
  for(i in c(1:k)){
    X.train<-do.call(rbind,lapply(Xfolds[-i],as.matrix))
    X.test<-do.call(rbind,lapply(Xfolds[i],as.matrix)) 
    Y.train<-do.call(rbind,lapply(Yfolds[-i],as.matrix))
    Y.test<-do.call(rbind,lapply(Yfolds[i],as.matrix))
    parameters<-estimate_parameters(X.train,Y.train,classes,learning_rate,iterations)
    Y.hat<-predict(X.test,parameters,classes)
    eval<-modelEvaluation(Y.hat,Y.test)
    avgPercision<-avgPercision+eval$percision
    avgRecall<-avgRecall+eval$recall
    avgFmeasure<-avgFmeasure+eval$fmeasure
    avgAccuracy<-avgAccuracy+eval$accuarcy
  }
  return(list("avgAccuracy"=avgAccuracy/k,"avgPercision"=avgPercision/k,"avgRecall"=avgRecall/k,"avgFmeasure"=avgFmeasure/k))
}

#model evaluation
modelEvaluation<-function(prediction,truelabel){
  confusion_matrix<-as.matrix(table(prediction,truelabel))
  accuarcy<-sum(diag(confusion_matrix))/sum(confusion_matrix)
  percision<-diag(confusion_matrix)/colSums(confusion_matrix)
  recall<-diag(confusion_matrix)/rowSums(confusion_matrix)
  fmeasure<-2*((percision*recall)/(percision+recall))
  return(list("confusion_matrix"=confusion_matrix,"accuarcy"=accuarcy,"percision"=percision,"recall"=recall,"fmeasure"=fmeasure))
}

#iris dataset 
#load data
ND.Data<-load_data('iris.data.txt')
names(ND.Data)<-c('sepal.length','sepal.width','petal.length','petal.width','class')

# uncomment two lines for 2 classes
ND.Data<-ND.Data[ND.Data$class!='Iris-versicolor',]
ND.Data$class<-factor(ND.Data$class)

#uncomment for non-linear model
#remove sepal.length and petal.width

plot(ND.Data,col=ND.Data[,length(ND.Data)])
#X,Y split
X=ND.Data[,-length(ND.Data)]
Y=ND.Data[,length(ND.Data)]
#add one 
X$'theta_0'<-rep(1,length(Y))
classes<-as.vector(unique(Y))

#train and test split
train_test_split<-split.train.test(X,Y)
X.train<-train_test_split$X.train
X.test<-train_test_split$X.test
Y.train<-train_test_split$Y.train
Y.test<-train_test_split$Y.test

parameters<-estimate_parameters(X.train,Y.train,classes,0.001,500)
parameters
Y.hat<-predict(X.test,parameters,classes)
Y.hat
Y.test
modelEvaluation(Y.hat,Y.test)
doKfoldCrossValidation(X,Y,5,0.001,500)

#digit dataset
#load train data
data<-load_data('Digitdata.txt')

#change coloumn names
colnam <- paste("x", 1:ncol(data), sep="")
colnames(data)<-colnam

#keep only three classes 0,1,2
data<-data[data$x17 < 3 ,]

#X,Y split
X=data[,-length(data)]
Y=data[,length(data)]
Y<-factor(Y)

#reduce some features
X<-X[,c(1:5)]

#add one 
X$'theta_0'<-rep(1,length(Y))
classes<-as.vector(unique(Y))

#train and test split
train_test_split<-split.train.test(X,Y)
X.train<-train_test_split$X.train
X.test<-train_test_split$X.test
Y.train<-train_test_split$Y.train
Y.test<-train_test_split$Y.test


parameters<-estimate_parameters(X.train,Y.train,classes,0.001,50)
parameters
Y.hat<-predict(X.test,parameters,classes)
Y.hat
Y.test
modelEvaluation(Y.hat,Y.test)
doKfoldCrossValidation(X,Y,5,0.001,500)


