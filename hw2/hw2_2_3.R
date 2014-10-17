#libraries used
library (ROCR)
library(DAAG)
library(caret)

#load the iris dataset
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
#estimate parameters based on GDA
estimate_parameters<-function(X,Y){
  #get classes
  mean<-list()
  sigma<-list()
  priors<-list()
  classes<-as.vector(unique(Y))
  for(class in classes){
    priors[[class]]<-length(Y[Y==class])/length(Y)
    mean[[class]]<-as.matrix(colMeans(X[Y==class,]))
    sigma[[class]]<-as.matrix(cov(X[Y==class,]))
  }
  return(list("mean"=mean,"sigma"=sigma,"priors"=priors))
}

#formulae for ND-GDA
predict.formulae<-function(x,mean,sigma,prior){
  return(-log(det(sigma))- 0.5*(t((x-mean))%*%solve(sigma)%*%(x-mean)) + log(prior))
}
#predict using parameters ND-GDA formulae
predict<-function(X,parameters,classes){
  nclass<-length(classes)
  nrow<-nrow(X)
  retval<-c(1:nclass)
  predictVal<-c(1:nrow)
  for(i in 1:nrow){
    x<-as.matrix(X[i,])
    if(ncol(x) !=1 ){#if not coloumn matrix make it as coloumn matrix
      x<-t(x)
    }
    for(j in 1:nclass){
      mean<-as.matrix(parameters$mean[[classes[j]]])
      sigma<-as.matrix(parameters$sigma[[classes[j]]])
      prior<-as.double(parameters$priors[[classes[j]]])
      retval[j]<-predict.formulae(x,mean,sigma,prior)
    }
    predictVal[i]<-classes[which.max(retval)]
  }
  return(predictVal)
}
#does k fold cross validation 
doKfoldCrossValidation<-function(X,Y,k=5){
  
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
    parameters<-estimate_parameters(X.train,Y.train)
    Y.hat<-predict(X.test,parameters,classes)
    eval<-modelEvaluation(Y.hat,Y.test)
    avgPercision<-avgPercision+eval$percision
    avgRecall<-avgRecall+eval$recall
    avgFmeasure<-avgFmeasure+eval$fmeasure
    avgAccuracy<-avgAccuracy+eval$accuarcy
  }
  return(list("avgAccuracy"=avgAccuracy/k,"avgPercision"=avgPercision/k,"avgRecall"=avgRecall/k,"avgFmeasure"=avgFmeasure/k))
}

modelEvaluation<-function(prediction,truelabel){
  confusion_matrix<-as.matrix(table(prediction,truelabel))
  accuarcy<-sum(diag(confusion_matrix))/sum(confusion_matrix)
  percision<-diag(confusion_matrix)/colSums(confusion_matrix)
  recall<-diag(confusion_matrix)/rowSums(confusion_matrix)
  fmeasure<-2*((percision*recall)/(percision+recall))
  return(list("confusion_matrix"=confusion_matrix,"accuarcy"=accuarcy,"percision"=percision,"recall"=recall,"fmeasure"=fmeasure))
}


ND.Data<-load_data('iris.data.txt')
#rename coloumns
names(ND.Data)<-c('sepal.length','sepal.width','petal.length','petal.width','class')

# uncomment two lines for nd-2 classes
#ND.Data<-ND.Data[ND.Data$class!='Iris-versicolor',]
#ND.Data$class<-factor(ND.Data$class)

#split in to X an Y
X=ND.Data[,-length(ND.Data)]
Y=ND.Data[,length(ND.Data)]
classes<-as.vector(unique(Y))

#train and test split
train_test_split<-split.train.test(X,Y)
X.train<-train_test_split$X.train
X.test<-train_test_split$X.test
Y.train<-train_test_split$Y.train
Y.test<-train_test_split$Y.test

parameters<-estimate_parameters(X.train,Y.train)
parameters
Y.hat<-predict(X.test,parameters,classes)
Y.hat
Y.test
modelEvaluation(Y.hat,Y.test)
#cross validation
doKfoldCrossValidation(X,Y,5)




