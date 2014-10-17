#load_data used to load single vatiable data
load_data <-function(fileName){
  inputData <- read.table(fileName)
  return(inputData)
}
#last coloumn will be considered as target variable
#and 1 will be added to X matrix
split_taget_matrix<-function(data){
  lastCol<-ncol(data)
  X<-data[,-lastCol]
  Y<-data[,lastCol]
  X<-cbind(matrix(1, nrow=nrow(data)),X)
  return(list("X" = as.matrix(X), "Y" = as.matrix(Y))) 
}
#used to split data in to training and testing set
split_train_test<-function(X,Y,pct=0.75,seed=123){
  sampleSize <- floor(pct * nrow(X))
  set.seed(seed)
  Ind  <- sample(seq_len(nrow(X)), size = sampleSize)
  Xtrain  <- X[Ind, ]
  Xtest   <- X[-Ind, ]
  Ytrain  <- Y[Ind,]
  Ytest   <- Y[-Ind,]
  return(list("Xtrain" = Xtrain, "Xtest" = Xtest,"Ytrain"=Ytrain,"Ytest"=Ytest)) 
}
#calculate theta
fit_model<-function(X,Y){
  #theta<-solve(t(X) %*% X) %*% t(X) %*% Y 
  theta<-solve(t(X) %*% X) %*% t(X) %*% Y 
  return(as.matrix(theta))
}
#function used for model predict
predict_model<-function(theta,X){
  y_hat<-X%*%theta
  return(y_hat)
}
#used to calculate mean squared error
cal_MSE<-function(actual,predicted){
  mean((predicted-actual)^2)
}
#does K-fold cross validation and return average MSE 
do_k_fold_cross_validation<-function(X,Y,folds=10){
  sampleSize<-as.integer(nrow(Y)/folds)
  start<-1
  end<-sampleSize
  tot_mse<-0
  for (i in 1:folds ) {
    Xtrain<-X[-start:-end,]
    Xtest<-X[start:end,]
    Ytrain<-Y[-start:-end,]
    Ytest<-Y[start:end,]
    start<-end+1
    end<-start+sampleSize-1
    theta<-fit_model(Xtrain,Ytrain)
    predicted<-predict_model(theta,Xtest)
    tot_mse<-tot_mse+cal_MSE(Ytest,predicted)
  }
  return(tot_mse/folds)
}
#helper function for gradient decent
grad.predict<-function(theta,x){
  return(t(theta)%*%x)
}
#helper function for gradient decent
grad.error<-function(predicted,actual){
  return(predicted-actual)
}
#helper function for gradient decent
grad.mul<-function(error,x){
  return(as.matrix(error) %*% as.matrix(x))
}

#Performs the gradient desent given training set and returns theta (coefficient)
grad.decent<-function(X,Y,learningRate=0.1,noOfIterations=100,threshold=0.0001){
  nrow<-nrow(Y)
  ncol<-ncol(X)
  theta<-matrix(0, ncol=ncol)
  
  for(j in 1:noOfIterations){
    sum<-matrix(0, nrow=ncol)
    for (i in 1:nrow ) {
      predicted<-grad.predict(theta,X[i,])
      error<-grad.error(predicted,Y[i,])
      step<-grad.mul(error,X[i,])
      sum<-sum + step
    } 
    sum<-sum/nrow
    sum<-sum*learningRate
    new_theta<-theta-t(sum)
    theta<-new_theta
    print(theta)
  }
  return(theta)
}


data<-load_data('mvar-set4.dat')
XY<-split_taget_matrix(data)
X<-XY$X
Y<-XY$Y
split<-split_train_test(X,Y)
Xtrain<-split$Xtrain
Xtest<-split$Xtest
Ytrain<-split$Ytrain
Ytest<-split$Ytest
theta<-fit_model(Xtrain,Ytrain)
predicted<-predict_model(theta,Xtest)
mse<-cal_MSE(Ytest,predicted)
mse
predicted<-predict_model(theta,Xtrain)
mse<-cal_MSE(Ytrain,predicted)
mse
do_k_fold_cross_validation(X,Y)
grad.decent(X,Y)


