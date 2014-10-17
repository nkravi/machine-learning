library(polynom)


#load_data used to load single vatiable data
load_data <-function(fileName){
  inputData <- read.table(fileName)
  colnames(inputData)<-c("X","Y")
  return(inputData)
}

#this function splits data in to train and test set
split_train_test<-function(data,pct=0.75,seed=123){
  sampleSize <- floor(pct * nrow(data))
  set.seed(seed)
  trainInd  <- sample(seq_len(nrow(data)), size = sampleSize)
  trainData  <- inputData[trainInd, ]
  testData   <- inputData[-trainInd, ]
  return(list("trainData" = trainData, "testData" = testData)) 
}

#fits the model with given polynomial
fit_model<-function(dataSplit,poly=1){
  form <- Y~poly(X,poly,raw=TRUE)
  model <- lm(form, data=dataSplit$trainData)
  return(model)
}

#helper function
calMinMax<-function(data){
  return(c(min(data),max(data)))
}

#load data
inputData = load_data('svar-set1.dat')
#plot input data
plot(inputData,main='INPUT DATA',xlim=calMinMax(inputData$X),ylim=calMinMax(inputData$Y))
#split in to train and test
dataSplit<-split_train_test(inputData,.5)
#plot train and test
plot(dataSplit$trainData,main='TRAINING DATA',xlim=calMinMax(dataSplit$trainData$X),ylim=calMinMax(dataSplit$trainData$Y))
plot(dataSplit$testData,main='TEST DATA',xlim=calMinMax(dataSplit$testData$X),ylim=calMinMax(dataSplit$testData$Y))
#fit a model
model<-fit_model(dataSplit,5)
#print coefficient of predicted model
coefficients(model)

#plot model on testing set
plot(dataSplit$testData,main='MODEL FIT ON TEST DATA',xlim=calMinMax(dataSplit$testData$X),ylim=calMinMax(dataSplit$testData$Y))
par(new=T)
model_plot <- polynomial(coefficients(model))
plot(model_plot,xlim=calMinMax(dataSplit$testData$X),ylim=calMinMax(dataSplit$testData$Y),xaxt='n',yaxt='n', ann=FALSE)


#plot model on training set
plot(dataSplit$trainData,main='MODEL FIT ON TRAIN DATA',xlim=calMinMax(dataSplit$trainData$X),ylim=calMinMax(dataSplit$trainData$Y))
par(new=T)
model_plot <- polynomial(coefficients(model))
plot(model_plot,xlim=calMinMax(dataSplit$trainData$X),ylim=calMinMax(dataSplit$trainData$Y),xaxt='n',yaxt='n', ann=FALSE)

#predict using the model and calculate MSE on TESTDATA
predict<-predict(model,dataSplit$testData)
mean((predict-dataSplit$testData$Y)^2)

#predict using the model and calculate MSE on TRAINDATA
predict<-predict(model,dataSplit$trainData)
mean((predict-dataSplit$trainData$Y)^2)

library(DAAG)
fit <- lm(Y ~ X^3, data=inputData)
cv.lm(df=inputData, fit, m=10) 

