library("neuralnet")

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



ND.Data<-load_data('iris.data.txt')
names(ND.Data)<-c('sepal.length','sepal.width','petal.length','petal.width','class')
droplevels(ND.Data$class)
levels(droplevels(ND.Data$class))

Y<-c(1:nrow(ND.Data))

for(i in c(1:nrow(ND.Data))){
  if(ND.Data[i,5] == 'Iris-setosa'){
    Y[i]<-1
  }
  else if(ND.Data[i,5] == 'Iris-versicolor'){
    Y[i]<-2
  }
  else{
    Y[i]<-3
  }
}


ND.Data[ND.Data$class=='Iris-setosa',5]<-rep(1,)

Data <- subset(ND.Data, select = c(1:4))

Data[, "class"] <- Y

class~sepal.length+sepal.width+petal.length+petal.width

digitNet<-neuralnet(class~sepal.length+sepal.width+petal.length+petal.width, Data,hidden =1)
plot(digitNet, rep = "best")

creditnet.results <- compute(digitNet, Data[,-5])
creditnet.results
xnam <- paste("x", 1:(ncol(train_data)-1), sep="")
(fmla <- as.formula(paste("x17 ~ ", paste(xnam, collapse= "+"))))

digitNet<-neuralnet(fmla,train_data, hidden =4,rep=4 )

plot(digitNet, rep = "best")
