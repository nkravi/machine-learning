
########################################LIB USED#######################################
require(ggplot2)
require(kernlab)
#######################################################################################
set.seed(123)

##################################DATA GENERATION######################################
line<-function(x,m,b){
  y=m*x+b
  if(x<runif(1,0,1)){
    y=y-runif(1, 2.0, 7.5)
  }else{
    y=y+runif(1, 2.0, 7.5)
  }
  return(y)
}

is_positive<-function(point,m,b){
  x=point[1]
  y=point[2]
  val = y-(m*x+b)
  if(y-(m*x+b)>0){
    return(1)
  }
  return(-1)
}

generate_data<-function(noOfInstances=200,islinearlysep=T,misclasspercent=0.10,isbalanced=T){
  m=3
  b=4
  X=runif(noOfInstances,0,1)
  Y=sapply(X,line,m,b)
  tmp <- data.frame(X,Y)  
  C<-apply(tmp,1,is_positive,m,b)
  df=data.frame(X,Y,C)
  if(!islinearlysep){
    sample<-sample.int(noOfInstances,noOfInstances*misclasspercent)
    for(s in sample){
      df$C[s]<-df$C[s]*-1
    }
  }
  return(df)
}

#############################################################################################


########################KERNEL IMPLEMENTATION################################################

kernel<-function(x,y,conf){
  res<-0
  x<-as.matrix(x)
  y<-as.matrix(y)
  if(ncol(x) != 1){
    x<-t(x)
  }
  if(ncol(y) != 1){
    y<-t(y)
  }
  type<-conf[1]
  if(type=='radial'){
    gamma<-as.double(conf[2])
    distance2<-(dist(rbind(t(x), t(y))))^2 #squared eculedian distance
    res<-exp(gamma*distance2)
  }else if(type=='poly'){
    q<-as.double(conf[2])
    res<-((t(x)%*%y) + 1)^q
  }else{
    res<-t(x)%*%y
  }
  return(res)
}

###########################################################################################

######################HELPER FUNCTION KERNEL##############################################
compute_x<-function(X,conf){
  nrowx<-nrow(X)
  res<-matrix(c(1:nrowx*nrowx),
              nrow=nrowx,
              ncol=nrowx)
  for(i in c(1:nrowx)){
    for(j in c(1:nrowx)){
      res[i,j]<-kernel(X[i,],X[j,],conf)
    }
  }
  return(res)
}

####################################IpOP function to find support vectors###################
find_support_vectors<-function(X,Y,slack,conf){
  C<-as.matrix(rep(-1,nrow(X)))
  H<-(Y%*%t(Y)) * compute_x(X,conf)
  A<-t(Y)
  b=0
  r=0
  l=as.matrix(rep(0,nrow(X)))
  return(ipop(C,H,A,b,l,slack,r))
}

set_alpha<-function(primal,limit=10){
  support_vec_index<-sort(primal(soln),T,index.return=T)
  for(i in tail(support_vec_index$i,-limit)){
    primal[i]<-0.00
  }
  return(primal)
}

#############################################################################################

##################CALCULATE W and W0#########################################################
cal_w<-function(X,Y,alpha){
  w=rep(0,ncol(X))
  for(i in c(1:nrow(X))){
    w = w + (alpha[i]*Y[i]*X[i,])
  }
  return(w)
}

cal_w0<-function(X,Y,W,alpha){
  no_of_sv<-length(alpha[alpha!=0])
  w0 <-0
  
  for(i in c(1:nrow(X))){
    w0 = w0 + (alpha[i]* (Y[i]- (W%*%X[i,])))
  }
  return((1/no_of_sv)*w0)
}

cal_wt_x<-function(X,Y,alpha,x,conf){
  wt_x<-0
  for(i in c(1:nrow(X))){
    wt_x<- wt_x + alpha[i]*Y[i]*kernel(X[i,],x,conf)
  }
  return(wt_x)
}

###########################################################################################

###################PREDECIT################################################################
predict<-function(data,X,W0,alpha,conf){
  res<-c(1:nrow(X))
  for(i in c(1:nrow(X))){
    #val<-(W%*%X[i,]) + W0
    val<-cal_wt_x(data[,-ncol(data)],data[,ncol(data)],alpha,X[i,],conf) + W0
    if(val>0){
      res[i]<-1
    }else{
      res[i]<--1
    }
  }
  return(res)
}

############################################################################################


#############################EVALUATION FUNCTION############################################
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

###############################################################################################

##################CODE FOR MODEL SETUP AND EVALUATIION#########################################

data<-generate_data(200,T,0.05)

X<-as.matrix(data[,-3])
X<-scale(X)
Y<-as.matrix(data$C)
data<-data.frame(X,Y)
colnames(data)<-c("X1","X2","Y")
qplot(X1, X2, colour = Y,data = data)

slack<-as.matrix(rep(10^5,nrow(X)))
soln<-find_support_vectors(X,Y,slack,conf=c('poly',2))
soln
primal<-primal(soln)
alpha<-as.matrix(set_alpha(primal,10))
W<-cal_w(X,Y,alpha)
W0<-cal_w0(X,Y,W,alpha)
W
W0
y_hat<-predict(data,X,W0,alpha,conf=c('poly',2))
y_hat
table(y_hat==Y)

##############################################################################################

####################################TO PLOT SUPPORT VECTORS#######################################
support_vec_index<-sort(primal(soln),T,index.return=T)
support_vec_index$i
datasupport<-data
for(i in head(support_vec_index$ix,10)){
  datasupport$Y[i]<-3
}
datasupport$Y <- factor(datasupport$Y)
qplot(X1, X2, colour = Y,data = datasupport)

#############################################################################################