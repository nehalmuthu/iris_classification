iris1<-iris[51:150,]
# setosa if 1:100 
#versicolor if 51:150
#nam="setosa"
nam="versicolor"

data<-iris1
n <- nrow(data)
shuffled_df <- data[sample(n), ]
train_indices <- 1:round(0.6 * n)
train <- shuffled_df[train_indices, ]
test_indices <- (round(0.6* n) + 1):n
test <- shuffled_df[test_indices, ]

data<-train

x<-data[1:4]
x<-as.matrix(x)
fn<-data[,5]

w=c(.05,.05,.05,.05)
b=0.05
w<-as.matrix(w)
w<-t(w)

ypred=c(1:60)

rate=.05
y=c(1:60)

for(i in 1:nrow(data))
{
  if(fn[i]==nam)
    y[i]=1
  else
  y[i]=0
  
}

for(j in 1:10){

for(i in 1:nrow(x)){
  yin<-(x[i,]%*%t(w))+b
  z<-1/(1+exp(-yin))
  if(z>=0.5)
    ypred[i]=1
  else 
    ypred[i]=0
  
  w=w+rate*(y[i]-ypred[i])*x[i,]
  b=b+rate*(y[i]-ypred[i])
  
}
  
}


#test 


data<-test
x<-data[,1:4]
x<-as.matrix(x)
fn<-data[,5]
y1=c(1:40)
ypred1=c(1:40)


for(i in 1:nrow(data))
{
  if(fn[i]==nam)
    y1[i]=1
  else
    y1[i]=0
  
}


for(i in 1:nrow(x)){
  yin<-(x[i,]%*%t(w))+b
  z<-1/(1+exp(-yin))
  if(z>=0.5)
    ypred1[i]=1
  else 
    ypred1[i]=0
  
 
}


confusionMatrix(ypred1, y1)

ROCRpred <- prediction(ypred1, y1)

ROCRperf <- performance(ROCRpred, 'tpr','fpr')


plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

