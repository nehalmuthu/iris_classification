train <- iris[51:150,]
#install.packages('caTools')
#library(caTools)

set.seed(88)
split <- sample.split(train, SplitRatio = 0.75)

train <- subset(train, split == TRUE)
test <- subset(train, split == FALSE)

y=c(1:nrow(train))
fn<-train[,5]

for(i in 1:nrow(train))
{
  if(fn[i]=="versicolor")
  {y[i]=1}
  else
  {y[i]=0}
  
}

#model <- glm (formula=y~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width+Species, data = dresstrain, family = binomial)

model <- glm (formula=y~., data = train, family=binomial(link='logit'))

s<-summary(model)

#belongs to link https://www.analyticsvidhya.com/blog/2015/11/beginners-guide-on-logistic-regression-in-r/
#predict <- predict(model, type = 'response')
#table(y, predict > 0.5)


#belongs to link https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

y1=c(1:nrow(test))
fn<-test[,5]

for(i in 1:nrow(test))
{
  if(fn[i]=="versicolor")
  {y1[i]=1}
  else
  {y1[i]=0}
  
}
result <- predict(model,newdata=subset(test,select=c(1,2,3,4,5)),type='response')
result <- ifelse(result > 0.5,1,0)

misClasificError <- mean(result != y1)
print(paste('Accuracy',1-misClasificError))







ROCRpred <- prediction(predict, y)

ROCRperf <- performance(ROCRpred, 'tpr','fpr')


plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
