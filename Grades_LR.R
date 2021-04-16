# Loading the given data file
df <- read.csv('grades.csv')
#str(df)

#split into train and validation data set
set.seed(1234)
ds_train<-sample(2,nrow(df), replace = TRUE, prob=c(0.80,0.20))
TrainDataSet<-df[ds_train==1,]
ValidDataSet<-df[ds_train==2,]

#Model using Logistic Regression
mymodel<-glm(admit ~ ., data=TrainDataSet, family ='binomial')
summary(mymodel)

#prediction
pred1<-predict(mymodel,TrainDataSet,type='response')
summary(pred1)
#misclassification error
prederr1<-ifelse(pred1>0.5, 1,0)
tab1<-table(Predicted=prederr1,Actual=TrainDataSet$admit)
tab1