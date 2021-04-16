library("randomForest")

# Loading the given data file
df <- read.csv('grades-Copy.csv', header = T, colClasses = "factor")#Correct one
summary(df)

#split into train and validation data set
#Training set: Validation Set 70:30(random)
set.seed(100)
ds_train<-sample(2,nrow(df), replace = TRUE, prob=c(0.70,0.30))
TrainDataSet<-df[ds_train==1,]
ValidDataSet<-df[ds_train==2,]

###Version 1
#create RandomForest data with Default parameters
model1 <- randomForest(admit ~ ., data = TrainDataSet,importance=TRUE)
model1

#model with tuned condition
model2 <- randomForest(admit ~., data = TrainDataSet, ntree = 500, mtry = 3,importance=TRUE )
model2

#predicting on train set
predTrainDS<-predict(model2,TrainDataSet,type="class")
#checking classification accuracy
table(predTrainDS,TrainDataSet$admit)

#predicting on Validation set
predValidDS<-predict(model2,ValidDataSet,type="class")
#checking classification accuracy
mean(predValidDS ==ValidDataSet$admit )
table(predValidDS,ValidDataSet$admit)

#using a for loop to identify the right parameter for mtry for model
a=c()
i=4
for(i in 2:5)
{
  model3 <- randomForest(admit ~., data = TrainDataSet, ntree = 500, mtry = i,importance=TRUE )
  predValidDS<-predict(model3,ValidDataSet,type="class")
  a[i-1] = mean(predValidDS ==ValidDataSet$admit )
  }

a 
plot(2:5, a)
FindModel<-randomForest(admit ~., data = TrainDataSet, ntree = 500, mtry = 5,importance=TRUE )
FindModel

#predicting on Validation set
predValidDS<-predict(FindModel,ValidDataSet,type="class")
#checking classification accuracy
mean(predValidDS ==ValidDataSet$admit )
table(predValidDS,ValidDataSet$admit)


###Version 2
#create RandomForest data with Default parameters
model1 <- randomForest(admit ~ gpa + rank, data = TrainDataSet,importance=TRUE)
model1

#model with tuned condition
model2 <- randomForest(admit ~gpa + rank, data = TrainDataSet, ntree = 500, mtry = 3,importance=TRUE )
model2

#predicting on train set
predTrainDS<-predict(model2,TrainDataSet,type="class")
#checking classification accuracy
table(predTrainDS,TrainDataSet$admit)

#predicting on Validation set
predValidDS<-predict(model2,ValidDataSet,type="class")
#checking classification accuracy
mean(predValidDS ==ValidDataSet$admit )
table(predValidDS,ValidDataSet$admit)

#using a for loop to identify the right parameter for mtry for model
a=c()
i=4
for(i in 2:5)
{
  model3 <- randomForest(admit ~ gpa + rank, data = TrainDataSet, ntree = 500, mtry = i,importance=TRUE )
  predValidDS<-predict(model3,ValidDataSet,type="class")
  a[i-1] = mean(predValidDS ==ValidDataSet$admit )
}

a 
plot(2:5, a)
FindModel<-randomForest(admit ~ gpa + rank, data = TrainDataSet, ntree = 500, mtry = 5,importance=TRUE )
FindModel

#predicting on Validation set
predValidDS<-predict(FindModel,ValidDataSet,type="class")
#checking classification accuracy
mean(predValidDS ==ValidDataSet$admit )
table(predValidDS,ValidDataSet$admit)