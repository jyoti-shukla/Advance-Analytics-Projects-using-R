library("ggplot2")
library(party)
library(rpart)

# Loading the given data file
grade_data <- read.csv('grades.csv', header = T, colClasses = "factor")
colnames(grade_data)
summary(grade_data)

grade_data$admitf <- factor(grade_data$admit)
dataf<-data.frame(grade_data)
#Decision Tree
set.seed(12345)
tree<-ctree(admitf~gre+gpa+rank, data=dataf, controls=ctree_control(mincriterion = 0.75,minsplit = 10)) 
tree
plot(tree)
#prediction
ptrn<-predict(tree,dataf)
#confusion Matrix
cfmtrn<-table(ptrn,dataf$admitf)
print(cfmtrn)
1-sum(diag(cfmtrn))/sum(cfmtrn)

#split into train and validation data set
#Training set: Validation Set 70:30(random)
set.seed(100)
ds_train<-sample(2,nrow(grade_data), replace = TRUE, prob=c(0.70,0.30))
TrainDataSet<-grade_data[ds_train==1,]
ValidDataSet<-grade_data[ds_train==2,]

#We will prepare model 1 of RandomForest data with Decision Tree model
model_dt<-train(admit ~ ., data = TrainDataSet,method = "rpart")
#Running on training data set
model_dt_tr = predict(model_dt, TrainDataSet)
table(model_dt_tr,TrainDataSet$admit)
mean(model_dt_tr==TrainDataSet$admit)

#Running on validation data set
model_Vs_tr = predict(model_dt, ValidDataSet)
table(model_Vs_tr,ValidDataSet$admit)
mean(model_Vs_tr==ValidDataSet$admit)
