###Activity - Find Missing Values with the NA value rows removed
library("ggplot2")
library(party)
#Loading Data
cereal_data<-read.csv("cereal.csv", header = TRUE)
colnames(cereal_data)

#Data Cleaning and make relevant data
df_Fat<-data.frame(cereal_data$X70,cereal_data$X4, cereal_data$X0)

# Change colnames of all columns to Calories, Protein, Fat
colnames(df_Fat)<-c("Calories", "Protein", "Fat")
#Removing rows with NA from R dataframe
df_Fat <- df_Fat[complete.cases(df_Fat$Fat),]

#add factor, target must be a factor
df_Fat$Fatf<-factor(df_Fat$Fat)

#Decision Tree
set.seed(123)
tree<-ctree(Fatf~Calories+Protein, data=df_Fat, controls=ctree_control(mincriterion = 0.75,minsplit = 10)) 
tree
plot(tree)

#prediction
ptrn<-predict(tree,df_Fat)
#confusion Matrix
cfmtrn<-table(ptrn,df_Fat$Fatf)
print(cfmtrn)
1-sum(diag(cfmtrn))/sum(cfmtrn)
