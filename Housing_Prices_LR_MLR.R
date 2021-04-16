# Multiple Linear Regression
library(ggplot2)
# Importing the dataset
df = read.csv('housedata_2.csv', header = TRUE)
str(df)
set.seed(123)

#Simple linear Regression
slm = lm(price ~ sqft_living, data = df)
summary(slm)
scatter.smooth(df, main="Price Vs Sqft_living",col = "blue")
#prediction
predict(slm,data.frame(sqft_living=2160), interval='confidence')

pairs(df[1:4],col="blue")

#Multiple linear Regression
mlmresults<-lm(price ~ sqft_living + grade, df)
mlmresults
summary(mlmresults)

#compare models results: anova analysis
reduced<-lm(price ~ sqft_living, df)
partial<-lm(price ~ sqft_living + grade, df)
full<-lm(price ~ sqft_living + condition + grade, df)
anova(reduced,partial,full)

#prediction
predict(reduced,data.frame(sqft_living=2160), interval='confidence')
predict(partial,data.frame(sqft_living=2160, grade=6), interval='confidence')
predict(full,data.frame(sqft_living=2160, grade=6, condition=4), interval='confidence')
