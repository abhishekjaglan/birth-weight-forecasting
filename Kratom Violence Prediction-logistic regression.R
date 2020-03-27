###########################################################
# Building and testing multiple logistic Regression Model #
###########################################################
library(forecast)
library(readxl)
library(tidyverse)
library(ggplot2)
library(readr)
# reading the file
testdata1 <- read_csv(file ="~/Desktop/KratomSalesNew3.csv" )
testdata1n<- as.data.frame(testdata1)

###exploratory data analysis###
view(testdata1)
table(testdata1$DayType)
summary(testdata1)
# no NA's present but if there's any run this
testdata1 <- na.omit(testdata1)
str(testdata1)
testdata1$DayType <- as.factor(testdata1$DayType)
class(testdata1$DayType) #DayType(A=low crime, B= High crime)
par(mfrow = c(3,3))
hist(testdata1$Ratio,col = 'darkorchid')
hist(testdata1$WeekNum,col = 'darkorchid')
hist(testdata1$DateY,col = 'darkorchid')
hist(testdata1$TotalSales,col = 'darkorchid')
hist(testdata1$Transactions,col = 'darkorchid')
hist(testdata1$Units,col = 'darkorchid')
hist(testdata1$AverageT,col = 'darkorchid')
hist(testdata1$Rank1,col = 'darkorchid')
hist(testdata1$ViolentCrime,col = 'darkorchid')
par(mfrow = c(2,2))
plot(DayType ~ TotalSales,data = testdata1)
plot(DayType ~ ViolentCrime, data = testdata1)
plot(DayType ~ AverageT, data = testdata1)
#extra insights 
par(mfrow = c(3,3))
boxplot(TotalSales ~ DayType, xlab = "Day Type", ylab = "Total sales",col = "light blue",data = testdata1)
boxplot(Ratio ~ DayType, xlab = "Day Type", ylab = "X1",col = "light blue", data = testdata1)
boxplot(WeekNum ~ DayType, xlab = "Day Type", ylab = "Week Number", col = "light blue", data = testdata1)
boxplot(Transactions ~ DayType, xlab = "Day Type", ylab = "Transactions", col = "light blue", data = testdata1)
boxplot(Units ~ DayType, xlab = "Day Type", ylab = "Units", col = "light blue", data = testdata1)
boxplot(AverageT ~ DayType, xlab = "Day Type", ylab = "AverageT", col = "light blue", data = testdata1)
boxplot(Rank1 ~ DayType, xlab = "Day Type",ylab = "Rank1", col = "light blue", data = testdata1)
boxplot(ViolentCrime ~ DayType, xlab = "Day Type", ylab = "Crime", col = "light blue", data = testdata1)
boxplot(DateY ~ DayType, xlab = "Day Type", ylab = "Year", col = "light blue", data = testdata1)

###creating training and test sample sets
#training set
data1 <- testdata1[which (testdata1$DayType == "A"), ]
data2 <- testdata1[ which(testdata1$DayType == "B"), ]
set.seed(120)

trainingset1 <- sample(1:nrow(data1), 0.75*nrow(data1))
trainingset2 <- sample(1:nrow(data2), 0.75*nrow(data2))

trainingvalue1 <- testdata1[trainingset1, ]
trainingvalue2 <- testdata1[trainingset2, ]

trainingdata <- rbind(trainingvalue1,trainingvalue2)
#test set
test1 <- data1[-trainingset1, ]
test2 <- data2[-trainingset2, ]

testdata <- rbind(test1,test2)
### getting predicted values using random forest and Boruta method
#Random Forest
library(randomForest)
output.forest1 <- randomForest(DayType ~ TotalSales + WeekNum + DateY + Transactions + Units + AverageT + Rank1 + ViolentCrime + Ratio, data = testdata1)
randomForest::importance(output.forest1)
#Boruta method
library(Boruta)
boruta_output <- Boruta(DayType ~ ., data = na.omit(testdata1), doTrace = 2)
print(boruta_output)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed","Tentative")])
print(boruta_signif)
par(mfrow = c(1,1))
plot(boruta_output, cex.axis = .7, las = 2, xlab = "", main = "Variable Importance")

####building logistic Regression models
trainingdata <- as.data.frame(trainingdata)
class(trainingdata)
logisticmodel1 <- glm(DayType ~ WeekNum + DateY + Units + AverageT + Rank1 + ViolentCrime + Transactions + Ratio + TotalSales, data = trainingdata, family = binomial(link = "logit"))
AIC(logisticmodel1)
BIC(logisticmodel1)

logisticmodel2 <- glm(DayType ~ ViolentCrime + TotalSales + Units + Transactions, data = trainingdata, family = binomial(link = "logit"))
AIC(logisticmodel2)
BIC(logisticmodel2)
# logsiticmodel2 has higher accuracy
predicted <- predict(logisticmodel2,testdata)
#predicted scores back on dataset
testdata$predicted <- predicted


### test and graph the model
library(ggplot2)
ggplot(testdata1, aes(x = DayType, y = ViolentCrime))+
  geom_point() + stat_smooth(method = "glm",se = FALSE)
ggplot(testdata1, aes(x = DayType, y = ViolentCrime))+
  geom_point(position = "jitter") + stat_smooth(method = "glm",se = FALSE)
par(mfrow = c(1,1))
plot(DayType ~ ViolentCrime, data = trainingdata)
#analysis of variance
anova(logisticmodel2, test = "Chisq")
#high p-value supports the fact that violentcrime, Transactions and totalsales are significant predictors
anova(logisticmodel1,logisticmodel2, test = "Chisq")
summary(logisticmodel2)
# good small value of AIC, very high difference between null and residual deviance(all of this supports our model)
par(mfrow = c(1,1))
#finding and visualizing the probability of A or B Day
hist(logisticmodel2$fitted.values, main = "Histogram", xlab = "Probability of A or B Day", col = 'light green')
#confusion matrix - plots observed vs predicted values to show accuracy
trainingdata$predict <- ifelse(logisticmodel2$fitted.values > 0.50, "B","A")
mytable1 <- table(trainingdata$DayType,trainingdata$predict)
rownames(mytable1) <- c("Pred.neg","Pred.pos")
colnames(mytable1) <- c("Pred.neg","Pred.pos")
mytable1
#Same for observed versus predicted , so very high accuracy!
efficiency <- sum(diag(mytable1))/sum(mytable1)
efficiency
#1 = 100% efficiency
#final step for predicting logsiticmodel2 and scoring the values back on original dataset for comparison = 100% accuracy in this case
pred <- predict(logisticmodel2, testdata1, "link")
head(pred)
#new column for side by side comparison
testdata1$Class <- ifelse(pred > 0, "B","A")
#first and last 6 rows
head(testdata1)
tail(testdata1)





# we achieved high accuracy(AIC,BIC) but when the acuuracy is low we use the following code
library(InformationValue)
optcutoff <- optimalCutoff(testdata$DayType, predicted)[1]
modelsummary <- summary(logisticmodel2)
modelsummary$coefficients
AIC(logisticmodel2)
BIC(logisticmodel2)
