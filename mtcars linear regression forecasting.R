library(forecast)
library(tidyverse)
library(readxl)
library(datasets)
library(ggplot2)
mtcars
str(mtcars)
summary(mtcars)
mtcars1 <- replace(mtcars,TRUE , lapply(mtcars, na.aggregate)) # removes the NA's
summary(mtcars1)
cor(mtcars$mpg,mtcars$cyl) #strong
cor(mtcars$mpg,mtcars$wt) #strong
cor(mtcars$mpg,mtcars$hp) #strong
#all strong negative coorelation 
cor(mtcars$mpg,mtcars$gear) #medium positive coorelation
cor(mtcars$mpg,mtcars$carb) #medium negative coorelation
cor(mtcars$cyl,mtcars$wt) #strong positive coorelation


##### building the linear regression models  #####
linearRegModel1 <- lm(mpg ~ cyl, data = mtcars)
linearRegModel2 <- lm(mpg ~ wt, data = mtcars)
linearRegModel3 <- lm(mpg ~ hp, data = mtcars)
linearRegModel4 <- lm(mpg ~ gear, data = mtcars)
linearRegModel5 <- lm(cyl ~ wt, data = mtcars)

##printing the models
print(linearRegModel1)
print(linearRegModel2)
print(linearRegModel3)
print(linearRegModel4)
print(linearRegModel5)

## getting the r-squared and p values
summary(linearRegModel1)
summary(linearRegModel2)
summary(linearRegModel3)
summary(linearRegModel4)
summary(linearRegModel5)

##analysis of the variance
anova(linearRegModel1)
anova(linearRegModel2)
anova(linearRegModel3)
anova(linearRegModel4)
anova(linearRegModel5)

## plotting the data and the linear trend line
ggplot(mtcars, aes(cyl,mpg))+
  geom_point() + geom_smooth(method = lm)
ggplot(mtcars,aes(wt,mpg))+
  geom_point() + geom_smooth(method = lm)
ggplot(mtcars,aes(hp,mpg))+
  geom_point() + geom_smooth(method = lm)
ggplot(mtcars, aes(gear,mpg))+
  geom_point() + geom_smooth( method = lm)
ggplot(mtcars, aes(wt,cyl))+
  geom_point() + geom_smooth(method = lm)

##get predicted values and append them to the dataset
predlinearRegModel1 <- predict(linearRegModel1, interval = "prediction")
final_mtcars <- cbind(mtcars, predlinearRegModel1)
predlinearRegModel2 <- predict(linearRegModel2, interval = "prediction")
final_mtcars <- cbind(mtcars, predlinearRegModel2)
predlinearRegModel3 <- predict(linearRegModel3, interval = "prediction")
final_mtcars <- cbind(mtcars, predlinearRegModel3)
predlinearRegModel4 <- predict(linearRegModel4, interval = "prediction")
final_mtcars <- cbind(mtcars,predlinearRegModel4)
predlinearRegModel5 <- predict(linearRegModel5, interval = "prediction")
final_mtcars <- cbind(mtcars,predlinearRegModel5)

## creating regression line with confidence interval
plot1 <- ggplot(final_mtcars,aes(cyl,mpg))+
  geom_point()+stat_smooth(method = lm)
plot2 <- ggplot(final_mtcars,aes(wt,mpg))+
  geom_point()+stat_smooth(method = lm)
plot3 <- ggplot(final_mtcars,aes(hp,mpg))+
  geom_point()+stat_smooth(method = lm)
plot4 <- ggplot(final_mtcars, aes(gear,mpg))+
  geom_point() + stat_smooth(method = lm)
plot5 <- ggplot(final_mtcars,aes(wt,cyl))+
  geom_point() + stat_smooth(method = lm)

## adding prediction intervals
plot1 + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "green", linetype = "dashed")
plot2 + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "green", linetype = "dashed")
plot3+ geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "green", linetype = "dashed")
plot4 + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "green", linetype = "dashed")
plot5 + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "green", linetype = "dashed")
