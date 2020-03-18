library(MASS)
library(rpart) 
help(birthwt)
str(birthwt)
head(birthwt)
hist(birthwt$bwt,col="black")
table(birthwt$low)
table(birthwt$bwt)
attach(birthwt)
bw_subset <- subset(birthwt, select = c(age,lwt) )
boxplot(bw_subset, col=c("blue","red"),ylab="centimeters")
library(psych)
pairs.panels(birthwt[,c('age','lwt','race','smoke','low')])
cols  <- c('low','race','smoke','ui','ht')
birthwt[cols] <- lapply(birthwt[cols] , as.factor) #convert columns used for prediction to factors 
# separate training and test data...
set.seed(1)
train <- sample(1:nrow(birthwt), 0.75 *nrow(birthwt)) #using 75% of given data to train the model and rest for testing the result
test = birthwt[-train]
#building the model using rpart
library(rpart)
birthTree <- rpart(low ~ . - bwt, data= birthwt[train, ],method = 'class') #class = classification
# plot of birthTree
plot(birthTree, uniform= TRUE, main="Low BirthWeight Tree")
text(birthTree, use.n=TRUE, all=TRUE, cex=.8)

#better plot/rattle bug
library(rattle)
fancyRpartPlot(birthTree, main="Birth Weight")

#predicting model , training the model
birthPredict <- predict(birthTree, test, type = 'class')

#projecting it
library(gmodels)
CrossTable(x= test, y = birthPredict,prop.chisq=FALSE)
 
#############
# 2nd Model #
#############
library(class)

trainlabels <- birthwt[train, 1]
testlabels <- birthwt[-train, 1] 

newdata <- birthwt[-train,c('age','lwt','race','smoke','ptl','ht','ui','ftv')]
#knn - nearest model(classifies using nearest thing present and then assigns value)
library(class)
birth_pred2 <- knn(train = birthwt[train, ],test = birthwt[-train, ], cl = trainlabels, k = 2)
birth_pred2
#the confusing matrix
library(gmodels)
CrossTable(x = testlabels, y = birth_pred2, prop.chisq = FALSE)