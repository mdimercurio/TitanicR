library(data.table)

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- fread('./data/train.csv')
test <- fread('./data/test.csv')

# Dataset visualization =========================

head(train)
str(train)

# Preprocessing =================================

train[,Survived:=as.factor(Survived)]
train[,PclassFact:=as.factor(Pclass)]
train[,Sex:=as.factor(Sex)]
train[,SibSpFact:=as.factor(SibSp)]
train[,ParchFact:=as.factor(Parch)]
train[,Embarked:=as.factor(Embarked)]

#TODO extract title from Name
#TODO create age bin (5/10/20)
#TODO create SibSp bin (0 vs 1+)
#TODO create Parch bin (0 vs 1+)
#TODO merge SibSp and Parch
#TODO extract info from Ticket
#TODO create Fare bin
#TODO extract info from Cabin

# Compute the error rate on the training set
computeError <- function(train, predictions) {
  t <- table(predictions, train$Survived, useNA="ifany")
  
  errorRate <- (sum(t) - t[1,1] - t[2,2]) / sum(t)
  return(errorRate)  
}
# Create a file ready for submission on Kaggles website
# and a file describing the model used
createSubmissionFile <- function(train, test, predictions, model) {
  
  SubmissionFileName <- paste('./output/Titanic Submission ', gsub('[[:punct:]]', '-', Sys.time()), '.csv', sep='')
  DescriptionFileName <- paste ('./output/Titanic Submission Description ', gsub('[[:punct:]]', '-', Sys.time()), '.txt', sep='')
  
  # Replace NAs predictions by 0
  numNAs <- sum(is.na(predictions))
  predictions[is.na(predictions)] <- 0
    
  submit <- data.frame(PassengerId = test$PassengerId, Survived = predictions)
  
  write.csv(submit, file = SubmissionFileName, row.names = FALSE)
  
  # Create a description file, including the model description and the number of NAs 
  write(as.character(model$call),file=DescriptionFileName,append=TRUE)
  write(paste('Number of NAs: ', numNAs),file=DescriptionFileName,append=TRUE)
  # TODO add error on training set
}

# Modeling ======================================

# Logistic Regression
#TODO Look into http://data.princeton.edu/R/glms.html
logRegModel1 <- glm( Survived ~ 
       +  Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, family = binomial, data=train)

trainPredict <- predict.glm(logRegModel1, newdata=train, type="response")
trainPredictBool <- ifelse(trainPredict > 0.6, 1, 0)

testPredict <- predict.glm(logRegModel1, newdata=test, type="response")
testPredictBool <- ifelse(testPredict > 0.5, 1, 0)

createSubmissionFile(train, test, testPredictBool, logRegModel1)
 
# Decision Tree

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

#plot(fit)
#text(fit)
#fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")

createSubmissionFile(train, test, Prediction, fit)
