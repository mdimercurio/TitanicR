# Load libraries ================================

library(data.table)
library(rpart)
library(ggplot2)
library(RColorBrewer)
library(rattle)

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

# Read the data =================================

train <- read.csv('./data/train.csv')
train$SurvivedFact <- as.factor(train$Survived)

test <- read.csv('./data/test.csv')

# Exploratory analysis ==========================
# Understanding your data

fix(train)
summary(train)

# Count number of NAs
apply(train, 2, function(x) length(which(is.na(x))))
apply(test, 2, function(x) length(which(is.na(x))))

# Plot distributions
qplot(train$Fare, binwidth=1)
#qplot(train$Age, binwidth=1)

# Try to identify relevant variables
table(train$Survived, train$Sex)
prop.table(table(train$Survived, train$Sex),2)

aggregate(Survived ~ Sex, data=train, FUN=function(x) {round(sum(x)/length(x),digits=2)})
aggregate(Survived ~ Pclass, data=train, FUN=function(x) {round(sum(x)/length(x),digits=2)})
aggregate(Survived ~ Pclass + Sex, data=train, FUN=function(x) {round(sum(x)/length(x),digits=2)})

# Scatter plots
#qplot(Age, Fare, colour=SurvivedFact, data=train, geom="point", alpha = I(0.5)
#      , ylim=c(0, 300))

# Box plot
#ggplot(train, aes(x=SurvivedFact, y=Fare, fill=SurvivedFact)) + geom_boxplot() + 
#  guides(fill=FALSE) + coord_flip() + ylim(0, 200)

# Density plots
#ggplot(train, aes(x=Age, fill=SurvivedFact)) + geom_density(alpha=.3)
#ggplot(train, aes(x=Fare, fill=SurvivedFact)) + geom_density(alpha=.3)

# Create a file ready for submission on Kaggles website
# and a file describing the model used

# Logistic Regression ===========================
# Create a model
glm.fit <- glm( SurvivedFact ~ 
                  +  Sex + Pclass, family = binomial, data=train)

summary(glm.fit)

# Apply the model to the training set
trainPredict <- predict(glm.fit, type="response")
head(trainPredict)
# Convert the result to binary response
cutoff <- 0.5
trainPredict <- ifelse(trainPredict > cutoff, 1, 0)

# Evaluate the prediction on the training set
table(trainPredict, train$SurvivedFact, dnn=c('Predict','Actual'))
# Error rate
mean(trainPredict == train$SurvivedFact)

testPredict <- predict.glm(glm.fit, newdata=test, type="response")
testPredict <- ifelse(testPredict > cutoff, 1, 0)

createSubmissionFile(train, test, testPredict, glm.fit)

# Comparing two models ==========================

glm.fit1 <- glm( SurvivedFact ~ 
                   +  Sex + Pclass, family = binomial, data=train)
glm.fit2 <- glm( SurvivedFact ~ 
                   +  Sex * Pclass, family = binomial, data=train)

summary(glm.fit1)
summary(glm.fit2)

anova(glm.fit1, glm.fit2, test = "Chisq")

# Decision Tree =================================

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

plot(fit)
text(fit)
fancyRpartPlot(fit)
trainPrediction <- predict(fit, type = "class")

# Error rate
mean(trainPrediction == train$SurvivedFact)

Prediction <- predict(fit, test, type = "class")

createSubmissionFile(train, test, Prediction, fit)

# Going further =================================
# Cross validation
# Random forest
# Feature engineering