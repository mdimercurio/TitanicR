library(data.table)

train <- fread('./data/train.csv')

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

# Modeling ======================================

# Logistic Regression
#TODO Look into http://data.princeton.edu/R/glms.html
logRegModel1 <- glm( Survived ~ 
       +  Pclass + Sex + Age + Embarked , family = binomial, data=train)

logRegModel2 <- glm( Survived ~ 
                       +  PclassFact + Sex + Age + Embarked , family = binomial, data=train)

trainPredict <- predict.glm(logRegModel1, newdata=train, type="response")
trainPredict <- ifelse(trainPredict > 0.5, 1, 0)

t <- table(trainPredict, train$Survived, useNA="ifany")

errorRate <- (sum(t) - t[1,1] - t[2,2]) / sum(t)

errorRate
