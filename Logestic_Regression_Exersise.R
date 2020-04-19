## Logistic Regression is catogarical
## Outcome is 0 and 1 or Yes or No

framingha=read.csv("framingham.csv")
str(framingham)

framingham$male=as.factor(framingham$male)
framingham$education=as.factor(framingham$education)
framingham$currentSmoker=as.factor(framingham$currentSmoker)
framingham$prevalentStroke=as.factor(framingham$prevalentStroke)
framingham$prevalentHyp=as.factor(framingham$prevalentHyp)
framingham$diabetes=as.factor(framingham$diabetes)

sapply(framingham, function(x)sum(is.na(x)))

install.packages("DMwR")
library(DMwR)

data1=knnImputation(framingham)

sapply(data1, function(x)sum(is.na(x)))

###write.csv(data1,file="data1.csv", sep=",")

install.packages("caTools")

library(caTools)

sample=sample.split(data1,SplitRatio = 0.7)

train=subset(data1,sample==TRUE)
test=subset(data1,sample==FALSE)

glm_out=glm(TenYearCHD~.,data = train,family = binomial)
summary(glm_out)

install.packages("MASS")
library(MASS)

step(glm_out,direction="both")


glm_out1=glm(formula = TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + 
      totChol + sysBP + glucose, family = binomial, data = train)
summary(glm_out1)

glm_out2=stepAIC(glm_out)
summary(glm_out2)

predict_train=predict(glm_out1,newdata=train,type="response")

predict_test=predict(glm_out1,newdata=test,type="response")



predict_test

predict_train


confusion_matrix=table(train$TenYearCHD,predict_train>0.5)

accuracy=(confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)
accuracy                                              

predict_test=predict(glm_out1,type="response",newdata = test)

t2=table(test$TenYearCHD,predict_test>0.5)
t2

accuracy_test=(t2[1,1]+t2[2,2])/sum(t2)

accuracy_test
