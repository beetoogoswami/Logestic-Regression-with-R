rm(list = ls())

bank=read.table("bank.txt",header = TRUE,sep=";")

bank$outcome=ifelse(bank$y=="yes",1,0)

bank$outcome=as.factor(as.character(bank$outcome))

bank=subset(bank,select=-c(y))

set.seed(123)

sample=sample.split(bank$outcome,SplitRatio = .7)
train=subset(bank,sample==TRUE)
test=subset(bank,sample==FALSE)

glm_out=glm(outcome~.,family = "binomial",data=train)
summary(glm_out)

lm_out=lm(oucom~.,)

glm_aic=stepAIC(glm_out,direction = "both")

summary(glm_aic)

predict_glm_train=predict(glm_aic,type = "response",newdata = train)
predict_glm_test=predict(glm_aic,type="response",newdata = test)

cond1=ifelse(predict_glm_train>0.5,1,0)



t1=table(train$outcome,cond1)
t1
t1[2,2]
accurecy1=sum(t1[1,1],t1[2,2])/sum(t1)
accurecy1

recall1=t1[2,2]/sum(t1[2,])
recall1
precision1=t1[2,2]/sum(t1[,2])
precision1


cond2=ifelse(predict_glm_test>0.5,1,0)

t2=table(test$outcome,cond2)
t2

accurecy2=sum(t2[1,1],t2[2,2])/sum(t2)
accurecy2

recall2=t2[2,2]/sum(t2[2,])
recall2
precision2=t2[2,2]/sum(t2[,2])
precision2

####################################
#Imortant ROC and AOC
#######################################

install.packages("pROC")
library(pROC)

roc_info=roc(train$outcome,glm_aic$fitted.values,plot = TRUE,print.auc=TRUE,axis=TRUE)

roc_df=data.frame(tpp=roc_info$sensitivities*100,fpp=(1-roc_info$specificities)*100,
                  threshold=roc_info$thresholds)
roc_df
head(roc_df)                  

tail(roc_df)

install.packages("pROC")
library(pROC)

roc_cord=coords(roc_info)

coords(roc_info, "best", ret="threshold", transpose = FALSE)

coords(roc_info, "best", ret="all", transpose = FALSE)


# Checking correlation between the indepndent and Dependent

install.packages("corrplot")
library(corrplot)

bank1=subset(bank,select=-c(job,marital,education,default,housing,loan,contact,month,poutcome,outcome))

cor_relation=cor(bank1,use="everything")

cor_relation

corrplot(cor_relation,method = "circle",type = "upper",is.corr = TRUE)

pairs(bank1)

cor(bank$balance,bank$duration)

install.packages("DMwR")
library(DMwR)
regr.eval(test$outcome,glm_aic$fitted.values)


# library car

library(car)

vif(glm_aic)

#########
# alternate way to create confusion Matrics

install.packages("caret")
library(caret)

cond1=ifelse(predict_glm_train>0.5,1,0)

install.packages("e1071")
library(e1071)

t1=table(train$outcome,cond1)

confusionMatrix(t1)
confusionMatrix(t2)
