setwd("C:/Users/PC/Desktop/Data Science - Ayub/Lab Activity/Logistic_Regression_Class_Material")

file=read.csv("FlierResponse.csv")
str(file)

file$Response=as.factor(file$Response)

sapply(file, function(x)sum(is.na(x)))

install.packages("caTools")
library(caTools)

set.seed(123)

sample=sample.split(file$Response,SplitRatio =.7)

train=subset(file,sample==TRUE)
test=subset(file,sample==FALSE)

glm_out=glm(Response~Age,family = "binomial", data=train )

plot(glm_out)

glm_out
summary(glm_out)

fitted_values=glm_out$fitted.values
resdual_valus=glm_out$residuals


pre=predict(glm_out,test)
pre
