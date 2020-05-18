#Remove previous env details>>>
rm(list=ls())
#Import Data>>>
File1<-read.csv("D:\\Soft\\Data Science Files\\titanic.csv")
summary(File1)
#Missing value treatment and assigning value to NA>>>
hist(File1$age)
File1$age[is.na(File1$age)]=29
summary(File1)
head(File1)
#Creating New Variable or One hot encoding>>>
File1$female<-ifelse(File1$sex=="female",1,0)
File1$embarked_s<-ifelse(File1$embarked=="S",1,0)
File1$embarked_c<-ifelse(File1$embarked=="C",1,0)
summary(File1)

#Drop Categorical Values from the data>>>

Final_Data=File1[-c(3,4,9)]
head(Final_Data)


#Univariate Ananlysis>>>>
#age as variable>>
bx=boxplot(Final_Data$age)
bx$stats
quantile(Final_Data$age,seq(0,1,0.02))
#Trim data which are lying in outliers
Final_Data$age<-ifelse(Final_Data$age>=52,52,Final_Data$age)
Final_Data$age<-ifelse(Final_Data$age<=4,4,Final_Data$age)
boxplot(Final_Data$age)
#fare as variable>>
bx1=boxplot(Final_Data$fare)
bx1$stats
quantile(Final_Data$fare,seq(0,1,0.02))
Final_Data$fare<-ifelse(Final_Data$fare>=136,136,Final_Data$fare)
boxplot(Final_Data$fare)


#Bivariate Analysis>>>>
library("car")
scatterplot(Final_Data$age,Final_Data$survived)
scatterplot(Final_Data$fare,Final_Data$survived)


#Logistic Regression-Partioning the data>>>>

set.seed(222)
t=sample(1:nrow(Final_Data),0.7*nrow(Final_Data))
#',' is used for differentiating column
t_train=Final_Data[t,]
t_test=Final_Data[-t,]

library(car)
mod<-lm(survived~.,data=t_train)
t=vif(mod)
sort(t,decreasing = T)
mod1<-glm(as.factor(survived)~.,family = "binomial",data=t_train)
summary(mod1)
stpmod=step(mod1,direction = "both")
formula(stpmod)
summary(stpmod)


mod2<-glm(as.factor(survived) ~ pclass + age + sibsp + female + 
            embarked_c, family = "binomial", data = t_train)
summary(mod2)
stpmod=step(mod2,direction = "both")
formula(stpmod)
summary(stpmod)
#checking the probablity for each observation by creating a variable names>>
t_train$score=predict(mod2,newdata=t_train,type="response")
head(t_train$score)
tail(t_train$score)

?predict.glm

#Lets try to analyse the confusuin matrix and model accuracy
library(lattice)
library(ggplot2)
library(caret)
library(e1071)

prediction<-ifelse(t_train$score>=0.5,1,0)
confusionMatrix(as.factor(prediction),as.factor(t_train$survived),positive = "1")


#for Test>>>>>>
mod3<-lm(survived~.,data=t_test)
t=vif(mod3)
sort(t,decreasing = T)
mod4<-glm(as.factor(survived)~.,family = "binomial",data=t_test)
summary(mod4)
stpmod=step(mod1,direction = "both")
formula(stpmod)
summary(stpmod)


mod5<-glm(as.factor(survived) ~ pclass + age + sibsp + female + 
            embarked_c, family = "binomial", data = t_test)
summary(mod5)
stpmod=step(mod5,direction = "both")
formula(stpmod)
summary(stpmod)
#checking the probablity for each observation by creating a variable names>>
t_test$score=predict(mod5,newdata=t_test,type="response")
head(t_test$score)
tail(t_test$score)

?predict.glm

#Lets try to analyse the confusuin matrix and model accuracy
library(lattice)
library(ggplot2)
library(caret)
library(e1071)

prediction<-ifelse(t_test$score>=0.5,1,0)
confusionMatrix(as.factor(prediction),as.factor(t_test$survived),positive = "1")


library(InformationValue)
plotROC(actuals = t_train$survived,predictedScores = as.numeric(fitted(mod2)))
ks_plot(actuals = t_train$survived,predictedScores = as.numeric(fitted(mod2)))
ks_stat(actuals = t_train$survived,predictedScores = as.numeric(fitted(mod2)))




