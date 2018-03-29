#Analysing Titanic data

#libraries
library(ggplot2)
library(dplyr)
require(rpart)
require(rpart.plot)


df_train<-read.csv("/home/reen/Desktop/Titanic/train.csv",header=TRUE,sep=",")
df_test<-read.csv("/home/reen/Desktop/Titanic/test.csv",header=TRUE,sep=",")

#stats about training and test data
summary(df_train)
summary(df_test)

#number of rows
n_row_train<-nrow(df_train)
n_row_test<-nrow(df_test)

#data types of various variables
str(df_train)

#Adding survived to test data
df_test$Survived<-0

df_train$Pclass<-as.factor(df_train$Pclass)
df_train$Survived<-as.factor(df_train$Survived)
df_test$Pclass<-as.factor(df_test$Pclass)
df_test$Survived<-as.factor(df_test$Survived)
na.omit(df_train)
na.omit(df_test)

# A basic decision tree visualisation
x.rp <- rpart(Survived ~ Age + Sex + Pclass  + SibSp + Parch +Fare + Embarked, data=df_train)
x.rp.pred <- predict(x.rp, type="class", newdata=df_test)
x.rp.prob <- predict(x.rp, type="prob", newdata=df_test)
summary(x.rp)
prp(x.rp, type = 4, extra = 100,cex=0.6)

#Number of male vs female eeople on board
df_gender <- df_train['Sex']
df_gender_plot<-table(df_gender['Sex'])
df_gender_plot<-as.data.frame(df_gender_plot)
ggplot(data=df_gender_plot, aes(x=Var1, y=Freq)) + geom_bar(stat="identity",fill="turquoise3") + geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5)


#People of different ages
df_age<-df_train['Age']
df_age_table<-table(df_age)
with(df_age, hist(Age,col="turquoise3",main="People of different ages"))
plt_age=qplot(df_train$Age,geom='histogram',fill=I('pink'),xlab='Age', 
            ylab='Number People of different ages')


#Survival rate of people from different ages
survival <- data.frame(Age = df_train$Age, Survived = df_train$Survived)
ggplot(survival, aes(Age,fill = factor(Survived))) +
  geom_histogram()


#Age and Sex Violin Plot
p<-ggplot(df_train,aes(factor(Sex),Age)) + geom_violin(aes(fill=Survived))

#Scatter plot for Pclass,Age and Fare 
df_train$Pclass=as.factor(df_train$Pclass)
ggplot(data=df_train,mapping = aes(x=Age,y=Fare,shape=Pclass,color=Survived)) +geom_point()

#Pair wise Scatter Plot
pairs(df_train[, -c(1, 2)], col = as.numeric(df_train[, 2]) + 2, pch = 20)



