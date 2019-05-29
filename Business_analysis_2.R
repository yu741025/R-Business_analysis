data <- read.csv("~/Desktop/BostonHousing.csv")
#1
summary(data)
#2
library(gpairs)
gpairs(data)
plot(data)
#3
#4
model1=lm(MEDV~CRIM+CHAS+RM,data)
summary(model1)$coefficient
summary(model1) 
#model:MEDV= -28.8 - 0.26CRIM + 3.76CHAS + 8.28RM
#5
data[which(data$CRIM==0.1,data$CHAS==0,data$RM==6.982),]
#6
library("forecast")
data.df=data[1:506, ] 
selected.var=c(1,4,6,13)
set.seed(145)# set seed for reproducing the partition
train.index = sample(c(1:506),354,replace=FALSE) #partition data
train.df =data.df[train.index, selected.var]
valid.df =data.df[-train.index, selected.var]
data.lm=lm(MEDV~CRIM+CHAS+RM,data= train.df)
summary(data.lm)
#7
#8
library("forecast")
data.df=data[1:506, ] 
selected.var=c(1,4,6,13)
set.seed(666)# set seed for reproducing the partition
train.index = sample(c(1:506),354,replace=FALSE) #partition data
train.df =data.df[train.index, selected.var]
valid.df =data.df[-train.index, selected.var]
data.lm=lm(MEDV~CRIM+CHAS+RM,data= train.df)
data.lm.pred1 =predict(model1, valid.df) 
data.lm.pred2 =predict(data.lm, valid.df) 
accuracy(data.lm.pred1, valid.df$MEDV)
accuracy(data.lm.pred2, valid.df$MEDV)
#9
data.lm.null <- lm(MEDV~1, data= train.df)
data.lms = lm(MEDV ~ ., data = train.df)
summary(data.lms)
data.lms.step <- step(data.lm.null, scope=list(lower=data.lm.null, upper=data.lms), direction = "forward") #forward
summary(data.lms.step)
data.lms.step.pred=predict(data.lms.step, valid.df)

accuracy(data.lm.pred1, valid.df$MEDV)
accuracy(data.lm.pred2, valid.df$MEDV)
accuracy(data.lms.step.pred, valid.df$MEDV)
