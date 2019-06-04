#R_HW4_Group5

#105303037
#105303039
#105303095
#105304046


#Part1
#(a)
UniversalBank=read.csv("UniversalBank.csv")
UniversalBank=UniversalBank[,-c(1,5)] 
Under=ifelse(UniversalBank$Education==1,1,0) 
Grad=ifelse(UniversalBank$Education== 2,1,0) 
UniversalBank=cbind(UniversalBank,Under,Grad)
UniversalBank_2=UniversalBank[,-6] 
#(b)
library(caTools)
set.seed(1111)
table(UniversalBank_2$Personal.Loan) 
spl_1 = sample.split(UniversalBank_2$Personal.Loan, SplitRatio=0.7) 
Train_1 = subset(UniversalBank_2, spl_1==TRUE) 
Test_1 = subset(UniversalBank_2, spl_1==FALSE)
table(Train_1$Personal.Loan) 
#(c)
fit.logit_1=glm(Personal.Loan ~ Age + Experience + Income + Family +            
                  CCAvg + Mortgage + Securities.Account + CD.Account + 
                  Online + CreditCard + Under + Grad, data=Train_1,
                family=binomial(link="logit"))
logit.out_1=predict(fit.logit_1,newdata=Test_1,type="response")

pred.logit_1=prediction(logit.out_1,Test_1$Personal.Loan)
#(d)

UniversalTree = rpart(Personal.Loan ~ Age + Experience + Income + Family +            
                        CCAvg + Mortgage + Securities.Account + CD.Account + 
                        Online + CreditCard + Under + Grad, data=Train_1, method="class", minbucket=25, parms = list(split="information"))  
prp(UniversalTree)
prp(UniversalTree, type = 1, extra = 1, split.font = 1, varlen = -10)
library(rpart)
UniversalBankTree = rpart(Personal.Loan ~ Age + Experience + Income + Family +  CCAvg + Mortgage + Securities.Account + CD.Account + Online + CreditCard  + Under + Grad ,  data = Train_1, method="class", minbucket=25, parms = list(split="information"))
plot(UniversalBankTree)
text(UniversalBankTree,pretty=0)

prp(UniversalBankTree) 
prp(UniversalBankTree, type = 1, extra = 1, split.font = 1, varlen = -10)


#e
rf.UniversalBank=randomForest(as.factor(Personal.Loan)~ Age + Experience + Income + Family +            
                                CCAvg + Mortgage + Securities.Account + CD.Account + 
                                Online + CreditCard + Under + Grad,
                              data=Train_1 ,mtry = 4,ntree=500,minbucket=25,importance=TRUE)
rf.pred = predict(rf.UniversalBank, newdata=Test_1)
varImpPlot(rf.UniversalBank); importance(rf.UniversalBank)
#f
gbm.UniversalBank=gbm(Personal.Loan ~ Age + Experience + Income + Family +            
                        CCAvg + Mortgage + Securities.Account + CD.Account + 
                        Online + CreditCard + Under + Grad,
                      data=Train_1, distribution="bernoulli",
                      n.trees=1500,interaction.depth=4,
                      shrinkage=0.05,cv.folds=5) 
summary(gbm.UniversalBank)
#g
summary(fit.logit_1)
#h
Predict_glm = predict(fit.logit_1, newdata = Test_1, type = "response")
Predict_tree = predict(UniversalTree , newdata = Test_1, type = "prob")
Predict_rf = predict(rf.UniversalBank, newdata = Test_1, type = "prob")
Predict_gbm=predict(gbm.UniversalBank, newdata = Test_1, type = "response")
pred_tree = prediction(Predict_tree[,2], Test_1$Personal.Loan)
pred_forest = prediction(Predict_rf[,2], Test_1$Personal.Loan)
pred_glm=prediction(Predict_glm,Test_1$Personal.Loan)
pred_gbm=prediction(Predict_gbm,Test_1$Personal.Loan)
library(ROCR)
plot(performance(pred_tree, "tpr", "fpr"),col='green',lty=3,lwd=3)
plot(performance(pred_forest, "tpr", "fpr"),add=T,col='red',lty=3,lwd=3)
plot(performance(pred_glm, "tpr", "fpr"),add=T,col='blue',lty=3,lwd=3)
plot(performance(pred_gbm, "tpr", "fpr"),add=T,col='yellow',lty=3,lwd=3)
abline(0,1,lty=2)
(i)
performance(pred_tree, "auc")
performance(pred_forest, "auc")
performance(pred_glm, "auc")
performance(pred_gbm, "auc")

#Part2
ToyotaCorolla_1=read.csv("ToyotaCorolla.csv")
#a
car.data = ToyotaCorolla_1
selected.var_1 = c(3, 4, 7, 9, 10, 12, 13, 14, 17, 18)
car.data = car.data[ 1:1436, selected.var_1] 
summary(car.data)
#b
set.seed(5566)
spl1 = sample.split(car.data$Price, SplitRatio = 0.7) 
Train1 = subset(car.data, spl1==TRUE) 
Test1 = subset(car.data, spl1==FALSE)


#c
car.lr=lm(log(Price)~.,data=Train1)
summary(car.lr)
#d
cartree=rpart(Price~.,data=Train1,minbucket=25,
              parms = list(split="information"))
prp(cartree)

#e
carrf=randomForest(Price~., data=Train1,mtry=4
                   , ntree=500, nodesize=25,importance=TRUE)
varImpPlot(carrf)
importance(carrf)
#f
library(gbm)
gbm.car=gbm(Price ~ Age_08_04+KM+HP+Met_Color+Automatic+CC+Doors+ Quarterly_Tax+Weight,data=Train1,
            distribution="gaussian",
            n.trees=5000,interaction.depth=3,
            shrinkage=0.05,cv.folds=10)
summary(gbm.car)
#g
summary(car.lr) #linear regression model
varImpPlot(carrf)
importance(carrf) #RF
summary(gbm.car)  #GBM

#h
car.lm=lm(Price~.,data=Train1)
carp.lm=predict(car.lm,newdata = Test1) #(用Price, 非log(Price))
carp.tree=predict(cartree,newdata = Test1)
carp.rf=predict(carrf,newdata = Test1)
print(gbm.car)  #The best cross-validation iteration was n.
carp.gbm=predict(gbm.car,newdata =Test1,n.trees =  ) #Using n trees...

# i
library(forecast)
accuracy(carp.lm,Test1$Price)
accuracy(carp.tree,Test1$Price)
accuracy(carp.rf,Test1$Price)
accuracy(carp.gbm,Test1$Price)

