banks<-banks
ebay<-eBayAuctions
#1.1
logit.reg=glm(Financial.Condition~TotExp.Assets+TotLns.Lses.Assets,data=banks,family="binomial")
logit.reg
logit.reg.pred <- predict(logit.reg, bank[,c(4,5)], type = "response"); fitted(logit.reg)
str(bank)
logit.reg.pred
#1.2
logit<-(-14.721 +89.834 *0.11 +8.371 *0.6) 
logit
odd<-prob/(1-prob)
odd
prob<-exp(logit)/(1+exp(logit))
prob
table(true=bank$Financial.Condition,pred=round(fitted(logit.reg)>0.5))
#1.3
library("ROCR")
ROCpred=prediction(fitted(logit.reg),bank$Financial.Condition)
ROCRperf = performance(ROCpred, "tpr", "fpr") # Performance function
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))
#2
#2.1
counts1 <- table(ebay$Competitive., ebay$Category)
barplot(counts1, main="Competitive and non-competitive auctions in each product category",
        xlab="categories",ylab = "Quantities" ,col=c("darkblue","red"),
        legend = rownames(counts1), beside=TRUE)
table<-table(ebay$Competitive.,ebay$endDay)
table
table2= apply(table,2,function(x){x*100/sum(x)})
table2
table3 <- table2[c(2,1), c(2, 6, 7, 5, 1,3,4)]
table3
barplot(table3, main="Proportion of Competitive Auctions",
        xlab="",ylab= "Proportion of Competitive Auctions(%)", col=c("orange","white"))
#2.2
ebay$PriceChange= (ebay$ClosePrice-ebay$OpenPrice)
str(ebay)
#2.3
set.seed(232)
train.index=sample(1:nrow(ebay),1183,replace=FALSE)
training=ebay[train.index,]  
testing=ebay[-train.index,] ##train the data
fit.logit.1=glm(Competitive.~.,data=training,
                family=binomial(link="logit"))
fit.logit.2= glm(Competitive.~Category+currency+sellerRating+Duration+endDay+ClosePrice+OpenPrice+PriceChange,data=testing,
                 family=binomial(link="logit"))
summary(fit.logit.1)
summary(fit.logit.2)
#2.4
table(true=training$Competitive.,pred=round(fitted(fit.logit.1)))

#2.5
##generate out-of-sample prediction 
pred.out.1=predict(fit.logit.1,newdata=testing,type="response") 
pred.out.2=predict(fit.logit.2,newdata=testing,type="response") 
#instead of in-sample fit, use out-of-sample prediction
ROCpred1=prediction(pred.out.1, testing$Competitive.) 
ROCpred2=prediction(pred.out.2, testing$Competitive.) 
summary(ROCpred1)
summary(ROCpred2)
performance(ROCpred1,"acc")
performance(ROCpred2,"acc")
#2.6
ROCRperf1 = performance(ROCpred1, "acc")
ROCRperf2 = performance(ROCpred2, "acc")
plot(ROCRperf1,col='red',lty=2,lwd=2)#red:training data
plot(ROCRperf2,add=T,col='green',lty=3,lwd=2)
#2.7
competitive.lm.null <- lm(Competitive.~1, data = training) 
competitive.lm = lm(Competitive.~ ., data = training)
competitive.lm.step <- step(competitive.lm, direction = "forward")  
summary(competitive.lm.step)
competitive.lm.step.pred=predict(competitive.lm.step, testing)
summary(competitive.lm.step.pred)
#2.8
#2.9
fit.logit.3= glm(Competitive.~Category+currency+sellerRating+Duration+endDay,data=ebay,
                 family=binomial(link="logit"))
fit.logit.4=glm(Competitive.~.,data=training,
                family=binomial(link="logit"))
#performance
pred.out.3=predict(fit.logit.3,newdata=ebay,type="response") 
pred.out.4=predict(fit.logit.4,newdata=ebay,type="response")
ROCpred3=prediction(pred.out.3, ebay$Competitive.) 
ROCpred4=prediction(pred.out.4, ebay$Competitive.) 
ROCRperf3 = performance(ROCpred3, "acc")
ROCRperf4 = performance(ROCpred4, "acc")
plot(ROCRperf3,col='red')#red chosen
plot(ROCRperf4,add=T,col='green') #greenall
