#1/(1)
summary(sat)

install.packages(psych)
library(psych)
describe(sat)

install.packages(gpairs)
library(gpairs)
gpairs(sat)

#1/(2)
table(sat$num.child)
barchart(table(sat$num.child),xlab="num.child",ylab="count",horizontal = FALSE, col="pink")

#1/(3)
boxplot(sat$overall, xlab="overall satisfaction ",
        main="overall satisfaction", horizontal=TRUE)

#1/(4)
cor(sat[,2:8])

#1/(5)
gpairs(sat)

#1/(6)
table(sat$weekend)
barchart(table(sat$weekend),xlab="weekend", horizontal = FALSE)
chisq.test(table(sat$weekend))

#²Ä¤GÃD

#2/(1)
summary(laptop)
describe(laptop)

#2/(2)
table(laptop$Retail.Price)
barchart(table(laptop$Retail.Price),xlab="Retail Price" ,horizontal=FALSE)
chisq.test(table(laptop$Retail.Price))

#2/(3)
laptop.mean <- aggregate(Retail.Price~Store.Postcode, data=laptop, mean) 
library(lattice)
barchart(Retail.Price~Store.Postcode, data=laptop.mean, col="pink")

#2/(4)
laptop.aov <- aov(Retail.Price ~ Store.Postcode, data=laptop)
anova(laptop.aov)

#2/(5)
boxplot(Retail.Price~Store.Postcode,data=laptop, yaxt='n', ylab='Retail Prices')

#2/(6)
table(laptop$Store.Postcode)
barchart(table(laptop$Store.Postcode),horizontal = FALSE)
chisq.test(table(laptop$Store.Postcode))

#2/(7)
laptop$Configuration=as.character(laptop$Configuration)
table(laptop$Configuration,laptop$Store.Postcode)

#2(8)
by(laptop$Retail.Price, laptop$Store.Postcode, sum)