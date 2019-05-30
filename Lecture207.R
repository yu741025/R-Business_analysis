library(arules)
library(car)
library(arulesViz)
library(recommenderlab)

###example 1
library(arules)
data('Groceries') #embeded data in the package
inspect(head(Groceries,3))
summary(Groceries)
#density=0.026，代表矩陣密度低(客戶不可能一次購買169種產品)
#size 一張發票有幾個東西被購買
groc.rules=apriori(Groceries, parameter = list(supp=0.01,conf=0.3,target='rules'))
#設定supp與conf兩個的門檻
inspect(subset(groc.rules,lift>3))

###example 2
##raw data processing
#each transaction as one line in file
#read each line into retail.raw separate by " "
retail.raw <- readLines(con=file("retail.txt"))
head(retail.raw)
tail(retail.raw)
summary(retail.raw)
# convert the raw character lines into a list of item vectors
retail.list <- strsplit(retail.raw, " ")
names(retail.list) <- paste("Trans", 1:length(retail.list), sep="")
str(retail.list)
library(car)
some(retail.list)
rm(retail.raw)
#convert transaction list into transaction object for association rule: many rows of transaction{i1,i2}
retail.trans <- as(retail.list, "transactions")

##market basket anlaysis
inspect(head(retail.trans))
#what information we can get from summary()
summary(retail.trans)
rm(retail.list)

mkt.rules = apriori(retail.trans, parameter = list(supp=0.001,conf=0.4,target='rules'))
inspect(subset(mkt.rules,lift>3))
#sort the result by lift and see the first six rules
inspect(head(sort(mkt.rules, by='lift'),n=6))
#plot rule distribution
install.packages('arulesViz')
library(arulesViz)
plot(mkt.rules)
# interactive mode though not very handy
#one click to start region and second click to end region
#hit zoom in to see the subregion
#select a region agian
#hit inspect to show rules in that region
plot(mkt.rules,engine='interactive')
#可互動的

###example3: 
all.books.df <- read.csv("CharlesBookClub.csv")
#每個會員購買書的紀錄

## create a binary incidence matrix
count.books.df <- all.books.df[, 8:18]
#取出部分購買產品資料

incid.books.df <- ifelse(count.books.df > 0, 1, 0)
#只看有沒有購買，把大於0都視為1,其餘為0

#remove childBks for fewer rules found (simplicity)
incid.books.mat <- as.matrix(incid.books.df[,-1])
#先轉換格式

##  convert the binary incidence matrix into a transactions database
books.trans <- as(incid.books.mat, "transactions")
#轉換格式

inspect(head(books.trans))
summary(books.trans)
# plot data
itemFrequencyPlot(books.trans)
# run apriori function
rules <- apriori(books.trans, parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))
# inspect rules
inspect(sort(rules, by = "lift"))
#有些組合，並沒有太多資訊或重複，人為處理
test=head(sort(rules,by="lift"),20)
plot(test,method='graph',control=list(type='items'))
#利用網路圖，可以容易看出各產品的關係，甚至得到額外資訊，可分為幾類等
#購物籃分析較少統計分析理論，因此較不能外推

#購物籃並不是個人化分析，當購物清單可以與顧客身分連結，才可以進行個人化推薦(偕同過濾)
#若有交易與會員資料時也可以做mulitnomial regression
#如果不會做mult Reg 時，可個別購買當0,1做羅吉斯回歸


###Collaborative filtering
# load CSV version of jester subset
jester <- read.csv("jester-data-3.csv", header = FALSE)
dim(jester)
#值如果是99，代表沒有評分
#select 10000 samples
trows <- sample(nrow(jester), 10000)  #樣本太大
jester <- jester[trows,]
rm(trows)


#協同推薦
#first column contains a count of the number of jokes a user has rated, not rating
#第一欄是該使用對幾個笑話有評分
summary(jester$V1)
# remove first column since it does not contain user ratings
#拿掉第一欄
jester <- jester[,2:ncol(jester)]
# set all 99's to NA
#99代表沒有評分，去掉
jester[,][jester[,] == 99] <- NA
min(jester[][], na.rm = TRUE) #把NA去掉，最低分是幾分
max(jester[][], na.rm = TRUE) #把NA去掉，最高分是幾分
hist(as.vector(as.matrix(jester)),   #把資料轉換成每一個vector接在一起在用Hist畫出來
     main = "Distribution of Jester Ratings",
     col = "yellow", 
     xlab = "Ratings")

#不是NA值有幾個
length(na.omit(as.vector(as.matrix(jester))))

#number of rated items --> related to given inputs of new data
summary(apply(jester,1,function(x){length(na.omit(x))}))  
#(apply可選擇看資料的平均，ROW #1的意思#，要扣掉NA)


##user-based collaboratie filtering
library(recommenderlab)
# convert the jester data frame to a matrix
rmat <- as.matrix(jester)
# convert matrix to a recommenderlab realRatingMatrix: many rows of (user, item, preferences)
rmat <- as(rmat,"realRatingMatrix")
#使用這個套件的時候，要先轉換成rmat，因為資料太多NA，轉成(1,[1,3,5],[5,3,10])的向量
str(rmat) 

#calculate similarity
UB.Rec <- Recommender(rmat, "UBCF")   #(資料，使用方法:UBCF)
vignette("recommenderlab") #把使用者套件叫出來(PDF)
# get recommendations
pred <- predict(UB.Rec, rmat, type="ratings")

UB.Recl <- Recommender(rmat,"UBCF",param=list(method="pearson"))  #使用相關係數法(pearson)來算
#Can you verify your prediction?
as(pred, "matrix")[8,c(1:20)]  #看第8個人，1到20個笑話裡面的預測分數
jester[8,c(1:20)]
##推薦的的資料都是原始資料沒有的  (補上NA的值) 


##how to predict and verify during analysis
#for test data: given random 15 items as input
e <- evaluationScheme(rmat, method="split", train=0.8, given=15)
UB.Rec <- Recommender(getData(e, "train"), "UBCF") 
# compute predicted ratings
p1 <- predict(UB.Rec, getData(e, "known"), type="ratings")
# set all predictions that fall outside the valid range to the boundary values
p1@data@x[p1@data@x[] < -10] <- -10
p1@data@x[p1@data@x[] > 10] <- 10
#verify prediction
calcPredictionAccuracy(p1, getData(e, "unknown"))

##get recommendations:topNList
pred1 <- predict(UB.Rec, getData(e, "known"), n=10, type="topNList")
as(pred1, "matrix")[8,]
as(p1, "matrix")[8,]

##item-based collaborative filtering
IB.Rec <- Recommender(getData(e, "train"), "IBCF")
p2 <- predict(IB.Rec, getData(e, "known"), type="ratings")
# set all predictions that fall outside the valid range to the boundary values
p2@data@x[p1@data@x[] < -10] <- -10
p2@data@x[p1@data@x[] > 10] <- 10
#verify prediction
calcPredictionAccuracy(p2, getData(e, "unknown"))