#Part 1
EPI_data <- read.csv("~/Desktop/2010EPI_data.csv")
View(EPI_data)
names(EPI_data) <- as.matrix(EPI_data[1, ])
EPI_data <- EPI_data[-1, ]
EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character(x)))

#Load LEVELS in here somehow (ask question)

boxplot(EPI_data$ENVHEALTH,EPI_data$DALY,EPI_data$AIR_H,EPI_data$WATER_H) #not loading ENVHEALTH

lmENVH<-lm(EPI_data$ENVHEALTH~EPI_data$DALY+EPI_data$AIR_H+EPI_data$WATER_H)
cENVH<-coef(lmENVH)
new.speeds <- data.frame(
  speed = c(seq(5,95,5))
)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<-predict(lmENVH,newData = new.speeds,interval="prediction")
cENV<-predict(lmENVH,newData = new.speeds,interval="confidence") #FIX THIS

#AIR_E
boxplot(EPI_data$AIR_E,EPI_data$DALY,EPI_data$AIR_H,EPI_data$WATER_H)

lmENVH<-lm(EPI_data$AIR_E~EPI_data$DALY+EPI_data$AIR_H+EPI_data$WATER_H)
cENVH<-coef(lmENVH)
new.speeds <- data.frame(
  speed = c(seq(5,95,5))
)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<-predict(lmENVH,newData = new.speeds,interval="prediction")
cENV<-predict(lmENVH,newData = new.speeds,interval="confidence") #FIX THIS

#CLIMATE
boxplot(EPI_data$CLIMATE,EPI_data$DALY,EPI_data$AIR_H,EPI_data$WATER_H)

lmENVH<-lm(EPI_data$CLIMATE~EPI_data$DALY+EPI_data$AIR_H+EPI_data$WATER_H)
cENVH<-coef(lmENVH)
new.speeds <- data.frame(
  speed = c(seq(5,95,5))
)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<-predict(lmENVH,newData = new.speeds,interval="prediction")
cENV<-predict(lmENVH,newData = new.speeds,interval="confidence") #FIX THIS



#Part 2
abaloneData <- read.csv("~/Desktop/abalone.csv", header = FALSE, sep = ",")
dataset <- read.csv("~/Desktop/dataset_multipleRegression.csv")
unemploymentRates <- dataset[,3]
springGrads <- dataset[,4]
yearRate <- 7.0
yearHGrad <- 90000

install.packages("ggplot2")
library("ggplot2")
ggplot(dataset, mapping=aes(x=unemploymentRates, y=springGrads)) + geom_point()

#Task 1: Linear Regression
dataset.fit = lm(ROLL~unemploymentRates+springGrads, data=dataset)
summary(dataset.fit)


#Task 2: Classification


dim(abaloneData)
s_dataset = sample(150,100)
colnames(abaloneData) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight',
                           'rings' )

abaloneData$rings <- as.numeric(abaloneData$rings)
abaloneData$rings <- cut(abaloneData$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abaloneData$rings <- as.factor(abaloneData$rings)
summary(abaloneData$rings)

aba <- abaloneData
aba$sex <- NULL
normalize <- function(x) {
  return   ((as.numeric(x)-min(as.numeric(x))) / max((as.numeric(x)) - min(as.numeric(x))))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))


install.packages("class")
library(class)

summary(aba$shucked_wieght)
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918) 
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k  =55)
table(KNNpred)





#Task 3: 



library(datasets)
data(iris)
head(iris)
str(iris)

iris2 <- iris[-5]


sapply(iris2, var)
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + geom_point()

ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col = Species)) + geom_point()

set.seed(300)
k.max <- 12


wss <- sapply(1:k.max, function(k){kmeans(iris2,k,nstart = 20, iter.max = 1000)$tot.withinss})
wss
plot(1:k.max, wss, type = "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
Icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(Icluster$cluster,iris$Species) #compare your cluster to the column 5










