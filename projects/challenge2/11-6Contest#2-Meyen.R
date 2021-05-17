library(mlr)
library(caret)
library(caTools)
library(SDMTools)
library(Metrics)
set.seed(1)

#load training data
data <- read.csv("studentperf.csv")

#load HOLDOUT data
hold <- read.csv("HOLDOUT.csv")
#change categoricals to numeric in train--------------------

#GP = 1, MS = 0
data$school <- ifelse(data$school=='GP',1,0)
#Male=1, Female = 0
data$sex <- ifelse(data$sex=='M',1,0)
#rural = 0, urban = 1
data$address <- ifelse(data$address=="U",1,0)
#family size LE3 = 0, GE3= 1
data$Fsize <- ifelse(data$Fsize=="GT3",1,0)
#parental status T == 1, A == 0
data$Pstatus <- ifelse(data$Pstatus=="T",1,0)
#school support
data$schoolsup <- ifelse(data$schoolsup=="yes",1,0)
#family support
data$famsup <- ifelse(data$famsup=="yes",1,0)
#extra paid classes = yes=1
data$paid <- ifelse(data$paid=="yes",1,0)
#extracuricular 
data$activities <- ifelse(data$activities=='yes',1,0)
#nurseryschool
data$nschool<- ifelse(data$nschool=='yes',1,0)
#desire for higher ed
data$higher<-ifelse(data$higher=='yes',1,0)
#homeinternet
data$internet<-ifelse(data$internet=='yes',1,0)
#romantic relatonship
data$relationship<-ifelse(data$relationship=='yes',1,0)

#create dummies for the non binary features
data2 <- createDummyFeatures(data, method = "reference")


#change categoricals to numeric in HOLDOUT--------------------
  
  #GP = 1, MS = 0
hold$school <- ifelse(hold$school=='GP',1,0)
#Male=1, Female = 0
hold$sex <- ifelse(hold$sex=='M',1,0)
#rural = 0, urban = 1
hold$address <- ifelse(hold$address=="U",1,0)
#family size LE3 = 0, GE3= 1
hold$Fsize <- ifelse(hold$Fsize=="GT3",1,0)
#parental status T == 1, A == 0
hold$Pstatus <- ifelse(hold$Pstatus=="T",1,0)
#school support
hold$schoolsup <- ifelse(hold$schoolsup=="yes",1,0)
#family support
hold$famsup <- ifelse(hold$famsup=="yes",1,0)
#extra paid classes = yes=1
hold$paid <- ifelse(hold$paid=="yes",1,0)
#extracuricular 
hold$activities <- ifelse(hold$activities=='yes',1,0)
#nurseryschool
hold$nschool<- ifelse(hold$nschool=='yes',1,0)
#desire for higher ed
hold$higher<-ifelse(hold$higher=='yes',1,0)
#homeinternet
hold$internet<-ifelse(hold$internet=='yes',1,0)
#romantic relatonship
hold$relationship<-ifelse(hold$relationship=='yes',1,0)

#create dummies for the non binary features
hold <- createDummyFeatures(hold, method = "reference")

#data3 <- subset(data2, !(data2$Grade<2))


#create training/test----------------------
#samplesetz = sample.split(data3,SplitRatio = .75)
#trainz <- subset(data3,sampleset==TRUE)
#testz <- subset(data3,sampleset==FALSE)

#sampleset = sample.split(data2,SplitRatio = .75)
#train <- subset(data2,sampleset==TRUE)
#test <- subset(data2,sampleset==FALSE)

#model-------

#xgboost
parametersGrid <-  expand.grid(eta = 0.1, 
                               colsample_bytree=c(0.5,0.7),
                               max_depth=c(3,6),
                               nrounds=100,
                               gamma=1,
                               min_child_weight=2,
                               subsample = .8)

ControlParamteres <- trainControl(method = "cv",
                                  number = 5)
modelxgboost <- train(Grade~., 
                      data = data2,
                      method = "xgbTree",
                      trControl = ControlParamteres,
                      tuneGrid=parametersGrid)

xgpredict<-predict(modelxgboost,hold)
xgpredict

results<-cbind(hold$Grade, predict(modelxgboost,hold))

#MSE
mse(hold$Grade,predict(modelxgboost,hold))