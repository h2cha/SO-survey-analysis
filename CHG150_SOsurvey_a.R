library(tidyverse)
library(gridExtra)  # grid.arrange
#
library(caret)    # for data pre-processing; classification and regression training
library(pROC)     # for ROC Curves
library(ROCR)
library(doParallel)
#
source("source_dataMining2018F.R") 

####################################

#
jtable <- function(x) {a=table(x, exclude=NULL); print(a); sum(a)}  
ptable <- function(x) {a=table(x, exclude=NULL); print(round(prop.table(a), digits=2)); sum(a)} 
pmiss  <- function(x){sum(is.na(x))/length(x)*100}
myVar  <- function(Data, x) {which(names(Data) ==x)} 
hd     <- function(x) {head(as.data.frame(x))}

M <- read.csv("stack-overflow-2018-developer-survey/cleansed.csv")
M$Y <- M$ConvertedSalary
M$ConvertedSalary <- NULL


dummies <- dummyVars(Y ~ ., data = M)
X <- predict(dummies, newdata = M)
myM <- data.frame(X, Y = M$Y)

splitData2 <- splitDataFUN(myM)
trData2 <- splitData2[[1]]
dim(trData2)
teData2 <- splitData2[[2]]
dim(teData2)

my_trControl3 <- trainControl(method  = "repeatedcv",number  = 5,repeats = 6)

cores <- detectCores()
cl = makeCluster(cores-1)
registerDoParallel(cl)

model.rf2    <- caret::train(Y ~ ., method = "rf", data = trData, trControl = my_trControl3)
model.rpart2 <- caret::train(Y ~ ., method = "rpart", data = trData, trControl = my_trControl3)
model.svm2   <- caret::train(as.factor(Y) ~ ., method = "svmLinear", data = trData, trControl = my_trControl3)
model.nn2    <- caret::train(as.factor(Y) ~ ., method = "nnet", data = trData, trace = FALSE, maxit = 100, trControl = my_trControl3)


test_rocFUN(model.rf2, teData)
test_rocFUN(model.rpart2, teData)
test_rocFUN(model.svm2, teData)
test_rocFUN(model.nn2, teData)



confusionMatrix(model.rf2)
confusionMatrix(model.rpart2)
confusionMatrix(model.svm2)
confusionMatrix(model.nn2)


stopCluster(cl)