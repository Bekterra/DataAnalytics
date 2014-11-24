
#................Actual Code.....................#
# creating class for each of the marks code

Experiment1 <- read.csv("F:/course to study/data analytics/Project/project data/Experiment1Data.csv")
attach(Experiment1)

library(rpart)
index <- 1:nrow(Experiment1)

PredictorData = data.frame(L1_Marks_Class,L2_Marks_Class,L3_Marks_Class,S1_Marks_Class,S2_Marks_Class,S3_Marks_Class,NRC_CLASS)

#PredictorData = Group1_Data[,c(4,5,9,14,37)] # 14-> gender code, 9 -> age, 37-> p or f

trainDecisionTree <-  sample(1:nrow(PredictorData),nrow(PredictorData)/2)
testDecisionTree <- -trainDecisionTree

trainDataDecisionTree <- PredictorData[trainDecisionTree,]
testDataDecisionTree <- PredictorData[testDecisionTree,]

dtfit <- rpart(NRC_CLASS~L1_Marks_Class+L2_Marks_Class+L3_Marks_Class+S1_Marks_Class+S2_Marks_Class+S3_Marks_Class,data = trainDataDecisionTree, method="class")
summary(dtfit)

plot(dtfit, uniform=TRUE, compress=TRUE,main="classification")
text(dtfit, use.n=TRUE, all=TRUE, cex=.5)

post(dtfit, file = "G:/tree.ps", 
title = "Classification Tree for Marks")
dtpredict <-  predict(dtfit,testDataDecisionTree,type="class")
dtconfmat <- table(true = testDataDecisionTree$NRC_CLASS, pred = dtpredict)

dtconfmat
library(lattice)
library(ggplot2)
library(caret)
predicted <- dtpredict
confusionMatrix(testDataDecisionTree$NRC_CLASS,dtpredict)

