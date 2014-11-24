Experiment7Data <- read.csv("F:/course to study/data analytics/Project/project data/Experiment1Data.csv")
attach(Experiment7Data)
library(rpart)
index <- 1:nrow(Experiment7Data)
#Experiment7Data <- na.omit(Experiment7Data)
#PredictorData = data.frame(Experiment7Data$L3_Marks_Class+Experiment7Data$S2_Marks_Class+Experiment7Data$S3_Marks_Class,Experiment7Data$NRC_CLASS)

#PredictorData = Group1_Data[,c(4,5,9,14,37)] # 14-> gender code, 9 -> age, 37-> p or f

trainDecisionTree <-  sample(1:nrow(Experiment7Data),nrow(Experiment7Data)/2)
testDecisionTree <- -trainDecisionTree

trainDataDecisionTree <- Experiment7Data[trainDecisionTree,]
testDataDecisionTree <- Experiment7Data[testDecisionTree,]


testYourDecisionTree <- Experiment7Data$NRC_CLASS[testDecisionTree]
#yahan se start karna 
dtfit <- rpart(NRC_CLASS~L1_Marks_Class+L2_Marks_Class+L3_Marks_Class+S1_Marks_Class+S2_Marks_Class+S3_Marks_Class,data = trainDataDecisionTree, method="class")

plot(dtfit, uniform=TRUE, compress=TRUE,main="classification")
text(dtfit, use.n=TRUE, all=TRUE, cex=.5)

post(dtfit, file = "G:/tree.ps", 
     title = "Classification Tree for Marks")
dtpredict <-  predict(dtfit,testDataDecisionTree,type="class")
dtconfmat <- table(true = testDataDecisionTree$NRC_CLASS, pred = dtpredict)
dtconfmat





# Using Association Rule mining 


# First we have cretaed a decision tree to identify how the combinaation of three marks
# will help us to identify the person is pass or fail overall


# Our next task is to validate it using the Association rule Mining

# We use the AssociationRules in order to to create rules feom the original data. 
Data <- Experiment7Data[,c(L1_Marks_Class,L2_Marks_Class,L3_Marks_Class,S1_Marks_Class,S2_Marks_Class,S3_Marks_Class,NRC_CLASS)]

library(arules)


rules <- apriori(Experiment7Data, parameter = list(minlen=2,supp=0.1,conf=0.9))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules

subset.matrix <- is.subset(rules.sorted , rules.sorted)
subset.matrix[lower.tri(subset.matrix , diag=T)] <- NA
redundant <- colSums(subset.matrix , na.rm=T) >= 1
which(redundant)

#remove redundant rules

rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#Visualizing Association Rules
#install.packages("arulesViz")

library(arulesViz)
plot(rules.pruned)



# Now we will try to generate association rules using the values which are predicted 
# by the decision tree. 
# And try to identify what are the similarities in between the Association rules 
# and decision tree
