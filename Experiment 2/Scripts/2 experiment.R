# Subjected to minor changes

Group1_Data <- read.csv("F:/course to study/data analytics/Project/project data/Experiment2Data.csv", header=TRUE)
attach(Group1_Data)

# perform multi linear regression to identify different values of R^2. 
# Main aim to find a group of subjects which can successfully represent all the subjects
# in the experiment.

# cleaning the data to remove the N/A elements to perform linear regression


MarksInL1 <- L1_MARKS[L1_MARKS != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"]
MarksInL2 <- L2_MARKS_CORRECTED[L1_MARKS != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"]
MarksInL3 <- L3_MARKS_CORRECTED[L1_MARKS != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"]
MarksInS1 <- S1_MARKS_CORRECTED[L1_MARKS != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"]
MarksInS2 <- S2_MARKS_CORRECTED[L1_MARKS != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"]
MarksInS3 <- S3_MARKS_CORRECTED[L1_MARKS != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"]

Class <- NRC_CLASS[L1_MARKS != "N/A" & L2_MARKS_CORRECTED != "N/A"
                   & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                   & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"]

RESULT <- NRC_RESULT[L1_MARKS != "N/A" & L2_MARKS_CORRECTED != "N/A"
                     & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                     & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"]

Total <- TOTAL_MARKS[L1_MARKS != "N/A" & L2_MARKS_CORRECTED != "N/A"
                     & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                     & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"]

# Converting all the variables to numeric which are factor by default in excel

MarksInL1 <- as.numeric(MarksInL1)
MarksInL2 <- as.numeric(MarksInL2)
MarksInL3 <- as.numeric(MarksInL3)
MarksInS1 <- as.numeric(MarksInS1)
MarksInS2 <- as.numeric(MarksInS2)
MarksInS3 <- as.numeric(MarksInS3)
Total <- as.numeric(Total)

#Using formula for multi linear regression

    
linear_regression_Model.synergy <- lm(Total~MarksInL3+MarksInS2+MarksInS3,data=Group1_Data)
#Display summary of regression
summary(linear_regression_Model.synergy)

#coplot(LinearL1_Marks~LinearS1_Marks|LinearS2_Marks,panel = panel.smooth,Group1_Data)
plot(linear_regression_Model)

#Since we have to classify the result on the basis of 1,2, PASs, Fail we will classify marks 
# using KNN algo.

# Variables for different NRC_CLASS
# For L1Code

MarksInL1Distinct <- L1_MARKS[L1_MARKS != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]

MarksInL1First <- L1_MARKS[L1_MARKS != "NA" & L2_MARKS_CORRECTED !=
                  "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                  & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                  & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                    S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]

MarksInL1Second <- L1_MARKS[L1_MARKS != "NA" & L2_MARKS_CORRECTED !=
                                        "NA" & L3_MARKS_CORRECTED != "NA" & L1_MARKS != "N/A" 
                                      & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                      & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                        S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInL1Pass <- L1_MARKS[L1_MARKS != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_MARKS != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" &  NRC_CLASS == "PASS"]

MarksInL1Fail <- L1_MARKS[L1_MARKS != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]


# For L2Code

MarksInL2Distinct <- L2_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]

MarksInL2First <- L2_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]


MarksInL2Second <- L2_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                        "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                      & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                      & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                        S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInL2Pass <- L2_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "PASS"]

MarksInL2Fail <- L2_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]


# For L3Code
MarksInL3Distinct <- L3_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]

MarksInL3First <- L3_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]

MarksInL3Second  <- L3_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                           "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                         & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                         & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                           S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInL3Pass <- L3_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                         "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                       & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                       & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                         S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "PASS"]

MarksInL3Fail <- L3_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                         "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                       & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                       & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                         S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]

# For S1Code

MarksInS1Distinct <- S1_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]
MarksInS1First <- S1_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]

MarksInS1Second <- S1_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                           "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                         & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                         & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                           S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInS1Pass <- S1_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "PASS"]

MarksInS1Fail <- S1_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]


# For S2Code

MarksInS2Distinct <- S2_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]

MarksInS2First <- S2_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]

MarksInS2Second <- S2_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                        "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                      & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                      & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                        S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInS2Pass <- S2_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "PASS"]

MarksInS2Fail <- S2_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]


# For S3Code

MarksInS3Distinct <- S3_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]

MarksInS3First <- S3_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]

MarksInS3Second <- S3_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                        "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                      & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                      & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                        S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInS3Pass <- S3_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "PASS"]

MarksInS3Fail <- S3_MARKS_CORRECTED[L1_Marks != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]


# Since the length of the variables may differ depanding on # of student belonging to each 
# we'll take samples

#Sampling for each of the variables taking 3000 entries for each of the variable



# For L1Code

MarksInL1DistinctSample <- sample(MarksInL1Distinct , 1000)
MarksInL1FirstSample <- sample(MarksInL1First , 1000)
MarksInL1SecondSample <- sample(MarksInL1Second , 1000)
MarksInL1PassSample <- sample(MarksInL1Pass , 1000)
MarksInL1FailSample <- sample(MarksInL1Fail , 1000)

# For L2Code

MarksInL2DistinctSample <- sample(MarksInL2Distinct , 1000)
MarksInL2FirstSample <- sample(MarksInL2First , 1000)
MarksInL2SecondSample <- sample(MarksInL2Second , 1000)
MarksInL2PassSample <- sample(MarksInL2Pass , 1000)
MarksInL2FailSample <- sample(MarksInL2Fail , 1000)

# For L3Code

MarksInL3DistinctSample <- sample(MarksInL3Distinct , 1000)
MarksInL3FirstSample <- sample(MarksInL3First , 1000)
MarksInL3SecondSample <- sample(MarksInL3Second , 1000)
MarksInL3PassSample <- sample(MarksInL3Pass , 1000)
MarksInL3FailSample <- sample(MarksInL3Fail , 1000)

# For S1Code

MarksInS1DistinctSample <- sample(MarksInS1Distinct , 1000)
MarksInS1FirstSample <- sample(MarksInS1First , 1000)
MarksInS1SecondSample <- sample(MarksInS1Second , 1000)
MarksInS1PassSample <- sample(MarksInS1Pass , 1000)
MarksInS1FailSample <- sample(MarksInS1Fail , 1000)

# For S2Code
MarksInS2DistinctSample <- sample(MarksInS2Distinct , 1000)
MarksInS2FirstSample <- sample(MarksInS2First , 1000)
MarksInS2SecondSample <- sample(MarksInS2Second , 1000)
MarksInS2PassSample <- sample(MarksInS2Pass , 1000)
MarksInS2FailSample <- sample(MarksInS2Fail , 1000)

# For S3Code

MarksInS3DistinctSample <- sample(MarksInS3Distinct , 1000)
MarksInS3FirstSample <- sample(MarksInS3First , 1000)
MarksInS3SecondSample <- sample(MarksInS3Second , 1000)
MarksInS3PassSample <- sample(MarksInS3Pass , 1000)
MarksInS3FailSample <- sample(MarksInS3Fail , 1000)


#cbind classes
Distinct = cbind(MarksInL3DistinctSample,MarksInS2DistinctSample,MarksInS3DistinctSample)
First = cbind(MarksInL3FirstSample,MarksInS2FirstSample,MarksInS3FirstSample)
Second = cbind(MarksInL3SecondSample,MarksInS2SecondSample,MarksInS3SecondSample)
Pass = cbind(MarksInL3PassSample,MarksInS2PassSample,MarksInS3PassSample)
Fail = cbind(MarksInL3FailSample,MarksInS2FailSample,MarksInS3FailSample)

# train

train = rbind(Distinct,First,Second,Pass,Fail) 

# classify each of the values

c1 = factor(c(rep("Distinct",1000),rep("First",1000),rep("Second",1000),rep("Pass",1000),rep("Fail",1000)))


test=c(10,10,10)

library(class)

# call knn and get its summary

summary(knn(train,test,c1,k=1))

# The End

# Using SVM algorithm 

library(e1071)
Group1_Data <- read.csv("F:/course to study/data analytics/Project/project data/Group1_Data.csv",header=T,sep=',')
attach(Group1_Data)
dataSVM <- data.frame(Group1_Data$L3_MARKS_CORRECTED,Group1_Data$S2_MARKS_CORRECTED,Group1_Data$S3_MARKS_CORRECTED,Group1_Data$NRC_CLASS)
dataFrame <-na.omit(dataSVM)
#index<- 1:nrow(dataSVM)
index=sample(1:nrow(dataFrame),1000)
indexTest = sample(1:nrow(dataFrame[-index,]),2000)
length(index)

#testindex <- sample(index , trunc(length(index)/3))
testrecords <- dataFrame[indexTest,]
trainrecords <- dataFrame[index,]
svmmodel <- svm(trainrecords$Group1_Data.NRC_CLASS~., data = trainrecords,cost = 100, gamma = 1)

svmpredict <- predict(svmmodel, testrecords[,-4])

svmconfmat <- table(true=testrecords[,4],pred = svmpredict)
#plot(testrecords[,7] , svmpredict)
svmconfmat
library(lattice)
library(ggplot2)
library(caret)
test <- testrecords[,4]
confusionMatrix(svmpredict,test)

confusionMatrix(svmpredict, testrecords[,4])
