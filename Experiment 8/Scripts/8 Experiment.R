Experiment8 <- read.csv("F:/course to study/data analytics/Project/project data/Experiment8.csv")
attach(Experiment8)

# partioning data on the basis of gender
# For boys

MarksInL1Boys <- L1_Marks_Corrected[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A" 
                                & NRC_GENDER_CODE=="B"]

MarksInL2Boys <- L2_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & NRC_GENDER_CODE=="B"]

MarksInL3Boys <- L3_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & NRC_GENDER_CODE=="B"]

MarksInS1Boys <- S1_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & NRC_GENDER_CODE=="B"]

MarksInS2Boys <- S2_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & NRC_GENDER_CODE=="B"]

MarksInS3Boys <- S3_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & NRC_GENDER_CODE=="B"]

NRCClassBoys <- NRC_CLASS[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                      & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                      & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                      & NRC_GENDER_CODE=="B"]


# Converting all the variables to numeric which are factor by default in excel

MarksInL1Boys <- as.numeric(MarksInL1Boys)
MarksInL2Boys <- as.numeric(MarksInL2Boys)
MarksInL3Boys <- as.numeric(MarksInL3Boys)
MarksInS1Boys <- as.numeric(MarksInS1Boys)
MarksInS2Boys <- as.numeric(MarksInS2Boys)
MarksInS3Boys <- as.numeric(MarksInS3Boys)


# Sampling 4000 entries out of the entire sheet

dataMarksBoys <- data.frame(MarksInL1Boys,MarksInL2Boys,MarksInL3Boys,MarksInS1Boys,MarksInS2Boys,MarksInS3Boys,NRCClassBoys) 

# preserving the indices out of the data

myBoys <- sample(1:nrow(dataMarksBoys),1000,replace=FALSE)

# Taking the values of all the marks and the corresponding class 
MarksBoys <- dataMarksBoys[myBoys,]

# Storing up data in individual variable

MarksInL1SampleBoys <- MarksBoys$MarksInL1Boys
MarksInL2SampleBoys <- MarksBoys$MarksInL2Boys
MarksInL3SampleBoys <- MarksBoys$MarksInL3Boys
MarksInS1SampleBoys <- MarksBoys$MarksInS1Boys
MarksInS2SampleBoys <- MarksBoys$MarksInS2Boys
MarksInS3SampleBoys <- MarksBoys$MarksInS3Boys
NRCClassSampleBoys <- MarksBoys$NRCClassBoys

dataBoys <- cbind(MarksInL1SampleBoys,MarksInL2SampleBoys,MarksInL3SampleBoys,MarksInS1SampleBoys,MarksInS2SampleBoys,MarksInS3SampleBoys)

dataFrameBoys <- data.frame(MarksInL1SampleBoys,MarksInL2SampleBoys,MarksInL3SampleBoys,MarksInS1SampleBoys,MarksInS2SampleBoys,MarksInS3SampleBoys)


dataClusterBoys <- model.matrix(~.+0, data=dataFrameBoys)

# Result associated with boys data
resultsBoys <- kmeans(dataClusterBoys,5)
resultsBoys
resultsBoys$size



## Summary statistics for cluster
# Visualization of different clusters

table(NRCClassSampleBoys , resultsBoys$cluster)

plot(Marks[c("MarksInL1Boys", "MarksInL2Boys")], xlim = c(0,150), ylim = c(0,150), col = resultsBoys$cluster)

plot(Marks[c("MarksInL1Boys", "MarksInL2Boys")], xlim = c(0,150), ylim = c(0,150), col = NRCClassSampleBoys)

library(cluster) 

clusplot(dataClusterBoys, resultsBoys$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
#plot of all the data been clutered
install.packages("fpc")
library(fpc)

plotcluster(dataClusterBoys, resultsBoys$cluster)

#....................................................................................#
# For Girls

MarksInL1Girls <- L1_Marks_Corrected[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A" 
                                    & NRC_GENDER_CODE=="G"]

MarksInL2Girls <- L2_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                    & NRC_GENDER_CODE=="G"]

MarksInL3Girls <- L3_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                    & NRC_GENDER_CODE=="G"]

MarksInS1Girls <- S1_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                    & NRC_GENDER_CODE=="G"]

MarksInS2Girls <- S2_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                    & NRC_GENDER_CODE=="G"]

MarksInS3Girls <- S3_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                    & NRC_GENDER_CODE=="G"]

NRCClassGirls <- NRC_CLASS[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                          & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                          & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                          & NRC_GENDER_CODE=="G"]


# Converting all the variables to numeric which are factor by default in excel

MarksInL1Girls <- as.numeric(MarksInL1Girls)
MarksInL2Girls <- as.numeric(MarksInL2Girls)
MarksInL3Girls <- as.numeric(MarksInL3Girls)
MarksInS1Girls <- as.numeric(MarksInS1Girls)
MarksInS2Girls <- as.numeric(MarksInS2Girls)
MarksInS3Girls <- as.numeric(MarksInS3Girls)


# Sampling 4000 entries out of the entire sheet

dataMarksGirls <- data.frame(MarksInL1Girls,MarksInL2Girls,MarksInL3Girls,MarksInS1Girls,MarksInS2Girls,MarksInS3Girls,NRCClassGirls) 

# preserving the indices out of the data

myGirls <- sample(1:nrow(dataMarksGirls),1000,replace=FALSE)

# Taking the values of all the marks and the corresponding class 
MarksGirls <- dataMarksGirls[myGirls,]

# Storing up data in individual variable

MarksInL1SampleGirls <- MarksGirls$MarksInL1Girls
MarksInL2SampleGirls <- MarksGirls$MarksInL2Girls
MarksInL3SampleGirls <- MarksGirls$MarksInL3Girls
MarksInS1SampleGirls <- MarksGirls$MarksInS1Girls
MarksInS2SampleGirls <- MarksGirls$MarksInS2Girls
MarksInS3SampleGirls <- MarksGirls$MarksInS3Girls
NRCClassSampleGirls <- MarksGirls$NRCClassGirls

dataGirls <- cbind(MarksInL1SampleGirls,MarksInL2SampleGirls,MarksInL3SampleGirls,MarksInS1SampleGirls,MarksInS2SampleGirls,MarksInS3SampleGirls)

dataFrameGirls <- data.frame(MarksInL1SampleGirls,MarksInL2SampleGirls,MarksInL3SampleGirls,MarksInS1SampleGirls,MarksInS2SampleGirls,MarksInS3SampleGirls)


dataClusterGirls <- model.matrix(~.+0, data=dataFrameGirls)

# Result associated with boys data
resultsGirls <- kmeans(dataClusterGirls,5)
resultsGirls
resultsGirls$size



## Summary statistics for cluster
# Visualization of different clusters

table(NRCClassSampleGirls , resultsGirls$cluster)

plot(Marks[c("MarksInL1Girls", "MarksInL2Girls")], xlim = c(0,150), ylim = c(0,150), col = resultsBoys$cluster)

plot(Marks[c("MarksInL1Girls", "MarksInL2Girls")], xlim = c(0,150), ylim = c(0,150), col = NRCClassSampleBoys)

library(cluster) 

clusplot(dataClusterGirls, resultsGirls$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
#plot of all the data been clutered
install.packages("fpc")
library(fpc)

plotcluster(dataClusterGirls, resultsGirls$cluster)

# Checking for comparison in between different clusters
d <- dist(Experiment8, method = "euclidean") 
cluster.stats(d, resultsBoys$cluster, resultsGirls$cluster)

#...................................For overall population..............................#

MarksInL1 <- L1_Marks_Corrected[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A" 
                                    ]

MarksInL2 <- L2_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                    ]

MarksInL3 <- L3_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                    ]

MarksInS1 <- S1_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                    ]

MarksInS2 <- S2_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                    ]

MarksInS3 <- S3_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                    & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                    & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                    ]

NRCClass <- NRC_CLASS[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                          & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                          & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                          ]


# Converting all the variables to numeric which are factor by default in excel

MarksInL1 <- as.numeric(MarksInL1)
MarksInL2 <- as.numeric(MarksInL2)
MarksInL3 <- as.numeric(MarksInL3)
MarksInS1 <- as.numeric(MarksInS1)
MarksInS2 <- as.numeric(MarksInS2)
MarksInS3 <- as.numeric(MarksInS3)


# Sampling 4000 entries out of the entire sheet

dataMarks <- data.frame(MarksInL1,MarksInL2,MarksInL3,MarksInS1,MarksInS2,MarksInS3,NRCClass) 

# preserving the indices out of the data

my <- sample(1:nrow(dataMarks),1000,replace=FALSE)

# Taking the values of all the marks and the corresponding class 
Marks <- dataMarks[my,]

# Storing up data in individual variable

MarksInL1Sample <- Marks$MarksInL1
MarksInL2Sample <- Marks$MarksInL2
MarksInL3Sample <- Marks$MarksInL3
MarksInS1Sample <- Marks$MarksInS1
MarksInS2Sample <- Marks$MarksInS2
MarksInS3Sample <- Marks$MarksInS3
NRCClassSample <- Marks$NRCClass

data <- cbind(MarksInL1Sample,MarksInL2Sample,MarksInL3Sample,MarksInS1Sample,MarksInS2Sample,MarksInS3Sample)

dataFrame <- data.frame(MarksInL1Sample,MarksInL2Sample,MarksInL3Sample,MarksInS1Sample,MarksInS2Sample,MarksInS3Sample)


dataCluster <- model.matrix(~.+0, data=dataFrame)

# Result associated with boys data
results <- kmeans(dataCluster,5)
results
results$size



## Summary statistics for cluster
# Visualization of different clusters

table(NRCClassSample , results$cluster)


library(cluster) 

clusplot(dataCluster, results$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
#plot of all the data been clutered
install.packages("fpc")
library(fpc)

plotcluster(dataCluster, results$cluster)



#..................................................................................#

# Creating a decision tree for Boys

library(rpart)
PredictorData = data.frame(L1_Marks_Class,L2_Marks_Class,L3_Marks_Class,S1_Marks_Class,S2_Marks_Class,S3_Marks_Class,NRC_CLASS)





