# perform the clustering operation on all the subject marks available in the database

#First aim to perform clustering

#In order to perform the clustering operation we need to clean the data to 
# obtain numerical values of all the marks in different subjects.

# Importing the data to R

Group1_Data <- read.csv("F:/course to study/data analytics/Project/project data/Experiment3.csv")
attach(Group1_Data)

# cleaning the data to remove the N/A elements to perform linear regression

# ******************************************************************************#


MarksInL1Distinct <- L1_Marks_Corrected[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]

MarksInL1First <- L1_Marks_Corrected[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]

MarksInL1Second <- L1_Marks_Corrected[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                        "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                      & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                      & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                        S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInL1Pass <- L1_Marks_Corrected[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" &  NRC_CLASS == "PASS"]

MarksInL1Fail <- L1_Marks_Corrected[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]


# For L2Code

MarksInL2Distinct <- L2_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]

MarksInL2First <- L2_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]


MarksInL2Second <- L2_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                        "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                      & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                      & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                        S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInL2Pass <- L2_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "PASS"]

MarksInL2Fail <- L2_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]


# For L3Code
MarksInL3Distinct <- L3_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]

MarksInL3First <- L3_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]

MarksInL3Second  <- L3_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                         "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                       & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                       & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                         S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInL3Pass <- L3_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "PASS"]

MarksInL3Fail <- L3_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]

# For S1Code

MarksInS1Distinct <- S1_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]
MarksInS1First <- S1_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]

MarksInS1Second <- S1_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                        "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                      & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                      & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                        S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInS1Pass <- S1_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "PASS"]

MarksInS1Fail <- S1_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]


# For S2Code

MarksInS2Distinct <- S2_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]

MarksInS2First <- S2_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]

MarksInS2Second <- S2_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                        "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                      & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                      & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                        S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInS2Pass <- S2_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "PASS"]

MarksInS2Fail <- S2_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "FAIL"]


# For S3Code

MarksInS3Distinct <- S3_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                          "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                        & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                        & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                          S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "D   "]

MarksInS3First <- S3_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                       "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                     & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                     & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                       S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "1"]

MarksInS3Second <- S3_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                        "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                      & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                      & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                        S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "2"]

MarksInS3Pass <- S3_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
                                    & L2_MARKS_CORRECTED != "N/A" & L3_MARKS_CORRECTED != "N/A" 
                                    & S1_MARKS_CORRECTED !="N/A" & S2_MARKS_CORRECTED !="N/A" & 
                                      S3_MARKS_CORRECTED !="N/A" & NRC_CLASS == "PASS"]

MarksInS3Fail <- S3_MARKS_CORRECTED[L1_Marks_Corrected != "NA" & L2_MARKS_CORRECTED !=
                                      "NA" & L3_MARKS_CORRECTED != "NA" & L1_Marks_Corrected != "N/A" 
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




#*******************************************************************************#



MarksInL1 <- L1_Marks_Corrected[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]

MarksInL2 <- L2_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]

MarksInL3 <- L3_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]
MarksInS1 <- S1_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]
MarksInS2 <- S2_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]
MarksInS3 <- S3_MARKS_CORRECTED[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]

MarksInL1Class <- L1_Marks_Class[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]

MarksInL2Class <- L2_Marks_Class[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]

MarksInL3Class <- L3_Marks_Class[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]

MarksInS1Class <- Group1_Data$S1_Marks_Class[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]

MarksInS2Class <- Group1_Data$S2_Marks_Class[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]

MarksInS3Class <- Group1_Data$S3_Marks_Class[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]

NRCClass <- NRC_CLASS[L1_Marks_Corrected != "N/A" & L2_MARKS_CORRECTED != "N/A"
                & L3_MARKS_CORRECTED != "N/A" & S1_MARKS_CORRECTED != "N/A"
                & S2_MARKS_CORRECTED != "N/A" & S3_MARKS_CORRECTED != "N/A"
                & L1_Marks_Class!="N/A" & L2_Marks_Class != "N/A" 
                & L3_Marks_Class!="N/A" & S1_Marks_Class != "N/A"
                & S2_Marks_Class!="N/A" & S3_Marks_Class != "N/A"]

NRCClass <- as.character(NRCClass)
NRCClass[NRCClass == "D   "] <- "D"
NRCClass <- as.factor(NRCClass)

# Converting all the variables to numeric which are factor by default in excel

MarksInL1 <- as.numeric(MarksInL1)
MarksInL2 <- as.numeric(MarksInL2)
MarksInL3 <- as.numeric(MarksInL3)
MarksInS1 <- as.numeric(MarksInS1)
MarksInS2 <- as.numeric(MarksInS2)
MarksInS3 <- as.numeric(MarksInS3)


# Sampling 4000 entries out of the entire sheet

dataMarks <- data.frame(MarksInL1,MarksInL2,MarksInL3,MarksInS1,MarksInS2,MarksInS3,MarksInL1Class,MarksInL2Class,MarksInL3Class,MarksInS1Class,MarksInS2Class,MarksInS3Class,NRCClass) 

# preserving the indices out of the data
set.seed(1000)
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

MarksInL1ClassSample <- Marks$MarksInL1Class
MarksInL2ClassSample <- Marks$MarksInL2Class
MarksInL3ClassSample <- Marks$MarksInL3Class
MarksInS1ClassSample <- Marks$MarksInS1Class
MarksInS2ClassSample <- Marks$MarksInS2Class
MarksInS3ClassSample <- Marks$MarksInS3Class

data <- cbind(MarksInL1Sample,MarksInL2Sample,MarksInL3Sample,MarksInS1Sample,MarksInS2Sample,MarksInS3Sample)

dataFrame <- data.frame(MarksInL1Sample,MarksInL2Sample,MarksInL3Sample,MarksInS1Sample,MarksInS2Sample,MarksInS3Sample)


dataCluster <- model.matrix(~.+0, data=dataFrame)

# K-Means Cluster Analysis
# fit <- kmeans(dataCluster, centers=4) # 5 cluster solution
# get cluster means 
# aggregate(model.matrix(~.+0, data=dataFrame),by=list(fit$cluster),FUN=mean)
# append cluster assignment
# mydata <- data.frame(model.matrix(~.+0, data=dataFrame), fit$cluster)

# wssplot(dataCluster)                                                #2
# library(NbClust)
# set.seed(1234)
# nc <- NbClust(dataCluster, min.nc=2, max.nc=15, method="kmeans")
#table(nc$Best.n[1,])

#barplot(table(nc$Best.n[1,]), 
#          xlab="Numer of Clusters", ylab="Number of Criteria()
#          main="Number of Clusters Chosen by 26 Criteria")

results <- kmeans(dataCluster,5)
results

results$size

table(NRCClassSample , results$cluster)



#We have created clusters therefore successfully divided all the marks on the basis
# of characterstics
# More details can be obtained by creating confusion matrix for predicted and actual 
# values

# To validate the cluster we will use association rules to generate the data.
results$cluster <- as.character(results$cluster)
results$cluster[results$cluster == "1"] <- "D" #check
results$cluster[results$cluster == "2"] <- "2" 
results$cluster[results$cluster == "3"] <- "1"
results$cluster[results$cluster == "4"] <- "FAIL"
results$cluster[results$cluster == "5"] <- "PASS"
results$cluster <- as.factor(results$cluster)

result <- results$cluster
levels(result)
levels(NRCClassSample)
library(caret)
confusionMatrix(result,NRCClassSample)

# create a data frame from the value

Data <- data.frame(MarksInL1ClassSample,MarksInL2ClassSample,MarksInL3ClassSample,MarksInS1ClassSample,MarksInS2ClassSample,MarksInS3ClassSample,results$cluster)

library(arules)
rules = apriori(Data)

inspect(rules)

rules <- apriori(Data, parameter = list(minlen=2,supp=0.02,conf=0.9),appearance=list(rhs=c("results$cluster=D", "results$cluster=1", "results$cluster=2", "results$cluster=PASS", "results$cluster=FAIL"), default="lhs"))
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
