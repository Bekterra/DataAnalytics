Experiment4DataSet  <- read.csv("D:/IIIT-B Study Material/3 Semester/Data Analytics/Project/Data Sets/Experiment 4-District Dataset.csv", header=TRUE)

attach(Experiment4DataSet)

library(sqldf)

DistrictWiseDataSet <- na.omit(Experiment4DataSet)

numberOfStudentsPassedQuery <- sqldf( ' 	select 	DIST_CODE ,
																								COU NT(NRC_RESULT) AS passNumber 
																				FROM  DistrictWiseDataSet 
																				WHERE NRC_RESULT = "P" 
																				GROUP BY DIST_CODE																				
																			')
																			
totalNumberOfStudentsQuery <- sqldf('  SELECT	 DIST_CODE ,
																							COUNT( NRC_RESULT )  AS totalNumber 
																		FROM	 DistrictWiseDataSet 
																		GROUP BY  DIST_CODE
																	')
																	
StudentPassPercentDataFrame <- data.frame(DIST_CODE = numberOfStudentsPassedQuery$DIST_CODE, Pass_Percentage = numberOfStudentsPassedQuery$passNumber /  totalNumberOfStudentsQuery$totalNumber )

Mean <- mean(StudentPassPercentDataFrame$Pass_Percentage)

Error <- qt(0.975,df=length(StudentPassPercentDataFrame$DIST_CODE)-1)*sd(StudentPassPercentDataFrame$Pass_Percentage)/sqrt(length(StudentPassPercentDataFrame$DIST_CODE))

StudentPassPercentDataFrame$Result <- ifelse(StudentPassPercentDataFrame$Pass_Percentage < Mean - Error , "Attention" , ifelse( StudentPassPercentDataFrame$Pass_Percentage > Mean + Error , "Reward", "Normal"))

write.table(StudentPassPercentDataFrame, file= "D:/IIIT-B Study Material/3 Semester/Data Analytics/Project/Data Sets/Experiment 4 - District Confidence Interval Result.csv", sep=",", row.names=FALSE, col.names=TRUE)



