Experiment4DataSet  <- read.csv("D:/IIIT-B Study Material/3 Semester/Data Analytics/Project/Data Sets/Experiment 4-School Dataset.csv", header=TRUE)

attach(Experiment2DataSet)

library(sqldf) 

SchoolWiseDataSet <- na.omit( Experiment4DataSet   )

numberOfStudentsPassedQuery <- sqldf(	'SELECT 		DISTINCT( T.SCHOOL_CODE) AS SCHOOL_CODE, 
																										IFNULL(T2.PassNumber, 0) AS passNumber
																				FROM SchoolWiseDataSet AS T 
																				LEFT JOIN 
																				( 
																					SELECT     T1.SCHOOL_CODE AS SCHOOL_CODE,
																										COUNT( T1.NRC_RESULT) AS PassNumber 
																					FROM SchoolWiseDataSet AS T1 
																					WHERE T1.NRC_RESULT = "P"
																					GROUP BY T1.SCHOOL_CODE 
																				) AS T2 
																					ON T.SCHOOL_CODE = T2.SCHOOL_CODE 
																			   ORDER BY SCHOOL_CODE
																		')
					
totalNumberOfStudentsQuery <- sqldf('  		SELECT		SCHOOL_CODE ,
																									COUNT(NRC_RESULT) AS totalNumber 
																				FROM SchoolWiseDataSet 
																				GROUP BY SCHOOL_CODE
																	')

StudentPassPercentDataFrame <- data.frame(SCHOOL_CODE = numberOfStudentsPassedQuery$SCHOOL_CODE,Pass_Percentage = numberOfStudentsPassedQuery$passNumber / totalNumberOfStudentsQuery$totalNumber )

Mean <- mean(StudentPassPercentDataFrame$Pass_Percentage)

Error <- qt(0.975,df=length(StudentPassPercentDataFrame$SCHOOL_CODE)-1) * sd (StudentPassPercentDataFrame$Pass_Percentage)/sqrt(length(StudentPassPercentDataFrame$SCHOOL_CODE))

StudentPassPercentDataFrame$Result <- ifelse(StudentPassPercentDataFrame$Pass_Percentage < Mean - Error , "Attention" , ifelse(StudentPassPercentDataFrame$Pass_Percentage > Mean + Error , "Reward", "Normal"))

write.table(StudentPassPercentDataFrame, file= "D:/IIIT-B Study Material/3 Semester/Data Analytics/Project/Data Sets/Experiment 4 - School Confidence Interval Result.csv", sep=",", row.names=FALSE, col.names=TRUE)
