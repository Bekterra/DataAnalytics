#Importing data


Experiment5DataOtherDetails <- read.csv("F:/course to study/data analytics/Project/project data/Experimant5DataOtherDetails.csv")
attach(Experiment5DataOtherDetails)

NRC_CASTE_CODE <- as.factor(NRC_CASTE_CODE)
dataFrame <- data.frame(SCHOOL_TYPE,URBAN_RURAL,NRC_CASTE_CODE,NRC_GENDER_CODE,NRC_MEDIUM,NRC_PHYSICAL_CONDITION,CANDIDATE_TYPE)
#Using Association Rules

library(arules)


rules = apriori(dataFrame)
inspect(rules)

rules <- apriori(dataFrame, parameter = list(minlen=2,supp=0.1,conf=0.7),appearance=list(rhs=c("URBAN_RURAL=U", "URBAN_RURAL=R"), default="lhs"))
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

library(arulesViz)
plot(rules.pruned)



# For the second kind of operation

Experiment5DataPerformance <- read.csv("F:/course to study/data analytics/Project/project data/Experiment5DataPerformance.csv")
attach(Experiment5DataPerformance)

#Using Association Rules

library(arules)


rules = apriori(Experiment5DataPerformance)
inspect(rules)

rules <- apriori(Experiment5DataPerformance, parameter = list(minlen=2,supp=0.005,conf=0.7),appearance=list(rhs=c("URBAN_RURAL=U", "URBAN_RURAL=R"), default="lhs"))
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

library(arulesViz)
plot(rules.pruned)

