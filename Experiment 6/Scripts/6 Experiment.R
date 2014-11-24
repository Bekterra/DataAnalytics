#Importing data

# did not think that it was a good experiment 
# there should have been assciation Rules for predicting the values of Distinction 
#however, in the above dataset for certain values of confidence and support the rules for 
# distinction are not generated.

Experiment6 <- read.csv("F:/course to study/data analytics/Project/project data/Experiment6.csv")
attach(Experiment6)

NRC_CASTE_CODE <- as.factor(NRC_CASTE_CODE)
dataFrame <- data.frame(SCHOOL_TYPE,URBAN_RURAL,NRC_CASTE_CODE,NRC_GENDER_CODE,NRC_MEDIUM,NRC_PHYSICAL_CONDITION,CANDIDATE_TYPE,NRC_CLASS)
#Using Association Rules

library(arules)


rules = apriori(dataFrame)
inspect(rules)

rules <- apriori(dataFrame, parameter = list(minlen=2,supp=0.01,conf=0.8),appearance=list(rhs=c("NRC_CLASS=D   ", "NRC_CLASS=FAIL"), default="lhs"))
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
