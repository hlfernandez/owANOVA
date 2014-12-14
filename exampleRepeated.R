source("anova.R")

confLevel <- 0.95

# One-way repeated measures ANOVA with the implemented function
data 	<- read.csv("data/testDataRepeated.csv")
result 	<- owRepeatedAnova(data)


cat("\nResult using owRepeatedAnova(data):\n")
print(result)

# One-way repeated measures ANOVA with the aov function
data 	<- read.csv("data/testDataRepeatedAOV.csv")
dataF 	<- within(data, { factor <- factor(factor) ; subject <- factor(subject)} )
anova 	<- aov(response ~  factor , data=dataF)
anova	<- aov(response ~ factor + Error(subject/factor), data=dataF)

cat("\nResult using aov(response ~ factor + Error(subject/factor), data=dataF):\n")
print(summary(anova))