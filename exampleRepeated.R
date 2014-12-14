source("anova.R")

confLevel <- 0.95

result <- owRepeatedAnova(read.csv("data/testDataRepeated.csv"))
print(result)
