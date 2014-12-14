source("anova.R")

confLevel <- 0.95

result <- owAnova(read.csv("data/testDataOneWay.csv"))
print(result)

if(getPValue(result) < (1-confLevel)) {
  sheffeResult <- scheffe(result, confLevel)
  print(sheffeResult)
}
