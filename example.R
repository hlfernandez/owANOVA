source("anova.R")

confLevel <- 0.95

# One-way ANOVA with the implemented function
data	<- read.csv("data/testDataOneWay.csv")
result 	<- owAnova(data)

cat("\nResult using owAnova(data):\n")
print(result)

if(result@pValue < (1-confLevel)) {
  sheffeResult <- scheffe(result, confLevel)
  cat("\nScheffé pairwise comparisons:\n")
  for(i in 1:length(sheffeResult)) {
    cat("\t",scheffeResultToString(sheffeResult[[i]]),"\n")
  }
}

# One-way ANOVA with the aov function
data 	<- read.csv("data/testDataOneWayAOV.csv")
dataF 	<- within(data, { factor <- factor(factor) } )
anova 	<- aov(response ~  factor , data=dataF)

cat("\nResult using aov(response ~  factor , data=dataF):\n")
print(summary(anova))