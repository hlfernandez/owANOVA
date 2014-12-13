anovaFactor <- setClass("anovaFactor", 
	representation(
		a="numeric",
		aSquared="numeric",
		n="numeric"
)); 

setMethod("show", "anovaFactor",
  function(object){
    cat("(A = ", getA(object),", A^2 = ",getASquared(object),", n = ",getN(object),")","\n");
  }
)

getA <- function(f){
  attributes(f)$a
}

getASquared <- function(f){
  attributes(f)$aSquared
}

getN <- function(f){
  attributes(f)$n
}

owAnovaResult <- setClass("owAnovaResult", 
	representation(
		F="numeric",
		
		intraSC="numeric",
		intraDF="numeric",
		intraMC="numeric",
		
		interSC="numeric",
		interDF="numeric",
		interMC="numeric",
		
		totalSC="numeric",
		totalDF="numeric"
)); 


getF <- function(f){
  attributes(f)$F
}

getIntraSC <- function(f){
  attributes(f)$intraSC
}

getIntraMC <- function(f){
  attributes(f)$intraMC
}

getIntraDF <- function(f){
  attributes(f)$intraDF
}

getInterSC <- function(f){
  attributes(f)$interSC
}

getInterMC <- function(f){
  attributes(f)$interMC
}

getInterDF <- function(f){
  attributes(f)$interDF
}

getTotalSC <- function(f){
  attributes(f)$totalSC
}

getTotalDF <- function(f){
  attributes(f)$totalDF
}

computeFactors <- function(data) {
  anovaFactors = vector()

  for (i in 1:ncol(data) ) {
    factor <- data[,i]
    factor <- factor[factor != 0]
    
    factorSum = sum(factor)
    factorSquaredSum = sum(factor*factor)
    factorSize = length(factor)
    
    anovaFactors = c(anovaFactors, anovaFactor(a=factorSum, aSquared=factorSquaredSum, n=factorSize))
  }
  
  anovaFactors
}

owAnova <- function(data){  
  data[is.na(data)] <- 0

  anovaFactors <- computeFactors(data)

  T = 0
  sumSquared = 0;
  N = 0  
  for (i in 1:length(anovaFactors) ) {
    factor <- anovaFactors[[i]]
    T = T + getA(factor)
    N = N + getN(factor)
    sumSquared = sumSquared + getASquared(factor)
    
  }
  mean = T/N

  bT = T*T/N
  bA = 0
  bY = 0
  for (i in 1:length(anovaFactors) ) {
    factor <- anovaFactors[[i]]
    bA = bA + (getA(factor)*getA(factor)/getN(factor))
    bY = bY + getASquared(factor)
  }

  SCinter = bA-bT
  SCintra = bY-bA
  SCtotal = bY-bT

  interDF = length(anovaFactors) - 1
  MCinter = SCinter / interDF

  intraDF = N - length(anovaFactors)
  MCintra = SCintra / intraDF

  F = MCinter / MCintra
   
  owAnovaResult(
    F=F,
    
    intraSC=SCintra,
    intraDF=intraDF,
    intraMC=MCintra,
    
    interSC=SCinter,
    interDF=interDF,
    interMC=MCinter,
    
    totalSC=SCtotal,
    totalDF=N-1
  )
}

# result <- owAnova(read.csv("data/testData.csv"));
