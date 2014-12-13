anovaFactor <- setClass("anovaFactor", 
	representation(
		name="character",
		a="numeric",
		aSquared="numeric",
		n="numeric",
		mean="numeric"
)); 

setMethod("show", "anovaFactor",
  function(object){
    cat("(A = ", getA(object),", A^2 = ",getASquared(object),", n = ",getN(object),")","\n");
  }
)

getName <- function(f){
  attributes(f)$name
}

getA <- function(f){
  attributes(f)$a
}

getASquared <- function(f){
  attributes(f)$aSquared
}

getN <- function(f){
  attributes(f)$n
}

getMean <- function(f){
  attributes(f)$mean
}

owAnovaResult <- setClass("owAnovaResult", 
	representation(
		F="numeric",
		pValue="numeric",
		
		intraSC="numeric",
		intraDF="numeric",
		intraMC="numeric",
		
		interSC="numeric",
		interDF="numeric",
		interMC="numeric",
		
		totalSC="numeric",
		totalDF="numeric",
		
		factors="list"
)); 

getF <- function(f){
  attributes(f)$F
}

getPValue <- function(f){
  attributes(f)$pValue
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

getFactors <- function(f){
  attributes(f)$factors
}

scheffeComparison <- setClass("scheffeComparison", 
	representation(
		a="anovaFactor",
		b="anovaFactor",
		CR="numeric",
		meanDifference="numeric"
)); 

getFactorA <- function(f){
  attributes(f)$a
}

getFactorB <- function(f){
  attributes(f)$b
}

getCR <- function(f){
  attributes(f)$CR
}

getMeanDifference <- function(f){
  attributes(f)$meanDifference
}

scheffeResultToString <- function(object) {
  factorA <- getFactorA(object)
  factorB <- getFactorB(object)
  meanDifference <- getMeanDifference(object)
  CR <- getCR(object)
  comparison <- " = ";
  differences <- ""
  if(meanDifference > CR){
    comparison <- " > ";
    differences <- "*"
  } else {
    comparison <- " < ";
  }
  paste("|mean(",getName(factorA),") - mean(",getName(factorB),")| = ","|",getMean(factorA)," - ",getMean(factorB),"| = ",round(meanDifference,4),comparison," CR (",round(CR,4),") ",differences,sep="")
}

setMethod("show", "scheffeComparison",
  function(object){
    cat(scheffeResultToString(object))
  }
)

omputeFactors <- function(data) {
  anovaFactors = vector()  

  for (i in 1:ncol(data) ) {
    factor <- data[,i]
    factor <- factor[factor != 0]
    
    factorName		= colnames(data)[i]
    factorSum 		= sum(factor)
    factorSquaredSum 	= sum(factor*factor)
    factorSize 		= length(factor)
    factorMean 		= mean(factor)
    
    anovaFactors = c(anovaFactors, anovaFactor(name=factorName,a=factorSum, aSquared=factorSquaredSum, n=factorSize, mean=factorMean))
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
  pValue 	<- 1-pf(F, interDF, intraDF)
   
  owAnovaResult(
    F=F,
    pValue=pValue,
    
    intraSC=SCintra,
    intraDF=intraDF,
    intraMC=MCintra,
    
    interSC=SCinter,
    interDF=interDF,
    interMC=MCinter,
    
    totalSC=SCtotal,
    totalDF=N-1,
    
    factors=anovaFactors
  )
}

scheffe <- function(anovaResult, confLevel) {
  comparisons	<- vector()
  anovaFactors 	<- getFactors(anovaResult)
  intraMC 	<- getIntraMC(anovaResult)
  interDF	<- getInterDF(anovaResult)
  intraDF	<- getIntraDF(anovaResult)
  for (i in 1:(length(anovaFactors)-1)) {
    for (j in (i+1):length(anovaFactors)) {
      CR 		<- sqrt(interDF*qf(confLevel, interDF, intraDF)) * sqrt(intraMC * ( (1/ getN(anovaFactors[[i]]) + (1/ getN(anovaFactors[[j]])))))
      absMeanDiff 	<- abs(getMean(anovaFactors[[i]]) - getMean(anovaFactors[[j]]))      
      comparisons <- c(comparisons, scheffeComparison(
		      a=anovaFactors[[i]],
		      b=anovaFactors[[j]],
		      CR=CR,
		      meanDifference=absMeanDiff
	  )
	)
    }
  }
  comparisons
}