source("anova.R")

anovaSubject <- setClass("anovaSubject", 
	representation(
		name="character",
		a="numeric",
		aSquared="numeric",
		n="numeric",
		mean="numeric"
));

owRepeaedAnovaResult <- setClass("owRepeaedAnovaResult",
	representation(
		F="numeric",
		pValue="numeric",
		
		SCa="numeric",
		DFa="numeric",
		MCa="numeric",
		
		SCs="numeric",
		DFs="numeric",
		MCs="numeric",

		SCas="numeric",
		DFas="numeric",
		MCas="numeric",
		
		SCtotal="numeric",
		DFtotal="numeric"
));

getF <- function(f){
  attributes(f)$F
}

getPValue <- function(f){
  attributes(f)$pValue
}

getSCa <- function(f){
  attributes(f)$SCa
}

getDFa <- function(f){
  attributes(f)$DFa
}

getMCa <- function(f){
  attributes(f)$MCa
}

getSCs <- function(f){
  attributes(f)$SCs
}

getDFs <- function(f){
  attributes(f)$DFs
}

getMCs <- function(f){
  attributes(f)$MCs
}

getSCas <- function(f){
  attributes(f)$SCas
}

getDFas <- function(f){
  attributes(f)$DFas
}

getMCas <- function(f){
  attributes(f)$MCas
}

getSCtotal <- function(f){
  attributes(f)$SCtotal
}

getDFtotal <- function(f){
  attributes(f)$DFtotal
}

owRepeatedAnova <- function(data){  

  s = nrow(data)
  a = ncol(data)
  
  anovaFactors <- vector()

  for (i in 1:ncol(data) ) {
    factor <- data[,i]
    factor <- factor[!is.na(factor)]
    
    factorName		= colnames(data)[i]
    factorSum 		= sum(factor)
    factorSquaredSum 	= sum(factor*factor)
    factorSize 		= length(factor)
    factorMean 		= mean(factor)
    
    anovaFactors = c(anovaFactors, anovaFactor(name=factorName,a=factorSum, aSquared=factorSquaredSum, n=factorSize, mean=factorMean))
  }
  
  anovaSubjects <- vector()

  for (i in 1:nrow(data) ) {
    subject <- data[i,]
    subject <- subject[!is.na(subject)]
    
    subjectName		= as.character(i)
    subjectSum 		= sum(subject)
    subjectSquaredSum 	= sum(subject*subject)
    subjectSize 		= length(subject)
    subjectMean 		= mean(subject)
    
    anovaSubjects = c(anovaSubjects, anovaSubject(name=subjectName,a=subjectSum, aSquared=subjectSquaredSum, n=subjectSize, mean=subjectMean))
  }
  
  A 	<- 0
  AS 	<- 0
  T	<- 0
  for (i in 1:length(anovaFactors) ) {
    factor <- anovaFactors[[i]]
    A 	<- A + (getA(factor)*getA(factor))
    AS 	<- AS + getASquared(factor)
    T = T + getA(factor)
  }
  A	<- A/s
  T	<- T/(a*s)
  
  S	<- 0
  for (i in 1:length(anovaSubjects) ) {
    subject <- anovaSubjects[[i]]
    S	<- + (getA(subject)*getA(subject))
  }
  S	<- S/a

  
  SCa	<- A-T
  DFa	<- a-1
  MCa	<- SCa / DFa
  
  SCs	<- S-T
  DFs	<- s-1
  MCs	<- SCs / DFs
  
  SCas	<- AS - A - S + T
  DFas	<- a*s - a - s + 1
  MCas	<- SCas / DFas
  
  SCtotal	<- AS - T
  DFtotal	<- a*s - 1

  F = MCa / MCas
  pValue 	<- 1-pf(F, DFa, DFas)
   
  owRepeaedAnovaResult(
    F=F,
    pValue=pValue,
    
    SCa=SCa,
    DFa=DFa,
    MCa=MCa,
    
    SCs=SCs,
    DFs=DFs,
    MCs=MCs,

    SCas=SCas,
    DFas=DFas,
    MCas=MCas,
    
    SCtotal=SCtotal,
    DFtotal=DFtotal
  )
}
