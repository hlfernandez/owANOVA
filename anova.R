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
    cat("[Factor ",object@name, "] A =", object@a,", A^2 = ",object@aSquared,", n = ",object@n,")","\n");
  }
)

anovaSubject <- setClass("anovaSubject", 
	representation(
		name="character",
		a="numeric",
		aSquared="numeric",
		n="numeric",
		mean="numeric"
));

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

setMethod("show", "owAnovaResult",
  function(object){
    result	<- object
    F 		<- result@F
    pValue 	<- result@pValue
    SCintra 	<- result@intraSC
    intraDF 	<- result@intraDF
    MCintra 	<- result@intraMC
    SCinter 	<- result@interSC
    interDF 	<- result@interDF
    MCinter 	<- result@interMC
    SCtotal 	<- result@totalSC
    totalDF 	<- result@totalDF
    
    cat("S.V\tS.C.\tDF\tM.C.\tF\n")
    cat(paste("Inter",round(SCinter,3),round(interDF,3),round(MCinter,3),paste(round(F,3)," (p = ",round(pValue,4),")",sep=""),"\n",sep="\t"))
    cat(paste("Intra",round(SCintra,3),round(intraDF,3),round(MCintra,3),"\n",sep="\t"))
    cat(paste("Total",round(SCtotal,3),round(totalDF,3),"\n",sep="\t"))   
  }
)

scheffeComparison <- setClass("scheffeComparison", 
	representation(
		a="anovaFactor",
		b="anovaFactor",
		CR="numeric",
		meanDifference="numeric"
)); 

owRepeatedAnovaResult <- setClass("owRepeatedAnovaResult",
	representation(
		aF="numeric",
		apValue="numeric",
		
		sF="numeric",
		spValue="numeric",
		
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

setMethod("show", "owRepeatedAnovaResult",
  function(object){
    result 	<- object
    
    aF 		<- result@aF
    apValue 	<- result@apValue
    
    sF 		<- result@sF
    spValue 	<- result@spValue
    
    SCa	<- result@SCa
    DFa	<- result@DFa
    MCa	<- result@MCa
    
    SCs	<- result@SCs
    DFs	<- result@DFs
    MCs	<- result@MCs
    
    SCas <- result@SCas
    DFas <- result@DFas
    MCas <- result@MCas
    
    SCtotal <- result@SCtotal
    DFtotal <- result@DFtotal
      
    cat("S.C.\tDF\tM.C.\tF\n")
    cat(paste("Factor (A)"	,round(SCa,3),round(DFa,3),round(MCa,3),paste(round(aF,3)," (p = ",round(apValue,4),")",sep=""),"\n",sep="\t"))
    cat(paste("Subject (S)"	,round(SCs,3),round(DFs,3),round(MCs,3),paste(round(sF,3)," (p = ",round(spValue,4),")",sep=""),"\n",sep="\t"))
    cat(paste("Error (AxS)"	,round(SCas,3),round(DFas,3),round(MCas,3),"\n",sep="\t"))
    cat(paste("Total"		,round(SCtotal,3),round(DFtotal,3),"\n",sep="\t"))       
  }
)

scheffeResultToString <- function(object) {
  factorA 		<- object@a
  factorB 		<- object@b
  CR 			<- object@CR
  meanDifference 	<- object@meanDifference
  
  comparison 	<- " = ";
  differences 	<- ""
  if(meanDifference > CR){
    comparison 	<- " > ";
    differences <- "*"
  } else {
    comparison 	<- " < ";
  }
  paste("|mean(",factorA@name,") - mean(",factorB@name,")| = ","|",factorA@mean," - ",factorB@mean,"| = ",round(meanDifference,4),comparison," CR (",round(CR,4),") ",differences,sep="")
}

setMethod("show", "scheffeComparison",
  function(object){
    cat(scheffeResultToString(object))
  }
)

computeFactors <- function(data) {
  anovaFactors = vector()  

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
  
  anovaFactors
}

owAnova <- function(data){  
  anovaFactors <- computeFactors(data)

  T 		<- 0
  sumSquared 	<- 0;
  N 		<- 0  
  for (i in 1:length(anovaFactors) ) {
    factor <- anovaFactors[[i]]
    T 		<- T + factor@a
    N 		<- N + factor@n
    sumSquared 	<- sumSquared + factor@aSquared
    
  }
  mean <- T/N

  bT <- T*T/N
  bA <- 0
  bY <- 0
  for (i in 1:length(anovaFactors) ) {
    factor <- anovaFactors[[i]]
    bA 	<- bA + (factor@a*factor@a/factor@n)
    bY 	<- bY + factor@aSquared
  }

  SCinter <- bA-bT
  SCintra <- bY-bA
  SCtotal <- bY-bT

  interDF <- length(anovaFactors) - 1
  MCinter <- SCinter / interDF
  intraDF <- N - length(anovaFactors)
  MCintra <- SCintra / intraDF

  F 		<- MCinter / MCintra
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
  anovaFactors 	<- anovaResult@factors
  intraMC 	<- anovaResult@intraMC
  interDF	<- anovaResult@interDF
  intraDF	<- anovaResult@intraDF
  for (i in 1:(length(anovaFactors)-1)) {
    for (j in (i+1):length(anovaFactors)) {
      CR 		<- sqrt(interDF*qf(confLevel, interDF, intraDF)) * sqrt(intraMC * ( (1/ anovaFactors[[i]]@n + (1/ anovaFactors[[j]]@n))))
      absMeanDiff 	<- abs(anovaFactors[[i]]@mean - anovaFactors[[j]]@mean)      
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
    A 	<- A + (factor@a*factor@a)
    AS 	<- AS + factor@aSquared
    T 	<- T + factor@a
  }
  A	<- A/s
  T	<- (T*T)/(a*s)
  
  S	<- 0
  for (i in 1:length(anovaSubjects) ) {
    subject <- anovaSubjects[[i]]
    S	<- S + (subject@a*subject@a)
  }
  S	<- S/a

  
  SCa <- A-T
  DFa <- a-1
  MCa <- SCa / DFa
  
  SCs <- S-T
  DFs <- s-1
  MCs <- SCs / DFs
  
  SCas <- AS - A - S + T
  DFas <- a*s - a - s + 1
  MCas <- SCas / DFas
  
  SCtotal <- AS - T
  DFtotal <- a*s - 1

  aF 		<- MCa / MCas
  apValue 	<- 1-pf(aF, DFa, DFas)

  sF 		<- MCs / MCas
  spValue 	<- 1-pf(sF, DFs, DFas)
   
  owRepeatedAnovaResult(
    aF=aF,
    apValue=apValue,
    
    sF=sF,
    spValue=spValue,
    
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
