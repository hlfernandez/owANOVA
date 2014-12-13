factorAnova <- setClass("factorAnova", 
	representation(
		a="numeric",
		aSquared="numeric",
		n="numeric"
)); 

setMethod("show", "factorAnova",
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

append <- function(dest, text) {
  result <- paste(dest, text, sep="")
  result
}

owAnova <- function(data){
  txtResult = "";
  data[is.na(data)] <- 0

  anovaFactors = vector()

  T = 0
  sumSquared = 0;
  N = 0
  for (i in 1:ncol(data) ) {
    factor <- data[,i]
    factor <- factor[factor != 0]
    factorSum = sum(factor)
    factorSquaredSum = sum(factor*factor)
    factorSize = length(factor)
    factorMean = mean(factor)
    T = T + factorSum
    N = N + factorSize
    sumSquared = sumSquared + factorSquaredSum
    anovaFactors = c(anovaFactors, factorAnova(a=factorSum, aSquared=factorSquaredSum, n=factorSize))
  }

  mean = T/N

  
  txtResult <- append(txtResult, paste("<h4>Grand summary statistics</h4>\n", sep=""))
  txtResult <- append(txtResult,"<ul>\n")
  txtResult <- append(txtResult, paste("<li>Sum (T) = ",T,"</li>\n", sep=""))
  txtResult <- append(txtResult, paste("<li>Sum squared = ",T,"</li>\n", sep=""))
  txtResult <- append(txtResult, paste("<li>Size (N) = ",N,"</li>\n"))
  txtResult <- append(txtResult, paste("<li>Mean = ", mean,"</li>\n"))
  txtResult <- append(txtResult,"</ul>\n")
  txtResult <- append(txtResult, "\n")

  # Compute de basic reasons
  bT = T*T/N
  bA = 0
  bY = 0
  for (i in 1:length(anovaFactors) ) {
    factor <- anovaFactors[[i]]
    bA = bA + (getA(factor)*getA(factor)/getN(factor))
    bY = bY + getASquared(factor)
  }

  #txtResult <- append(txtResult, paste("[T] = ",bT,"\n"))
  #txtResult <- append(txtResult, paste("[A] = ",bA,"\n"))
  #txtResult <- append(txtResult, paste("[Y] = ", bY,"\n"))
  #txtResult <- append(txtResult, "\n")

  SCinter = bA-bT
  SCintra = bY-bA
  SCtotal = bY-bT

  #txtResult <- append(txtResult, paste("SC-inter = ",SCinter,"\n"))
  #txtResult <- append(txtResult, paste("SC-intra = ",SCintra,"\n"))
  #txtResult <- append(txtResult, paste("SC-total = ",SCtotal,"\n"))
  #txtResult <- append(txtResult, "\n")

  interDF = length(anovaFactors) - 1
  MCinter = SCinter / interDF

  intraDF = N - length(anovaFactors)
  MCintra = SCintra / intraDF

  F = MCinter / MCintra

  txtResult <- append(txtResult,"<h4>ANOVA table for fixed model, single factor, fully randomized experiment</h4>\n")
  txtResult <- append(txtResult,"<table><tr>\n")
  txtResult <- append(txtResult, paste("<th>Source of variation</th><th>Sum of squares</th><th>Degrees of freedom</th><th>Mean square</th><th>F</th>\n"))
  txtResult <- append(txtResult,"</tr><tr>\n")
  txtResult <- append(txtResult, paste("<td>Intra (error)</td><td>",round(SCintra,4),"</td><td>",round(intraDF,4),"</td><td>",round(MCintra,4),"</td><td>",round(F,4),"</td>\n"))
  txtResult <- append(txtResult,"</tr><tr>\n")
  txtResult <- append(txtResult, paste("<td>Inter (treatments)</td><td>",round(SCinter,4),"</td><td>",round(interDF,4),"</td><td>",round(MCinter,4),"</td>\n"))
  txtResult <- append(txtResult,"</tr><tr>\n")
  txtResult <- append(txtResult, paste("<td>Total</td><td>",round(SCtotal,4),"</td><td>",round(SCtotal,4),"</td><td>",N-1,"</td>\n"))
  txtResult <- append(txtResult,"</tr></table>\n")
  txtResult
}

#cat(owAnova(read.csv("data/testData.csv")))