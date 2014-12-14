owANOVA
========================

A simple web application implemented with R and Shiny to perform one way and repeated measures ANOVA with didactical purposes, allowing students to see all the ANOVA required calculations.

Requirements
------------
To run this application you need:
  - R (http://www.r-project.org/)
  - Shiny (http://shiny.rstudio.com/, it can be installed with `install.packages("shiny")`)
  
Installation and usage
------------
### 1. Download
You can also download the owANOVA project directly from Github using the following command:
`git clone https://github.com/hlfernandez/owANOVA`

### 2. Launch the application

Go to the owANOVA's project base directory, run R and then run the following commands:
```R
library("shiny")
runApp("./")
```

The application is opened in the default web browser.

![Screenshot](https://raw.github.com/hlfernandez/owANOVA/master/screenshots/screenshot.png)

Tests
------------
### 1. One-way ANOVA

To test the correctness of the implementation, a test script called `example.R` checks that the results obtained are the same.

```
> source("example.R")

Result using owAnova(data):
S.V     S.C.    DF      M.C.    F
Inter   66.671  2       33.335  5.569 (p = 0.0166)
Intra   83.8    14      5.986
Total   150.471 16

Scheffé pairwise comparisons:
         |mean(A1) - mean(A2)| = |6.2 - 10.5| = 4.3 >  CR (4.0512) * 
         |mean(A1) - mean(A3)| = |6.2 - 6.5| = 0.3 <  CR (4.0512)  
         |mean(A2) - mean(A3)| = |10.5 - 6.5| = 4 >  CR (3.8626) * 

Result using aov(response ~  factor , data=dataF):
            Df Sum Sq Mean Sq F value Pr(>F)  
factor       2  66.67   33.34   5.569 0.0166 *
Residuals   14  83.80    5.99                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

### 2. Repeated measures ANOVA

To test the correctness of the implementation, a test script called `exampleRepeated.R` checks that the results obtained are the same.

```
> source("exampleRepeated.R")

Result using owRepeatedAnova(data):
S.C.    DF      M.C.    F
Factor (A)      267     3       89      1.333 (p = 0.3486)
Subject (S)     336.167 2       168.083 2.518 (p = 0.1607)
Error (AxS)     400.5   6       66.75
Total   1003.667        11

Result using aov(response ~ factor + Error(subject/factor), data=dataF):

Error: subject
          Df Sum Sq Mean Sq F value Pr(>F)
Residuals  2  336.2   168.1               

Error: subject:factor
          Df Sum Sq Mean Sq F value Pr(>F)
factor     3  267.0   89.00   1.333  0.349
Residuals  6  400.5   66.75        
```