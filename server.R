library(shiny)
source("anova.R");

# Define server logic required to perform the analysis
shinyServer(function(input, output) {

  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
    
  loadDataset <- reactive({
    if (is.null(input$file)) {
      return (read.csv("data/testData.csv"));
    } else {
      return (read.csv(input$file$datapath));
    }
  });
  
  output$analysis <- renderUI({         
    
    data 	<- loadDataset()
    result 	<- owAnova(data)
    F 		<- getF(result)
    SCintra 	<- getIntraSC(result)
    intraDF 	<- getIntraDF(result)
    MCintra 	<- getIntraMC(result)
    SCinter 	<- getInterSC(result)
    interDF 	<- getInterDF(result)
    MCinter 	<- getInterMC(result)
    SCtotal 	<- getTotalSC(result)
    totalDF 	<- getTotalDF(result)
    
    confLevel 	<- (input$confLevel / 100)
    QF		<- qf(confLevel, interDF, intraDF)
    pValue 	<- 1-pf(F, interDF, intraDF)
    if (F > QF) {
      testResult 	<- "Reject H<sub>0</sub> and accept H<sub>1</sub>";
      explanation1 	<- paste("F (",round(F,4),") > F<sub>",confLevel,",",interDF,",",intraDF,"</sub> (",round(QF,4),").",sep="")
      explanation2 	<- paste("p (",round(pValue,4),") < &alpha; (",round(1-confLevel,2),").",sep="")
    } else {
      testResult 	<- "Can't reject H<sub>0</sub>";
      explanation1 	<- paste("F (",round(F,4),") < F<sub>",confLevel,",",interDF,",",intraDF,"</sub> (",round(QF,4),").",sep="")
      explanation2 	<- paste("p (",round(pValue,4),") > &alpha; (",round(1-confLevel,2),").",sep="")
    }
    
    HTML(
      "<h4>ANOVA table for fixed model, single factor, fully randomized experiment</h4>",
      "<table><tr>",
      "<th>Source of variation</th><th>Sum of squares</th><th>Degrees of freedom</th><th>Mean square</th><th>F</th>",
      "</tr><tr>",
      "<td>Intra (error)</td><td>",round(SCintra,4),"</td><td>",round(intraDF,4),"</td><td>",round(MCintra,4),"</td><td>",round(F,4),"</td>",
      "</tr><tr>",
      "<td>Inter (treatments)</td><td>",round(SCinter,4),"</td><td>",round(interDF,4),"</td><td>",round(MCinter,4),"</td>",
      "</tr><tr>",
      "<td>Total</td><td>",round(SCtotal,4),"</td><td>",totalDF,"</td>",
      "</tr></table>",
      "<h5>",testResult,"</h5><ul>",
      "<li>",explanation1,"</li>",
      "<li>",explanation2,"</li></ul>"
   )
  })
  
  output$downloadSample2 <- downloadHandler(
    filename = function() { "sampleData.csv" },
    content = function(file) {
      write.csv(read.csv("data/testData.csv"), file, row.names=FALSE, quote=FALSE);
    }
  )  
})
 
