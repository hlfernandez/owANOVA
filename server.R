library(shiny)
source("anova.R")

# Define server logic required to perform the analysis
shinyServer(function(input, output) {

  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
    
  loadDataset <- reactive({
    if (is.null(input$file)) {
      if(input$repeatedMeasures == FALSE){
	return (read.csv("data/testDataOneWay.csv"));
      } else {
	return (read.csv("data/testDataRepeated.csv"));
      }
    } else {
      return (read.csv(input$file$datapath));
    }
  });
  
  output$analysis <- renderUI({         
    
    if(input$repeatedMeasures == FALSE){
    
      data 	<- loadDataset()
      result 	<- owAnova(data)
      
      F 	<- result@F
      pValue 	<- result@pValue
      SCintra 	<- result@intraSC
      intraDF 	<- result@intraDF
      MCintra 	<- result@intraMC
      SCinter 	<- result@interSC
      interDF 	<- result@interDF
      MCinter 	<- result@interMC
      SCtotal 	<- result@totalSC
      totalDF 	<- result@totalDF
      
      confLevel 	<- (input$confLevel / 100)
      QF		<- qf(confLevel, interDF, intraDF)
      
      if (F > QF) {
	testResult 	<- "Reject H<sub>0</sub> and accept H<sub>1</sub> because:";
	explanation1 	<- paste("F (",round(F,4),") > F<sub>",confLevel,",",interDF,",",intraDF,"</sub> (",round(QF,4),").",sep="")
	explanation2 	<- paste("p (",round(pValue,4),") < &alpha; (",round(1-confLevel,2),").",sep="")
      } else {
	testResult 	<- "Can't reject H<sub>0</sub> because:	";
	explanation1 	<- paste("F (",round(F,4),") < F<sub>",confLevel,",",interDF,",",intraDF,"</sub> (",round(QF,4),").",sep="")
	explanation2 	<- paste("p (",round(pValue,4),") > &alpha; (",round(1-confLevel,2),").",sep="")
      }
      
      if(pValue < (1-confLevel)) {
	scheffeResult <- scheffe(result, confLevel)
	scheffeText <- "<h5>Scheffé's pairwise comparisons:</h5><ul>";
	for (i in 1:length(scheffeResult)){
	  scheffeText <- paste(scheffeText,"<li>", scheffeResultToString(scheffeResult[[i]]),"</li>",sep="")
	}
	scheffeText <- paste(scheffeText,"</ul>",sep="")
      } else {
	scheffeText <- ""
      }
      
      HTML(
	"<h4>ANOVA table for fixed model, single factor, fully randomized experiment</h4>",
	"<table><tr>",
	"<th>Source of variation</th><th>Sum of squares</th><th>Degrees of freedom</th><th>Mean square</th><th>F</th>",
	"</tr><tr>",
	"<td>Inter (treatments)</td><td>",round(SCinter,4),"</td><td>",round(interDF,4),"</td><td>",round(MCinter,4),"<td>",round(F,4),"</td>",
	"</tr><tr>",
	"<td>Intra (error)</td><td>",round(SCintra,4),"</td><td>",round(intraDF,4),"</td><td>",round(MCintra,4),"</td>",
	"</tr><tr>",
	"<td>Total</td><td>",round(SCtotal,4),"</td><td>",totalDF,"</td>",
	"</tr></table>",
	"<h5>",testResult,"</h5><ul>",
	"<li>",explanation1,"</li>",
	"<li>",explanation2,"</li></ul>",
	scheffeText
    )
   } else{
      # Repeated measures ANOVA
      data 	<- loadDataset()
      result 	<- owRepeatedAnova(data)
      
      aF 	<- result@aF
      apValue 	<- result@apValue
      
      sF 	<- result@sF
      spValue 	<- result@spValue
      
      SCa	<- result@SCa
      DFa	<- result@DFa
      MCa	<- result@MCa
      
      SCs	<- result@SCs
      DFs	<- result@DFs
      MCs	<- result@MCs
      
      SCas	<- result@SCas
      DFas	<- result@DFas
      MCas	<- result@MCas
      
      SCtotal 	<- result@SCtotal
      DFtotal 	<- result@DFtotal
      
      HTML(
	"<h4>ANOVA table for repeated measures, single factor, fully randomized experiment</h4>",
	"<table><tr>",
	"<th>Source of variation</th><th>Sum of squares</th><th>Degrees of freedom</th><th>Mean square</th><th>F</th>",
	"</tr><tr>",
	"<td>Factor (A)</td><td>",round(SCa,4),"</td><td>",round(DFa,4),"</td><td>",round(MCa,4),"<td>",round(aF,4)," (p = ",round(apValue,4),")</td>",
	"</tr><tr>",
	"<td>Subject (S)</td><td>",round(SCs,4),"</td><td>",round(DFs,4),"</td><td>",round(MCs,4),"<td>",round(sF,4)," (p = ",round(spValue,4),")</td>",
	"</tr><tr>",
	"<td>Error (AxS)</td><td>",round(SCas,4),"</td><td>",round(DFas,4),"</td><td>",round(MCas,4),"</td>",
	"</tr><tr>",
	"<td>Total</td><td>",round(SCtotal,4),"</td><td>",DFtotal,"</td>",
	"</tr></table>"
      )
    }
  })
  
  output$downloadSample <- downloadHandler(
    filename = function() { 
      if(input$repeatedMeasures == FALSE){
	"sampleDataOneWay.csv" 
      } else {
	"sampleDataRepeated.csv" 
      }            
    },
    
    content = function(file) {
    if(input$repeatedMeasures == FALSE){
	write.csv(read.csv("data/testDataOneWay.csv"), file, row.names=FALSE, quote=FALSE);
      } else {
	write.csv(read.csv("data/testDataRepeated.csv"), file, row.names=FALSE, quote=FALSE);
      }      
    }
  )  
})
 
