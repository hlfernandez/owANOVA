library(shiny)

shinyUI(fluidPage(
  titlePanel("Welcome to owANOVA!"),

  # Sidebar with a controls panel and a main panel with output
  sidebarLayout(
    sidebarPanel(    
      h4("One-way analysis of variance"),
      a("http://en.wikipedia.org/wiki/One-way_analysis_of_variance"),
      tags$hr(),      
      
      h3("Input data"),
      fileInput('file', 'Choose the file containing your sample data:',
	accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')
	),                
	
      sliderInput(
	"confLevel",
        "Confidence level:",
        min = 1,
        max = 100,
        value = 95
      ),	
      
      tags$hr(),      
      h4("Data format"),
      helpText("A CSV file with one column by factor"),
      pre("
	A1,A2,A3
	1,2,3
	1,3,4"
      ),
      downloadButton("downloadSample2", "Download sample data")
     ),     
      
    mainPanel(
      htmlOutput("analysis")   
    )
  )
))
 
