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
    data <- loadDataset()
    result <- owAnova(data)
    HTML(
      result
   )
  })
  
  output$downloadSample2 <- downloadHandler(
    filename = function() { "sampleData.csv" },
    content = function(file) {
      write.table(read.csv("data/testData.csv",header=FALSE), file, col.names=FALSE, row.names=FALSE, quote=FALSE);
    }
  )  
})
 
