
library(DEploid)
source("tmp.r")
function(input, output, session) {
  dataFile <- reactive({
    infile <- input$vcfFile
    if (is.null(infile)) {
      return(NULL)
    }
#    read.csv(infile$datapath, header=input$header, sep=input$sep, 
#             quote=input$quote)

  })
  
  output$orig_data <-renderTable({
    dataFile()
  })
  
  output$plot <- renderPlot({
    data <- input$vcfFile$datapath
    coverage <- extractCoverageFromVcf(data)
    plotAltVsRef(coverage$refCount, coverage$altCount)
  })
}
