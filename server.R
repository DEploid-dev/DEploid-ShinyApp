
library(DEploid)
source("tmp.r")
source("histWSAF.R")
function(input, output, session) {
  VCFFile <- reactive({
    infile <- input$vcfFile
    if (is.null(infile)) {
      return(NULL)
    }
  })
  PLAFFile <- reactive({
    infile <- input$plafFile
    if (is.null(infile)) {
      return(NULL)
    }
  })

  
  output$orig_data <-renderTable({
    vcf <- input$vcfFile$datapath
    coverage <- extractCoverageFromVcf(vcf)
    head(coverage, n = 20)
  })
  
  output$plaf_data <-renderTable({
    
    plaf <- input$plafFile$datapath
    explaf <- extractPLAF(plaf)
    head(explaf, n = 20)
  })
  
  output$plot <- renderPlot({
    vcf <- input$vcfFile$datapath
    coverage <- extractCoverageFromVcf(vcf)
    plotAltVsRef(coverage$refCount, coverage$altCount)
  })
  
  output$text1 <- renderText(
    "i.e. Allele Frequency within Sample")
  
  output$wsaf <- renderPlot({
    vcf <- input$vcfFile$datapath
    coverage <- extractCoverageFromVcf(vcf)
    obsWSAF = computeObsWSAF(coverage$altCount, coverage$refCount )
    histWSAF(obsWSAF)
  })
}
