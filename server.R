library(DEploid)
source("tmp.r")
function(input, output) {
  output$plot <- renderPlot({

    input$newplot

    tmpDir = getwd()
    vcfFile = paste(tmpDir, "/Data/PG0390-C.test.vcf.gz", sep="")
#    vcfFile <- input$vcfFile

#    if (is.null(vcfFile))
#      return(NULL)

    coverage <- extractCoverageFromVcf(vcfFile)

    plotAltVsRef(coverage$refCount, coverage$altCount)
  })
}
