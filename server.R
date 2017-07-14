library(DEploid)

function(input, output) {
  output$plot <- renderPlot({

    input$newplot

    vcfFile = "~/DEploid-ShinyApp/Data/PG0390-C.test.vcf.gz"
#    vcfFile <- input$vcfFile

#    if (is.null(vcfFile))
#      return(NULL)

    coverage <- extractCoverageFromVcf(vcfFile)

    plot(coverage$refCount, coverage$altCount)

  })
}
