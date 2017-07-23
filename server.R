library(dplyr)
library(quantmod)

source("plotAltVsRef.plotly.R")
source("histWSAF.plotly.R")
source("plotWSAFvsPLAF.plotly.R")
source("plot.total.coverage.R")
source("chromosome.plotly.R")
source("chromosome.dygraphs.R")


function(input, output, session) {
  VCF <- reactive({
    infile <- input$File1
    if (is.null(infile)) {
      return(NULL)
    }
  }
  )

  PLAF <- reactive({
    infile <- input$File2
    if (is.null(infile)) {
      return(NULL)
    }
  }
  )

  ########## tabPanel 1. Data Display
  output$orig_data <-renderTable({
    vcfFile <- input$File1$datapath
    coverageGlobal <<- extractCoverageFromVcf(vcfFile)
    head(coverageGlobal, n = 20)
  })

  output$plaf_data <-renderTable({
    vcfFile <- input$File1$datapath
    plafFile <- input$File2$datapath
    plaf <- read.table(plafFile, header=T)
    decovlutedGlobal <<- dEploid(paste("-vcf", vcfFile, "-plaf", plafFile, "-noPanel", "-nSample 100"))
    propGlobal <<- decovlutedGlobal$Proportions[dim(decovlutedGlobal$Proportions)[1],]
    expWSAFGlobal <<- t(decovlutedGlobal$Haps) %*% propGlobal
    head(plaf, n = 20)
  })

  ########## tabPanel 2. Total Coverage
  output$text <- renderText({
    HTML(paste("Description", "Red dots represent ouliars being eliminated",
               sep="<br/>"))
    })

  output$total <- renderPlot({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    plot.total.coverage(coverageGlobal$refCount, coverageGlobal$altCount, coverageGlobal$CHROM, cex.lab = 1, cex.main = 1, cex.axis = 1,
                        threshold = 0.995, window.size = 10)
  })


  output$plot <- renderPlotly({
    plotAltVsRef.plotly(coverageGlobal$refCount, coverageGlobal$altCount)
  })

}
