
library(DEploid)
library(plotly)
library(ggplot2)
source("tmp.r")
source("histWSAF.R")
source("plotWSAFvsPLAF.R")
source("plot.total.coverage.R")

function(input, output, session) {
  VCF <- reactive({
    infile <- input$File1
    if (is.null(infile)) {
      return(NULL)
    }
  })
  PLAF <- reactive({
    infile <- input$File2
    if (is.null(infile)) {
      return(NULL)
    }
  })

  ########## tabPanel 1. Data Display
  output$orig_data <-renderTable({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    head(coverage, n = 20)
  })
  
  output$plaf_data <-renderTable({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    plafFile <- input$File2$datapath
    plaf <- as.data.frame(extractPLAF(plafFile))
    combine <- data.frame(coverage$CHROM, coverage$POS, plaf)
    colnames(combine) <- c("CHROM", "POS", "PLAF")
    head(combine, n = 20)
  })
  
  ########## tabPanel 2. Total Coverage 
  output$text <- renderText({
    HTML(paste("Description", "Red dots represent ouliars being eliminated",
               sep="<br/>"))
    })
  
  output$total <- renderPlot({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    ref <- coverage$refCount
    alt <- coverage$altCount
    chroms = coverage$CHROM
    plot.total.coverage(ref, alt, chroms, cex.lab = 1, cex.main = 1, cex.axis = 1,  
                        threshold = 0.995, window.size = 10)
  })  
  
  ########## tabPanel 3. ALT vs REF 
  output$text1 <- renderText({
    HTML(paste("Description", "Scatter Plot demonstrates relationship between ref and alt",
               sep="<br/>")
    )})
  
  output$plot <- renderPlot({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    plotAltVsRef(coverage$refCount, coverage$altCount)
  })
  

  ########## tabPanel 4. WSAF
  output$text2 <- renderText({
    HTML(paste("WSAF Description", "Allele Frequency within sample.",
               sep="<br/>")
    )})
  
  output$text3 <- renderText({
    HTML(paste("WSAF v.s.PLAF Description", "Allele Frequency within sample vs within population.",
               sep="<br/>")
    )})
  
  output$wsaf <- renderPlotly({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    obsWSAF = computeObsWSAF(coverage$altCount, coverage$refCount )
    histWSAF(obsWSAF)
    # plot_ly(mtcars, x = ~mpg, y = ~wt)
  })
  
  output$wsvspl <- renderPlotly({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    obsWSAF = computeObsWSAF(coverage$altCount, coverage$refCount )
    plafFile <- input$File2$datapath
    plaf <- extractPLAF(plafFile)
    # PG0390CoverageVcf.deconv = dEploid(paste("-vcf", vcfFile, "-plaf", plafFile, "-noPanel"))
    # prop = PG0390CoverageVcf.deconv$Proportions[dim(PG0390CoverageVcf.deconv$Proportions)[1],]
    # expWSAF = t(PG0390CoverageVcf.deconv$Haps) %*% prop
    plotWSAFvsPLAF(plaf, obsWSAF)
  })
  

}




