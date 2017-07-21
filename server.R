library(DEploid)
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

  ########## tabPanel 3. WSAF across Chromosome
  output$text4 <- renderText({
    HTML(paste("Description", "Allele frequency across different chromosome.",
               sep="<br/>")
    )})

  output$chromList <- renderUI({
  vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)

    checkft = as.character(unique(coverage$CHROM))
    return(checkft)
  })

  output$dygraph <- renderDygraph ({
    vcfFile <- input$File1$datapath
    plafFile <- input$File2$datapath

    coverage <- extractCoverageFromVcf(vcfFile)

    checkft = as.character(unique(coverage$CHROM))

    type=""
    for(i in input$select){
        type = paste(type,checkft[as.integer(i)], sep = "")
    }

    obsWSAF = computeObsWSAF( coverage$altCount, coverage$refCount )

    ### ADD ACTION FOR DECONVOLUTION
    deconvoluted = dEploid(paste("-vcf", vcfFile, "-plaf", plafFile, "-noPanel"))
    prop = deconvoluted$Proportions[dim(deconvoluted$Proportions)[1],]
    expWSAF = t(deconvoluted$Haps) %*% prop

    chroms = unique(coverage$CHROM)

    wsaf.list = list()
    for (chromi in 1:length(chroms)){
        idx = which(coverage$CHROM == chroms[chromi])
        wsaf.list[[as.character(chroms[chromi])]] = data.frame(
          pos = coverage$POS[idx], obsWSAF = obsWSAF[idx], expWSAF = expWSAF[idx])
    }

    plot.wsaf.vs.pos.dygraph (wsaf.list[[type]], chrom = type)
  })




  ########## tabPanel 4. ALT vs REF
  output$text1 <- renderText({
    HTML(paste("Description", "Scatter plot demonstrates relationship between numbers of reference and alternative alleles",
               sep="<br/>")
    )})

  output$plot <- renderPlotly({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    ref <- coverage$refCount
    alt <- coverage$altCount
    plotAltVsRef.plotly(ref, alt)
  })


  ########## tabPanel 5. WSAF
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
    obsWSAF = computeObsWSAF(coverage$altCount, coverage$refCount)
    histWSAF.plotly(obsWSAF)
  })

  output$wsvspl <- renderPlotly({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    obsWSAF = computeObsWSAF(coverage$altCount, coverage$refCount)
    plafFile <- input$File2$datapath
    plaf <- extractPLAF(plafFile)
    # PG0390CoverageVcf.deconv = dEploid(paste("-vcf", vcfFile, "-plaf", plafFile, "-noPanel"))
    # prop = PG0390CoverageVcf.deconv$Proportions[dim(PG0390CoverageVcf.deconv$Proportions)[1],]
    # expWSAF = t(PG0390CoverageVcf.deconv$Haps) %*% prop
    plotWSAFvsPLAF.plotly(plaf, obsWSAF)
  })


}




