library(DEploid)
library(dplyr)
library(quantmod)

source("plotAltVsRef.plotly.R")
source("histWSAF.plotly.R")
source("plotWSAFvsPLAF.plotly.R")
source("plot.total.coverage.R")
source("chromosome.plotly.R")
source("chromosome.dygraphs.R")

coverageGlobal = c()
decovlutedGlobal = list()
expWSAFGlobal = c()

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

  ########## tabPanel 3. WSAF across Chromosome
  output$text4 <- renderText({
    HTML(paste("Description", "Allele frequency across different chromosome.",
               sep="<br/>")
    )})

  output$chromList <- renderUI({
#    vcfFile <- input$File1$datapath
#    coverage <- extractCoverageFromVcf(vcfFile)

    checkft = as.character(unique(coverageGlobal$CHROM))
    return(checkft)
  })

  output$dygraph <- renderDygraph ({
#    vcfFile <- input$File1$datapath
#    plafFile <- input$File2$datapath

    checkft = as.character(unique(coverageGlobal$CHROM))

    type=""
    for(i in input$select){
        type = paste(type,checkft[as.integer(i)], sep = "")
    }

    obsWSAF = computeObsWSAF( coverageGlobal$altCount, coverageGlobal$refCount )
    ### ADD ACTION FOR DECONVOLUTION
    chroms = unique(coverageGlobal$CHROM)

    wsaf.list = list()
    for (chromi in 1:length(chroms)){
        idx = which(coverageGlobal$CHROM == chroms[chromi])
        wsaf.list[[as.character(chroms[chromi])]] = data.frame(
          pos = coverageGlobal$POS[idx], obsWSAF = obsWSAF[idx], expWSAF = expWSAFGlobal[idx])
    }

    plot.wsaf.vs.pos.dygraph (wsaf.list[[type]], chrom = type)
  })




  ########## tabPanel 4. ALT vs REF
  output$text1 <- renderText({
    HTML(paste("Description", "Scatter plot demonstrates relationship between numbers of reference and alternative alleles",
               sep="<br/>")
    )})

  output$plot <- renderPlotly({
    plotAltVsRef.plotly(coverageGlobal$refCount, coverageGlobal$altCount)
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




