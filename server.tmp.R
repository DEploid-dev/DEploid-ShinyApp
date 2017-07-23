
library(DEploid)
library(plotly)
library(ggplot2)
library(dplyr)
library(circlize)

source("plotAltVsRef.plotly.R")
source("histWSAF.plotly.R")
source("plotWSAFvsPLAF.plotly.R")
source("plot.total.coverage.R")
source("chromosome.plotly.R")
source("circlize.wsaf.tmp.tmp.R")

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
  
  output$chromo <- renderPlotly({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    coverage2 = merge(coverage, location, 
                      by.x = "CHROM", by.y = "CHROM",
                      all.x = T)
    coverage2$WSAF = coverage2$altCount/(coverage2$altCount + coverage2$refCount)
    # location$size2 = cumsum(location$CHROMSIZE)
    # replace = c()
    # replace = append(location$size2[1:13], replace)
    # replace = append(0,replace)
    # location$size3 = replace
    # coverage2 = merge(coverage2, location, 
    #                   by.x = "CHROM", by.y = "CHROM", all.x = TRUE)
    # coverage2$POS2 = coverage2$POS + coverage2$size3
    
    checkft = c("Pf3D7_01_v3", "Pf3D7_02_v3", "Pf3D7_03_v3", "Pf3D7_04_v3",
                "Pf3D7_05_v3", "Pf3D7_06_v3", "Pf3D7_07_v3", "Pf3D7_08_v3",
                "Pf3D7_09_v3", "Pf3D7_10_v3", "Pf3D7_11_v3", "Pf3D7_12_v3",
                "Pf3D7_13_v3", "Pf3D7_14_v3")
    type=""
    for(i in input$select){
      if(i==1){
        type = paste(type,checkft[1], sep = "")  }
      if(i==2){
        type = paste(type,checkft[2], sep = "")  }
      if(i==3){
        type = paste(type,checkft[3], sep = "")  }
      if(i==4){
        type = paste(type,checkft[4], sep = "")  }
      if(i==5){
        type = paste(type,checkft[5], sep = "")  }
      if(i==6){
        type = paste(type,checkft[6], sep = "")  }
      if(i==7){
        type = paste(type,checkft[7], sep = "")  }
      if(i==8){
        type = paste(type,checkft[8], sep = "")  }
      if(i==9){
        type = paste(type,checkft[9], sep = "")  }
      if(i==10){
        type = paste(type,checkft[10], sep = "")  }
      if(i==11){
        type = paste(type,checkft[11], sep = "")  }
      if(i==12){
        type = paste(type,checkft[12], sep = "")  }
      if(i==13){
        type = paste(type,checkft[13], sep = "")  }
      if(i==14){
        type = paste(type,checkft[14], sep = "")  }
    }
    coverage3 = filter(coverage2, CHROM %in% type)
    plot.wsaf.vs.chromosome.plotly(coverage3)
  }) 
  

  
  
  ########## tabPanel 4. ALT vs REF 
  output$text1 <- renderText({
    HTML(paste("Description", "Scatter Plot demonstrates relationship between ref and alt",
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
  
  ########## tabPanel 6. Circle WSAF
  output$circle <- renderPlot({
    vcfFile <- input$File1$datapath
    coverage <- extractCoverageFromVcf(vcfFile)
    obsWSAF = computeObsWSAF(coverage$altCount, coverage$refCount)
    plafFile <- input$File2$datapath
    PG0390CoverageVcf.deconv = dEploid(paste("-vcf", vcfFile, "-plaf", plafFile, "-noPanel"))
    prop = PG0390CoverageVcf.deconv$Proportions[dim(PG0390CoverageVcf.deconv$Proportions)[1],]
    expWSAF = t(PG0390CoverageVcf.deconv$Haps) %*% prop
    plot.wsaf.vs.index.ring(coverage)
  })  
  

}




