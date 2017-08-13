rm(list=ls())
library(quantmod)
library(RCurl)

# allow maximum vcf upload to 100mb
options(shiny.maxRequestSize=100*1024^2)


source("src.R")

rancoor <- read.csv("data/randomCoordinates.csv")
cencoor <- read.csv("data/centerCoordinates.csv")
originlist <- cencoor$ID

urlfile <- read.csv("data/fetchPLAFUrls.csv")
geneDrugZone <- read.csv("data/geneDrugZone.csv")
pvgff <- read.delim("data/PlasmoDB-33_PvivaxSal1.gff",
                    header=F, comment.char="#")
pfgff <- read.delim("data/PlasmoDB-33_Pfalciparum3D7.gff",
                    header=F, comment.char="#")

coverageUntrimmedGlobal = NULL
coverageTrimmedGlobal = NULL
plafUntrimmedGlobal = NULL
plafTrimmedGlobal = NULL
deconvolutedGlobal = NULL






isBothPlafVcfTrimmed = FALSE
deconvolutionIsCompleted = FALSE



function(input, output, session) {

  ########## tabPanel 1. Sample Info

  output$inputOriginUI <- renderUI({
    if (is.null(input$inputSample)){
      return()}
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$inputSample,
           "Plasmodium Falciparum" =
             selectInput(
               "inputOrigin", "Where was the sample collected?",
               c("Malawi" = "af1_1", "Congo" = "af1_2",
                 "Ghana (Kassena)" = "af2",
                 "Nigeria" = "af3_1", "Senegal" = "af3_2", "Mali" = "af3_3",
                 "Gambia" = "af4_1", "Guinea" = "af4_2",
                 "Ghana (Kintampo)" = "af4_3",
                 "Cambodia (Pursat)" = "as5_1", "Cambodia (Pailin)" = "as5_2",
                 "Thailand (Sisakhet)" = "as5_3",
                 "Vietnam" = "as6_1", "Laos" = "as6_2",
                 "Cambodia (Ratanakiri)" = "as6_3",
                 "Cambodia (Preah Vihear)" = "as6_4",
                 "Bangladesh" = "as7_1", "Myanmar" = "as7_2",
                 "Thailand (Mae Sot)" = "as7_3", "Thailand (Ranong)" = "as7_4",
                 "Lab" = "lab")),

           "Plasmodium Vivax" =
             selectInput(
               "inputOrigin", "Where is it coming from?",
               c("Thailand" = "pv1",
                 "Indonesia" = "pv2_1", "Malaysia" = "pv2_2",
                 "Papua New Guinea" = "pv2_3",
                 "Cambodia" = "pv3_1", "Vietnam" = "pv3_2", "Laos" = "pv3_3",
                 "Myanmar (Burma)" = "pv4_1", "China" = "pv4_2",
                 "Madagascar" = "pv4_3", "Sri Lanka" = "pv4_4",
                 "Brazil" = "pv4_5", "India" = "pv4_6")))
  })


  output$panelSampleInfoMap <- renderLeaflet({
    lats = cencoor$lats
    longs = cencoor$longs

    # SET DEFAULT MAP TO af1 group
    coor.level = "af1"
    p = 1

    if (! is.null(input$inputOrigin)){
      coor.level = str_sub(input$inputOrigin, 1, 3)
      p = which(originlist == input$inputOrigin)
    } else {
      leaflet() %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap)
    }

    p1 = longs[p]
    p2 = lats[p]

    rancoortmp = rancoor %>%
      filter(ID == coor.level)
    x = c()
    y = c()
    for (i in 1:nrow(rancoortmp)) {
      set.seed(321)
      xtmp = runif(rancoortmp$sample.size[i],rancoortmp$lats.min[i],
                   rancoortmp$lats.max[i])
      x = append(x, xtmp)
      set.seed(123)
      ytmp = runif(rancoortmp$sample.size[i],rancoortmp$longs.min[i],
                   rancoortmp$longs.max[i])
      y = append(y, ytmp)
    }
    df = data.frame(y, x)
    colnames(df) = c("lng", "lat")

    leaflet(df) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addCircleMarkers(radius = 1.7, color = "#ff0048",
                       stroke = FALSE, fillOpacity = 0.7) %>%
      addMarkers(lng = p1, lat = p2, popup = "Origin") %>%
      addCircleMarkers(lng = p1, lat = p2, radius = 18, color = "blue")
  })


  fetchPLAF <- reactive({
    if (is.null(input$inputOrigin)){
      cat("Log: no input, cann't fetch\n")
      return()
    }

    cat("Log: fetching PLAF\n")

    # since is it's new data, need to trim, and rework with the data again
    isBothPlafVcfTrimmed <<- FALSE

    urls <- as.vector(urlfile$URL)
    p = which(originlist == input$inputOrigin)
    positionlist <- c(1,1,
                      2,
                      3,3,3,
                      4,4,4,
                      5,5,5,
                      6,6,6,6,
                      7,7,7,7,
                      8,
                      9,9,9,
                      10,10,10,
                      11,11,11,11,
                      12)
    urls.position = positionlist[p]
    url_content = urls[urls.position]
    myfile <- getURL(url_content)
    plafUntrimmedGlobal <<- read.table(textConnection(myfile), header=T)
  })


  fetchVCF <- reactive({
    if (is.null(input$inputVCFfile)){
      cat("Log: no VCF, cann't fetch\n")
      return()
    }

    cat("Log: fetching VCF\n")

    # since is it's new data, need to trim, and rework with the data again
    isBothPlafVcfTrimmed <<- FALSE

    vcfFile <- input$inputVCFfile$datapath
    coverageUntrimmedGlobal <<- extractCoverageFromVcf(vcfFile)
  })



  output$panelDataCoverageTable <-renderTable({

    if (is.null(input$inputVCFfile)){
      return(NULL)
    }

    if ( is.null(coverageUntrimmedGlobal) ){
      return (NULL)
    }

    return(head(coverageUntrimmedGlobal, n = 5))
  })


  output$panelDataTotalCoverage <- renderDygraph({
    if (is.null(input$inputVCFfile)){
      validate(
        need(input$inputVCFfile != "", "Please provide a VCF file")
      )
      return(NULL)
    }
    cat ("log: panelDataTotalCoverage\n")
    print(head(coverageTrimmedGlobal))
    threshold <- input$panelDataTotalCoverageThreshold
    window.size <- input$panelDataTotalCoverageWindow

    totalDepth = coverageTrimmedGlobal$refCount + coverageTrimmedGlobal$altCount
    x = 1:length(totalDepth)
    # range(totalDepth)
    tmpQ = quantile(totalDepth, threshold)
    tmpIdx = which((totalDepth > tmpQ ))
    potentialOutliers = fun.find.more(tmpIdx, window.size)
    tmp = data.frame(x, totalDepth)

    return(plot.total.coverage.dygraphs(coverageTrimmedGlobal$refCount,
                                        coverageTrimmedGlobal$altCount,
                                        coverageTrimmedGlobal,
                                        threshold, window.size,
                                        potentialOutliers, tmp))
  })


  output$panelDataAltVsRef <- renderPlotly({
    if (is.null(input$inputVCFfile)){
      validate(
        need(input$inputVCFfile != "", "Please provide a VCF file")
      )
      return(NULL)
    }

    if (is.null(coverageTrimmedGlobal)){
      return (NULL)
    }

    if (!isBothPlafVcfTrimmed){
      if (is.null(coverageUntrimmedGlobal)){
        stop("coverage should have been loaded")
      }

      if (is.null(plafUntrimmedGlobal)){
        stop("plaf should have been loaded")
      }
      cat ("log: Reload plaf and VCF\n")
    }

    cat ("log: panelDataAltVsRef\n")
    print(head(coverageTrimmedGlobal))

    ### find out outliers
    threshold <- input$panelDataTotalCoverageThreshold
    window.size <- input$panelDataTotalCoverageWindow

    totalDepth = coverageTrimmedGlobal$refCount + coverageTrimmedGlobal$altCount
    x = 1:length(totalDepth)
    # range(totalDepth)
    tmpQ = quantile(totalDepth, threshold)
    tmpIdx = which((totalDepth > tmpQ))
    potentialOutliers = fun.find.more(tmpIdx, window.size)
    plotAltVsRefPlotly(coverageTrimmedGlobal$refCount,
                       coverageTrimmedGlobal$altCount,
                       potentialOutliers = potentialOutliers)
  })


  output$panelDataHistWSAF <- renderPlotly({
    if (is.null(input$inputVCFfile)){
      validate(
        need(input$inputVCFfile != "", "Please provide a VCF file")
      )
      return(NULL)
    }

    if (is.null(coverageTrimmedGlobal)){
      return (NULL)
    }

    if (!isBothPlafVcfTrimmed){
      if (is.null(coverageUntrimmedGlobal)){
        stop("coverage should have been loaded")
      }

      if (is.null(plafUntrimmedGlobal)){
        stop("plaf should have been loaded")
      }
      cat ("log: Reload plaf and VCF\n")
    }

    cat ("log: panelDataHistWSAF2\n")
    tmpobsWSAF <- coverageTrimmedGlobal$altCount / (
      coverageTrimmedGlobal$refCount + coverageTrimmedGlobal$altCount)
    plotHistWSAFPlotly(tmpobsWSAF)
  })


  output$panelDataWSAFVsPLAF <- renderPlotly({
    if (is.null(input$inputVCFfile)){
      validate(
        need(input$inputVCFfile != "", "Please provide a VCF file")
      )
      return(NULL)
    }

    if (is.null(input$inputSample)){
      return (NULL)
    }

    if (is.null(coverageTrimmedGlobal)){
      return (NULL)
    }


    print(isBothPlafVcfTrimmed)
    if (!isBothPlafVcfTrimmed){
      if (is.null(coverageUntrimmedGlobal)){
        stop("coverage should have been loaded")
      }

      if (is.null(plafUntrimmedGlobal)){
        stop("plaf should have been loaded")
      }
      cat ("log: Reload plaf and VCF\n")
    }
    cat ("log: panelDataWSAFVsPLAF\n")

    tmpobsWSAF <- coverageTrimmedGlobal$altCount/(
      coverageTrimmedGlobal$refCount + coverageTrimmedGlobal$altCount)
    head(plafTrimmedGlobal, 5)

    threshold <- input$panelDataTotalCoverageThreshold
    window.size <- input$panelDataTotalCoverageWindow

    totalDepth = coverageTrimmedGlobal$refCount + coverageTrimmedGlobal$altCount
    x = 1:length(totalDepth)
    # range(totalDepth)
    tmpQ = quantile(totalDepth, threshold)
    tmpIdx = which((totalDepth > tmpQ ))
    potentialOutliers = fun.find.more(tmpIdx, window.size)

    plotWSAFVsPLAFPlotly(plafTrimmedGlobal[,3], tmpobsWSAF,
                         coverageTrimmedGlobal$refCount,
                         coverageTrimmedGlobal$altCount,
                         potentialOutliers = potentialOutliers)
  })


#  deconvolute <- reactive({
#    if (is.null(input$inputVCFfile)){
#      cat("Log: no VCF, cann't deconvolute\n")
#      return()
#    }

#  deconvolutedGlobal <<- dEploid(paste("-ref", "tmpREF.txt",
#                                       "-alt", "tmpALT.txt",
#                                       "-plaf", "tmpPLAF.txt", "-noPanel"))
#    vcfFile <- input$inputVCFfile$datapath
#    coverageUntrimmedGlobal <<- extractCoverageFromVcf(vcfFile)
#  })


  output$inputCHROMUI <- renderUI({
    if (is.null(input$inputSample)){
      return()}
    vcfCHROMlist <- as.vector(unique(coverageTrimmedGlobal$CHROM))
    switch(input$inputSample,
           "Plasmodium Falciparum" = selectInput("inputCHROM",
                                                 h5("Choose a CHROMOSOME"),
                                                 vcfCHROMlist),

           "Plasmodium Vivax" = selectInput("inputCHROM",
                                            h5("Choose a CHROMOSOME"),
                                            vcfCHROMlist))
  })


  output$inputGeneUI <- renderUI({
    if (is.null(input$inputSample)){
      return()}
    CRTlist <- as.vector(unique(geneDrugZone$CHROM[
      geneDrugZone$gene == "CRT" &
        geneDrugZone$species %in% input$inputSample]))
    DHFRlist <- as.vector(unique(geneDrugZone$CHROM[
      geneDrugZone$gene == "DHFR" &
        geneDrugZone$species %in% input$inputSample]))

    DHPSlist <- as.vector(unique(geneDrugZone$CHROM[
      geneDrugZone$gene == "DHPS" &
        geneDrugZone$species %in% input$inputSample]))

    Kelchlist <- as.vector(unique(geneDrugZone$CHROM[
      geneDrugZone$gene == "Kelch" &
        geneDrugZone$species %in% input$inputSample]))

    MDRlist <- as.vector(unique(geneDrugZone$CHROM[
      geneDrugZone$gene == "MDR1" &
        geneDrugZone$species %in% input$inputSample]))

    Plasmepsinlist <- as.vector(unique(geneDrugZone$CHROM[
      geneDrugZone$gene == "Plasmepsin2&3" &
        geneDrugZone$species %in% input$inputSample]))
    switch(input$panelSequenceDeconWSAFVsPOSGene,
           "CRT" = selectInput("inputZone",
                               h5("Choose a CHROMOSOME"), CRTlist),
           "DHFR" = selectInput("inputZone",
                                h5("Choose a CHROMOSOME"), DHFRlist),
           "DHPS" = selectInput("inputZone",
                                h5("Choose a CHROMOSOME"), DHPSlist),
           "Kelch" = selectInput("inputZone",
                                 h5("Choose a CHROMOSOME"), Kelchlist),
           "MDR1" = selectInput("inputZone",
                                h5("Choose a CHROMOSOME"), MDRlist),
           "Plasmepsin2&3" = selectInput("inputZone",
                                         h5("Choose a CHROMOSOME"),
                                         Plasmepsinlist))
  })


  output$panelSequenceDeconWSAFVsPOS <- renderDygraph ({
    if (is.null(input$inputVCFfile)){
      validate(
        need(input$inputVCFfile != "", "Please provide a VCF file")
      )
      return(NULL)
     }
#     deconvolute()
     if (is.null(deconvolutedGlobal)){
       return(NULL)
     }

     deconvolutionIsCompleted <- TRUE

     prop = deconvolutedGlobal$Proportions[dim(
       deconvolutedGlobal$Proportions)[1],]
     expWSAF = t(deconvolutedGlobal$Haps) %*% prop
     obsWSAF <- coverageTrimmedGlobal$altCount/(
       coverageTrimmedGlobal$refCount + coverageTrimmedGlobal$altCount)

     vcfCHROMlist <- as.vector(unique(coverageTrimmedGlobal$CHROM))
     chroms = unique(coverageTrimmedGlobal$CHROM)

     wsaf.list = list()
     gene.list = list()
     exon.list = list()
     zone.list = list()


#     for(i in input$inputCHROM){
       type = input$inputCHROM
#     }

     if (is.null(type)){
       return(NULL)
     }

     for (chromi in 1:length(chroms)){
       if (input$inputSample == "Plasmodium Falciparum"){
         gene <- pfgff %>%
           filter(V3 == "gene") %>%
           filter(V1 %in% chroms) %>%
           droplevels()
         exon <- pfgff %>%
           filter(V3 == "exon") %>%
           filter(V1 %in% chroms) %>%
           droplevels()
       }
       if (input$inputSample == "Plasmodium Vivax"){
         gene <- pvgff %>%
           filter(V3 == "gene") %>%
           filter(V1 %in% chroms) %>%
           droplevels()
         exon <- pvgff %>%
           filter(V3 == "exon") %>%
           filter(V1 %in% chroms) %>%
           droplevels()
       }
       ### wsaf.list
       idx = which(coverageTrimmedGlobal$CHROM == chroms[chromi])
       wsaf.list[[as.character(chroms[chromi])]] = data.frame(
         pos = coverageTrimmedGlobal$POS[idx],
         obsWSAF = obsWSAF[idx],
         expWSAF = expWSAF[idx])
       ### gene.list
       idx2 = which(gene$V1 == chroms[chromi])
       gene = gene[idx2, ]
       pos1 = gene$V4
       pos2 = gene$V5
       gene.list[[as.character(chroms[chromi])]] = data.frame(
         pos1, pos2)
       ### exon.list
       idx4 = which(exon$V1 == chroms[chromi])
       exon = exon[idx4, ]
       pos3 = exon$V4
       pos4 = exon$V5
       exon.list[[as.character(chroms[chromi])]] = data.frame(
         pos3, pos4)
     }

     if (is.null(input$inputVCFfile)){
       return (NULL)
     }

     ### Add checkboxinput
     checkBoxGene <- "Gene" %in% input$panelSequenceDeconWSAFVsPOSShades
     checkBoxExon <- "Exon" %in% input$panelSequenceDeconWSAFVsPOSShades

     ### Show gene zone when selected
     zone <- geneDrugZone %>%
       filter(species == input$inputSample)
     if (input$panelSequenceDeconWSAFVsPOSControlGene == FALSE){
       zone <- input$inputCHROMUI
       type3 <- type
     } else {
       for (i in input$panelSequenceDeconWSAFVsPOSGene) {
         zone <- geneDrugZone %>%
           filter(gene == input$panelSequenceDeconWSAFVsPOSGene) %>%
           filter(CHROM == input$inputZone)
         pos5 = zone$start
         pos6 = zone$end
         zone.list[[as.character(paste(input$panelSequenceDeconWSAFVsPOSGene,
                                       input$inputZone, sep = ":"))]] = data.frame(pos5, pos6)
       }
     }

     ### type2
     type2 <- as.character(paste(input$panelSequenceDeconWSAFVsPOSGene,
                                 input$inputZone, sep = ":"))
     type3 <- as.character(input$inputZone)

     ### plot
     plotWSAFVsPOSDygraphs(wsaf.list[[type]], gene.list[[type]],
       exon.list[[type]], checkBoxGene, checkBoxExon,
       input$panelSequenceDeconWSAFVsPOSControlGene, zone.list[[type2]],
       wsaf.list[[type3]])
  })


  output$panelSequenceDeconObsVsExpWSAF <- renderPlotly({
    if (is.null(input$inputVCFfile)){
      validate(
        need(input$inputVCFfile != "", "Please provide a VCF file")
      )
      return(NULL)
    }
    deconvolutionIsCompleted <- TRUE
    prop = deconvolutedGlobal$Proportions[dim(
      deconvolutedGlobal$Proportions)[1],]
    expWSAF = t(deconvolutedGlobal$Haps) %*% prop
    obsWSAF <- coverageTrimmedGlobal$altCount/(coverageTrimmedGlobal$refCount +
                                                 coverageTrimmedGlobal$altCount)
    plotObsExpWSAFPlotly(obsWSAF, expWSAF)
  })


  output$panelMCMCProportions <- renderPlotly({
    if (is.null(deconvolutedGlobal)){
      return(NULL)
    }
    deconvolutionIsCompleted <- TRUE
    prop = as.data.frame(deconvolutedGlobal$Proportions)
    pnum = as.numeric(ncol(prop))
    prop$x = c(1:nrow(prop))
    plotProportionsPlotly(prop, pnum)
  })


  output$panelMCMCLLK <- renderPlotly({
    if (is.null(deconvolutedGlobal)){
      return(NULL)
    }
    deconvolutionIsCompleted <- TRUE
    llk = deconvolutedGlobal$llks
    llkEvent = deconvolutedGlobal$llksStates
    plotLLKPlotly(llk, llkEvent)
  })


  observeEvent(input$do, {
    if (is.null(input$inputVCFfile)){
      return (NULL)
    }

    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())

    progress$set(message = "Deconvolution in progress, ",
                 detail = "this may take a few mins ...")

    fetchPLAF()
    fetchVCF()

    letsTrimPlafVcf(coverageUntrimmedGlobal, plafUntrimmedGlobal)




    deconvolutedGlobal <<- dEploid(paste("-ref", "/var/tmp/tmpREF.txt",
                                         "-alt", "/var/tmp/tmpALT.txt",
                                         "-plaf", "/var/tmp/tmpPLAF.txt",
                                         "-noPanel", "-nSample 100 -rate 5"))
    vcfFile <- input$inputVCFfile$datapath
    coverageUntrimmedGlobal <<- extractCoverageFromVcf(vcfFile)
  })


#  observe({
#    if (is.null(deconvolutedGlobal) | (is.null(input$inputVCFfile))){
#      shinyjs::disable("downloadHaplotypes")
#    } else {
#      shinyjs::enable("downloadHaplotypes")
#    }
#  })

  output$downloadHaplotypes <- downloadHandler(
    filename = function() {
      paste("/var/tmp/haplotypes.txt", sep = "")
    },
    content = function(file) {
      write.table(t(deconvolutedGlobal$Haps), file, sep = "\t",
                  col.names = T, row.names = F, quote = F)
    }
  )

  ####################### Explanation boxes #########################

  output$panelSampleInfoExplainSample <- renderText({
    HTML("Note: We use the genomic information extracted from the nearby (marked by red dots shown in the map) parasite sequences to deconvolve the input sequence data")
  })


  output$panelDataExplainTotalCoverage <- renderText({
    if (is.null(input$inputVCFfile)){
      return (NULL)
    }
    cat ("log: panelDataExplainTotalCoverage\n")
    HTML(paste("", "The total coverage is computed as the sum of reference and alternative allele counts at every site. Our experience is that heterozygous sites with high counts for both reference allele and alternative allele can cause over-fitting.",
               sep="<br/>"))
  })

  output$panelDataExplainAltVsRef <- renderText({
    if (is.null(input$inputVCFfile)){
      return (NULL)
    }
    cat ("log: panelDataExplainAltVsRef\n")
    HTML(" the alternative allele count against the reference allele count. As P. falciparum genomes are haploid, in clonal samples, one woule expect to see either alternative or reference allele at any sites. Heterozygous sites are indications of mixed infection.")
  })


  output$panelDataExplainHistWSAF <- renderText({
    if (is.null(input$inputVCFfile)){
      return (NULL)
    }
    cat ("log: panelDataExplainHistWSAF\n")
    HTML("Histogram of the allele frequency within sample. Note that we exclude markers with WSAF strictly equal to 0s and 1s in the histogram.")
  })


  output$panelDataExplainWSAFVsPLAF <- renderText({
    if (is.null(input$inputVCFfile)){
      return (NULL)
    }

    ### need to wait deconvolution finished ...

    cat ("log: panelDataExplainWSAFVsPLAF\n")
    HTML("Allele frequency within sample, compare against the population average.")
  })


  output$panelSequenceDeconExplainWSAFVsPOS <- renderText({
    if (is.null(input$inputVCFfile)){
      return (NULL)
    }
    cat ("log: panelSequenceDeconExplainWSAFVsPOS\n")
    HTML("Allele frequencies within sample across all 14 chromosomes. Expected and observed WSAF are marked in blue and red respectively.")
  })

  ############# Require input data or patience ######################

  output$serverDataState <- renderText({
    if (is.null(input$inputVCFfile)){
      return (NULL)
    } else if (!isBothPlafVcfTrimmed){
      HTML("Loading ... ")
    } else {
      return (NULL)
    }
  })

  output$severDeconvolutionState <- renderText({
    if (deconvolutionIsCompleted){
      return (NULL)
    } else {
#      HTML("Loading ... ")
    }
  })

  output$severMcMcState <- renderText({
    if (deconvolutionIsCompleted){
      return (NULL)
    } else {
#      HTML("Loading ... ")
    }
  })

  ############# Documentation ######################
  output$citeMe <- renderText({
    HTML(paste(toBibtex(citation(package="DEploid")), collapse="\n"))
  })

}

