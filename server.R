rm(list=ls())
library(quantmod)
library(RCurl)

# allow maximum vcf upload to 100mb
options(shiny.maxRequestSize=100*1024^2)


# source("plaf.get.R")
source("src.R")
source("dEploidPlotly.R")

rancoor <- read.csv("data/random.coordinates.csv")
cencoor <- read.csv("data/center.coordinates.csv")
pvgff <- read.delim("data/PlasmoDB-33_PvivaxSal1.gff", header=F, comment.char="#")
pfgff <- read.delim("data/PlasmoDB-33_Pfalciparum3D7.gff", header=F, comment.char="#")

coverageUntrimmedGlobal = NULL
coverageTrimmedGlobal = NULL
plafUntrimmedGlobal = NULL
plafTrimmedGlobal = NULL

deconvolutedGlobal = NULL


#emptyVcfReminder <- function(){
#  plot(c(0,1),c(0,1),type="n", xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")
#  text(.5, .5, labels = "Please provide a VCF file in the \"Sample infos\" page.", cex = 3)
#}


#trimmingReminder <- function(){
#  plot(c(0,1),c(0,1),type="n", xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")
#  text(.5, .5, labels = "Working hard on the data.", cex = 3)
#}


#loadingReminder <- function(){
#  plot(c(0,1),c(0,1),type="n", xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")
#  text(.5, .5, labels = "Loading ...", cex = 3)
#}


letsTrimPlafVcf <- function (coverageVCF, plafFile) {
  cat("Log: trim VCF and plaf")
  coverageVCF$MATCH <- paste(coverageVCF$CHROM, coverageVCF$POS, sep = "-")
  plafFile$MATCH <- paste(plafFile$CHROM, plafFile$POS, sep = "-")
  ### assertion: take a look at r-package: assertthat

  # Two stage of trimming
  #  1. trim them so they have the same sites
  #  2. trim off sites where REF+ALT<5 and PLAF == 0

  ### Trim1
  # get index of plaf
  plafIndex <- which(plafFile$MATCH %in% coverageVCF$MATCH)
  plafFileTrim1 <- plafFile[plafIndex, ]
  # get index of coverage
  coverageIndex <- which(coverageVCF$MATCH %in% plafFile$MATCH)
  coverageTrim1 <- coverageVCF[coverageIndex, ]


  trimIdx = which((plafFileTrim1$PLAF != 0) &
                        (coverageTrim1$refCount + coverageTrim1$altCount >= 5))
  ### Trim2
  plafTrim2 <- plafFileTrim1[trimIdx,]
  coverageTrim2 <- coverageTrim1[trimIdx,]


  ### write files
  plafTrim2[["MATCH"]] <- NULL
  altTrim2 <- data.frame(CHROM = coverageTrim2$CHROM,
                         POS = coverageTrim2$POS,
                         altCount = coverageTrim2$altCount)
  refTrim2 <- data.frame(CHROM = coverageTrim2$CHROM,
                         POS = coverageTrim2$POS,
                         refCount = coverageTrim2$refCount)
  # obsWSAFtmp <- alttmp/(reftmp + alttmp)

  write.table(plafTrim2, file = "tmpPLAF.txt", sep = "\t", quote = F, row.names = F)
  write.table(altTrim2, file = "tmpALT.txt", sep = "\t", quote = F, row.names = F)
  write.table(refTrim2, file = "tmpREF.txt", sep = "\t", quote = F, row.names = F)

  isBothPlafVcfTrimmed <<- TRUE
  coverageTrimmedGlobal <<- extractCoverageFromTxt("tmpREF.txt", "tmpALT.txt")
  plafTrimmedGlobal <<- plafTrim2
  return (NULL)
}


originlist <- cencoor$ID


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
           "Plasmodium Falciparum" = selectInput("inputOrigin", "Where was the sample collected?",
                                                 c("Malawi" = "af1_1", "Congo" = "af1_2",
                                                   "Ghana (Kassena)" = "af2",
                                                   "Nigeria" = "af3_1", "Senegal" = "af3_2", "Mali" = "af3_3",
                                                   "Gambia" = "af4_1", "Guinea" = "af4_2", "Ghana (Kintampo)" = "af4_3",
                                                   "Cambodia (Pursat)" = "as5_1", "Cambodia (Pailin)" = "as5_2", "Thailand (Sisakhet)" = "as5_3",
                                                   "Vietnam" = "as6_1", "Laos" = "as6_2", "Cambodia (Ratanakiri)" = "as6_3", "Cambodia (Preah Vihear)" = "as6_4",
                                                   "Bangladesh" = "as7_1", "Myanmar" = "as7_2", "Thailand (Mae Sot)" = "as7_3", "Thailand (Ranong)" = "as7_4",
                                                   "Lab" = "lab")),

           "Plasmodium Vivax" = selectInput("inputOrigin", "Where is it coming from?",
                                            c("Thailand" = "pv1",
                                              "Indonesia" = "pv2_1", "Malaysia" = "pv2_2", "Papua New Guinea" = "pv2_3",
                                              "Cambodia" = "pv3_1", "Vietnam" = "pv3_2", "Laos" = "pv3_3",
                                              "Myanmar (Burma)" = "pv4_1", "China" = "pv4_2", "Madagascar" = "pv4_3", "Sri Lanka" = "pv4_4", "Brazil" = "pv4_5", "India" = "pv4_6")))
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
      xtmp = runif(rancoortmp$sample.size[i],rancoortmp$lats.min[i], rancoortmp$lats.max[i])
      x = append(x, xtmp)
      set.seed(123)
      ytmp = runif(rancoortmp$sample.size[i],rancoortmp$longs.min[i], rancoortmp$longs.max[i])
      y = append(y, ytmp)
    }
    df = data.frame(y, x)
    colnames(df) = c("lng", "lat")

    leaflet(df) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addCircleMarkers(radius = 1.7, color = "#ff0048", stroke = FALSE, fillOpacity = 0.7) %>%
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

    urls = c("https://ndownloader.figshare.com/files/8916217?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8916220?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8916223?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8916226?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8916229?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8916232?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8916235?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8947990?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8947993?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8947996?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8947999?private_link=f09830a270360a4fe4a5",
             "https://ndownloader.figshare.com/files/8948002?private_link=f09830a270360a4fe4a5")
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



#  output$panelDataPlafTable <-renderTable({ # set the default to lab,
#    if (is.null(input$inputOrigin)){
#      return(NULL)
#    }

#    fetchPLAF()

#    if (is.null(plafUntrimmedGlobal)){
#      return (NULL)
#    }

#    head(plafUntrimmedGlobal, 5)
#  })


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
    return(plot.total.coverage.dygraphs(coverageTrimmedGlobal$refCount, coverageTrimmedGlobal$altCount,
                                   coverageTrimmedGlobal, cex.lab = 1, cex.main = 1, cex.axis = 1,
                                   threshold = 0.995, window.size = 10))
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
    plotAltVsRefPlotly(coverageTrimmedGlobal$refCount, coverageTrimmedGlobal$altCount)
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

      #      letsTrimPlafVcf(coverageUntrimmedGlobal, plafUntrimmedGlobal)
    }

    cat ("log: panelDataHistWSAF2\n")
    tmpobsWSAF <- coverageTrimmedGlobal$altCount/(coverageTrimmedGlobal$refCount + coverageTrimmedGlobal$altCount)
    plotHistWSAFPlotly(tmpobsWSAF)
  })

#  ### match VCF and PLAF by CHROM and POS instead

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

#      letsTrimPlafVcf(coverageUntrimmedGlobal, plafUntrimmedGlobal)
    }
    cat ("log: panelDataWSAFVsPLAF\n")

    tmpobsWSAF <- coverageTrimmedGlobal$altCount/(coverageTrimmedGlobal$refCount + coverageTrimmedGlobal$altCount)
    head(plafTrimmedGlobal, 5)

#    decovlutedGlobal <<- dEploid(paste("-ref", "tmpREF.txt", "-alt", "tmpALT.txt", "-plaf", "tmpPLAF.txt", "-noPanel"))
#    propGlobal <<- decovlutedGlobal$Proportions[dim(decovlutedGlobal$Proportions)[1],]
#    expWSAFGlobal <<- t(decovlutedGlobal$Haps) %*% propGlobal
    plotWSAFVsPLAFPlotly(plafTrimmedGlobal[,3], tmpobsWSAF, coverageTrimmedGlobal$refCount, coverageTrimmedGlobal$altCount)
  })


#  deconvolute <- reactive({
#    if (is.null(input$inputVCFfile)){
#      cat("Log: no VCF, cann't deconvolute\n")
#      return()
#    }

#    deconvolutedGlobal <<- dEploid(paste("-ref", "tmpREF.txt", "-alt", "tmpALT.txt", "-plaf", "tmpPLAF.txt", "-noPanel"))
#    vcfFile <- input$inputVCFfile$datapath
#    coverageUntrimmedGlobal <<- extractCoverageFromVcf(vcfFile)
#  })


#  uiOutput("inputOriginUI")
  output$inputCHROMUI <- renderUI({
    if (is.null(input$inputSample)){
      return()}
    # Depending on input$input_type, we'll generate a different
    # CHROMOSOME and send it to the client.
    switch(input$inputSample,
           "Plasmodium Falciparum" = selectInput("inputCHROM", h4("Choose a CHROMOSOME"),
                                                 c("Pf3D7_01_v3" = "1", "Pf3D7_02_v3" = "2", "Pf3D7_03_v3" = "3",
                                                   "Pf3D7_04_v3" = "4", "Pf3D7_05_v3" = "5", "Pf3D7_06_v3" = "6",
                                                   "Pf3D7_07_v3" = "7", "Pf3D7_08_v3" = "8", "Pf3D7_09_v3" = "9",
                                                   "Pf3D7_10_v3" = "10", "Pf3D7_11_v3" = "11", "Pf3D7_12_v3" = "12",
                                                   "Pf3D7_13_v3" = "13", "Pf3D7_14_v3" = "14")),

           "Plasmodium Vivax" = selectInput("inputCHROM", h4("Choose a CHROMOSOME"),
                                            c("Pv_Sal1_chr01" = "1", "Pv_Sal1_chr02" = "2", "Pv_Sal1_chr03" = "3",
                                              "Pv_Sal1_chr04" = "4", "Pv_Sal1_chr05" = "5", "Pv_Sal1_chr06" = "6",
                                              "Pv_Sal1_chr07" = "7", "Pv_Sal1_chr08" = "8", "Pv_Sal1_chr09" = "9",
                                              "Pv_Sal1_chr10" = "10", "Pv_Sal1_chr11" = "11", "Pv_Sal1_chr12" = "12",
                                              "Pv_Sal1_chr13" = "13", "Pv_Sal1_chr14" = "14")))
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

     prop = deconvolutedGlobal$Proportions[dim(deconvolutedGlobal$Proportions)[1],]
     expWSAF = t(deconvolutedGlobal$Haps) %*% prop
     obsWSAF <- coverageTrimmedGlobal$altCount/(coverageTrimmedGlobal$refCount + coverageTrimmedGlobal$altCount)

     chroms = unique(coverageTrimmedGlobal$CHROM)
     par.level = str_sub(input$inputCHROM, 1, 2)

     wsaf.list = list()
     gene.list = list()
     exon.list = list()
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
         idx = which(coverageTrimmedGlobal$CHROM == chroms[chromi])
         wsaf.list[[as.character(chroms[chromi])]] = data.frame(
           pos = coverageTrimmedGlobal$POS[idx], obsWSAF = obsWSAF[idx], expWSAF = expWSAF[idx])
         idx2 = which(gene$V1 == chroms[chromi])
         gene = gene[idx2, ]

         row.names(gene) = c(1:nrow(gene))
         p = c()
         for (i in 1:nrow(gene)) {
           vec = !(wsaf.list[[as.character(chroms[chromi])]]$pos %in% c(gene$V4[i]:gene$V5[i]))
           if (all(vec, na.rm = TRUE)) {
             p = append(p, i)}
         }
         allrow = as.numeric(row.names(gene))
         idx3 = allrow[!(allrow %in% p)]
         pos1 = gene$V4[idx3]
         pos2 = gene$V5[idx3]
         gene.list[[as.character(chroms[chromi])]] = data.frame(pos1, pos2)
         ###
         idx4 = which(exon$V1 == chroms[chromi])
         exon = exon[idx4, ]

         row.names(exon) = c(1:nrow(exon))
         p2 = c()
         for (j in 1:nrow(exon)) {
           vec2 = !(wsaf.list[[as.character(chroms[chromi])]]$pos %in% c(exon$V4[j]:exon$V5[j]))
           if (all(vec2, na.rm = TRUE)) {
             p2 = append(p2, j)}
         }
         allrow2 = as.numeric(row.names(exon))
         idx5 = allrow2[!(allrow2 %in% p2)]
         pos3 = exon$V4[idx5]
         pos4 = exon$V5[idx5]
         exon.list[[as.character(chroms[chromi])]] = data.frame(pos3, pos4)
     }

     checkft = as.character(unique(coverageTrimmedGlobal$CHROM))
     type=""
     for(i in input$inputCHROM){
       type = paste(type, checkft[as.integer(i)], sep = "")
     }
     if (is.null(input$inputVCFfile)){
       return (NULL)
     }
     plotWSAFVsPOSDygraphs(wsaf.list[[type]], gene.list[[type]],
       exon.list[[type]])
  })


  output$panelSequenceDeconObsVsExpWSAF <- renderPlotly({
    if (is.null(input$inputVCFfile)){
      validate(
        need(input$inputVCFfile != "", "Please provide a VCF file")
      )
      return(NULL)
    }
    deconvolutionIsCompleted <- TRUE
    prop = deconvolutedGlobal$Proportions[dim(deconvolutedGlobal$Proportions)[1],]
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
    prop$x = c(1:nrow(prop))
    plotProportionsPlotly(prop)
  })
  
  
  output$panelMCMCLLK <- renderPlotly({
    if (is.null(deconvolutedGlobal)){
      return(NULL)
    }
    deconvolutionIsCompleted <- TRUE
    llk = coverageTrimmedGlobal$llks
    llkEvent = coverageTrimmedGlobal$llksStates
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

    deconvolutedGlobal <<- dEploid(paste("-ref", "tmpREF.txt", "-alt", "tmpALT.txt", "-plaf", "tmpPLAF.txt", "-noPanel", "-nSample 100 -rate 5"))
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
      paste("haplotypes.txt", sep = "")
    },
    content = function(file) {
      write.table(t(deconvolutedGlobal$Haps), file, sep = "\t", col.names = T, row.names = F, quote = F)
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

