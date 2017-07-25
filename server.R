library(dplyr)
library(quantmod)
library(RCurl)

# source("plaf.get.R")
source("plot.total.coverage.R")
source("plotAltVsRef.plotly.R")
source("histWSAF.plotly.R")
source("plotWSAFvsPLAF.plotly.R")

source("chromosome.plotly.R")
source("chromosome.dygraphs.R")
# location=read.csv("location.txt", header = TRUE, sep = "\t")

# plaf <<- plafFile$PLAF
# decovlutedGlobal <<- dEploid(paste("-vcf", vcfFile, "-plaf", plafFile, "-noPanel", "-nSample 100"))
# propGlobal <<- decovlutedGlobal$Proportions[dim(decovlutedGlobal$Proportions)[1],]
# expWSAFGlobal <<- t(decovlutedGlobal$Haps) %*% propGlobal

function(input, output, session) {
  
  ########## tabPanel 1. Sample Info
  
  output$ui <- renderUI({
    if (is.null(input$sample))
      return()
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$sample,
           "Plasmodium Falciparum" = selectInput("origins", "Where is it coming from?", 
                              c("Malawi" = "af1_1", "Congo" = "af1_2",
                                "Ghana (Kassena)" = "af2",
                                "Nigeria" = "af3_1", "Senegal" = "af3_2", "Mali" = "af3_3",
                                "Gambia" = "af4_1", "Guinea" = "af4_2", "Ghana (Kintampo)" = "af4_3",
                                "Cambodia (Pursat)" = "as5_1", "Cambodia (Pailin)" = "as5_2", "Thailand (Sisakhet)" = "as5_3",
                                "Vietnam" = "as6_1", "Laos" = "as6_2", "Cambodia (Ratanakiri)" = "as6_3", "Cambodia (Preah Vihear)" = "as6_4",
                                "Bangladesh" = "as7_1", "Myanmar" = "as7_2", "Thailand (Mae Sot)" = "as7_3", "Thailand (Ranong)" = "as7_4")),
           
           "Plasmodium Vivax" = selectInput("origins", "Where is it coming from?", 
                              c("Thailand" = "pv1",
                                "Indonesia" = "pv2_1", "Malaysia" = "pv2_2", "Papua New Guinea" = "pv2_3",
                                "Cambodia" = "pv3_1", "Vietnam" = "pv3_2", "Laos" = "pv3_3",
                                "Myanmar (Burma)" = "pv4_1", "China" = "pv4_2", "Sri Lanka" = "pv4_3", "India" = "pv11_4")))
  })
  
  output$mymap <- renderLeaflet({
    originlist <<- c("af1_1","af1_2",
                   "af2",
                   "af3_1","af3_2","af3_3",
                   "af4_1","af4_2","af4_3",
                   "as5_1","as5_2","as5_3",
                   "as6_1","as6_2","as6_3","as6_4",
                   "as7_1","as7_2","as7_3","as7_4",
                   "pv1",
                   "pv2_1", "pv2_2", "pv2_3",
                   "pv3_1", "pv3_2", "pv3_3",
                   "pv4_1", "pv4_2", "pv4_3", "pv4_4")
    p = which(originlist == input$origins)

    lats = c(-13.950000, -4.316667, 10.884722, 9.066667, 14.666667, 12.650000, 13.466667, 9.516667,
             8.052222, 12.533333, 12.850556, 15.120000, 16.166667, 17.966667, 13.733333, 14.390000, 
             23.7, 19.75, 16.713056, 9.966944, 13.75, -6.175, 3.133333, -9.5, 11.55, 16.166667,
             17.966667, 19.75, 39.916667, 6.933333, 28.613333)
    longs = c(33.700000, 15.316667, -1.090278, 7.483333, -17.416667, -8.000000, -16.600000, -13.700000,
              -1.734722, 103.916667, 102.609444, 104.321667, 107.833333, 102.600000, 107.000000,
              104.680000, 90.350000, 96.100000, 98.574722, 98.635556, 100.483333, 106.828333, 101.683333,
              147.116667, 104.916667, 107.833333, 102.6, 96.1, 116.383333, 79.866667, 77.208333)
      
    p1 = longs[p]
    p2 = lats[p]
    coor = data.frame(lat = p2,lng = p1)
    
    leaflet(coor) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addMarkers(lng = p1, lat = p2, popup = "Origin") %>%
      addCircleMarkers(radius = 18, color = c("red")) %>%
      setView(lng = p1, lat = p2, zoom = 4)
    })
  


  ########## tabPanel 2. Sample sequence exploration
  ### check if data is ready
  output$coverage <-renderTable({
    vcfFile <- input$File1$datapath
    coverageGlobal <<- extractCoverageFromVcf(vcfFile)
    head(coverageGlobal, n = 5)
  })
  output$plaf <-renderTable({
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
            "https://ndownloader.figshare.com/files/8947999?private_link=f09830a270360a4fe4a5")
    p = which(originlist == input$origins)
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
                     11,11,11,11)
    # urls.position = as.numeric(str_sub(input$origins,3,3))
    urls.position = positionlist[p]
    url_content = urls[urls.position]
    myfile <- getURL(url_content)
    plafFile <- read.table(textConnection(myfile), header=T)
    plaf <<- plafFile$PLAF
    head(plafFile, 5)
  })

  output$total <- renderPlot({
    plot.total.coverage(coverageGlobal$refCount, coverageGlobal$altCount, 
                        coverageGlobal$CHROM, cex.lab = 1, cex.main = 1, cex.axis = 1,
                        threshold = 0.995, window.size = 10)
  })

  output$altvsref <- renderPlotly({
    plotAltVsRef.plotly(coverageGlobal$refCount, coverageGlobal$altCount)
  })
  
  output$wsafhist <- renderPlotly({
    obsWSAF <<- computeObsWSAF(coverageGlobal$refCount, coverageGlobal$altCount)
    histWSAF.plotly(obsWSAF)
  })
  
  ### onlye works when plaf and obsWSAF have same length
  ### match by CHROM and POS instead???
  output$wsvspl <- renderPlotly({
    plaf2 = plaf[1:length(coverageGlobal$CHROM)]
    plotWSAFvsPLAF.plotly(plaf2, obsWSAF)
  })
  

}
