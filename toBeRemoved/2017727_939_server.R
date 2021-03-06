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
rancoor <<- read.csv("C:/Users/Hermosa/Desktop/random.coordinates.csv")
location <- read.csv("~/GitHub/DEploid-ShinyApp/Data/location.csv")


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
                                "Myanmar (Burma)" = "pv4_1", "China" = "pv4_2", "Madagascar" = "pv4_3", "Sri Lanka" = "pv4_4", "Brazil" = "pv4_5", "India" = "pv4_6")))
  })
  
  
  output$note1 <- renderText({
    HTML(paste("Title", "Joe Explain",
               sep="<br/>"))
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
                   "pv4_1", "pv4_2", "pv4_3", "pv4_4", "pv4_5", "pv4_6")
    p = which(originlist == input$origins)

    lats = c(-16.166667, -4.316667, 
             10.884722, 
             8.5, 14.783333, 12.650000, 
             13.466667, 7.75, 8.052222, 
             12.533333, 12.850556, 15.120000, 
             11.769167, 14.8, 13.733333, 14.390000, 
             21.458333, 18.25, 16.713056, 9.966944, 
             13.75, 
             -6.175, 3.133333, -9.5, 
             11.55, 16.166667, 17.966667, 
             19.75, 39.916667, -18.916667, 6.933333, -15.79, 28.613333)
    longs = c(34.75, 15.316667, 
              -1.090278, 
              4.55, -16.916667, -8.000000, 
              -16.600000, -8.816667, -1.734722, 
              103.916667, 102.609444, 104.321667, 
              107.237222, 106.833, 107.000000, 104.680000, 
              92.1, 96, 98.574722, 98.635556, 
              100.483333, 
              106.828333, 101.683333, 147.116667, 
              104.916667, 107.833333, 102.6, 
              96.1, 116.383333, 47.516667, 79.866667, -47.88, 77.208333)
      
    p1 = longs[p]
    p2 = lats[p]
    coor = data.frame(lat = p2,lng = p1)
    
    ###### generate random samples
    coor.level = str_sub(input$origins,1,3)
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

  output$note2 <- renderText({
    HTML(paste("Title", "Joe Explain",
               sep="<br/>"))
  })
  
  output$altvsref <- renderPlotly({
    plotAltVsRef.plotly(coverageGlobal$refCount, coverageGlobal$altCount)
  })
  
  output$note3 <- renderText({
    HTML(paste("Title", "Joe Explain",
               sep="<br/>"))
  })
  
  output$wsafhist <- renderPlotly({
    obsWSAF <<- computeObsWSAF(coverageGlobal$refCount, coverageGlobal$altCount)
    histWSAF.plotly(obsWSAF)
  })
  
  output$note4 <- renderText({
    HTML(paste("Title", "Joe Explain",
               sep="<br/>"))
  })
  
  ### onlye works when plaf and obsWSAF have same length
  ### match by CHROM and POS instead???
  output$wsvspl <- renderPlotly({
    plaf2 = plaf[1:length(coverageGlobal$CHROM)]
    plotWSAFvsPLAF.plotly(plaf2, obsWSAF)
  })
  
  output$note5 <- renderText({
    HTML(paste("Title", "Joe Explain",
               sep="<br/>"))
  })
  

}
