# 1. source("plot.total.coverage.R")
# 2. source("plotAltVsRef.plotly.R")
# 3. source("histWSAF.plotly.R")
# 4. source("plotWSAFvsPLAF.plotly.R")
# 5. source("chromosome.dygraphs.R")


############ 1. source("plot.total.coverage.R")
############ 1. plot.totalCoverage.base
# ref = coverage$refCount
# alt = coverage$altCount
# chroms = coverage$CHROM
# threshold = 0.995, window.size = 10
fun.find.more <- function (outliers.idx, window.size){
  idx.out = c()
  for ( i in 1:length(outliers.idx)){
    near.outliers.idx = which(((outliers.idx[i] - window.size) < outliers.idx) & (outliers.idx < (outliers.idx[i] + window.size)))
    idx.len = length(near.outliers.idx)
    if ( length(near.outliers.idx)>1 ){
      idx.out = c(idx.out, outliers.idx[near.outliers.idx[1]]:outliers.idx[near.outliers.idx[idx.len]])
    } else{
      idx.out = c(idx.out, outliers.idx[near.outliers.idx[1]])
    }
  }
  return(unique(idx.out))
}

plot.totalCoverage.base <- function(ref, alt, chroms, cex.lab = 1,
                                cex.main = 1, cex.axis = 1,  threshold, window.size){
  totalDepth = ref + alt
  x = 1:length(totalDepth)
  tmpQ = quantile(totalDepth, threshold)
  outliers.idx = which((totalDepth > tmpQ ))
  potentialOutliers = fun.find.more(outliers.idx, window.size)
  
  chromCol = (as.numeric(chroms) %% 2 )
  chromCol[chromCol==1] = NA
  chromCol[chromCol==0] = 8
  plot(x, totalDepth, type="n", cex.axis = cex.axis, cex.lab = cex.lab, cex.main = cex.main,
       ylab="Coverage depth", xlab="SNP index", main = "Coverage across the sequence")
  rect(x[-1],
       0,
       x[-length(x)],
       max(totalDepth)*1.5, col = chromCol, border = "transparent")
  points(x, totalDepth, pch = 16)
  abline(h = tmpQ, col = "red")
  points(x[potentialOutliers], totalDepth[potentialOutliers], col = "red", pch = "x", cex = 2)
  return (potentialOutliers)
}




############ 2. source("plotAltVsRef.plotly.R")
############ 2. plot.AltVsRef.plotly 
plot.AltVsRef.plotly <- function (ref, alt){
  
  ratios <- ref/(ref + alt + 0.0000001)
  tmpRange <- 1.1 * mean(max(alt), max(ref))
  legend.name <- "Ref/(Ref+Alt) Ratio"
  plot_ly(x = ref, y = alt, type = "scatter", mode = "markers",
          color = ~ ratios, colors=c("#de2d26", "#2b8cbe"), alpha = 0.8,
          marker = list(size = 3, line = list(color = "black", width = 0.3),
                        colorbar = list(title = legend.name)
          ),
          text = paste("RefCount: ", ref, " ;  ", "AltCount: ", alt)) %>%
    layout(margin = list(l = 65, r = 25, b = 50, t = 80, pad = 0),
           title = "Alt vs Ref", font = list(size = 18, colot = "black"),
           legend = list(font = list(size = 5)),
           xaxis = list(title = "Reference # Reads", range = c(-5, 200),
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")),
           yaxis = list(title = "Alternative # Reads", range = c(-10, 200),
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")),
           shapes = list(list(type = "line", fillcolor = "black", line = list(color = "black", width = 1.2, dash = "dot"),
                              opacity = 0.8, x0 = 50, x1 = 50, y0 = 0, y1 = 200),
                         list(type = "line", fillcolor = "black", line = list(color = "black", width = 1.2, dash = "dot"),
                              opacity = 0.8, x0 = 0, x1 = 200, y0 = 50, y1 = 50),
                         list(type = "line", fillcolor = "grey", line = list(color = "grey", width = 2.5, dash = "dot"),
                              opacity = 0.8, x0 = 0, x1 = 200, y0 = 0, y1 = 200))
           
    )
  
  # if ( length(potentialOutliers) > 0 ){
  #   points(ref[potentialOutliers], alt[potentialOutliers], col="black", pch="x", cex = 2)
  # }
}



############ 3. source("histWSAF.plotly.R")
############ 3. histWSAF
#' # Example 2
#' vcfFile = system.file("extdata", "PG0390-C.test.vcf.gz", package = "DEploid")
#' PG0390CoverageVcf = extractCoverageFromVcf(vcfFile)
#' obsWSAF = computeObsWSAF( PG0390CoverageVcf$altCount, PG0390CoverageVcf$refCount )
#' histWSAF(obsWSAF)
#' myhist = histWSAF(obsWSAF, FALSE)
#'
histWSAF <- function ( obsWSAF, exclusive = TRUE,
                       title ="Histogram 0<WSAF<1",
                       cex.lab = 1, cex.main = 1, cex.axis = 1 ){
  tmpWSAFIndex <- 1:length(obsWSAF)
  if ( exclusive ){
    tmpWSAFIndex <- which( ( (obsWSAF < 1) * (obsWSAF > 0) ) == 1)
  }
  return (hist(obsWSAF[tmpWSAFIndex], main = title,
               breaks = seq(0, 1, by = 0.1), xlab = "WSAF", col = "gray",
               cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis))
}

############ 3. source("histWSAF.plotly.R")
############ 3. plot.histWSAF.plotly
#' vcfFile = system.file("extdata", "PG0390-C.test.vcf.gz", package = "DEploid")
#' PG0390CoverageVcf = extractCoverageFromVcf(vcfFile)
#' obsWSAF = computeObsWSAF( PG0390CoverageVcf$altCount, PG0390CoverageVcf$refCount )
#' histWSAF(obsWSAF)
plot.histWSAF.plotly <- function ( obsWSAF, exclusive = TRUE,
                              title ="Histogram 0<WSAF<1",
                              cex.lab = 1, cex.main = 1, cex.axis = 1 ){
  tmpWSAFIndex <- 1:length(obsWSAF)
  if ( exclusive ){
    tmpWSAFIndex <- which( ( (obsWSAF < 1) * (obsWSAF > 0) ) == 1)
  }
  # return (hist(obsWSAF[tmpWSAFIndex], main = title,
  #              breaks = seq(0, 1, by = 0.1), xlab = "WSAF", col = "grey",
  #              cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis))
  xb = list(
    start = 0,
    end = 1,
    size = 0.1)
  return (plot_ly(x = obsWSAF[tmpWSAFIndex], 
                  type = "histogram", 
                  xbins = xb, marker = list(color = "#5f9fe8", 
                                            line = list(color = "white", width = 1))) %>%
            layout(margin = list(l = 65, r = 25, b = 50, t = 80, pad = 0),
                   title = "Histogram 0<WSAF<1", font = list(size = 18, colot = "black"),
                   xaxis = list(title = "WSAF", range = c(0,1), 
                                titlefont = list(size = 18, color = "black"),
                                tickfont = list(size = 14, color = "black")),
                   yaxis = list(title = "Frequency", 
                                titlefont = list(size = 18, color = "black"),
                                tickfont = list(size = 14, color = "black"))))
}



############ 4. source("plotWSAFvsPLAF.plotly.R")
############ 4. plot.WSAFVsPLAF.plotly
#' vcfFile = system.file("extdata", "PG0390-C.test.vcf.gz", package = "DEploid")
#' PG0390CoverageVcf = extractCoverageFromVcf(vcfFile)
#' obsWSAF = computeObsWSAF( PG0390CoverageVcf$altCount, PG0390CoverageVcf$refCount )
#' plafFile = system.file("extdata", "labStrains.test.PLAF.txt", package = "DEploid")
#' plaf = extractPLAF(plafFile)
#' plotWSAFvsPLAF(plaf, obsWSAF)
#'
# plotWSAFvsPLAF <- function ( plaf, obsWSAF, expWSAF = c(), potentialOutliers = c(),
#                              title = "WSAF vs PLAF",
#                              cex.lab = 1, cex.main = 1, cex.axis = 1 ){
plot.WSAFVsPLAF.plotly <- function (plaf, obsWSAF, ref, alt){
  # plot ( plaf, obsWSAF, cex = 0.5, xlim = c(0, 1), ylim = c(0, 1),
  #        col = "red", main = title, xlab = "PLAF", ylab = "WSAF",
  #        cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis)
  
  plot_ly(x = plaf, y = obsWSAF, type = "scatter", mode = "markers",
          marker = list(size = 2,
                        color = "rgba(255, 182, 193, .9)",
                        line = list(color = "rgba(152, 0, 0, .8)",
                                    width = 1)),
          text = paste("RefCount: ", ref, " ;  ", "AltCount: ", alt)) %>%
    layout(margin = list(l = 65, r = 25, b = 50, t = 80, pad = 0),
           title = "WSAF vs PLAF", font = list(size = 18, colot = "black"),
           xaxis = list(title = "PLAF", range = c(0,1),
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")),
           yaxis = list(title = "WSAF", range = c(0,1),
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")))
  # if ( length(expWSAF) > 0 ){
  #   points ( plaf, expWSAF, cex = 0.5, col = "blue")
  # }
  # if ( length(potentialOutliers) > 0 ){
  #   points(plaf[potentialOutliers], obsWSAF[potentialOutliers], col="black", pch="x", cex = 2)
  # }
}





############ 5. source("chromosome.dygraphs.R")
############ 5. plot.WSAFVsPOS.dygraphs
plot.WSAFVsPOS.dygraphs <- function (wsaf, chromName = ""){
  dygraph(wsaf, xlab = "Positions", ylab="WSAF", main = chromName)  %>%
    dySeries('obsWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'red', pointSize = 3)  %>%
    dySeries('expWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'blue', pointSize = 3)  %>%
    dyRangeSelector(dateWindow = c(1, max(wsaf$pos)))
}

#rm(list=ls())

#plot.wsaf.vs.pos.dygraph <- function (coverageGlobal, chrom = "Pf3D7_01_v3", obsWSAF){

#  tmpWSAF = as.data.frame(coverageGlobal[,c(3,4)])
#  wsaf.list = list()
#  chroms = unique(coverageGlobal$CHROM)
#  for (chromi in 1:length(chroms)){
#    idx = which(coverageGlobal$CHROM == chroms[chromi])
#    #    tmp = as.matrix(obsWSAF[idx], c(length(idx), 1))
#    tmp = data.frame(pos = coverageGlobal$POS[idx],  wsaf = obsWSAF[idx])

#    wsaf.list[[as.character(chroms[chromi])]] = tmp

#  }

#  dygraph(wsaf.list[[chrom]], xlab = "Positions", ylab="WSAF", main = chrom)  %>%
#    dySeries('wsaf', drawPoints = TRUE, strokeWidth = 0, color = '#ea002f', pointSize = 3)  %>%
#    dyRangeSelector(dateWindow = c(1, max(wsaf.list[[chrom]]$pos)))


#}


















