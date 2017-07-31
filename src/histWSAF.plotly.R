

#' vcfFile = system.file("extdata", "PG0390-C.test.vcf.gz", package = "DEploid")
#' PG0390CoverageVcf = extractCoverageFromVcf(vcfFile)
#' obsWSAF = computeObsWSAF( PG0390CoverageVcf$altCount, PG0390CoverageVcf$refCount )
#' histWSAF(obsWSAF)
histWSAF.plotly <- function ( obsWSAF, exclusive = TRUE,
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



