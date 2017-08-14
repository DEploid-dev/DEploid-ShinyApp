
#' vcfFile = system.file("extdata", "PG0390-C.test.vcf.gz", package = "DEploid")
#' PG0390CoverageVcf = extractCoverageFromVcf(vcfFile)
#' obsWSAF = computeObsWSAF( PG0390CoverageVcf$altCount, PG0390CoverageVcf$refCount )
#' plafFile = system.file("extdata", "labStrains.test.PLAF.txt", package = "DEploid")
#' PG0390CoverageVcf.deconv = dEploid(paste("-vcf", vcfFile, "-plaf", plafFile, "-noPanel"))
#' prop = PG0390CoverageVcf.deconv$Proportions[dim(PG0390CoverageVcf.deconv$Proportions)[1],]
#' expWSAF = t(PG0390CoverageVcf.deconv$Haps) %*% prop
#' plotObsExpWSAF(obsWSAF, expWSAF)


plot.ObsExpWSAF.plotly <- function (obsWSAF, expWSAF){
  compare = data.frame(obsWSAF, expWSAF)
  plot_ly(compare, x = ~obsWSAF, y = ~expWSAF, type = "scatter", mode = "markers",
          marker = list(color = "blue", size = 3)) %>%
    layout(margin = list(l = 65, r = 25, b = 50, t = 80, pad = 0),
           title = "WSAF(observed vs expected)", font = list(size = 18, colot = "black"),
           xaxis = list(title = "Observed WSAF (ALT/(ALT+REF))", range = c(0,1),
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")),
           yaxis = list(title = "Expected WSAF (h%*%p)", range = c(0,1),
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")),
           shapes = list(list(type = "line", fillcolor = "black", line = list(color = "black", width = 1.2, dash = "dot"),
                              opacity = 0.8, x0 = 0, x1 = 1, y0 = 0, y1 = 1)))
  
}
plot.ObsExpWSAF.plotly(obsWSAF, expWSAF)


