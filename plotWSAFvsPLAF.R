
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
plotWSAFvsPLAF <- function ( plaf, obsWSAF){  
  # plot ( plaf, obsWSAF, cex = 0.5, xlim = c(0, 1), ylim = c(0, 1),
  #        col = "red", main = title, xlab = "PLAF", ylab = "WSAF",
  #        cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis)
  plot_ly(x = plaf, y = obsWSAF, type = "scatter", color = "blue")
  # if ( length(expWSAF) > 0 ){
  #   points ( plaf, expWSAF, cex = 0.5, col = "blue")
  # }
  # if ( length(potentialOutliers) > 0 ){
  #   points(plaf[potentialOutliers], obsWSAF[potentialOutliers], col="black", pch="x", cex = 2)
  # }
}

