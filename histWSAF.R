

#' vcfFile = system.file("extdata", "PG0390-C.test.vcf.gz", package = "DEploid")
#' PG0390CoverageVcf = extractCoverageFromVcf(vcfFile)
#' obsWSAF = computeObsWSAF( PG0390CoverageVcf$altCount, PG0390CoverageVcf$refCount )
#' histWSAF(obsWSAF)
histWSAF <- function ( obsWSAF, exclusive = TRUE,
                       title ="Histogram 0<WSAF<1",
                       cex.lab = 1, cex.main = 1, cex.axis = 1 ){
  tmpWSAFIndex <- 1:length(obsWSAF)
  if ( exclusive ){
    tmpWSAFIndex <- which( ( (obsWSAF < 1) * (obsWSAF > 0) ) == 1)
  }
  return (hist(obsWSAF[tmpWSAFIndex], main = title,
               breaks = seq(0, 1, by = 0.1), xlab = "WSAF", col = "grey",
               cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis))
}