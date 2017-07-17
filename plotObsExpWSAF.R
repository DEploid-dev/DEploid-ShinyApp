

#' vcfFile = system.file("extdata", "PG0390-C.test.vcf.gz", package = "DEploid")
#' PG0390CoverageVcf = extractCoverageFromVcf(vcfFile)
#' obsWSAF = computeObsWSAF( PG0390CoverageVcf$altCount, PG0390CoverageVcf$refCount )
#' plafFile = system.file("extdata", "labStrains.test.PLAF.txt", package = "DEploid")
#' PG0390CoverageVcf.deconv = dEploid(paste("-vcf", vcfFile, "-plaf", plafFile, "-noPanel"))
#' prop = PG0390CoverageVcf.deconv$Proportions[dim(PG0390CoverageVcf.deconv$Proportions)[1],]
#' expWSAF = t(PG0390CoverageVcf.deconv$Haps) %*% prop
#' plotObsExpWSAF(obsWSAF, expWSAF)
 
plotObsExpWSAF <- function (obsWSAF, expWSAF,
                            title = "WSAF(observed vs expected)",
                            cex.lab = 1, cex.main = 1, cex.axis = 1 ){
  plot(obsWSAF, expWSAF, pch = 19, col = "blue",
       xlab = "Observed WSAF (ALT/(ALT+REF))", ylab = "Expected WSAF (h%*%p)",
       main = title, xlim = c(-0.05, 1.05), cex = 0.5, ylim = c(-0.05, 1.05),
       cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis)
  abline(0, 1, lty = "dotted");
}