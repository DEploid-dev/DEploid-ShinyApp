
plot.wsaf.vs.index.ring <- function ( coverage){
  ###
  # vcfFile = system.file("extdata", "PG0390-C.test.vcf.gz", package = "DEploid")
  # plafFile = system.file("extdata", "labStrains.test.PLAF.txt", package = "DEploid")

  # coverage <- extractCoverageFromVcf(vcfFile)
  # obsWSAF = computeObsWSAF(coverage$altCount, coverage$refCount)

  # PG0390CoverageVcf.deconv = dEploid(paste("-vcf", vcfFile, "-plaf", plafFile, "-noPanel"))
  # prop = PG0390CoverageVcf.deconv$Proportions[dim(PG0390CoverageVcf.deconv$Proportions)[1],]
  # expWSAF = t(PG0390CoverageVcf.deconv$Haps) %*% prop
  ###
  chromCol = (as.numeric(1:length(levels(coverage$CHROM)) %% 2 ))
  chromCol[chromCol==1] = NA
  chromCol[chromCol==0] = 8
  
  circos.par("track.height" = 0.3)
  circos.initialize(factors = coverage$CHROM, xlim = cbind(1, location$CHROMSIZE))
  
  circos.track(factors = coverage$CHROM, y = obsWSAF,
               bg.col = chromCol,
               panel.fun = function (x, y) {
                 circos.text(CELL_META$xcenter,
                             CELL_META$cell.ylim[2] + uy(4, "mm"),
                             CELL_META$sector.index)
               })
  
  ### First Track: points
  col = rep(c("magenta", "cornflowerblue"), 4)
  circos.trackPoints(coverage$CHROM, coverage$POS, obsWSAF, col = "red", pch = 16, cex = 0.005)
  circos.trackPoints(coverage$CHROM, coverage$POS, expWSAF, col="blue", pch = 16, cex = 0.005)
}