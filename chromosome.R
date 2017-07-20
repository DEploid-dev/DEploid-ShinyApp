

plot.wsaf.vs.index <- function ( coverage, expWSAF = c(), expWSAFChrom = c(), exclude, titlePrefix = "" ){
  chromList = levels(coverage$CHROM)
  ref = coverage$refCount
  alt = coverage$altCount
  obsWSAF = computeObsWSAF ( alt, ref )
  nFigures = length(chromList)
  for ( chromI in chromList ){
    tmpWSAF = obsWSAF[coverage$CHROM==chromI]
    tmpWSAF = obsWSAF[coverage$CHROM=="Pf3D7_01_v3"]
    plot(tmpWSAF , col="red", ylim=c(0,1), main = paste(titlePrefix, chromI, "WSAF"), ylab = "WSAF",
         cex.axis = 3.5, cex.lab = 3.5, cex.main = 4, xaxt = "n", yaxt = "n")
    newXaxt = round(seq(1, length(tmpWSAF), length.out = 6))
    axis(1, at = newXaxt, labels = as.character(newXaxt),
         cex.axis= 3.5)
    newYaxt = seq(0, 1, length.out = 3)
    axis(2, at = newYaxt, labels = as.character(newYaxt),
         cex.axis= 3.5)
    
    
    #if ( length(expWSAF) > 0 ){
     # plotIndex = c()
      #if (exclude$excludeBool){
       # tmpCoveragePos = coverage$POS[coverage$CHROM==chromI]
        #tmpExcludePos = exclude$excludeTable$POS[exclude$excludeTable$CHROM==chromI]
        #excludeLogic = ( tmpCoveragePos %in% tmpExcludePos )
        #excludeindex = which(excludeLogic)
        #plotIndex = which(!excludeLogic)
      #} else {
      #  plotIndex = c(1:length(obsWSAF[coverage$CHROM==chromI]))
      #}
      #points(plotIndex, expWSAF[expWSAFChrom == chromI], col="blue")
    #}
  }
}
plot.wsaf.vs.index(coverage)
