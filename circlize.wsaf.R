plot.wsaf.vs.index.ring <- function ( coverage, expWSAF = c(), expWSAFChrom = c(), exclude, titlePrefix = "" ){
  chromCol = (as.numeric(1:length(levels(coverage$CHROM)) %% 2 ))
  chromCol[chromCol==1] = NA
  chromCol[chromCol==0] = 8
  
  circlize::circos.trackPlotRegion(factor = expWSAFChrom, ylim=c(0,1), track.height = 0.18, bg.col = chromCol, 
                                   panel.fun=function(x,y){
    name = circlize::get.cell.meta.data("sector.index")
    xlim = circlize::get.cell.meta.data("xlim")
    ylim = circlize::get.cell.meta.data("ylim")
    chromRegion = coverage[coverage$CHROM==name,]
    
    ref = chromRegion$refCount
    alt = chromRegion$altCount
    obsWSAF = computeObsWSAF ( alt, ref )
    
    nSnp = dim(chromRegion)[1]
    
    if (exclude$excludeBool){
      tmpCoveragePos = coverage$POS[coverage$CHROM==name]
      tmpExcludePos = exclude$excludeTable$POS[exclude$excludeTable$CHROM==name]
      excludeLogic = ( tmpCoveragePos %in% tmpExcludePos )
      plotIndex = which(!excludeLogic)
    } else {
      plotIndex = c(1:nSnp)
    }
    circlize::circos.points(plotIndex, obsWSAF[plotIndex], col="red", pch = 16)
    circlize::circos.points(plotIndex, expWSAF[expWSAFChrom == name], col="blue", pch = 16)
  })
}
