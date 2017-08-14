
plot.wsaf.vs.index.ring <- function ( coverage, expWSAF = c(), exclude, titlePrefix = "" ){
  # PG0390CoverageVcf.deconv = dEploid(paste("-vcf", vcfFile, "-plaf", plafFile, "-noPanel"))
  # prop = PG0390CoverageVcf.deconv$Proportions[dim(PG0390CoverageVcf.deconv$Proportions)[1],]
  # expWSAF = t(PG0390CoverageVcf.deconv$Haps) %*% prop
  coverage2 = merge(coverage, location, 
                    by.x = "CHROM", by.y = "CHROM",
                    all.x = T)
  chromCol = (as.numeric(1:length(levels(coverage$CHROM)) %% 2 ))
  chromCol[chromCol==1] = NA
  chromCol[chromCol==0] = 8
  circos.initialize(factors = coverage$CHROM, xlim = cbind(1, location$CHROMSIZE))
  
  circos.track(factor = coverage$CHROM,
               ylim=c(0,1), track.height = 0.18,
               bg.col = chromCol,
               panel.fun=function(x,y){
                 circos.text(CELL_META$xcenter,
                             CELL_META$cell.ylim[2] + uy(2, "mm"),
                             CELL_META$sector.index)
                 name = circlize::get.cell.meta.data("sector.index")
                 xlim = circlize::get.cell.meta.data("xlim")
                 ylim = circlize::get.cell.meta.data("ylim")
                 chromRegion = coverage[coverage$CHROM==name,]
                 ref = chromRegion$refCount
                 alt = chromRegion$altCount
                 obsWSAF = computeObsWSAF(alt, ref)
                 nSnp = dim(chromRegion)[1]
                 })

                                     # chromRegion = coverage[coverage$CHROM==name,]
                                     
                                     # ref = chromRegion$refCount
                                     # alt = chromRegion$altCount
                                     # obsWSAF = computeObsWSAF(alt, ref)
                                     
                                     # nSnp = dim(chromRegion)[1]

    # if (exclude$excludeBool){
    #  tmpCoveragePos = coverage$POS[coverage$CHROM==name]
    #  tmpExcludePos = exclude$excludeTable$POS[exclude$excludeTable$CHROM==name]
    #  excludeLogic = ( tmpCoveragePos %in% tmpExcludePos )
    #  plotIndex = which(!excludeLogic)
    #} else {
      # plotIndex = c(1:nSnp)
    #}
    # circlize::circos.points(coverage$POS[plotIndex], obsWSAF[plotIndex], col="red", pch = 16, cex = 0.1)
    circos.points(chromRegion$CHROM,chromRegion$POS, obsWSAF, col="red", pch = 16, cex = 0.1)
    circos.clear()
    # circlize::circos.points(plotIndex, expWSAF[coverage$CHROM == name], col="blue", pch = 16)
  # }
  
}

plot.wsaf.vs.index.ring(coverage)
#location$CHROMSIZE
#chroms = levels(coverage$CHROM)
#list = c()
#for (i in 1:14) {
#p = range(coverage2$POS[coverage$CHROM==chroms[i]])
#list = append(list,p)
#}
#list
table(coverage$CHROM)


