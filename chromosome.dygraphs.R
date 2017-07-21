#rm(list=ls())

plot.wsaf.vs.pos.dygraph <- function (coverage, chrom = "Pf3D7_01_v3"){
    obsWSAF = computeObsWSAF( coverage$altCount, coverage$refCount )
    tmpWSAF = as.data.frame(coverage[,c(3,4)])
    wsaf.list = list()
    chroms = unique(coverage$CHROM)
    for (chromi in 1:length(chroms)){
        idx = which(coverage$CHROM == chroms[chromi])
    #    tmp = as.matrix(obsWSAF[idx], c(length(idx), 1))
    tmp = data.frame(pos = coverage$POS[idx],  wsaf = obsWSAF[idx])

        wsaf.list[[as.character(chroms[chromi])]] = tmp

    }

    dygraph(wsaf.list[[chrom]], xlab = "Positions", ylab="WSAF", main = chrom)  %>%
        dySeries('wsaf', drawPoints = TRUE, strokeWidth = 0, color = 'red', pointSize = 3)  %>%
        dyRangeSelector(dateWindow = c(1, max(wsaf.list[[chrom]]$pos)))


}
