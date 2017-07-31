plot.wsaf.vs.pos.dygraph <- function (wsaf, chromName = ""){
    dygraph(wsaf, xlab = "Positions", ylab="WSAF", main = chromName)  %>%
        dySeries('obsWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'red', pointSize = 3)  %>%
        dySeries('expWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'blue', pointSize = 3)  %>%
        dyRangeSelector(dateWindow = c(1, max(wsaf$pos)))
}

# #rm(list=ls())
# 
# plot.wsaf.vs.pos.dygraph <- function (coverageGlobal, chrom = "Pf3D7_01_v3", obsWSAF){
# 
#   tmpWSAF = as.data.frame(coverageGlobal[,c(3,4)])
#   wsaf.list = list()
#   chroms = unique(coverageGlobal$CHROM)
#   for (chromi in 1:length(chroms)){
#     idx = which(coverageGlobal$CHROM == chroms[chromi])
#     #    tmp = as.matrix(obsWSAF[idx], c(length(idx), 1))
#     tmp = data.frame(pos = coverageGlobal$POS[idx],  wsaf = obsWSAF[idx])
#     
#     wsaf.list[[as.character(chroms[chromi])]] = tmp
#     
#   }
#   
#   dygraph(wsaf.list[[chrom]], xlab = "Positions", ylab="WSAF", main = chrom)  %>%
#     dySeries('wsaf', drawPoints = TRUE, strokeWidth = 0, color = '#ea002f', pointSize = 3)  %>%
#     dyRangeSelector(dateWindow = c(1, max(wsaf.list[[chrom]]$pos)))
#   
#   
# }


