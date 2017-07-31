

# ref = coverage$refCount
# alt = coverage$altCount
# chroms = coverage$CHROM
# threshold = 0.995, window.size = 10



fun.find.more <- function (outliers.idx, window.size){
  idx.out = c()
  for ( i in 1:length(outliers.idx)){
    near.outliers.idx = which(((outliers.idx[i] - window.size) < outliers.idx) & (outliers.idx < (outliers.idx[i] + window.size)))
    idx.len = length(near.outliers.idx)
    if ( length(near.outliers.idx)>1 ){
      idx.out = c(idx.out, outliers.idx[near.outliers.idx[1]]:outliers.idx[near.outliers.idx[idx.len]])
    } else{
      idx.out = c(idx.out, outliers.idx[near.outliers.idx[1]])
    }
  }
  return(unique(idx.out))
}

plot.total.coverage.dygraphs <- function(ref, alt, coverage, cex.lab = 1,
                                cex.main = 1, cex.axis = 1,  threshold, window.size){
  totalDepth = ref + alt
  x = 1:length(totalDepth)
  tmpQ = quantile(totalDepth, threshold)
  outliers.idx = which((totalDepth > tmpQ ))
  potentialOutliers = fun.find.more(outliers.idx, window.size)
  
  
  # chromCol = (as.numeric(chroms) %% 2 )
  # chromCol[chromCol==1] = NA
  # chromCol[chromCol==0] = 8
  # plot(x, totalDepth, type="n", cex.axis = cex.axis, cex.lab = cex.lab, cex.main = cex.main,
  #      ylab="Coverage depth", xlab="SNP index", main = "Coverage across the sequence")
  # rect(x[-1],
  #      0,
  #      x[-length(x)],
  #      max(totalDepth)*1.5, col = chromCol, border = "transparent")
  # points(x, totalDepth, pch = 16)
  # abline(h = tmpQ, col = "red")
  # points(x[potentialOutliers], totalDepth[potentialOutliers], col = "red", pch = "x", cex = 2)
  tmp = data.frame(x, totalDepth)
  # tmpout = data.frame(x[potentialOutliers], totalDepth[potentialOutliers])
  chromgroup = coverage %>%
    group_by(CHROM) %>%
    summarise(count = n())
  chromgroup$position = cumsum(chromgroup$count)
  chromgroup$cut = as.numeric(rownames(chromgroup))%%2
  pos1 = chromgroup$position[chromgroup$cut==1]
  pos2 = chromgroup$position[chromgroup$cut==0]
  
  dygraph(tmp, xlab = "SNP Index", ylab="Coverage Depth", main = "Coverage across the sequence")  %>%
    dySeries("totalDepth", drawPoints = TRUE, strokeWidth = 0, color = 'black', pointSize = 3)  %>%
    dyRangeSelector(dateWindow = c(1, max(tmp$x))) %>%
    dyLimit(as.numeric(tmpQ), color = "red") %>%
    dyShading(from = pos1[1]-1, to = pos2[1], color = "#dae0e8") %>%
    dyShading(from = pos1[2]-1, to = pos2[2], color = "#dae0e8") %>%
    dyShading(from = pos1[3]-1, to = pos2[3], color = "#dae0e8") %>%
    dyShading(from = pos1[4]-1, to = pos2[4], color = "#dae0e8") %>%
    dyShading(from = pos1[5]-1, to = pos2[5], color = "#dae0e8") %>%
    dyShading(from = pos1[6]-1, to = pos2[6], color = "#dae0e8") %>%
    dyShading(from = pos1[7]-1, to = pos2[7], color = "#dae0e8")

  # return (potentialOutliers)
}

plot.total.coverage.dygraphs(coverage$refCount, coverage$altCount, coverage, cex.lab = 1, cex.main = 1, cex.axis = 1,
                             threshold = 0.995, window.size = 10)
