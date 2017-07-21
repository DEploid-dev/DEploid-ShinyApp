

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

plot.total.coverage <- function(ref, alt, chroms, cex.lab = 1,
                                cex.main = 1, cex.axis = 1,  threshold, window.size){
  totalDepth = ref + alt
  x = 1:length(totalDepth)
  tmpQ = quantile(totalDepth, threshold)
  outliers.idx = which((totalDepth > tmpQ ))
  potentialOutliers = fun.find.more(outliers.idx, window.size)

  chromCol = (as.numeric(chroms) %% 2 )
  chromCol[chromCol==1] = NA
  chromCol[chromCol==0] = 8
  plot(x, totalDepth, type="n", cex.axis = cex.axis, cex.lab = cex.lab, cex.main = cex.main,
       ylab="Coverage depth", xlab="SNP index", main = "Coverage across the sequence")
  rect(x[-1],
       0,
       x[-length(x)],
       max(totalDepth)*1.5, col = chromCol, border = "transparent")
  points(x, totalDepth, pch = 16)
  abline(h = tmpQ, col = "red")
  points(x[potentialOutliers], totalDepth[potentialOutliers], col = "red", pch = "x", cex = 2)
  return (potentialOutliers)
}


