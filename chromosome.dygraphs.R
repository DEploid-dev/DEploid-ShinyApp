plot.wsaf.vs.pos.dygraph <- function (wsaf, chromName = ""){
    dygraph(wsaf, xlab = "Positions", ylab="WSAF", main = chromName)  %>%
        dySeries('obsWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'red', pointSize = 3)  %>%
        dySeries('expWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'blue', pointSize = 3)  %>%
        dyRangeSelector(dateWindow = c(1, max(wsaf$pos)))
}

