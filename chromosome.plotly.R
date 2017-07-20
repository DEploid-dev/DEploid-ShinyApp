

plot.wsaf.vs.chromosome.plotly <- function (coverage3){
  # coverage2$WSAF = coverage2$altCount/(coverage2$altCount + coverage2$refCount)
  # tmpWSAF = coverage2$WSAF[coverage2$CHROM == levels(coverage2$CHROM)[1]]
  # pos = coverage2$POS[coverage2$CHROM == levels(coverage2$CHROM)[1]]
  # plot_ly(x = pos, y = tmpWSAF, type = "scatter", mode = "markers",
  #         marker = list(size = 6, color = "coral",
  #                       line = list(color = "black", width = 0.7))) %>%
  #   layout(xaxis = list(title = levels(coverage2$CHROM)[1], range = c(0,max(coverage2$CHROMSIZE)),
  #                       titlefont = list(size = 14, color = "black"),
  #                       tickfont = list(size = 14, color = "black")),
  #          yaxis = list(title = "WSAF", range = c(0,1),
  #                       titlefont = list(size = 14, color = "black"),
  #                       tickfont = list(size = 14, color = "black")),
  #          showlegend = FALSE)
  plot_ly(coverage3, x = ~POS, y = ~WSAF, 
          type = "scatter", mode = "markers", color = ~CHROM,
          marker = list(size = 6,
                        line = list(color = "black", width = 0.7))) %>%
    layout(xaxis = list(title = "Chromosome Position", range = c(0,max(coverage2$CHROMSIZE)),
                        titlefont = list(size = 14, color = "black"),
                        tickfont = list(size = 14, color = "black")),
           yaxis = list(title = "WSAF", range = c(0,1),
                        titlefont = list(size = 14, color = "black"),
                        tickfont = list(size = 14, color = "black")),
           legend = list(orientation = "h"))
  

}

# plot.wsaf.vs.chromosome.plotly(coverage2)   
    
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
#  subplot(p1, p2, p3, p4, p5, p6, p7, p8,
#          p9, p10, p11, p12, p13, p14, nrows = 14)



