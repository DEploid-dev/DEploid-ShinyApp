# 1. source("plot.total.coverage.R") (base)
# 1. source("plot.total.coverage.R") (plotly)
# 2. source("plotAltVsRef.plotly.R")-------------------in deploidPlotly.R
# 3. source("histWSAF.R")            (base)
# 3. source("histWSAF.plotly.R")     (plotly)----------in deploidPlotly.R
# 4. source("plotWSAFvsPLAF.plotly.R")-----------------in deploidPlotly.R
# 5. source("chromosome.dygraphs.R") (old)
# 5. source("chromosome.dygraphs.R") (new)
# 6. source("plot.proportions.plotly.R")
# 7. source("plot.ObsExpWSAF.plotly")------------------in deploidPlotly.R



# ############ 1. source("plot.total.coverage.R")
# ############ 1. plot.totalCoverage.base
# # ref = coverage$refCount
# # alt = coverage$altCount
# # chroms = coverage$CHROM
# # threshold = 0.995, window.size = 10
# fun.find.more <- function (outliers.idx, window.size){
#   idx.out = c()
#   for ( i in 1:length(outliers.idx)){
#     near.outliers.idx = which(((outliers.idx[i] - window.size) < outliers.idx) & (outliers.idx < (outliers.idx[i] + window.size)))
#     idx.len = length(near.outliers.idx)
#     if ( length(near.outliers.idx)>1 ){
#       idx.out = c(idx.out, outliers.idx[near.outliers.idx[1]]:outliers.idx[near.outliers.idx[idx.len]])
#     } else{
#       idx.out = c(idx.out, outliers.idx[near.outliers.idx[1]])
#     }
#   }
#   return(unique(idx.out))
# }
#
# plot.totalCoverage.base <- function(ref, alt, chroms, cex.lab = 1,
#                                 cex.main = 1, cex.axis = 1,  threshold, window.size){
#   totalDepth = ref + alt
#   x = 1:length(totalDepth)
#   tmpQ = quantile(totalDepth, threshold)
#   outliers.idx = which((totalDepth > tmpQ ))
#   potentialOutliers = fun.find.more(outliers.idx, window.size)
#
#   chromCol = (as.numeric(chroms) %% 2 )
#   chromCol[chromCol==1] = NA
#   chromCol[chromCol==0] = 8
#   plot(x, totalDepth, type="n", cex.axis = cex.axis, cex.lab = cex.lab, cex.main = cex.main,
#        ylab="Coverage depth", xlab="SNP index", main = "Coverage across the sequence")
#   rect(x[-1],
#        0,
#        x[-length(x)],
#        max(totalDepth)*1.5, col = chromCol, border = "transparent")
#   points(x, totalDepth, pch = 16)
#   abline(h = tmpQ, col = "red")
#   points(x[potentialOutliers], totalDepth[potentialOutliers], col = "red", pch = "x", cex = 2)
#   return (potentialOutliers)
# }



############ 1. source("plot.total.coverage.R")
############ 1. plot.totalCoverage.dygraphs

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
    dySeries("totalDepth", drawPoints = TRUE, strokeWidth = 0, color = 'black', pointSize = 1)  %>%
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



# ############ 2. source("plotAltVsRef.plotly.R")
# ############ 2. plot.AltVsRef.plotly
# plot.AltVsRef.plotly <- function (ref, alt){
#
#   ratios <- ref/(ref + alt + 0.0000001)
#   tmpRange <- 1.1 * mean(max(alt), max(ref))
#   legend.name <- "Ref/(Ref+Alt) Ratio"
#   plot_ly(x = ref, y = alt, type = "scatter", mode = "markers",
#           color = ~ ratios, colors=c("#de2d26", "#2b8cbe"), alpha = 0.8,
#           marker = list(size = 3, line = list(color = "black", width = 0.3),
#                         colorbar = list(title = legend.name)
#           ),
#           text = paste("RefCount: ", ref, " ;  ", "AltCount: ", alt)) %>%
#     layout(margin = list(l = 65, r = 25, b = 50, t = 80, pad = 0),
#            title = "Alt vs Ref", font = list(size = 18, colot = "black"),
#            legend = list(font = list(size = 5)),
#            xaxis = list(title = "Reference # Reads", range = c(-5, 200),
#                         titlefont = list(size = 18, color = "black"),
#                         tickfont = list(size = 16, color = "black")),
#            yaxis = list(title = "Alternative # Reads", range = c(-10, 200),
#                         titlefont = list(size = 18, color = "black"),
#                         tickfont = list(size = 16, color = "black")),
#            shapes = list(list(type = "line", fillcolor = "black", line = list(color = "black", width = 1.2, dash = "dot"),
#                               opacity = 0.8, x0 = 50, x1 = 50, y0 = 0, y1 = 200),
#                          list(type = "line", fillcolor = "black", line = list(color = "black", width = 1.2, dash = "dot"),
#                               opacity = 0.8, x0 = 0, x1 = 200, y0 = 50, y1 = 50),
#                          list(type = "line", fillcolor = "grey", line = list(color = "grey", width = 2.5, dash = "dot"),
#                               opacity = 0.8, x0 = 0, x1 = 200, y0 = 0, y1 = 200))
#
#     )
#
#   # if ( length(potentialOutliers) > 0 ){
#   #   points(ref[potentialOutliers], alt[potentialOutliers], col="black", pch="x", cex = 2)
#   # }
# }



# ############ 3. source("histWSAF.R")
# ############ 3. histWSAF
# histWSAF <- function ( obsWSAF, exclusive = TRUE,
#                        title ="Histogram 0<WSAF<1",
#                        cex.lab = 1, cex.main = 1, cex.axis = 1 ){
#   tmpWSAFIndex <- 1:length(obsWSAF)
#   if ( exclusive ){
#     tmpWSAFIndex <- which( ( (obsWSAF < 1) * (obsWSAF > 0) ) == 1)
#   }
#   return (hist(obsWSAF[tmpWSAFIndex], main = title,
#                breaks = seq(0, 1, by = 0.1), xlab = "WSAF", col = "gray",
#                cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis))
# }



# ############ 3. source("histWSAF.plotly.R")
# ############ 3. plot.histWSAF.plotly
# plot.histWSAF.plotly <- function ( obsWSAF, exclusive = TRUE,
#                               title ="Histogram 0<WSAF<1",
#                               cex.lab = 1, cex.main = 1, cex.axis = 1 ){
#   tmpWSAFIndex <- 1:length(obsWSAF)
#   if ( exclusive ){
#     tmpWSAFIndex <- which( ( (obsWSAF < 1) * (obsWSAF > 0) ) == 1)
#   }
#   xb = list(
#     start = 0,
#     end = 1,
#     size = 0.1)
#   return (plot_ly(x = obsWSAF[tmpWSAFIndex],
#                   type = "histogram",
#                   xbins = xb, marker = list(color = "#5f9fe8",
#                                             line = list(color = "white", width = 1))) %>%
#             layout(margin = list(l = 65, r = 25, b = 50, t = 80, pad = 0),
#                    title = "Histogram 0<WSAF<1", font = list(size = 18, colot = "black"),
#                    xaxis = list(title = "WSAF", range = c(0,1),
#                                 titlefont = list(size = 18, color = "black"),
#                                 tickfont = list(size = 14, color = "black")),
#                    yaxis = list(title = "Frequency",
#                                 titlefont = list(size = 18, color = "black"),
#                                 tickfont = list(size = 14, color = "black"))))
# }



############ 4. source("plotWSAFvsPLAF.plotly.R")
############ 4. plot.WSAFVsPLAF.plotly
# plot.WSAFVsPLAF.plotly <- function (plaf, obsWSAF, ref, alt){
#   # plot ( plaf, obsWSAF, cex = 0.5, xlim = c(0, 1), ylim = c(0, 1),
#   #        col = "red", main = title, xlab = "PLAF", ylab = "WSAF",
#   #        cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis)
#   plot_ly(x = plaf, y = obsWSAF, type = "scatter", mode = "markers",
#           marker = list(size = 2,
#                         color = "#c64343",
#                         line = list(color = "rgba(152, 0, 0, .8)",
#                                     width = 1)),
#           text = paste("RefCount: ", ref, " ;  ", "AltCount: ", alt)) %>%
#     layout(margin = list(l = 65, r = 25, b = 50, t = 80, pad = 0),
#            title = "WSAF vs PLAF", font = list(size = 18, colot = "black"),
#            xaxis = list(title = "PLAF", range = c(0,1),
#                         titlefont = list(size = 18, color = "black"),
#                         tickfont = list(size = 16, color = "black")),
#            yaxis = list(title = "WSAF", range = c(0,1),
#                         titlefont = list(size = 18, color = "black"),
#                         tickfont = list(size = 16, color = "black")))
#   # if ( length(expWSAF) > 0 ){
#   #   points ( plaf, expWSAF, cex = 0.5, col = "blue")
#   # }
#   # if ( length(potentialOutliers) > 0 ){
#   #   points(plaf[potentialOutliers], obsWSAF[potentialOutliers], col="black", pch="x", cex = 2)
#   # }
# }



############ 5. source("chromosome.dygraphs.R")
############ 5. plot.WSAFVsPOS.dygraphs.new

# plot.WSAFVsPOS.dygraphs <- function (wsaf, chromName = ""){
#   dygraph(wsaf, xlab = "Positions", ylab="WSAF", main = chromName)  %>%
#     dySeries('obsWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'red', pointSize = 3)  %>%
#     dySeries('expWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'blue', pointSize = 3)  %>%
#     dyRangeSelector(dateWindow = c(1, max(wsaf$pos)))
# }
plotWSAFVsPOSDygraphs <- function (wsaf, gene, exon){
  d1 <- dygraph(wsaf, xlab = "Positions", ylab="WSAF", main = "")  %>%
    dySeries('obsWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'red', pointSize = 3)  %>%
    dySeries('expWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'blue', pointSize = 3)  %>%
    dyRangeSelector(dateWindow = c(1, max(wsaf$pos)))

  for (i in 1:nrow(gene)) {
    d1 <- d1 %>%
      dyShading(from = gene$pos1[i], to = gene$pos2[i], color = "#d5d1ff")
  }
  for (i in 1:nrow(exon)) {
    d1 <- d1 %>%
      dyShading(from = exon$pos3[i], to = exon$pos4[i], color = "#b7e5e2")
  }
  d1
}



############ 5. source("chromosome.dygraphs.R")
############ 5. plot.WSAFVsPOS.dygraphs.old
#rm(list=ls())

#plot.wsaf.vs.pos.dygraph <- function (coverageGlobal, chrom = "Pf3D7_01_v3", obsWSAF){

#  tmpWSAF = as.data.frame(coverageGlobal[,c(3,4)])
#  wsaf.list = list()
#  chroms = unique(coverageGlobal$CHROM)
#  for (chromi in 1:length(chroms)){
#    idx = which(coverageGlobal$CHROM == chroms[chromi])
#    #    tmp = as.matrix(obsWSAF[idx], c(length(idx), 1))
#    tmp = data.frame(pos = coverageGlobal$POS[idx],  wsaf = obsWSAF[idx])

#    wsaf.list[[as.character(chroms[chromi])]] = tmp

#  }

#  dygraph(wsaf.list[[chrom]], xlab = "Positions", ylab="WSAF", main = chrom)  %>%
#    dySeries('wsaf', drawPoints = TRUE, strokeWidth = 0, color = '#ea002f', pointSize = 3)  %>%
#    dyRangeSelector(dateWindow = c(1, max(wsaf.list[[chrom]]$pos)))


#}



############# 6. source("plot.proportions.plotly.R")
############# 6. plot.proportions.plotly

plotProportionsPlotly <- function (proportions, pnum){
  colors = c("#3e9fa0", "#ffc700", "#4297f7", "#ff0000", "#afb55a", "#7c0101", "#8bb7ba", "#9b8352")
  names = c("Parasite 1", "Parasite 2", "Parasite 3", "Parasite 4", "Parasite 5", "Parasite 6", "Parasite 7", "Parasite 8")
  
  p0 = plot_ly(proportions, x = ~x, y = proportions[ ,1], type = 'bar',
               name = names[1], marker = list(color = colors[1])) %>%
    layout(margin = list(l = 70, r = 25, b = 30, t = 80, pad = 0),
           barmode = "relative",
           title = "Components", font = list(size = 18, colot = "black"),
           legend = list(font = list(size = 15)),
           xaxis = list(title = "Iteration", range = c(0, length(proportions$x)),
                        titlefont = list(size = 18, color = "black"),
                        showticklabels = FALSE),
           yaxis = list(title = "Component Proportion", range = c(0, 1),
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")))

  for (i in 2:pnum) {
    p0 = add_trace(p0, proportions, x = ~x, y = proportions[ ,i], 
                   name = names[i], marker = list(color = colors[i]))
  }
  p0
}




# ############# 7. source("plot.ObsExpWSAF.plotly")
#
# plot.ObsExpWSAF.plotly <- function (obsWSAF, expWSAF){
#   compare = data.frame(obsWSAF, expWSAF)
#   plot_ly(compare, x = ~obsWSAF, y = ~expWSAF, type = "scatter", mode = "markers",
#           marker = list(color = "blue", size = 3)) %>%
#     layout(margin = list(l = 65, r = 25, b = 50, t = 80, pad = 0),
#            title = "WSAF(observed vs expected)", font = list(size = 18, colot = "black"),
#            xaxis = list(title = "Observed WSAF (ALT/(ALT+REF))", range = c(0,1),
#                         titlefont = list(size = 18, color = "black"),
#                         tickfont = list(size = 16, color = "black")),
#            yaxis = list(title = "Expected WSAF (h%*%p)", range = c(0,1),
#                         titlefont = list(size = 18, color = "black"),
#                         tickfont = list(size = 16, color = "black")),
#            shapes = list(list(type = "line", fillcolor = "black", line = list(color = "black", width = 1.2, dash = "dot"),
#                               opacity = 0.8, x0 = 0, x1 = 1, y0 = 0, y1 = 1)))
#
# }



############ 8. source("plot.llk.plotly")
# 
# ############################
# fun.llk <- function(cov.ref, cov.alt, f.samp, err=0.01, fac=100) {
#   f.samp<-f.samp+err*(1-2*f.samp);
#   llk<-lbeta(cov.alt+f.samp*fac, cov.ref+(1-f.samp)*fac)-lbeta(f.samp*fac,(1-f.samp)*fac);
#   #  llk<-lgamma(fac*f.samp+cov.alt)+lgamma(fac*(1-f.samp)+cov.ref)-lgamma(fac*f.samp)-lgamma(fac*(1-f.samp));
#   if (sum(is.nan(llk))>1){
#     print("f.samp = ")
#   }
#   return(llk);
# }
# 
# #############################
# fun.dic.by.llk.var <- function ( tmpllk ){
#   return (  mean(-2*tmpllk) + var(-2*tmpllk)/2 )# D_bar + 1/2 var (D_theta), where D_theta = -2*tmpllk, and D_bar = mean(D_theta)
# }
# 
# #############################
# fun.dic.by.theta <- function ( tmpllk, thetallk ){
#   DIC.WSAF.bar = -2 * sum(thetallk)
#   return (  mean(-2*tmpllk) + (mean(-2*tmpllk) - DIC.WSAF.bar) ) # D_bar + pD, where pD = D_bar - D_theta, and D_bar = mean(D_theta)
# }

#############################
plotLLKPlotly <- function (llk, llkEvent){
  
  #    llk_sd = sd(llk)
  #    llk_range = range(llk)
  
  #    dic.by.var = fun.dic.by.llk.var(llk)
  #    dic.by.theta = fun.dic.by.theta(llk, fun.llk(ref, alt, expWSAF))
  
  x = c(1:length(llk))
  llks = data.frame(x, llk, llkEvent)
  llks$single = ifelse(llkEvent == 1, llk, NA)
  llks$both = ifelse(llkEvent == 2, llk, NA)
  llks$prop = ifelse(llkEvent == 0, llk, NA)
  
  # plot_ly(llks, x = ~x, y = ~single, type = "scatter", mode = "markers",
  #         marker = list(size = 3, color = "#ff0054"), name = "Single") %>%
  #   add_trace(y = ~both, mode = "markers", 
  #             marker = list(size = 3, color = "#005dff"), name = "Both") %>%
  #   add_trace(y = ~prop, mode = "markers", 
  #             marker = list(size = 3, color = "#00ff87"), name = "Prop") %>%
  #   
  #   add_trace(y = ~llk, mode = "lines+markers", name = "llk",
  #             line = list(color = "black", width = 1, dash = "dot"))
  
  plot_ly() %>% 
    add_data(llks) %>%
    add_trace(x = ~x, y = ~llk, name = 'llk', type = 'scatter', mode = 'lines',
              line = list(color = "black", width = 1, dash = "dot")) %>%
    add_trace(x = ~x, y = ~single, name = 'Sinlge', type = 'scatter', mode = 'markers',
              marker = list(size = 5, color = "#ff0054")) %>%
    add_trace(x = ~x, y = ~both, name = 'Both', type = 'scatter', mode = 'markers',
              marker = list(size = 5, color = "#005dff")) %>%
    add_trace(x = ~x, y = ~prop, name = 'Prop', type = 'scatter', mode = 'markers',
              marker = list(size = 5, color = "#06e579")) %>%
    layout(margin = list(l = 85, r = 25, b = 50, t = 80, pad = 0),
           title = "LLK", font = list(size = 18, colot = "black"),
           xaxis = list(title = "Iteration", 
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 14, color = "black")),
           yaxis = list(title = "LLK",
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 14, color = "black")))
}




