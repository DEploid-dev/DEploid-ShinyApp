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

plot.total.coverage.dygraphs <- function(ref, alt, coverage,
                                         threshold, window.size,
                                         potentialOutliers, tmp){
  tmp$outliers = ifelse(rownames(tmp) %in% potentialOutliers, tmp$totalDepth, NA)
  chromgroup = coverage %>%
    group_by(CHROM) %>%
    summarise(count = n())
  chromgroup$position = cumsum(chromgroup$count)
  chromgroup$cut = as.numeric(rownames(chromgroup))%%2
  pos1 = chromgroup$position[chromgroup$cut==1]
  pos2 = chromgroup$position[chromgroup$cut==0]
  shadenum <- length(pos1)
  d0 <- dygraph(tmp, xlab = "SNP Index", ylab="Coverage Depth",
                main = "Coverage across the sequence")  %>%
    dySeries("totalDepth", drawPoints = TRUE, strokeWidth = 0,
             color = 'black', pointSize = 1)  %>%
    dySeries("outliers", drawPoints = TRUE, strokeWidth = 0,
             color = 'red', pointSize = 1) %>%
    dyRangeSelector(dateWindow = c(1, max(tmp$x))) %>%
    dyLimit(as.numeric(quantile(tmp$totalDepth, threshold)), color = "red")
  for (i in 1: shadenum) {
    d0 <- d0 %>%
      dyShading(from = pos1[i]-1, to = pos2[i], color = "#dae0e8")
  }
  d0
}


plotWSAFVsPOSDygraphs <- function (wsaf, gene, exon,
                                   checkBoxGene, checkBoxExon,
                                   control, zone, wsaf2){

  if (control == FALSE) {
    d1 <- dygraph(wsaf, xlab = "Positions", ylab="WSAF", main = "")  %>%
      dySeries('obsWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'red', pointSize = 3)  %>%
      dySeries('expWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'blue', pointSize = 3)  %>%
      dyRangeSelector(dateWindow = c(1, max(wsaf$pos)))
    if (checkBoxGene == FALSE && checkBoxExon == FALSE) {
      d1 <- dygraph(wsaf, xlab = "Positions", ylab="WSAF", main = "")  %>%
        dySeries('obsWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'red', pointSize = 3)  %>%
        dySeries('expWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'blue', pointSize = 3)  %>%
        dyRangeSelector(dateWindow = c(1, max(wsaf$pos)))
    }
    if (checkBoxGene) {
      for (i in 1:nrow(gene)) {
        d1 <- d1 %>%
          dyShading(from = gene$pos1[i], to = gene$pos2[i], color = "#b3b4fc")
      }
    }
    if (checkBoxExon) {
      for (i in 1:nrow(exon)) {
        d1 <- d1 %>%
          dyShading(from = exon$pos3[i], to = exon$pos4[i], color = "#b7e5e2")
      }
    }
    if (checkBoxGene && checkBoxExon) {
      for (i in 1:nrow(gene)) {
        d1 <- d1 %>%
          dyShading(from = gene$pos1[i], to = gene$pos2[i], color = "#b3b4fc")
      }
      for (i in 1:nrow(exon)) {
        d1 <- d1 %>%
          dyShading(from = exon$pos3[i], to = exon$pos4[i], color = "#b7e5e2")
      }
    }
  }

  if (control == TRUE) {
    d1 <- dygraph(wsaf2, xlab = "Positions", ylab="WSAF", main = "")  %>%
      dySeries('obsWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'red', pointSize = 3)  %>%
      dySeries('expWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'blue', pointSize = 3)  %>%
      dyRangeSelector(dateWindow = c(min(zone$pos5)-10000, max(zone$pos6)+10000))
    for (i in 1:nrow(zone)) {
      d1 <- d1 %>%
        dyShading(from = zone$pos5[i], to = zone$pos6[i], color = "#ffd951")
    }
    if (checkBoxGene == FALSE && checkBoxExon == FALSE) {
      d1 <- dygraph(wsaf2, xlab = "Positions", ylab="WSAF", main = "")  %>%
        dySeries('obsWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'red', pointSize = 3)  %>%
        dySeries('expWSAF', drawPoints = TRUE, strokeWidth = 0, color = 'blue', pointSize = 3)  %>%
        dyRangeSelector(dateWindow = c(min(zone$pos5)-10000, max(zone$pos6)+10000))
      for (i in 1:nrow(zone)) {
        d1 <- d1 %>%
          dyShading(from = zone$pos5[i], to = zone$pos6[i], color = "#ffd951")
      }
    }
    if (checkBoxGene) {
      for (i in 1:nrow(gene)) {
        d1 <- d1 %>%
          dyShading(from = gene$pos1[i], to = gene$pos2[i], color = "#b3b4fc")
      }
    }
    if (checkBoxExon) {
      for (i in 1:nrow(exon)) {
        d1 <- d1 %>%
          dyShading(from = exon$pos3[i], to = exon$pos4[i], color = "#b7e5e2")
      }
    }
    if (checkBoxGene && checkBoxExon) {
      for (i in 1:nrow(gene)) {
        d1 <- d1 %>%
          dyShading(from = gene$pos1[i], to = gene$pos2[i], color = "#b3b4fc")
      }
      for (i in 1:nrow(exon)) {
        d1 <- d1 %>%
          dyShading(from = exon$pos3[i], to = exon$pos4[i], color = "#b7e5e2")
      }
    }
  }
  d1
}


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
                        tickfont = list(size = 16, color = "black")), bargap = 0)

  for (i in 2:pnum) {
    p0 = add_trace(p0, proportions, x = ~x, y = proportions[ ,i],
                   name = names[i], marker = list(color = colors[i]))
  }
  p0
}


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
    layout(margin = list(l = 95, r = 25, b = 50, t = 80, pad = 0),
           title = "Log Likelihood of MCMC", font = list(size = 18, colot = "black"),
           xaxis = list(title = "Iteration",
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 14, color = "black")),
           yaxis = list(title = "LLK",
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 14, color = "black")))
}
