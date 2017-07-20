
plotAltVsRef.plotly <- function (ref, alt){
  
  ratios <- ref/(ref + alt + 0.0000001)
  tmpRange <- 1.1 * mean(max(alt), max(ref))
  legend.name <- "Ref/(Ref+Alt) Ratio"
  plot_ly(x = ~ref, y = ~alt, type = "scatter", mode = "markers",
          color = ~ ratios, colors=c("#de2d26", "#2b8cbe"), alpha = 0.8,
          marker = list(size = 3, line = list(color = "black", width = 0.3),
                        colorbar = list(title = legend.name)
                        )) %>%
    layout(margin = list(l = 65, r = 25, b = 50, t = 80, pad = 0),
           title = "Alt vs Ref", font = list(size = 18, colot = "black"),
           legend = list(font = list(size = 5)),
           xaxis = list(title = "Reference # Reads", range = c(-5, 200), 
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")),
           yaxis = list(title = "Alternative # Reads", range = c(-10, 200), 
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")),
           shapes = list(list(type = "line", fillcolor = "black", line = list(color = "black", width = 1.2, dash = "dot"),
                              opacity = 0.8, x0 = 50, x1 = 50, y0 = 0, y1 = 200),
                         list(type = "line", fillcolor = "black", line = list(color = "black", width = 1.2, dash = "dot"),
                              opacity = 0.8, x0 = 0, x1 = 200, y0 = 50, y1 = 50),
                         list(type = "line", fillcolor = "grey", line = list(color = "grey", width = 2.5, dash = "dot"),
                              opacity = 0.8, x0 = 0, x1 = 200, y0 = 0, y1 = 200))
           
    )
  
  # if ( length(potentialOutliers) > 0 ){
  #   points(ref[potentialOutliers], alt[potentialOutliers], col="black", pch="x", cex = 2)
  # }
}

