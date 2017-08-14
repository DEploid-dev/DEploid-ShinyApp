
plot.Proportions <- function (proportions){
  proportions = PG0390CoverageTxt.deconv$Proportions
  proportions = as.data.frame(proportions)         
  pnum = as.numeric(ncol(proportions))
  proportions$x = rownames(proportions)
  colors = c("#3e9fa0", "#ffc700", "#4297f7", "#ff0000", "#afb55a")
  names = c("Parasite 1", "Parasite 2", "Parasite 3", "Parasite 4", "Parasite 5")
  
  p0 = plot_ly(proportions, x = ~x, y = ~V1, type = 'bar', name = 'Parasite 1', 
               marker = list(color = colors[1]), width = 0.15) %>%
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
    p0 = add_trace(p0, x = ~x, y = ~proportions[ ,i], name = names[i], 
                   marker = list(color = colors[i]), width = 0.15)
  }
  p0
}