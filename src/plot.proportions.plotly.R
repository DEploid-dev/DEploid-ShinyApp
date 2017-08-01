
plot.Proportions <- function (proportions){
  plot_ly(proportions, x = ~x, y = ~V1, type = 'bar', name = 'Parasite 1', 
          marker = list(color = "#3e9fa0")) %>%
    add_trace(y = ~V2, name = 'Parasite 2', marker = list(color = "#ff0000")) %>%
    add_trace(y = ~V3, name = 'Parasite 3', marker = list(color = "#4297f7")) %>%
    add_trace(y = ~V4, name = 'Parasite 4', marker = list(color = "#afb55a")) %>%
    add_trace(y = ~V5, name = 'Parasite 5', marker = list(color = "#ffc700")) %>%
    layout(margin = list(l = 70, r = 25, b = 30, t = 80, pad = 0),
           barmode = "relative",
           title = "Components", font = list(size = 18, colot = "black"),
           legend = list(font = list(size = 15)),
           xaxis = list(title = "Iteration", range = c(0,  max(proportions$x)),
                        titlefont = list(size = 18, color = "black"),
                        showticklabels = FALSE),
           yaxis = list(title = "Component Proportion", range = c(0, 1),
                        titlefont = list(size = 18, color = "black"),
                        tickfont = list(size = 16, color = "black")))
}