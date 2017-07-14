plotAltVsRef <- function ( ref, alt, title = "Alt vs Ref",
                           exclude.ref = c(), exclude.alt = c(), potentialOutliers = c(),
                           cex.lab = 1, cex.main = 1, cex.axis = 1 ){
  cr <- colorRampPalette(colors = c("#de2d26", "#2b8cbe"))
  colors <- cr(31)
  ratios <- ref / (ref + alt + 0.0000001)
  tmpRange <- 1.1 * mean(max(alt), max(ref))
  plot ( ref, alt, xlim = c(0, tmpRange), ylim = c(0, tmpRange),
         pch = 20, col = scales::alpha(colors[ceiling(ratios * 30) + 1], 0.7),
         xlab = "Reference # Reads", ylab = "Alternative # Reads", main = title,
         cex = 0.5, cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis)
  legend("topright", legend = c("100% Alt", "100% Ref", "50/50"),
         fill = colors[c(1, 31, 15)], cex = cex.lab, border = NA, box.lwd = 0,
         box.col = "white", bg = NA)
  abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")
  points(exclude.ref, exclude.alt, col = "red")
  abline(v = 50, untf = FALSE, lty = 2)
  abline(h = 50, untf = FALSE, lty = 2)
  
  abline(h = 150, untf = FALSE, lty = 2)
  abline(v = 150, untf = FALSE, lty = 2)
  
  if ( length(potentialOutliers) > 0 ){
    points(ref[potentialOutliers], alt[potentialOutliers], col="black", pch="x", cex = 2)
  }
}