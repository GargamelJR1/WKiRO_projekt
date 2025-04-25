plot_confidence_intervals <- function(bc_pairs, labels, n, alpha, ci_fun) {
  # Compute confidence intervals for each pair
  ci_mat <- t(sapply(bc_pairs, function(x) ci_fun(x[1], x[2], n, alpha)))
  mids <- rowMeans(ci_mat)

  # Set up plot
  plot(
    seq_along(labels), mids,
    ylim = range(ci_mat),
    xaxt = "n", xlab = "", ylab = "Confidence Interval",
    pch = 19, main = "Pair-wise comparison of models (i,j)", type = "n"
  )
  axis(1, at = seq_along(labels), labels = labels, las = 1)

  # horizontal line at y = 0
  abline(h = 0, lty = 2)

  for (i in seq_along(labels)) {
    # Vertical line
    lines(c(i, i), ci_mat[i,], lwd = 2)
    # Horizontal lines at top and bottom
    segments(i - 0.2, ci_mat[i, 1], i + 0.2, ci_mat[i, 1], lwd = 2)
    segments(i - 0.2, ci_mat[i, 2], i + 0.2, ci_mat[i, 2], lwd = 2)
    # Points at the center of the intervals
    points(i, mids[i], pch = 19, cex = 2)
  }
}
