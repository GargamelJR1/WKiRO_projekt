plot_confidence_intervals <- function(ci_mat, labels, ci_holm_mat = NULL) {
  mids <- rowMeans(ci_mat)

  # Combine for ylim if holm_mat is provided
  ylim_range <- if (!is.null(ci_holm_mat)) range(c(ci_mat, ci_holm_mat)) else range(ci_mat)

  # Set up plot
  plot(
    seq_along(labels), mids,
    ylim = ylim_range,
    xaxt = "n", xlab = "", ylab = "Confidence Interval",
    pch = 19, main = "Pair-wise comparison of models (i,j)", type = "n"
  )
  axis(1, at = seq_along(labels), labels = labels, las = 1)

  # horizontal line at y = 0
  abline(h = 0, lty = 2)

  # Add x offset if Holm's intervals are provided
  offset <- if (is.null(ci_holm_mat)) 0 else 0.15

  dot_size <- 1.8
  line_width <- 2.2

  for (i in seq_along(labels)) {
    # Vertical line
    lines(c(i - offset, i - offset), ci_mat[i,], lwd = line_width)
    # Horizontal lines at top and bottom
    segments(i - offset - 0.1, ci_mat[i, 1], i - offset + 0.1, ci_mat[i, 1], lwd = line_width)
    segments(i - offset - 0.1, ci_mat[i, 2], i - offset + 0.1, ci_mat[i, 2], lwd = line_width)
    # Points at the center of the intervals
    points(i - offset, mids[i], pch = 19, cex = dot_size)

    # If Holm's intervals are provided, plot them as well
    if (!is.null(ci_holm_mat)) {
      lines(c(i + offset, i + offset), ci_holm_mat[i,], lwd = line_width)
      segments(i + offset - 0.1, ci_holm_mat[i, 1], i + offset + 0.1, ci_holm_mat[i, 1], lwd = line_width)
      segments(i + offset - 0.1, ci_holm_mat[i, 2], i + offset + 0.1, ci_holm_mat[i, 2], lwd = line_width)
      points(i + offset, rowMeans(ci_holm_mat)[i], pch = 19, cex = dot_size)
    }
  }
}