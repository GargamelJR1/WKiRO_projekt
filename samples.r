source("plots.r")
source("functions.R")


## b and c values from article below, written by Daniel Berrara and Jose A. Lozanob
# "Significance tests or confidence intervals: which are preferable for the comparison of classifiers?"
bc_pairs <- list(
  c(3, 21), c(1, 15), c(4, 20), c(4, 19), c(2, 14),
  c(4, 17), c(13, 10), c(0, 2), c(11, 15), c(14, 15)
)
labels <- c(
  "(1,4)", "(1,3)", "(2,4)", "(1,5)", "(2,3)",
  "(2,5)", "(4,5)", "(1,2)", "(3,4)", "(3,5)"
)
n <- 50
alpha <- 0.05
m <- length(bc_pairs)

# Holm's adjusted alpha values
alphas_holm <- sapply(seq_len(m), function(k) alpha / (m - k + 1))

### McNemar tests ###

results <- data.frame(
  Comparison = labels,
  b = sapply(bc_pairs, function(x) x[1]),
  c = sapply(bc_pairs, function(x) x[2]),
  X2 = signif(sapply(bc_pairs, function(x)(abs(x[1] - x[2])) ^ 2 / (x[1] + x[2])), 3),
  p_value = formatC(sapply(bc_pairs, function(x) mc_nemar_test(x[1], x[2])), format = "e", digits = 1),
  corrected_X2 = signif(sapply(bc_pairs, function(x)(abs(x[1] - x[2]) - 1) ^ 2 / (x[1] + x[2])), 3),
  corrected_p = formatC(sapply(bc_pairs, function(x) mc_nemar_test(x[1], x[2], correct = TRUE)), format = "e", digits = 1),
  exact_p = formatC(sapply(bc_pairs, function(x) mc_nemar_exact_test(x[1], x[2])), format = "e", digits = 1),
  alpha = formatC(alphas_holm, format = "e", digits = 1)
)
print(results, row.names = FALSE)

### Confidence intervals ###

# Unadjusted confidence intervals
ci_unadj <- t(sapply(bc_pairs, function(x) confidence_interval(x[1], x[2], n, alpha)))

# Holm-adjusted confidence intervals
ci_holm <- matrix(NA, nrow = m, ncol = 2)
for (k in seq_along(alphas_holm)) {
  idx <- k # Index of k-th smallest p-value
  alpha_k <- alphas_holm[k] # Holm-adjusted alpha for this comparison
  b <- bc_pairs[[idx]][1]
  c <- bc_pairs[[idx]][2]
  ci_holm[idx,] <- confidence_interval(b, c, n, alpha_k)
}

# Print comparison table
cat(sprintf("%12s %5s %5s %25s %25s\n",
            "Comparison", "b", "c",
            sprintf("Unadj. %.0f%% CI", 100 - alpha * 100),
            sprintf("Holm-adj. %.0f%% CI", 100 - alpha * 100)))
for (i in seq_len(m)) {
  b <- bc_pairs[[i]][1]
  c <- bc_pairs[[i]][2]
  u <- sprintf("[%.2f, %.2f]", ci_unadj[i, 1], ci_unadj[i, 2])
  h <- sprintf("[%.2f, %.2f]", ci_holm[i, 1], ci_holm[i, 2])
  cat(sprintf("%12s %5d %5d %25s %25s\n", labels[i], b, c, u, h))
}


### Plot ###
plot_confidence_intervals(ci_unadj, labels, ci_holm)