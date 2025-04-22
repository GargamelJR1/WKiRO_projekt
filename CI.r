confidence_interval <- function(b, c, n, alpha) {
  # b = probability pb
  # b = number of cases misclassified by model A, but not B
  # c = probability pc
  # c = number of cases misclassified by model B, but not A
  # n = total number of test cases
  # alpha = significance level

  # calculate probabilities
  pb <- b / n
  pc <- c / n

  # critical value
  chi_sq <- qchisq(1 - alpha, df = 1)

  # common fraction 1
  common_frac1 <- (n * abs(pb - pc)) / (n + chi_sq)

  # common fraction 2
  common_frac2 <- sqrt(chi_sq * ((pb + pc) * (n + chi_sq) - n * (pb - pc) ^ 2)) / (n + chi_sq)

  # lower bound
  lower_bound <- common_frac1 - common_frac2
  # upper bound
  upper_bound <- common_frac1 + common_frac2

  # return the confidence interval
  return(c(lower_bound, upper_bound))
}