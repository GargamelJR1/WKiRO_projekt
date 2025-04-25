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
  numerator <- chi_sq * ((pb + pc) * (n + chi_sq) - n * (pb - pc) ^ 2)
  common_frac2 <- sqrt(numerator) / (n + chi_sq)

  # lower bound
  lower_bound <- common_frac1 - common_frac2
  # upper bound
  upper_bound <- common_frac1 + common_frac2

  # return the confidence interval
  c(lower_bound, upper_bound)
}

mc_nemar_test <- function(b, c, correct = FALSE) {
  # b = number of cases misclassified by model A, but not B
  # c = number of cases misclassified by model B, but not A

  # calculate the test statistic
  if (!correct) {
    test_statistic <- (abs(b - c)) ^ 2 / (b + c)
  } else {
    test_statistic <- (abs(b - c) - 1) ^ 2 / (b + c)
  }

  # calculate the p-value
  p_value <- pchisq(test_statistic, df = 1, lower.tail = FALSE)

  # return the p-value
  p_value
}

mc_nemar_exact_test <- function(b, c) {
  # b = number of cases misclassified by model A, but not B
  # c = number of cases misclassified by model B, but not A

  # size parameter of the binomial distribution
  n <- b + c

  # calculate exact p-value using binomial distribution
  k <- max(b, c)
  p_value <- 2 * sum(dbinom(k:n, size = n, prob = 0.5))

  # return the p-value
  p_value
}
