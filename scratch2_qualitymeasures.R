# means, correlations, alphas

r_bias <- function(theta, theta_p) {
  (mean(theta, na.rm = TRUE) - theta_p) /
    theta_p
}

st_bias <- function(theta, theta_p) {
  (mean(theta, na.rm = TRUE) - theta_p) /
    sd(theta)
}

mse <- function(theta, theta_p) {
  mean((theta - theta_p)^2, na.rm = TRUE)
}

mae <- function(theta, theta_p) {
  mean(abs(theta - theta_p), na.rm = TRUE)
}

ci_cover <- function(ci, theta_p) {
  if (ncol(ci) != 2) stop("ci must be a 2 column matrix or data frame")

  cic <- ci[, 1] <= theta_p & theta_p <= ci[, 2]
  sum(cic)
}


alpha_ci <- function(a, n, p, sig = 0.05) {
  ci_lower <- 1 - (1 - a) * qf(1 - sig/2
                              , df1 = n - 1
                              , df2 = (n - 1)*(p - 1)
  )
  ci_upper <- 1 - (1 - a) * qf(sig/2
                              , df1 = n - 1
                              , df2 = (n - 1)*(p - 1)
  )
  if (ci_lower < 0) ci_lower <- 0
  return(c(ci_lower, ci_upper))
}

# don't forget to quote mse ratio

# alpha confidence intervals by using bootstrapping of funciton in psych


