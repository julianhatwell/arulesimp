# means, correlations, alphas

#' Relative Bias
#'
#' Calculate the relative bias of a parameter with respect to another value, usually the population parameter
#' @param theta A numeric scalar or vector containing the parameter to be tested
#' @param theta_p A scalar numeric. The population or reference value
rel_bias <- function(theta, theta_p) {
  (mean(theta, na.rm = TRUE) - theta_p) /
    theta_p
}

#' Standardised Bias
#'
#' Calculate the standardised bias of a parameter with respect to another value, usually the population parameter
#' @param theta A numeric vector containing the parameter to be tested. A scalar value will return Inf
#' @param theta_p A scalar numeric. The population or reference value
st_bias <- function(theta, theta_p) {
  if (length(theta > 1)) {
    (mean(theta, na.rm = TRUE) - theta_p) /
      sd(theta) } else {
      warning("scalar parameter has no standard deviation. divide by zero occurred")
      return(Inf)
    }
}

#' Mean Squared Error
#'
#' Calculate the mean squared error of a parameter with respect to another value, usually the population parameter
#' @param theta A numeric vector containing the parameter to be tested. A scalar value will return Inf
#' @param theta_p A scalar numeric.
mse <- function(theta, theta_p) {
  mean((theta - theta_p)^2, na.rm = TRUE)
}

#' Mean Absolute Error
#'
#' Calculate the mean absolute error of a parameter with respect to another value, usually the population parameter
#' @param theta A numeric vector containing the parameter to be tested.
#' @param theta_p A scalar numeric. The population or reference value
mae <- function(theta, theta_p) {
  mean(abs(theta - theta_p), na.rm = TRUE)
}

#' Quality Measures of Parameter Estimate
#'
#' Calculate various quality measures for a parameter with respect to a population or reference value
#' @param theta A numeric scalar or vector containing the parameter to be tested
#' @param theta_p A scalar numeric. The population or reference value
#'
#' @export
quality_measures <- function(theta, theta_p) {
  mse <- mse(theta, theta_p)
  return(list(
    mean = mean(theta, na.rm = TRUE)
    , sd = sd(theta, na.rm = TRUE)
    , iqr = IQR(theta, na.rm = TRUE)
    , rel_bias = rel_bias(theta, theta_p)
    , st_bias = st_bias(theta, theta_p)
    , mse = mse
    , rmse = sqrt(mse)
    , mae = mae(theta, theta_p)
    , mad = mad(theta, center = theta_p)
  ))
}

#' Confidence Interval Coverage
#'
#' Calculate the confidence interval coverage of a parameter with respect to another value, usually the population parameter. Returns the proportion of CIs that contain the reference value.
#' @param ci A two column matrix or data frame containing the lower and upper confidence bounds,
#' @param theta_p A scalar numeric. The population or reference value
#'
#' @export
ci_cover <- function(ci, theta_p) {
  if (ncol(ci) != 2) stop("ci must be a 2 column matrix or data frame")
  cic <- ci[, 1] <= theta_p & theta_p <= ci[, 2]
  return(sum(cic)/length(cic))
}

#' Cronbach's alpha Confidence Interval
#'
#' Calculate a confidence interval for Cronbach's alpha using the F-distribution
#' @param a A scalar numeric. The value of alpha to be tested
#' @param n A scalar numeric. The number of cases (subject, respondents).
#' @param p A scalar numeric. The number of item in the scale.
#' @param sig A scalar numeric between 0 and 1. Default 0.05 (95% confidence).
#'
#' @export
alpha_ci <- function(a, n, p, sig = 0.05, include_a = TRUE) {
  if (class(include_a) != "logical" && length(include_a) != 1) stop("include_a must be a logical scalar")
  if (sig < 0 || sig > 1) stop("sig must be between 0 and 1")
  ci_lower <- 1 - (1 - a) * qf(1 - sig/2
                               , df1 = n - 1
                               , df2 = (n - 1)*(p - 1)
  )
  ci_upper <- 1 - (1 - a) * qf(sig/2
                               , df1 = n - 1
                               , df2 = (n - 1)*(p - 1)
  )
  ci_lower <- ifelse(ci_lower < 0
                     , 0, ci_lower)
  if (include_a) {
    return(cbind(alpha = a
                 , ci_lower = ci_lower
                 , ci_upper = ci_upper))
  } else {
    return(cbind(ci_lower = ci_lower
                 , ci_upper = ci_upper))
  }

}
