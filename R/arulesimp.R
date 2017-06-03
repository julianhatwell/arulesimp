#' arulesimp: An association rules based method for imputing missing values.
#'
#' The arulesimp package provides an alternative imputation method.
#' The method requires a data frame containing factors and logical types only.
#'
#' @section pre-conditions
#' Continuous variables should be discretized using typical methods
#'  such as \code{cut()}, \code{arules::discretize()} or \code{lattice::equal.count()}.
#'
#'  @docType package
#'  @name arulesimp
NULL

#' Young People's Survey
#'
#' A dataset containing responses to a 150 item questionnaire
#' on preferences and attitudes of young 1010 young people
#'
#' @format a data frame with 1010 rows and 150 variables:
#' \describe{
#'   \item{Music}{I enjoy listening to music.:
#'   Strongly disagree 1-2-3-4-5 Strongly agree (integer)}
#'   \item{Slow.songs.or.fast.songs}
#'   {Slow paced music 1-2-3-4-5 Fast paced music (integer)}
#' }
#' @source \url{https://www.kaggle.com/miroslavsabo/young-people-survey}
#' "responses"
