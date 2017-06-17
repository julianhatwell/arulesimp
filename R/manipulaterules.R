# the next step is a necessary a workaround because
# arules provides no way to manipulate the rules programmatically
# but they can be written out to a csv file
# and then imported again as a data frame

#' Converts rules into a format that is more programmable
extract_cars <- function(cars, v) {
  # sadly apriori doesn't seem to have an easy way
  # to extract all the rules. This is a workaround

  on.exit(if (file.exists("tempcars.csv")) file.remove("tempcars.csv"))
  if (length(cars) == 0) {
    return(list(
      antecedent = list(data.frame(key = ""
                              , value = NA))
      , consequent = list(c(v, NA))
    ))
  }

  arules::write(cars
      , file = "tempcars.csv"
      , sep = ","
      , row.names = FALSE)

  carscsv <- read.csv("tempcars.csv"
                      , stringsAsFactors = FALSE)

  # process rules in order of decreasing lift
  carscsv <- carscsv[order(carscsv$lift
                           , carscsv$confidence
                           , carscsv$support
                           , decreasing = TRUE)
                     , ]
  # extract the antecedents and consequents, lhsrhs

  lhsrhs <- data.frame(strsplit(as.character(carscsv$rules)
                                  , split = " => "))
  lhsrhs <- data.frame(t(lhsrhs)
            , stringsAsFactors = FALSE)
  names(lhsrhs) <- c("lhs", "rhs")
  rownames(lhsrhs) <- 1:nrow(lhsrhs)

  # rules conditions need to be extracted
  # using string manipulations
  # right hand side
  consequent <- strsplit(sub("\\}", "" # get rid of curly brace right
                             , sub("\\{", "" # get rid of curly brace left
                                   , lhsrhs$rhs))
                         , split = "=")
  # real shame I have to go through all that
  # before I can do this check for unique consequents:
  if (length(unique(lhsrhs$rhs)) == 1) {
    return(list(
      antecedent = list(data.frame(key = ""
                              , value = NA))
      , consequent = consequent[1]
    ))
  }

  # left hand side two steps
  # step one - extract each condition
  antecedent_preproc <- strsplit(sub("\\}", "" # get rid of curly brace right
                             , sub("\\{", "" # get rid of curly brace left
                                   , lhsrhs$lhs))
                         , split = ",")
  # step two - convert keys and values to a data frame for each rule
  antecedent <- lapply(antecedent_preproc, function(x) {
    if (length(x) == 0) return(data.frame(key = ""
                                          , value = NA))
    conditions <- data.frame(t(as.data.frame(lapply(x
                    , strsplit
                    , split = "="))))
    names(conditions) <- c("key", "value")
    row.names(conditions) <- 1:nrow(conditions)
    return(conditions)
  })

  return(list(
#    lhs = lhsrhs$lhs
#  , rhs = lhsrhs$rhs
#  , support = carscsv$support
#  , confidence = carscsv$confidence
#  , lift = carscsv$lift
#  ,
    antecedent = antecedent
  , consequent = consequent))
}

#' Creat classification rules
#'
#' @description Create a list of classification rules for each target viable
#'
#' @param ruleset An object of class rules (package arules)
#' @param var_names A character vector with the names of target variables in the dataset which the rules came from
#' @param min_supp A scalar value between 0 and 1. Minimum support to filter rules.
#' @param min_conf A scalar value between 0 and 1. Minimum confidence to filter rules.
#' @param min_lift A scalar value between 0 and 1. Minimum lift to filter rules.
#' @export
make_cars <- function(ruleset
                      , var_names
                      , min_supp = 0.1
                      , min_conf = 0.1
                      , min_lift = 1) {
  cars <- list()
  for (v in var_names) {
    cars[[v]] <- arules::subset(ruleset
                                , subset = rhs %pin% paste0(v, "=") &
                                  support >= min_supp &
                                  confidence >= min_conf &
                                  lift >= min_lift)
  }

  cars_api <- list()
  for (v in var_names) {
    cars_api[[v]] <- extract_cars(cars[[v]], v)

  }
  return(cars_api)
}
