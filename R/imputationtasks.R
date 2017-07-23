# imputation routines

#' Find rows to impute
#'
#' Find the rows with missing values to impute for each target variable
#'
#' @param dt A data frame or an object of class mim
#' @param which_cols A vector of variable names to search.
#' @export
find_rows_to_impute <- function(dt, which_cols) {
  if ("data.frame" %in% class(dt)) dt <- missing_matrix(dt)
  if (!("mim" %in% class(dt))) stop("provide a mim object or data frame")

  if (missing(which_cols)) return(dt$B_comp_j)

  all_cols <- 1:ncol(dt$mim)
  which_cols <- which(which_cols %in% all_cols)
  return(dt$B_comp_j[which_cols])
}

#' Find rows to impute
#'
#' Find the columns (variables) with missing values to impute for each target row
#'
#' @param dt A data frame or an object of class mim
#' @param which_rows A integer vector of row numbers to search.
#' @export
find_cols_to_impute <- function(dt, which_rows) {
  if ("data.frame" %in% class(dt)) dt <- missing_matrix(dt)
  if (!("mim" %in% class(dt))) stop("provide a mim object or data frame")

  if (missing(which_rows)) return(dt$A_comp_i)

  all_rows <- 1:nrow(dt$mim)
  which_rows <- which(which_rows %in% all_rows)
  return(dt$A_comp_i[which_rows])
}

#' Row and Column Means
#'
#' Calculate all the i and j means for a data frame
#' @param dt A data frame
#' @export
ij_means <- function(dt) {
  if (class(dt) != "data.frame") stop("dt must be a data frame")
  i_means <- rowMeans(dt, na.rm = TRUE)
  j_means <- colMeans(dt, na.rm = TRUE)
  i_sums <- rowSums(dt, na.rm = TRUE)
  j_sums <- colSums(dt, na.rm = TRUE)
  num <- sum(!is.na(dt))
  t_mean <- sum(i_sums) / num

  # for each i, mean of the j_means for each observed j
  if (sum(is.na(dt)) == 0) {
    ij_means <- rep(mean(j_means), nrow(dt))
  } else {
    dt_mim <- missing_matrix(dt)
    ij_means <- unlist(lapply(1:nrow(dt), function(x) {
      mean(j_means[dt_mim$A_i[[x]]])
      }))
  }
  # correlations
  corr <- corrr::correlate(dt)
  corrs <- list()
  for (nm in corr$rowname) {
    corrs[[nm]] <- unlist(corr[corr$rowname == nm, -1])
  }
  return(list(i_means = i_means
              , j_means = j_means
              , ij_means = ij_means
              , i_sums = i_sums
              , j_sums = j_sums
              , num = num
              , t_mean = t_mean
              , corrs = corrs))
}

#' Non-deterministic Rounding
#'
#' Round up or down using a weighted Bernoulli distribution
#' @param x numeric vector
#'
#' @export
nd_round <- function(x) {
  int_part <- floor(x)
  frac_part <- x - int_part
  return(int_part + rbinom(length(x)
                           , 1
                           , prob = frac_part))
}

#' Person Mean Imputation
#'
#' Impute missing values by substituting the row mean of observed values
#' @param dt A data frame containing some missing values
#' @param rounding A scalar character, indicating the type of rounding to use
#'
#' @export
LikertImpute <- function(dt, dt_mim
                         , method = "PM"
                         , rounding
                         , top_code = FALSE
                         , bottom_code = FALSE) {
  if (missing(dt_mim)) dt_mim <- missing_matrix(dt)
  ij <- ij_means(dt[, dimnames(dt_mim$mim)[[2]]])

  for (j in names(dt_mim$B_comp_j)) {

    means <- switch(method
      , PM = ij$i_means
      , CIM = ij$i_means /
        ij$ij_means *
        ij$j_means[j]
      , TW = ij$i_means +
        ij$j_means[j] - ij$t_mean
      , ICS = sapply(1:nrow(dt), function(x) {
          if (length(dt_mim$A_i[[x]]) == 0 ||
              all(names(dt_mim$A_i[[x]]) == j)) {
            NA
          } else {
            dt[x, names(ij$corrs[[j]][names(which.max(ij$corrs[[j]][names(dt_mim$A_i[[x]])]))])]
            }
        })
      )

    to_impute <- means[dt_mim$B_comp_j[[j]]]
    if (!missing(rounding) && method != "ICS") to_impute <- do.call(rounding, list(to_impute))
    dt[[j]][dt_mim$B_comp_j[[j]]] <- to_impute
  }
  if (top_code) dt <- sapply(dt, function(x) ifelse(x > top_code, top_code, x))
  if (bottom_code) dt <- sapply(dt, function(x) ifelse(x < bottom_code, bottom_code, x))

  return(dt)
}

#' Control parameters for make_cars
#'
#' Control the parameters for generating patterns of missing data
#'
#' @param ruleset An object of class rules (package arules)
#' @param var_names A character vector with the names of target variables in the dataset which the rules came from
#' @param support A scalar value between 0 and 1. Minimum support to filter rules.
#' @param confidence A scalar value between 0 and 1. Minimum confidence to filter rules.
#' @param lift An optional scalar value between 0 and 1. Minimum lift to filter rules.
#' @param sort_by A character string (default "confidence") indicating which quality measure to sort the rules by
#'
#' @export
cars_control <- function(support
                              , confidence
                              , lift
                              , sort_by = "confidence") {
  cc <- list(support = support
    , confidence = confidence
    , sort_by = sort_by
  )
  class(cc) <- c("cars_control", "list")
  if (!missing(lift)) cc$lift <- lift
  return(cc)
}

#' Convert arules object to cars
#'
#' Get the classification association rules from a rule set for named variables
#'
#' @export
rules_to_cars <- function(ruleset, var_names, sort_by) {
  # get the labels out of the rules object and clean the text up
  clean_rhs <- function(x) {
    strsplit(sub("\\}", "" # get rid of curly brace right
                 , sub("\\{", "" # get rid of curly brace left
                       , x))
             , split = "=")

  }

  clean_lhs <- function(x) {
    pre_step <- strsplit(sub("\\}", "" # get rid of curly brace right
                             , sub("\\{", "" # get rid of curly brace left
                                   , x))
                         , split = ",")
    lapply(pre_step, function(ps) {
      if (length(ps) == 0) return(data.frame(key = ""
                                             , value = NA))
      conditions <- data.frame(t(as.data.frame(lapply(ps
                                                      , strsplit
                                                      , split = "="))))
      names(conditions) <- c("key", "value")
      row.names(conditions) <- 1:nrow(conditions)
      return(conditions)
    })
  }

  qual <- arules::quality(ruleset)
  antecedent <- arules::labels(arules::lhs(ruleset))
  consequent <- arules::labels(arules::rhs(ruleset))

  non_empty_rules <- which(antecedent != "{}")
  qual <- qual[non_empty_rules, ]
  antecedent <- antecedent[non_empty_rules]
  consequent <- consequent[non_empty_rules]

  antecedent <- clean_lhs(antecedent)[order(qual[[sort_by]], decreasing = TRUE)]
  consequent <- clean_rhs(consequent)[order(qual[[sort_by]], decreasing = TRUE)]
  qual <- qual[order(qual[[sort_by]], decreasing = TRUE), ]

  cars_out <- list()
  for (v in var_names) {
    idx <- which(sapply(consequent, function(x) {
      x[[1]] == v
    }) == TRUE)

    cars_out[[v]] <- lapply(idx, function(i) {
      cons <- consequent[[i]][2]
      ante <- antecedent[[i]]
      qual <- qual[i, ]
      list(antecedent = ante
           , consequent = cons
           , quality = qual)
    })
  }
  return(cars_out)
}

#' Creat classification rules
#'
#' @description Create a list of classification rules for each target viable
#'
#' @export
make_cars <- function(dt, var_names
                      , c_control = cars_control(method = "best_rule"
                                      , support = 0.1
                                      , confidence = 0.1
                                      , sort_by = "confidence")) {
  if (!(class(dt) %in% c("transactions", "data.frame"))) stop("provide an arules transaction object if possible, or a data.frame where all variables are factors")
  if (!("cars_control" %in% class(c_control))) stop("please provide control paramaters with arulesimp_control function")

  rules <- arules::apriori(dt
                           , parameter = list(
                             support = c_control$support
                             , confidence = c_control$confidence)
                           , control = list(verbose = FALSE))
  cars <- rules_to_cars(rules, var_names, sort_by = c_control$sort_by)
  return(cars)
}

#' ARImputation
#'
#' Association Rules-based Imputation
#'
#' @export
ARImpute <- function(cars, dt, var_names
                     , use_default_classes = TRUE
                     , rows_to_impute) {
  if (class(dt) != "data.frame" ||
      no_missing_check(dt)
  ) stop("provide a data frame with missing values to impute")
  if (missing(var_names)) {
    warning("no var_names requested. imputing all where missing values are found")
    var_names <- names(missing_values(dt))
  }
  if (missing(rows_to_impute)) rows_to_impute <- find_rows_to_impute(dt)

  for (v in var_names) {
    # another loop here over each row to impute
    for (k in seq_along(rows_to_impute[[v]])) {
      # initialize for the antecedent outer loop
      all_match <- FALSE
      num_rules <- length(cars[[v]])
      i <- 0
      # print(paste("initialised k loop", v, i, k))
      # start the antecedent outer loop
      while (
            !(all_match) &&
            i < num_rules) {
        # re-initialize for the antecedent inner loop
        i <- i + 1
        all_match <- FALSE
        # print(paste("initialised antecedent i loop", v, i, k, cars[[v]]$antecedent[[i]]))
        data_value <- as.character(dt[rows_to_impute[[v]][k]
                                  , as.character(cars[[v]][[i]]$antecedent$key)])
        condition_value <- as.character(cars[[v]][[i]]$antecedent$value)
        all_match <- identical(data_value, condition_value)
      }
      if (all_match && num_rules != 0) {
        print(paste("closing var:", v
                    , "rule:", i, paste(as.character(unlist(cars[[v]][[i]]$antecedent)), collapse = " ")
                    , "row:", rows_to_impute[[v]][k]
                    , "new value:", cars[[v]][[i]]$consequent))
        dt[rows_to_impute[[v]][[k]], v] <- cars[[v]][[i]]$consequent
      }
    }
  }
  remaining <- sum(colSums(sapply(dw_mnar1$imputed, is.na)))
  if (remaining > 0) {
    warning(paste("not every missing value had a covering rule.", remaining, "remaining"))
    if (use_default_classes) {
      default_classes <- sapply(sapply(dt[, var_names], table), which.max)
      names(default_classes) <- var_names
      for (v in var_names) {
        dt[[v]] <- ifelse(is.na(dt[[v]]), default_classes[v], dt[[v]])
      }
    }
  }
  return(dt)
}
