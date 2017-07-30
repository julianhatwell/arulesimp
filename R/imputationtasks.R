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
#' Control the parameters for generation of classification association rules
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
                              , sort_by = "confidence"
                              , maxlen) {
  cc <- list(support = support
    , confidence = confidence
    , sort_by = sort_by
  )
  class(cc) <- c("cars_control", "list")
  if (!missing(lift)) cc$lift <- lift
  if (!missing(maxlen)) cc$maxlen <- maxlen
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

#' Create classification rules
#'
#' @description Create a list of classification rules for each target viable
#'
#' @export
make_cars <- function(dt, var_names
                      , c_control = cars_control(support = 0.1
                                      , confidence = 0.1
                                      , sort_by = "confidence")) {
  if (!(class(dt) %in% c("transactions", "data.frame"))) stop("provide an arules transaction object if possible, or a data.frame where all variables are factors")
  if (!("cars_control" %in% class(c_control))) stop("please provide control paramaters with cars_control function")

  parameter <- list(
    support = c_control$support
    , confidence = c_control$confidence)

  if (!is.null(c_control$maxlen)) parameter$maxlen <- c_control$maxlen
  rules <- arules::apriori(dt
                           , parameter = parameter
                           , control = list(verbose = FALSE))
  cars <- rules_to_cars(rules, var_names, sort_by = c_control$sort_by)
  return(cars)
}

#' Control parameters for ARImputation
#'
#' Control the parameters for the AR imputation routine
#'
#' @param ruleset An object of class rules (package arules)
#' @param var_names A character vector with the names of target variables in the dataset which the rules came from
#' @param support A scalar value between 0 and 1. Minimum support to filter rules.
#' @param confidence A scalar value between 0 and 1. Minimum confidence to filter rules.
#' @param lift An optional scalar value between 0 and 1. Minimum lift to filter rules.
#' @param sort_by A character string (default "confidence") indicating which quality measure to sort the rules by
#'
#' @export
arulesimp_control <- function(method
                              , use_default_classes
                              , rows_to_impute
                              , top_n) {
  ac <- list(
    method = method
    , use_default_classes = use_default_classes)
  class(ac) <- c("arulesimp_control", "list")
  if (!(missing(rows_to_impute))) ac$rows_to_impute <- rows_to_impute
  if (!(missing(top_n))) ac$top_n <- top_n
  return(ac)
}


#' ARImputation
#'
#' Association Rules-based Imputation
#'
#' @export
ARImpute <- function(cars, dt, var_names
                     , ari_control = arulesimp_control(
                       method = "best_rule"
                     , use_default_classes = TRUE)
                     ) {
  if (class(dt) != "data.frame" ||
      no_missing_check(dt)
  ) stop("provide a data frame with missing values to impute")
  if (missing(var_names)) {
    warning("no var_names requested. imputing all where missing values are found")
    var_names <- names(missing_values(dt))
  }
  if (!("arulesimp_control" %in% class(ari_control))) stop("please provide control paramaters with arulesimp_control function")
  if (is.null(ari_control$rows_to_impute)) rows_to_impute <- find_rows_to_impute(dt)

  if (ari_control$method == "best_rule") {
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
          dt[rows_to_impute[[v]][[k]], v] <- cars[[v]][[i]]$consequent
        }
      }
    }
  } # end of best rule

  if (grepl("^top\\_n", ari_control$method)) {
    for (v in var_names) {
      # another loop here over each row to impute
      for (k in seq_along(rows_to_impute[[v]])) {
        # initialize for the antecedent outer loop
        n_rules <- list()
        num_rules <- length(cars[[v]])
        i <- 0
        n <- 0
        while (n < ari_control$top_n &&
               i < num_rules) {
          i <- i + 1
          data_value <- as.character(dt[rows_to_impute[[v]][k]
                                        , as.character(cars[[v]][[i]]$antecedent$key)])
          condition_value <- as.character(cars[[v]][[i]]$antecedent$value)
          if (identical(data_value, condition_value)) {
            n <- n + 1
            n_rules[[n]] <- cars[[v]][[i]]
          }
        }
        if (n > 0) {
          if (ari_control$method == "top_n_mean") {
            dt[rows_to_impute[[v]][[k]], v] <- as.character(
              nd_round(mean(
              sapply(n_rules, function(rule) {
              as.numeric(rule$consequent)
            }))))
          }
          if (ari_control$method == "top_n_majv") {
            majv <- names(which.max(table(
                sapply(n_rules, function(rule) {
                  as.numeric(rule$consequent)}))))
            if (length(majv > 1)) majv <- sample(majv, size = 1)
            dt[rows_to_impute[[v]][[k]], v] <- majv
          }
        }
      }
    }
  } # end of top_n

  if (ari_control$method %in% c("laplace", "weighted_chisq")) {
    num_classes <- max(sapply(dt, as.numeric), na.rm = TRUE) -
                    (min(sapply(dt, as.numeric), na.rm = TRUE) - 1)
    for (v in var_names) {
      print(paste("start calc stats", v))
      r_tot <- nrow(dt) - length(rows_to_impute[[v]])
      src_rows <- (1:2000)[-rows_to_impute[[v]]]
      num_rules <- length(cars[[v]])

      # pre-calculate statistics
      rule_stat <- sapply(seq_along(cars[[v]]), function(rule) {
        condition_value <- as.character(cars[[v]][[rule]]$antecedent$value)
        supp_c <- table(dt[src_rows, v])[cars[[v]][[rule]]$consequent]

        if (length(cars[[v]][[rule]]$antecedent$key) == 1) {
          all_match <- dt[src_rows, as.character(cars[[v]][[rule]]$antecedent$key)] == condition_value
          all_match <- ifelse(is.na(all_match), FALSE, all_match)
        } else {
          ante <- matrix(rep(condition_value, r_tot), ncol = length(condition_value), byrow = TRUE)
          all_match <- dt[src_rows, as.character(cars[[v]][[rule]]$antecedent$key)] == ante
          all_match <- ifelse(is.na(all_match), FALSE, all_match)
          all_match <- apply(all_match, 1, all)
        }

        supp_rule <- sum(all_match)

        if (ari_control$method == "laplace") {
          return((supp_c / r_tot + 1) / (supp_rule / r_tot + num_classes))
        }

        if(ari_control$method == "weighted_chisq") {
          e_max <- 1/(supp_rule * supp_c) +
            1/(supp_rule * (r_tot - supp_c)) +
            1/((r_tot - supp_rule) * supp_c) +
            1/((r_tot - supp_rule) * (r_tot - supp_c))
          max_chi <- (min(supp_rule, supp_c) - (supp_rule * supp_c)/r_tot)^2 * r_tot * e_max

          true_pos <- sum(dt[src_rows, v] == cars[[v]][[rule]]$consequent & all_match)
          false_pos <- sum(dt[src_rows, v] == cars[[v]][[rule]]$consequent & !all_match)
          false_neg <- sum(dt[src_rows, v] != cars[[v]][[rule]]$consequent & all_match)
          true_neg <- sum(dt[src_rows, v] != cars[[v]][[rule]]$consequent & !all_match)

          chi <- chisq.test(matrix(c(true_pos, false_pos, false_neg, true_neg), ncol = 2, nrow = 2))$statistic

          return(chi^2 / max_chi)
        }
      })
      print(paste("finish calc stats", v))
      for (k in seq_along(rows_to_impute[[v]])) {
        print(paste("start find rules for row", k))
        n_rules <- list()
        n <- 0
        for (i in 1:num_rules) {
          data_value <- as.character(dt[rows_to_impute[[v]][k]
                                        , as.character(cars[[v]][[i]]$antecedent$key)])
          condition_value <- as.character(cars[[v]][[i]]$antecedent$value)
          if (identical(data_value, condition_value)) {
            print(paste("found rule", i))
            n <- n + 1
            n_rules[[n]] <- cars[[v]][[i]]
            n_rules[[n]]$quality$statistic <- rule_stat[i]
          }
        }

        # best k for each class
        # aggregation and imputation step for laplace or chisq
        print(paste("imputing row", k))
        if (length(n_rules) > 0) {
          all_rules <- as.data.frame(t(sapply(n_rules, function(rule) {
            c(as.numeric(rule$consequent), rule$quality$support, rule$quality$statistic)
          })))
          names(all_rules) <- c("consequent", "support", "statistic")
          with(all_rules
               , tapply(statistic, consequent, mean))
          # best ave exp acc is pred
          dt[rows_to_impute[[v]][[k]], v] <- names(which.max(with(all_rules
                                            , tapply(statistic, consequent, mean))))
        }
      }
    }
  } # end of la place and chi

  remaining <- sum(colSums(sapply(dt, is.na)))
  if (remaining > 0) {
    warning(paste("not every missing value had a covering rule.", remaining, "remaining"))
    if (ari_control$use_default_classes) {
      print(paste("imputing default classes"))
      default_classes <- sapply(sapply(dt[, var_names], table), which.max)
      names(default_classes) <- var_names
      for (v in var_names) {
        dt[[v]] <- ifelse(is.na(dt[[v]]), default_classes[v], dt[[v]])
      }
    }
  }
  return(dt) # TO DO: return the ability stats
}
