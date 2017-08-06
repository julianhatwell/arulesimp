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
cars_control <- function(antecedent
                        , support
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
  if (!missing(antecedent)) cc$antecedent <- antecedent
  return(cc)
}

#' Convert arules object to cars
#'
#' Get the classification association rules from a rule set for named variables
#'
#' @export
rules_to_cars <- function(ruleset, var_name, sort_by) {
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
  idx <- which(sapply(consequent, function(x) {
    x[[1]] == var_name
  }) == TRUE)

  cars_out[[var_name]] <- lapply(idx, function(i) {
    cons <- consequent[[i]][2]
    ante <- antecedent[[i]]
    qual <- qual[i, ]
    list(antecedent = ante
         , consequent = cons
         , quality = qual)
  })
  return(cars_out)
}

#' Create classification rules
#'
#' @description Create a list of classification rules for each target viable
#'
#' @export
make_cars <- function(dt, var_names
                      , c_control = cars_control(consequent = NULL
                                      , support = 0.1
                                      , confidence = 0.1
                                      , sort_by = "confidence")) {
  if (!("cars_control" %in% class(c_control))) stop("please provide control paramaters with cars_control function")
  if (!(class(dt) %in% c("transactions", "data.frame"))) stop("provide an arules transaction object if possible, or a data.frame where all variables are factors if you are setting an antecedent")
  if ((class(dt) == "transactions" && !(is.null(c_control$antecedent)))) stop("use a plain data frame when setting an antecedent")

  parameter <- list(
    support = c_control$support
    , confidence = c_control$confidence)

  if (!is.null(c_control$maxlen)) parameter$maxlen <- c_control$maxlen

  cars <- list()
  if (is.null(c_control$antecedent)) {
    dt <- as(dt, "transactions")
    rules <- arules::apriori(dt
              , parameter = parameter
              , control = list(verbose = FALSE))
    arules::quality(rules) <- cbind(
      arules::quality(rules)
      , interestMeasure(rules
                        , transactions = dt
                        , measure = c("chiSquared", "laplace"))
    )
    for (v in var_names) {
      sub_rules <- arules::subset(rules
                    , subset = arules::`%pin%`(rhs
                    ,  paste0(v, "=")))
      cars <- append(cars, rules_to_cars(sub_rules
                     , v
                     , sort_by = c_control$sort_by))
    }
  } else {
    # something in antecedent
    for (v in var_names) {
      sub_dt <- all_factor(dt[
        , c(v
        , c_control$antecedent[-grep(v, c_control$antecedent)])])
      sub_dt <- as(sub_dt, "transactions")
      rules <- arules::apriori(sub_dt
                    , parameter = parameter
                    , control = list(verbose = FALSE))
      arules::quality(rules) <- cbind(
        arules::quality(rules)
        , interestMeasure(rules
                          , transactions = dt
                          , measure = c("chiSquared", "laplace"))
      )
      sub_rules <- arules::subset(rules
                    , subset = arules::`%pin%`(rhs
                    ,  paste0(v, "=")))
      cars <- append(cars, rules_to_cars(sub_rules
                     , v
                     , sort_by = c_control$sort_by))
    }
  }
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

  mn <- min(sapply(dt, as.numeric), na.rm = TRUE)
  mx <- max(sapply(dt, as.numeric), na.rm = TRUE)
  num_classes <- mx - (mn - 1)

  for (v in var_names) {
    rule_matches <- lapply(seq_along(cars[[v]]), function(rule) {
      condition_value <- as.character(cars[[v]][[rule]]$antecedent$value)

      if (length(cars[[v]][[rule]]$antecedent$key) == 1) {
        all_match <- dt[rows_to_impute[[v]], as.character(cars[[v]][[rule]]$antecedent$key)] == condition_value
        all_match <- ifelse(is.na(all_match), FALSE, all_match)
        names(all_match) <- names(rows_to_impute[[v]])
      } else {
        ante <- matrix(rep(condition_value, length(rows_to_impute[[v]]))
                       , ncol = length(condition_value)
                       , byrow = TRUE)
        all_match <- dt[rows_to_impute[[v]], as.character(cars[[v]][[rule]]$antecedent$key)] == ante
        all_match <- ifelse(is.na(all_match), FALSE, all_match)
        all_match <- apply(all_match, 1, all)
      }

      if (sum(all_match) == 0) return(NULL)
      match_rows <- names(all_match[all_match == TRUE])
      return(match_rows)
    })

    row_matches <- matrix(
      rep(NA, length(rows_to_impute[[v]]) * length(rule_matches))
      , nrow = length(rows_to_impute[[v]]), ncol = length(rule_matches))
    rownames(row_matches) <- if (is.null(names(rows_to_impute[[v]]))) {
      as.character(rows_to_impute[[v]])
    } else {
      names(rows_to_impute[[v]])
    }
    for (rule in seq_along(rule_matches)) {
      row_matches[rule_matches[[rule]], rule] <- cars[[v]][[rule]]$consequent
    }

    if (ari_control$method == "best_rule") {
      dt[rows_to_impute[[v]], v] <- apply(row_matches, 1, function(rom) {
        rom[!is.na(rom)][1]
      })
    }

    if (grepl("^top\\_n", ari_control$method)) {
      dt[rows_to_impute[[v]], v] <- apply(row_matches, 1, function(rom) {
        top_n <- rom[!is.na(rom)][1:ari_control$top_n]
        if (all(is.na(top_n))) { NA } else
        {
          if (ari_control$method == "top_n_mean") {
            return(nd_round(mean(as.numeric(top_n), na.rm = TRUE)))
          }
          if (ari_control$method == "top_n_majv") {
            majv <- names(which.max(table(top_n)))
            if (length(majv > 1)) majv <- sample(majv, size = 1)
          }
        }
      })
    }

    if (ari_control$method == "consequent_frequency") {
      # add a dummy value for each class
      row_matches <- cbind(row_matches
                           , matrix(c(as.character(mn:mx))
                                    , ncol = num_classes
                                    , nrow = length(rows_to_impute[[v]])
                                    , byrow = TRUE))
      # count freqs
      row_matches <- apply(row_matches, 1, table)

      # remove dummy values
      row_matches <- row_matches - 1
      no_matches <- names(which(apply(row_matches, 2, sum) == 0))

      if (length(no_matches) > 0) {
        # remove an rows with no matching rule
        row_matches <- row_matches[, !(colnames(row_matches) %in% no_matches)]
        rows_to_impute[[v]] <- rows_to_impute[[v]][which(!(rows_to_impute[[v]] %in% no_matches))]
      }

      # impute
      dt[rows_to_impute[[v]], v] <- apply(row_matches, 2, function(rom) {
        sample(as.character(mn:mx), size = 1, prob = rom/sum(rom))
        })
    } # end of consequent freqency

    if (ari_control$method == "weighted_chisq") {
      for (v in var_names) {
        r_tot <- nrow(dt) - length(rows_to_impute[[v]])
        src_rows <- (1:nrow(dt))[-rows_to_impute[[v]]]
        num_rules <- length(cars[[v]])

        if (ari_control$method == "weighted_chisq") {
          max_chi <- sapply(seq_along(cars[[v]]), function(rule) {
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

            e_max <- 1/(supp_rule * supp_c) +
              1/(supp_rule * (r_tot - supp_c)) +
              1/((r_tot - supp_rule) * supp_c) +
              1/((r_tot - supp_rule) * (r_tot - supp_c))
            max_chi <- (min(supp_rule, supp_c) - (supp_rule * supp_c)/r_tot)^2 * r_tot * e_max

            return(max_chi)
          })
        }
        for (k in seq_along(rows_to_impute[[v]])) {
          print(paste("start find rules for row", rows_to_impute[[v]][k]))
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
              if (ari_control$method == "weighted_chisq") {
                n_rules[[n]]$quality$statistic <- n_rules[[n]]$quality$chiSquared / max_chi[i]
              }
              if (ari_control$method == "laplace") {
                n_rules[[n]]$quality$statistic <- n_rules[[n]]$quality$laplace
              }
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
            if (!(is.null(ari_control$top_n))) {
              consequent_stat <- with(all_rules
                                , tapply(statistic, consequent
                                         , function(st) {
                                           sort(st, decreasing = TRUE)[1:ari_control$top_n]
                                           }))
              consequent_stat <- names(which.max(lapply(consequent_stat, mean, na.rm = TRUE)))
            } else {
              dt[rows_to_impute[[v]][[k]], v] <- names(which.max(with(all_rules
                                              , tapply(statistic, consequent, mean))))
            }
          }
        }
      }
    } # end of la place and chi
  }
  remaining <- if (length(var_names) > 1) {
    sum(colSums(sapply(dt[, var_names], is.na)))
  } else {
    sum(is.na(dt[[var_names]]))
  }
  if (remaining > 0) {
    warning(paste("not every missing value had a covering rule.", remaining, "remaining"))
    if (ari_control$use_default_classes) {
      print(paste("imputing default classes"))
      default_classes <- if (length(var_names) > 1) {
        sapply(sapply(dt[, var_names], table), which.max)
      } else {
        names(which.max(table(dt[, var_names])))
      }

      names(default_classes) <- var_names
      for (v in var_names) {
        dt[[v]] <- ifelse(is.na(dt[[v]]), default_classes[v], dt[[v]])
      }
    }
  }
  return(dt) # TO DO: return the ability stats
}

#' Control parameters for AR_iter_Imputation
#'
#' Control the parameters for the AR iterative imputation routine
#'
#' @param method A character string. Allowed values are "none", "propensity"
#' @param splits A scalar numeric denoting the number of propensity groups. If not an integer will be rounded. Ignored if method = "none"
#' @param class_balance No class balancing will be applied if not set. A list of paramaters to be passed to ROSE::ovun.sample.
#' @param max_iter A scalar numeric. If not an integer will be rounded.
#' @param target_convergence A scalar numeric. If not an integer will be rounded.
#'
#' @export
iteration_control <- function(method
                         , splits
                         , max_iter
                         , target_convergence
                         , class_balance) {
  ic <- list(
    method = method
    , max_iter = max_iter
    , target_convergence = target_convergence)
  if (method == "propensity" && missing(splits)) {
    stop("method propensity requires splits to be set. integer value 2 <= x <= 5 recommended")
  }
  if (!missing(splits)) ic$splits = splits
  if (!missing(class_balance)) ic$class_balance = class_balance
  class(ic) <- c("iteration_control", "list")
  return(ic)
}


#' Iterative Imputation (better name)
#'
#' @export
ARImpute_iter <- function(dt, missing_vals
                          , ...) {
  dots <- list(...)
  if(length(dots) != 0) {
    if(!(is.null(dots$ari_control))) {
      ari_control <- dots$ari_control
      if (!("arulesimp_control" %in% class(ari_control))) {
        warning("ari_control provided was not valid. using default settings. try using arulesimp_control() to create ari_control")
        ari_control = arulesimp_control(method = "best_rule", use_default_classes = TRUE)
      }
    } else {
      ari_control = arulesimp_control(method = "best_rule", use_default_classes = TRUE)
    }
    if(!(is.null(dots$c_control))) {
      c_control <- dots$c_control
      if (!("cars_control" %in% class(c_control))) {
        warning("c_control provided was not valid. using default settings. try using cars_control() to create c_control")
        c_control = cars_control(support = 0.1, confidence = 0.1, sort_by = "confidence")
      }
    } else {
      c_control = cars_control(support = 0.1, confidence = 0.1, sort_by = "confidence")
    }
    if(!(is.null(dots$iter_control))) {
      iter_control <- dots$iter_control
      if (!("iteration_control" %in% class(iter_control))) {
        warning("iter_control provided was not valid. using default settings. try using iteration_control() to create iter_control")
        c_control = cars_control(support = 0.1, confidence = 0.1, sort_by = "confidence")
      } else { iter_control =
        iteration_control(method = "none"
        , max_iter = 10
        , target_convergence = 5 * length(missing_vals))
      }
    }
  }
  if(length(dots) == 0) {
    ari_control = arulesimp_control(method = "best_rule", use_default_classes = TRUE)
    c_control = cars_control(support = 0.1, confidence = 0.1, sort_by = "confidence")
  }

  var_names <- names(missing_vals)
  mn <- min(sapply(dt, as.numeric), na.rm = TRUE)
  mx <- max(sapply(dt, as.numeric), na.rm = TRUE)
  rows_to_impute <- find_rows_to_impute(dt)

  init_vals <- sapply(var_names, function(v) {
    probs <- numeric(length(mn:mx))
    chr_mn_mx <- as.character(mn:mx)
    names(probs) <- chr_mn_mx
    boot <- sample(dt[[v]], size = length(dt[[v]]), replace = TRUE)
    probs[chr_mn_mx] <- prop.table(table(dt[[v]]))[chr_mn_mx]
    ifelse(is.na(probs), 0, probs)
    sample(mn:mx, size = missing_vals[v], replace = TRUE)
  })

  dt_impute <- dt
  dt_temp_impute <- all_factor(dt)

  x <- capture.output(sapply(var_names, function(vv) {
    dt_impute[rows_to_impute[[vv]], vv] <<- init_vals[[vv]]
  }))

  ii <- 0
  convergence <- iter_control$target_convergence + 1
  while (ii < iter_control$max_iter &&
         mean(sqrt(convergence^2)) > iter_control$target_convergence) {
    ii <- ii + 1
    dt_temp_compare <- dt_impute
    splits <- as.matrix(1:nrow(dt), ncol = 1)
    for (v in var_names) {
      dt_impute[[v]] <- dt[[v]]
      if (iter_control$method == "propensity") {
        # flag the NA values for v
        dt_propensity <- dt_impute
        dt_propensity[[v]] <- ifelse(is.na(dt_propensity[[v]]), 1, 0)
        fmla <- as.formula(paste(v, "~ ."))
        if (!is.null(iter_control$class_balance)) {
          if (!is.null(iter_control$class_balance$N)) {
            N = iter_control$class_balance$N
          } else {
            N = nrow(dt)
          }
          if (!is.null(iter_control$class_balance$p)) {
            p = iter_control$class_balance$p
          } else {
            p = 0.5
          }
          if (!is.null(iter_control$class_balance$method)) {
            N = iter_control$class_balance$method
          } else {
            method = "both"
          }
          dt_propensity <- ROSE::ovun.sample(fmla
                                             , data=dt_propensity
                                             , N = N
                                             , p = p
                                             , method = method)$data
          }
        fit.propensity <- glm(fmla, data = dt_propensity, family = binomial)
        pred.propensity <- predict(fit.propensity, newdata = dt_impute, type = "response")
        splits.propensity <- lattice::equal.count(as.numeric(names(pred.propensity))[order(pred.propensity)]
                                                  , number = iter_control$splits
                                                  , overlap = 0)

        splits <- sapply(levels(splits.propensity), function(sh) {
          splits.propensity[ceiling(sh[1]):floor(sh[2])]
        })
      }
      for (jj in ncol(splits)) {
        if (ari_control$method == "weighted_chisq") c_control$sort_by <- "chiSquared"
        if (ari_control$method == "laplace") c_control$sort_by <- "laplace"
        cars <- make_cars(all_factor(dt_impute[splits[, jj], ])
                          , c_control = c_control
                          , var_names = v)

        dt_temp_impute[splits[, jj], v] <- ARImpute(cars
                                          , dt = all_factor(dt_impute[splits[, jj], ])
                                          , var_names = v
                                          , ari_control = ari_control)

        dt_impute[splits[, jj], v] <- as.numeric(as.character(dt_temp_impute[[v]][splits[, jj]]))
        print(paste("processed", v, "split", jj))
      }
      convergence <- sapply(var_names, function(v) {
        sum(dt_impute[[v]] - dt_temp_compare[[v]])
      })
      print(paste("imputed", v, "iteration", ii))
    }
    print(convergence)
    print(mean(sqrt(convergence^2)))
  }
  return(dt_impute)
}
