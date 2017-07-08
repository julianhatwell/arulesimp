# imputation routines

#' Find rows to impute
#'
#' Find the rows with missing values to impute for each target variable
#'
#' @param dt A data frame or an object of class mim
#' @param which_cols A vector of variable names to search.
#' @export
find_rows_to_impute <- function(dt, which_cols) {
  if (missing(which_cols)) which_cols <-
      if (class(dt) == "mim") {
        dimnames(dt$mim)[[2]]
      } else {
          names(dt)
        }
  if (class(which_cols) %in% c("numeric", "integer")) which_cols <- dt[, round(which_cols)]
  rows_to_impute <- list()
  for (var in which_cols) {
    rows_to_impute[[var]] <-
      if (class(dt) == "mim") {
        dt$B_comp_j[[var]]
      } else {
        which(is.na(dt[[var]]))
        }
  }
  return(rows_to_impute)
}

#' Find rows to impute
#'
#' Find the columns (variables) with missing values to impute for each target row
#'
#' @param dt A data frame or an object of class mim
#' @param which_rows A integer vector of row numbers to search.
#' @export
find_cols_to_impute <- function(dt, which_rows) {
  if (missing(which_rows)) which_rows <-
      if (class(dt) == "mim") {
        1:nrow(dt$mim)
        } else {
          1:nrow(dt)
        }
  cols_to_impute <- list()
  for (rw in which_rows) {
    cols_to_impute[[rw]] <-
      if (class(dt) == "mim") {
        dt$A_comp_i[[rw]]
      } else {
        which(is.na(dt[rw, ]))
      }
  }
  return(cols_to_impute)
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


#' ARImputation
#'
#' Association Rules-based Imputation
#'
#' @export
ARImpute <- function(df, var_names, rows_to_impute, cars) {
  # once for each set of cars i.e. each target variable
  for (var in var_names) {
    # another loop here over each row to impute
    for (k in seq_along(rows_to_impute[[var]])) {
      # initialize for the antecedent outer loop
      all_match <- FALSE
      num_rules <- length(cars[[var]]$antecedent)
      i <- 0
      print(paste("initialised k loop", var, i, j, k))
      # start the antecedent outer loop
      while (!(all_match) &
             i < num_rules) {
        # re-initialize for the antecedent inner loop
        i <- i + 1
        all_match <- TRUE
        num_conditions <- nrow(cars[[var]]$antecedent[[i]])
        j <- 0
        print(paste("initialised antecedent i loop", var, i, j, k, cars[[var]]$antecedent[[i]]))
        while (all_match &
               j < num_conditions) {
          j <- j + 1
          # the empty set
          if (is.na(cars[[var]]$antecedent[[i]][j, "value"])) {
            all_match <- TRUE # empty set match
            break
          }
          # otherwise test all conditions
          print(paste("in conditions j loop", var, i, j, k, cars[[var]]$antecedent[[i]][j, "key"], cars[[var]]$antecedent[[i]][j, "value"]))
          data_value <- as.character(responses[[rows_to_impute[[var]][[k]], as.character(cars[[var]]$antecedent[[i]][j, "key"])]])
          condition_value <- as.character(cars[[var]]$antecedent[[i]][j, "value"])
          all_match <- identical(data_value
                                 , condition_value)
        }
        if (all_match) break
      }
      print(paste("closing", var, i, j, k, cars[[var]]$antecedent[[i]][j, "key"], cars[[var]]$antecedent[[i]][j, "value"]))
      df[[rows_to_impute[[var]][[k]], var]] <- cars[[var]]$antecedent[[i]][j, "value"]
    }
  }
  return(df)
}
