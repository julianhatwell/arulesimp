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

  # for each i, mean of the j_means for each observed j
  if (sum(is.na(dt)) == 0) {
    ij_means <- rep(mean(j_means), nrow(dt))
  } else {
    dt_mim <- missing_matrix(dt)
    ij_means <- lapply(1:nrow(dt), function(x) {
      mean(j_means[dt_mim$A_i[[x]]])
      })
    }
  return(list(i_means = i_means
              , j_means = j_means
              , ij_means = ij_means))
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
PMImpute <- function(dt, rounding = "nd") {
  dt_mim <- missing_matrix(dt)
  ij <- ij_means(dt)
  cols_to_impute <- find_cols_to_impute(dt)
  for (i in 1:nrow(dt)) {
    dt[i, cols_to_impute[[i]]] <- ij$i_means[i]
  }
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
