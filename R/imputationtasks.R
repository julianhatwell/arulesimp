# imputation routines

#' Find rows to impute
#'
#' Find the rows with missing values to impute for each target variable
#'
#' @param df A data frame
#' @param which_cols A vector of variable names to search.
#' @export
find_rows_to_impute <- function(df, which_cols) {
  rows_to_impute <- list()
  for (var in which_cols) {
    rows_to_impute[[var]] <- which(is.na(df[[var]]))
  }
  rows_to_impute
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
