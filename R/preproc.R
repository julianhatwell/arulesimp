# clean up and preproc functions

#' Replace character strings with NA.
#'
#' @description Useful for data with empty strings (default) or other character values to indicate missing
#' @param dt A data frame
#' @param char_string A character string (default = "")
#' @return A copy of the input with all target strings replaced by NA
#' @examples
#' char_to_na(data.frame(x = c("foo", ""), b = c("", "bar")))
#' char_to_na(data.frame(x = c("foo", "_"), b = c("_", "bar")), char_string = "_")
#' @export
char_to_na <- function(dt
                       , char_string = "") {
  cla <- sapply(dt, class)
  fac <- names(cla[cla == "factor"])
  cha <- names(cla[cla == "character"])
  faccha <- c(fac, cha)

  for (f in faccha) {
    dt[, f] <- ifelse(dt[, f] == char_string
                      , NA
                      , as.character(dt[, f]))
  }
  for (f in fac) {
    dt[, f] <- factor(dt[, f])
  }
  return(dt)
}

#' Convert all variables to factors.
#'
#' @description arules wants only logical or factor. This function converts everything it finds into a factor
#' @param dt A data frame
#' @param refactor Logical. Do you want it to also work on variables that are already factors? (Default is FALSE)
#' @param ord A vector of Logical indicating which columns of dt should be converted to ordered factors. Will be recycled as necessary.
#' @return A copy of the input where all variables except logicals have been converted to factors
#' @examples
#' str(all_factor(data.frame(x = c("foo", "bar"), y = c(10, 20))))
#' @export
all_factor <- function(dt
                       , refactor = FALSE
                       , ord = FALSE) {
  # refactor will apply to all vars
  # regardless if already a factor
  if (refactor) { vars <- names(dt)
  } else {
    cla <- sapply(dt, class)
    vars <- names(cla[!(cla %in% c("factor", "logical"))])
    lenv <- length(vars)
    if (lenv < 1) {
      print("No variables to convert")
      return()
    }
    leno <- length(ord)

    # make sure the ord object is the right length
    # don't want to raise any errors
    o_in_v <- floor(lenv / leno)
    o_remain <- lenv %% leno
    if (o_remain > 0) {
      warning("In cbind(vars, ord) :
  number of rows of result is not a multiple of vector length (arg 2)")
      ord <- c(rep(ord, o_in_v), ord[1:(lenv %% leno)])
      }

    # now can bind them
    vars_ord <- data.frame(vars, ord
                           , stringsAsFactors = FALSE)
  }
  for (i in 1:nrow(vars_ord)) {
    dt[, vars_ord$vars[i]] <- factor(dt[, vars_ord$vars[i]]
                                     , ordered = vars_ord$ord[i])
  }
  return(dt)
}

#' Find the variables that contain any missing (NA) values.
#'
#' @param dt A data frame
#' @param sorted A logical vector. Do you want to sort the results (Default = TRUE)
#' @return A named numeric. Names are the variable names from the input dt. Values are the count of NA found in that variable
#' @examples
#' missing_values(data.frame(x = c("foo", NA), y = c(NA, "bar")))
#' @export
missing_values <- function(dt
                           , sorted = TRUE) {
  mv <- sapply(dt, function(x) {
    sum(is.na(x))
  })
  mv <- mv[which(mv > 0)]
  if (sorted) mv <- sort(mv)
  return(mv)
}

#' Create a list of unique values in a dataset
#'
#' @description Useful for searching for rules
#'
#' @param dt A data frame
#' @param which_cols A vector of variable names to search.
#' @return A named list. Names are the input variable names.
#' List values are vectors of unique values from the input dt.
#' @examples
#' missing_values(data.frame(x = c("foo", NA), y = c(NA, "bar")))
#' @export
unique_values <- function(dt, which_cols) {
  uv <- lapply(which_cols, function(wc) {
    unique(dt[[wc]])
  })
  names(uv) <- which_cols
  return(uv)
}

#' Missing Indicator Matrix Statistics
#'
#' @description Counts and means of missing indicator matrix
#'
#' @param dt A data frame
#' @param one_as_missing A scalar boolean. If true, 1 represents missing and 0 represents observed data. Otherwise the reverse true.
#' @return A list of ...
#' @examples
#' mims(matrix(c(1, rep(0, 7), 1), ncol = 3))
#' @export
mimstats <- function(dt, one_as_missing = TRUE) {

  # count whatever has one_as_missing value
  is <- abs(rowSums(dt) - ncol(dt) * !(one_as_missing))
  js <- abs(colSums(dt) - nrow(dt) * !(one_as_missing))

  # collect all those with no missing
  i_none_missing <- which(is == 0)
  j_none_missing <- which(js == 0)


  which_missing <- function(x) {
    which(x == one_as_missing * 1)
  }
  which_not_missing <- function(x) {
    which(x != one_as_missing * 1)
  }

  missing_for_i <- apply(dt, 1
                         , which_missing)
  missing_for_j <- apply(dt, 2
                         , which_missing)
  not_missing_for_i <- apply(dt, 1
                         , which_not_missing)
  not_missing_for_j <- apply(dt, 2
                         , which_not_missing)

  return(list(i_none_missing = i_none_missing
              , j_none_missing = j_none_missing
              , A_i = not_missing_for_i
              , B_j = not_missing_for_j
              , A_comp_i = missing_for_i
              , B_comp_j = missing_for_j))
}

#' Create a missing indicator matrix for a dataset
#'
#' @description Converts a data frame into a missing indicator matrix
#'
#' @param dt A data frame
#' @param one_as_missing A scalar boolean. If true, 1 represents missing and 0 represents observed data. Otherwise the reverse true.
#' @return A list containing matrix of 1 and 0 having the same dimensions as the input and Carpita's measures.
#' @examples
#' missing_matrix(data.frame(x = c("foo", NA), y = c(NA, "bar")))
#' @export
missing_matrix <- function(dt, one_as_missing = TRUE) {
  mm <- is.na(dt)
  if (!(one_as_missing)) {
    mm <- !(mm)
  }
  mm <- 1 * mm
  mims <- mimstats(mm)
  mims$mim <- mm
  class(mims) <- "mim"
  return(mims)
}
