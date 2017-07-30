# clean up and preproc functions

# could do with a routine check for data frames all numeric, numeric matrix, likert type (e.g. all same range)

#' Routine check for presence of missing data
#'
no_missing_check <- function(dt) {
  if (sum(is.na(dt)) == 0) {
    warning("input has no missing data. no mim to return")
    return(TRUE)
  } else {
    return(FALSE)
  }
}


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
#' @description Convenience function to converts everything it finds into a factor
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
  if (class(dt) == "mim") {
    mv <- colSums(dt$mim)
  } else {
    mv <- sapply(dt, function(x) {
      sum(is.na(x))
    })
  }
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
  if(no_missing_check(dt)) return()
  if (!(one_as_missing)) {
    mm <- !(mm)
  }
  mm <- 1 * mm
  mims <- mimstats(mm)
  mims$mim <- mm
  class(mims) <- c("mim", "list")
  return(mims)
}

# Discretizing functions
create_rhemtulla_cutpoints <- function() {
  if(!(exists("ari.env")))
    ari.env <<- new.env(parent = emptyenv())

  ari.env$rhemtulla_thresholds <- list(
    symmetric = list()
    , moderate_asym = list()
    , severe_asym = list()
  )

  ari.env$rhemtulla_thresholds$symmetric[[2]] <- 0.00
  ari.env$rhemtulla_thresholds$symmetric[[3]] <- c(-.83, .83)
  ari.env$rhemtulla_thresholds$symmetric[[5]] <- c(-1.50, -.50, .50, 1.50)
  ari.env$rhemtulla_thresholds$symmetric[[7]] <- c(-1.79, -1.07, -.36, .36, 1.07, 1.79)
  ari.env$rhemtulla_thresholds$moderate_asym[[2]] <- .36
  ari.env$rhemtulla_thresholds$moderate_asym[[3]] <- c(-.50, .76)
  ari.env$rhemtulla_thresholds$moderate_asym[[5]] <- c(-.70, .39, 1.16, 2.05)
  ari.env$rhemtulla_thresholds$moderate_asym[[7]] <- c(-1.43, -.43, .38, .94, 1.44, 2.54)
  ari.env$rhemtulla_thresholds$severe_asym[[2]] <- 1.04
  ari.env$rhemtulla_thresholds$severe_asym[[3]] <- c(.58, 1.13)
  ari.env$rhemtulla_thresholds$severe_asym[[5]] <- c(.05, .44, .84, 1.34)
  ari.env$rhemtulla_thresholds$severe_asym[[7]]<- c(-.25, .13, .47, .81, 1.18, 1.64)
}

remove_rhemtulla_cutpoints <- function() {
  if (exists("ari.env")) rm(ari.env, envir = .GlobalEnv)
}

#' Normal to Item Scale
#'
#' Discretize normally distributed data to ordinal scales based on desired distribution and number of levels
#'
#' @export
cut_rhemtulla <- function(dt, dist = "symmetric"
                          , levs = 5, labels = FALSE) {
  if (!(dist %in% c("symmetric", "moderate_asym", "severe_asym"))) stop("dist must be a character scalar or vector. only \"symmetric\", \"moderate_asym\" and \"severe_asym\" are allowed")
  if (!(all(levs %in% c(2, 3, 5, 7)))) stop("levs must be an integer scalar or vector. only 2, 3, 5, and 7 are allowed")
  if (!(all(sapply(dt, class) == "numeric"))) stop("only real numeric values allowed. integers won't be processed")
  if (length(dist) != 1 || length(levs) != 1) stop("dist and levs must be scalar of length = 1")

  on.exit(remove_rhemtulla_cutpoints())
  create_rhemtulla_cutpoints()
  if (class(dt) == "data.frame") {
    return(data.frame(lapply(dt, function(x) {
      cut(x, breaks=c(-Inf
                      , ari.env$rhemtulla_thresholds[[dist]][[levs]]
                      , Inf)
          , labels=labels)
    })))
  } else {
    return(cut(dt, breaks=c(-Inf
                            , ari.env$rhemtulla_thresholds[[dist]][[levs]]
                            , Inf)
               , labels=labels))
  }
}

#' Equal cuts to item scale
#'
#' COnvenience function for generating discrete data from continuous
#'
#' @export
cut_equal <- function(dt, dist = "equal_distance"
                      , levs = 5, labels = FALSE) {

  return(data.frame(lapply(dt, function(x) {

    cutpoints <- if (dist == "equal_count") {
      coint <- co.intervals(x, number = levs, overlap = 0)
      c(coint[, 1], coint[levs, 2])
    } else {
      levs
    }
    cut(x, breaks = cutpoints, labels = labels)
  })))
}

#' Ordinal to cumulative binary representation
#'
#' Expand ordinal variables to a group of binary variables representing cumulative values
#'
#' @export
ord_cum_expand <- function(dt, var_names
                           , mn, mx
                           , keep_orig = FALSE) {
  if (class(dt) != "data.frame") stop("provide a data frame")
  if (any(!(sapply(dt, class) %in% c("numeric", "integer")))) stop("all variables must be numeric, ideally integer")
  if (any(sapply(dt, class) == "numeric")) {
    warning("non-integer values were found. they will be rounded and converted")
    dt <- sapply(dt, as.integer)
  }
  if (missing(var_names)) var_names <- names(dt)

  if(missing(mn)) mn <- min(dt, na.rm = TRUE)
  if(missing(mx)) mx <- max(dt, na.rm = TRUE)

  lts_seq <- (mn + 1):mx
  gts_seq <- mn:(mx - 1)

  expanded <- lapply(var_names, function(v) {
    lts <- sapply(lts_seq, function(i) {
      ifelse(dt[[v]] < i, 1, 0)
    })

    gts <- sapply(gts_seq, function(i) {
      ifelse(dt[[v]] > i, 1, 0)
    })
    return(cbind(lts, gts))
  })

  names(expanded) <- var_names
  for (v in var_names) {
    colnames(expanded[[v]]) <- c(paste0(v, "_lt_", lts_seq)
                                 , paste0(v, "_gt_", gts_seq))
  }
  names(expanded) <- NULL
  expanded <- as.data.frame(expanded)
  if (keep_orig) expanded <- cbind(dt, expanded)
  return(expanded)
}

#' Ordinal to cumulative binary representation
#'
#' Expand ordinal variables to a group of binary variables representing cumulative values
#'
#' @export
ord_cum_combine <- function(dt, var_names
                           , mn, mx
                           , stride = 1
                           , keep_orig = FALSE) {
  if (class(dt) != "data.frame") stop("provide a data frame")
  if (any(!(sapply(dt, class) %in% c("numeric", "integer")))) stop("all variables must be numeric, ideally integer")
  if (any(sapply(dt, class) == "numeric")) {
    warning("non-integer values were found. they will be rounded and converted")
    dt <- sapply(dt, as.integer)
  }
  if (missing(var_names)) var_names <- names(dt)
  if (!(class(stride) %in% c("numeric", "integer"))) stop("stride must be scalar numeric, ideally integer")
  if (class(stride) == "numeric" &&
      round(stride) != stride) {
    warning("non-integer value for stride will be rounded")
    stride <- as.integer(round(stride))
  }

  if(missing(mn)) mn <- min(dt, na.rm = TRUE)
  if(missing(mx)) mx <- max(dt, na.rm = TRUE)

  step <- mn
  sequences <- list()
  while (step + stride <= mx) {
    sequences <- append(sequences
                , list(step:(step + stride)))
    step <- step + 1
  }

  combined <- lapply(var_names, function(v) {
    combs <- sapply(sequences, function(s) {
      ifelse(dt[[v]] %in% s, 1, 0)
    })
  })

  names(combined) <- var_names
  for (v in var_names) {
    colnames(combined[[v]]) <- paste0(v, "_in_"
      , sapply(sequences, function(x) {
        paste(x, collapse = "_")
        }))
  }
  names(combined) <- NULL
  combined <- as.data.frame(combined)
  if (keep_orig) expanded <- cbind(dt, expanded)
  return(combined)
}

#' Rounded average over Likert Scales
#'
#' Create a new ordinal variable by averaging and rounding to nearest integer over any combination of ordinal variables.
#'
#' @export
ord_combi_expand <- function(dt, likert_scales
                             , keep_orig = TRUE) {
  if (class(dt) != "data.frame") stop("provide a data frame")
  if (any(!(sapply(dt, class) %in% c("numeric", "integer")))) stop("all variables must be numeric, ideally integer")
  if (any(sapply(dt, class) == "numeric")) {
    warning("non-integer values were found. they will be rounded and converted")
    dt <- sapply(dt, as.integer)
  }
  if (class(likert_scales) != "list" ||
      any(lapply(likert_scales, class) != "character") ||
      is.null(names(likert_scales))) stop("provide a named list of character vectors of variable names to treat together as multiple response Likert scales")
  if (!(all(unlist(likert_scales) %in% names(dt)))) stop("some variable names given in likert_scales are not in the data frame dt")

  combis <- lapply(likert_scales, function(x) {
    res <- matrix(NA, nrow = nrow(dt)
                  , ncol = length(x))
    for(i in seq_along(x)) {
      y <- setdiff(x, x[i])
      res[, i] <- round(rowMeans(dt[, y], na.rm = TRUE))
    }
    return(res)
  })

  cnames <- sapply(names(likert_scales), function(x) {
    res <- paste0(x, "_ex_", likert_scales[[x]])
  })

  names(combis) <- names(likert_scales)
  for (v in names(likert_scales)) {
    colnames(combis[[v]]) <- cnames[, v]
  }
  names(combis) <- NULL
  combis <- as.data.frame(combis)

  if (keep_orig) combis <- cbind(dt, combis)
  return(combis)
}
