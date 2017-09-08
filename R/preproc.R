# clean up and preproc functions

# could do with a routine check for data frames all numeric, numeric matrix, likert type (e.g. all same range)

#' Check for any missing values
#'
#' This utility function is a simple, routine check for presence of missing data. Used internally, not exported.
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
#' This utility function is used to clean non-standard missing values.
#' Converts the supplied char_string (default = "") to \code{NA}.
#'
#' @param dt A vector, matrix or data frame.
#' @param char_string A character string. Default = "".
#' @return A copy of the input with all target strings replaced by NA.
#' @examples
#' char_to_na(c("foo", "", "bar"))
#' char_to_na(matrix(c("foo", "", "bar")))
#' char_to_na(data.frame(x = c("foo", ""), b = c("", "bar")))
#' char_to_na(data.frame(x = c("foo", "_"), b = c("_", "bar")), char_string = "_")
#' @export
char_to_na <- function(dt
                       , char_string = "") {
  if (class(dt) == "data.frame") {

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
  } else {
    dt <- ifelse(dt == char_string
                      , NA
                      , as.character(dt))
  }
  return(dt)
}

#' Convert all variables to factors.
#'
#' This convenience function converts all columns of the supplied data frame
#'   into a factors. Existing factors can be ignored (default) or set again
#'   which would result in losing any special order or labels.
#'
#' @param dt A data frame.
#' @param refactor A logical scalar value. Do you want it to also work on variables
#'   that are already factors? Default = \code{FALSE}.
#' @param ord A logical vector, indicating which columns of dt should be
#'   converted to ordered factors. Will be recycled as necessary.
#' @return A copy of the input where all variables
#'   except logicals have been converted to factors.
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

#' Find missing values
#'
#' This utility function finds and counts any \code{NA} values
#'   in each variable of a data frame.
#'
#' @param dt A vector, matrix, data frame or S3 object of \code{mim} class.
#' @param sorted A logical scalar value. Do you want to sort the results?
#'   Default = \code{TRUE}.
#' @return A named numeric. Names are the column names if the
#'   input is a data frame, column numbers for a matrix or
#'   index positions if a vector.
#'   Values are the count of \code{NA} found in that variable
#' @examples
#' missing_values(c(1, 1, 1, NA, NA, 1))
#' missing_values(matrix(c(1, 1, NA, NA), ncol = 2))
#' missing_values(data.frame(x = c("foo", NA, NA), y = c(NA, "bar", "bar")))
#' missing_values(data.frame(x = c("foo", NA, NA), y = c(NA, "bar", "bar")), sorted = FALSE)
#' @seealso \code{\link{missing_matrix}}
#' @export
missing_values <- function(dt
                           , sorted = TRUE) {
  if ("mim" %in% class(dt)) {
    mv <- colSums(dt$mim)
  } else {
    if (class(dt) != "matrix") {
      mv <- sapply(dt, function(x) {
        sum(is.na(x))
      })
      if (is.null(names(mv))) names(mv) <- as.character(seq_along(mv))
    } else {
      mv <- colSums(is.na(dt))
      names(mv) <- as.character(seq_along(mv))
    }
  }
  mv <- mv[which(mv > 0)]
  if (sorted) mv <- sort(mv)
  return(mv)
}

#' Create a missing indicator information
#'
#' Converts a data frame into a mim object. This is a list
#'   of vectors showing the positions of missing data by rows and columns
#'   and a missing indicator matrix
#'
#' @param dt A data frame
#' @param one_as_missing A scalar boolean. If true, 1 represents missing and 0 represents observed data. Otherwise the reverse true.
#' @return A list containing vectors showing the position of missing data
#'   by rows and columns and a matrix of 1 and 0 having the same dimensions as the input.
#' @examples
#' missing_matrix(data.frame(x = c("foo", NA), y = c(NA, "bar")))
#' @export
missing_matrix <- function(dt, one_as_missing = TRUE) {
  if (class(dt) != "data.frame") stop("dt should be a data frame with some missing data")
  # function to generate Missing Indicator Matrix Statistics
  mimstats <- function(dt) {
    # count whatever has one_as_missing value
    is <- abs(rowSums(dt))
    js <- abs(colSums(dt))

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

  # generate mim object
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

#' Convert normal data to item scale
#'
#' Simulate Likert scales and ordinal data by discretizing normally distributed
#'   data based on desired distribution and number of levels.
#'
#' @param dt A data frame.
#' @param dist A character string. Allowed values are "symmetric"
#'   "moderate_asym" and "severe_asym" to select the desired distribution.
#'   Default = "symmetric".
#' @param levs A scalar integer to set the number of item levels.
#'   Allowed values are 2, 3, 5 or 7. Default = 5
#' @param labels A scalar boolean. Passed to the cut function.
#'   Determines whether labels or underlying factor keys are returned.
#'   Default = \code{FALSE}.
#' @examples
#' cut_rhemtulla(rnorm(50), levs = 7)
#' cut_rhemtulla(rnorm(50), levs = 7, dist = "severe_asym")
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

#' Convert continuous data to item scale
#'
#' Simulate Likert scales and ordinal data by discretizing continuous data
#'   making equal cuts by distance or count.
#'
#' @param dt A data frame.
#' @param dist A character string. Allowed values are "equal_distance"
#'   and "equal_count". Default = "equal_distance".
#' @param levs A scalar integer to set the number of item levels.
#'   Default = 5.
#' @param labels A scalar boolean. Passed to the cut function.
#'   Determines whether labels or underlying factor keys are returned.
#'   Default = \code{FALSE}.
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

#' Ordinal to grouped binary representation
#'
#' Expand ordinal variables to binary variables representing groups of adjacent values
#'
#' @export
ord_grp_combine <- function(dt, var_names
                           , mn, mx
                           , stride = 1
                           , keep_orig = TRUE) {
  if (class(dt) != "data.frame") stop("provide a data frame")
  if (any(!(sapply(dt, class) %in% c("numeric", "integer")))) stop("all variables must be numeric, ideally integer")
  if (any(sapply(dt, class) == "numeric")) {
    warning("non-integer types were found. they will be rounded and converted")
    temp_names <- names(dt)
    dt <- data.frame(sapply(dt, as.integer))
    names(dt) <- temp_names
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
    step <- step + stride
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
  if (keep_orig) combined <- cbind(dt, combined)
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
    res <- ifelse(is.nan(res), NA, res)
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
