#' Synthesise Likert scale data using dewinter distributions
#'
#' @export
synth_dewinter <- function(var_names
                         , dists
                         , n = 10000) {
  if (!(exists("dewinter_dist"))) data("dewinter_dist")
  dt <- list()
  for (i in seq_along(var_names)) {
    dt[[ var_names[i] ]] <- sample(5, size = n, replace = TRUE
                                   , prob = dewinter_dist[dists[i], 1:5])
  }
  return(as.data.frame(dt))
}

#' Compare a Likert item to dewinter distributions
#'
#' @export
compare_dewinter <- function(v, de_dist = "all") {
  if (!(exists("dewinter_dist"))) data("dewinter_dist")

  v <- v[!(is.na(v))]

  v_stats <- c(mean(v)
               , sd(v)
               , e1071::skewness(v)
               , e1071::kurtosis(v) + 3) # e1071 gives excess kurtosis

  if (de_dist == "all") {
    de_stats <- t(dewinter_dist[, 6:9])
    result <- v_stats - de_stats
    result <- rbind(result)
    return(result)
  } else {
    if (!(de_dist %in% rownames(dewinter_dist))) {
      stop("de_dist must be a dewinter distribution. See rownames(\"dewinter_dist\") for options")
    } else {
      de_stats <- as.numeric(dewinter_dist[de_dist, 6:9])
      result <- data.frame(v_stats, de_stats, dif_stats = v_stats - de_stats)
      rownames(result) <- c("mean", "sd", "skewness", "kurtosis")
      return(result)
    }
  }
}

#' Control parameters for synth_missing
#'
#' Control the parameters for generating patterns of missing data
#'
#' @param pattern A character string indicating whether missing data will occur in a MCAR, MAR or MNAR pattern
#' @param method Optional character string. Allowed values are "princomp", "carpita" and "wu_ranking" if pattern = MAR, and MNAR still TO DO
#' @param dep_cols A character vector containing names of columns to be used as covariates for patterns MAR and MNAR
#' @param nr_cols A character vector containing names of columns subject to missingness. If empty, all will be used.
#' @param betas A numeric vector of length = length(dep_cols) of coefficients for covariates to affect the missingness. Recommded values -3 < x < 3.
#'
#' @export
missing_control <- function(pattern
                            , method
                            , dep_cols
                            , nr_cols
                            , betas) {
  # check pattern

  if (missing(pattern) ||
      !(pattern %in% c("MCAR", "MAR", "MNAR"))) stop("pattern must be one of the following: MCAR, MAR, MNAR")
  if (pattern == "MCAR" &&
    !(all(missing(method)
   , missing(dep_cols)
   , missing(betas)))) warning("pattern is MCAR. other missing_control parameters will be ignored.")
  # check method
  if ((pattern == "MAR" && missing(method))
     || (pattern == "MAR" &&
        !(method %in% c("princomp"
                      , "wu_ranking"
                      , "carpita")))
    ) stop("method for MAR must be one of the following: princomp, princomp_exp, wu_ranking, carpita_exp")
  # check dep_cols
  if (pattern %in% c("MAR", "MNAR") &&
    missing(dep_cols)) stop(paste("dependent variables must be specified for method", method))
  # check betas
  if (pattern %in% c("MAR", "MNAR") &&
      !(missing(betas))) {
        if (length(betas) != length(dep_cols)) stop("provided betas length does not match dep_cols.")
        if (!is.numeric(betas)) stop("provided betas must be numeric vector")
  }
  # construct
  mc <- list(pattern = pattern)
  class(mc) <- "missyn_control"
  if (!missing(method)) mc$method <- method
  if (!missing(dep_cols)) mc$dep_cols <- dep_cols
  if (!missing(nr_cols)) mc$nr_cols <- nr_cols
  if (!missing(betas)) mc$betas <- betas
  return(mc)
}

#' Synthesise missing data patterns
#'
#' @export
synth_missing <- function(dt
  , syn_control = missing_control(pattern = "MCAR")
  , prob = 0.2
  , exact = TRUE
  ) {
  if (!(class(syn_control) == "missyn_control")) stop("please provide control paramaters with missing_control function")
  dims <- c(rows = nrow(dt)
            , cols = if (is.null(syn_control$nr_cols)) {
              ncol(dt) - if (is.null(syn_control$dep_cols)) {
                  0
                } else { length(syn_control$dep_cols)  }
              } else {
                length(syn_control$nr_cols)
              })
  n <- dims["rows"] * dims["cols"]
  nonresp_n <- round(n * prob)
  if (syn_control$pattern == "MCAR") {
    nonresp_vector <- c(rep(1, nonresp_n), rep(0, n - nonresp_n))
    nonresp_vector <- sample(nonresp_vector)

    nonresp <- matrix(nonresp_vector
                      , ncol = dims["cols"]
                      , nrow = dims["rows"])
  }
  if (syn_control$pattern == "MAR") {
    if (is.null(syn_control$nr_cols)) {
      syn_control$nr_cols <- names(dt)[!(names(dt) %in% syn_control$dep_cols)]
      warning(paste("no columns specified for non-response. using any columns not specified as MAR covariates:"
                    , paste(syn_control$nr_cols, collapse = ", "))
                    , "")
    }
    if (syn_control$method == "princomp") {
      if (any(sapply(dt[, syn_control$dep_cols], class) != "numeric")) warning("some dep_cols are non-numeric. attempting to convert for princomp")
      dt[, syn_control$dep_cols] <- sapply(dt[, syn_control$dep_cols], function(x) {
        as.numeric(as.vector(x))
      })
      # creates the char string of depend vars for fmla
      dep_cols <- sub("\\+$"
                      , ""
                      , paste(syn_control$dep_cols, "+"
                              , collapse = " "))
      # use first principle component of combined dep cols
      fmla <- as.formula(paste("~", dep_cols))
      pr <- princomp(fmla, data = dt)
      raw_prob <- exp(pr$scores[, 1]) /
          (1 + exp(pr$scores[, 1]))
    }
    if (syn_control$method == "wu_ranking") {
      if (sum(is.na(dt[, dep_cols])) > 0) stop("covariates for method wu_ranking must not contain NA values")
      raw_prob <- rank(rowSums(dt[, syn_control$dep_cols])) /
        nrow(dt)
    }
    # TO DO betas
    if (syn_control$method == "carpita") {
      if (sum(is.na(dt[, syn_control$dep_cols])) > 0) stop("covariates for method carpita must not contain NA vallues")
      dep_cols <- sapply(dt[, syn_control$dep_cols], scale)
      carpita_coefs <- rowSums(dep_cols)
      raw_prob <- exp(carpita_coefs) /
                (1 + exp(carpita_coefs))
    }
    nonresp_vector <- rep(0, n)
    probs <- rep(raw_prob, dims["cols"])
    chain <- sample(1:n)
    i <- 0
    while (sum(nonresp_vector) < nonresp_n) {
      if (i > length(chain)) i <- 0
      chain <- sample((1:n)[nonresp_vector == 0])
      i <- i + 1
      thresh <- runif(1)
      if (probs[chain[i]] > thresh) nonresp_vector[chain[i]] <- 1
      }
    nonresp <- matrix(nonresp_vector
                      , ncol = dims["cols"]
                      , nrow = dims["rows"])
  }
  # TO DO MNAR here

  dt
  if (is.null(syn_control$nr_cols)) {
    dt[nonresp == 1] <- NA
  } else {
    dt[nonresp == 1, syn_control$nr_cols] <- NA
  }
  return(dt)
}

# testing
# MNAR
