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
#' @param unobs_cols A character vector containing names of covariates that will not be included in the output.
#' @param beta_0 A numeric scalar for the intercept term in carpita models
#' @param betas A numeric vector of length = length(dep_cols) of coefficients for covariates to affect the missingness. Recommded values -3 < x < 3.
#' @param prob A numeric scalar with value 0 < prob < 1. mutually exclusive with exact. specifies the proportion of included variables that will contain missingness.
#' @param exact An integer scalar with value > 0 and < maximum number of data points subject to missingness. specifies an exact number of data points to substitue. mutually exclusive with prob.
#'
#' @export
missing_control <- function(pattern
                            , method
                            , nr_cols
                            , dep_cols
                            , unobs_cols
                            , beta_0
                            , betas
                            , prob
                            , exact) {
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
    ) stop("method for MAR must be one of the following: princomp, wu_ranking, carpita")
  # check dep_cols
  if (pattern %in% c("MAR", "MNAR") &&
    missing(dep_cols)) stop(paste("dependent variables must be specified for method", method))
  # check nr_cols and dep_cols don't overlap for MAR
  if (pattern == "MAR" &&
      !missing(nr_cols) &&
      !is.na(match(dep_cols, nr_cols))) stop("dependent variables cannot be subject to missingness when pattern is MAR")
  if (pattern == "MAR" && !missing(unobs_cols)) stop("there should be no unobserved variables for pattern MAR")
  # check nr_cols, dep_cols and unobs_cols work for MNAR
  if (pattern == "MNAR") {
    if (missing(nr_cols) &&
        !missing(unobs_cols) &&
        any(is.na(match(unobs_cols, dep_cols)))) stop("some variables subject to missingness are also set to unobserved. it doesn't make sense")
    if (missing(unobs_cols) &&
        !missing(nr_cols) &&
        all(is.na(match(dep_cols, nr_cols)))) stop("at least one covariate must either be subject to missingness or be unobserved")
    if (!missing(unobs_cols) &&
        !missing(nr_cols)) {
          if (any(!is.na(match(unobs_cols, nr_cols)))) stop("some variables subject to missingness are also set to unobserved. it doesn't make sense")
          if (all(is.na(match(unobs_cols, dep_cols)))) warning("none of the unobserved variables were used as covariates")
        }
  }
  # check betas
  if (!missing(beta_0) &&
      (!(class(beta_0) %in% c("numeric", "integer")) ||
      length(beta_0) > 1)) stop("beta_0 (intercept) should be a scalar numeric")
  if (pattern %in% c("MAR", "MNAR") &&
      !(missing(betas))) {
        if (length(betas) != length(dep_cols)) stop("provided betas length does not match dep_cols.")
        if (!(is.numeric(betas) || is.integer(betas))) stop("provided betas must be numeric vector")
  if (any(abs(c(beta_0, betas)) > 3)) warning("coefficients with an absolute value greater than 3 tend to overwhelm the probability generating steps")
  }
  # check prob or exact
  if ((missing(prob) && missing(exact)) ||
      (!missing(prob) && !missing(exact))) stop("provide either a prob between 0 and 1 or an exact count > 0 and < the total number of data points subject to missingness but not both")
  if (!missing(prob) &&
      (!(class(prob) %in% c("numeric", "integer")) ||
      prob < 0 || prob > 1 || length(prob) > 1)) stop("provide a scalar numeric prob between 0 and 1")
  if (!missing(exact) &&
      (!(class(exact) != "integer") ||
       exact < 1 || length(prob) > 1)) stop("provide a scalar integer exact > 0 and < number of data points subject to missingness")
  # construct
  mc <- list(pattern = pattern)
  class(mc) <- "missyn_control"
  if (!missing(method)) mc$method <- method
  if (!missing(nr_cols)) mc$nr_cols <- nr_cols
  if (!missing(dep_cols)) mc$dep_cols <- dep_cols
  if (!missing(unobs_cols)) mc$unobs_cols <- unobs_cols
  if (!missing(beta_0)) mc$beta_0 <- beta_0
  if (!missing(betas)) mc$betas <- betas
  if (!missing(prob)) mc$prob <- prob
  if (!missing(exact)) mc$exact <- exact
  return(mc)
}

#' Synthesise missing data patterns
#'
#' @export
synth_missing <- function(dt
  , syn_control = missing_control(pattern = "MCAR"
                                  , prob = 0.2)
  , plot_probs = FALSE
  , deps_in_mim = FALSE) {
  if (!(class(syn_control) == "missyn_control")) stop("please provide control paramaters with missing_control function")
  if (is.null(syn_control$dep_cols)) syn_control$nr_cols <- names(dt)
  if (is.null(syn_control$nr_cols)) {
    syn_control$nr_cols <- names(dt)[!(names(dt) %in% syn_control$dep_cols)]
    warning(paste("no variables specified for non-response. using all variables not specified as covariates:"
                  , paste(syn_control$nr_cols, collapse = ", "))
            , "")
  }
  dims <- c(rows = nrow(dt)
            , cols = if (is.null(syn_control$nr_cols)) {
              ncol(dt) - if (is.null(syn_control$dep_cols)) {
                  0
                } else { length(syn_control$dep_cols)  }
              } else {
                length(syn_control$nr_cols)
              })
  n <- dims["rows"] * dims["cols"]
  # determine the number of data points to be set missing
  nonresp_n <- if (!is.null(syn_control$prob)) {
    round(n * syn_control$prob)
  } else {
    syn_control$exact
  }

  if (syn_control$pattern == "MCAR") {
    nonresp_vector <- c(rep(1, nonresp_n), rep(0, n - nonresp_n))
    nonresp_vector <- sample(nonresp_vector)

    nonresp <- matrix(nonresp_vector
                      , ncol = dims["cols"]
                      , nrow = dims["rows"])
  }
  if (syn_control$pattern %in% c("MAR", "MNAR")) {
    if (syn_control$method == "princomp") {
      if (!(all(sapply(dt[, syn_control$dep_cols], class) %in%
                c("numeric", "integer")))) {
        warning("some dep_cols are non-numeric. attempting to convert for princomp")
        dt[, syn_control$dep_cols] <- sapply(dt[, syn_control$dep_cols], function(x) {
          as.numeric(as.vector(x))
        })
      }
      # creates the char string of depend vars for fmla
      fmla <- sub("\\+$"
                  , ""
                  , paste(syn_control$dep_cols, "+"
                          , collapse = " "))
      # use first principle component of combined dep cols
      fmla <- as.formula(paste("~", fmla))
      pr <- princomp(fmla, data = dt)
      raw_prob <- exp(pr$scores[, 1]) /
        (1 + exp(pr$scores[, 1]))
    }
    if (syn_control$method == "wu_ranking") {
      if (sum(is.na(dt[, syn_control$dep_cols])) > 0) stop("covariates for method wu_ranking must not contain NA values")
      raw_prob <- if (length(syn_control$dep_cols) == 1) {
        rank(dt[, syn_control$dep_cols])
      } else { rank(rowSums(dt[
        , syn_control$dep_cols]))
        } /
        nrow(dt)
    }
    if (syn_control$method == "carpita") {
      if (sum(is.na(dt[, syn_control$dep_cols])) > 0) stop("covariates for method carpita must not contain NA vallues")
      dep_cols <- if (length(syn_control$dep_cols) == 1) {
        as.vector(as.vector(scale(dt[, syn_control$dep_cols])))
      } else {
        sapply(dt[, syn_control$dep_cols], scale)
      }
      if (is.null(syn_control$beta_0)) {
        syn_control$beta_0 <- 0
        warning("no beta_0 (intercept) provided. setting beta_0 = 0")
      }
      if (is.null(syn_control$betas)) {
        syn_control$betas <- rep(1, length(syn_control$dep_cols))
        warning("no beta coefficients provided. setting all coefficients = 1")
      }
      carpita_model <- cbind(1, dep_cols) %*% c(syn_control$beta_0, syn_control$betas)
      raw_prob <- exp(carpita_model) /
                (1 + exp(carpita_model))
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
  # go through columns and set missing where indicated
  for (i in 1:dims["cols"]) {
      dt[nonresp[, i] == 1, syn_control$nr_cols[i]] <- NA
    }
  # remove any unobs cols for MNAR
  if (syn_control$pattern == "MNAR" &&
      is.null(syn_control$unobs_cols)) {
    syn_control$unobs_cols <- syn_control$dep_cols
    warning("no unobserved variables provided for pattern MNAR. removing all covariates")
  }
  if (!is.null(syn_control$unobs_cols)) {
    for (uc in syn_control$unobs_cols) {
      dt[[uc]] <- NULL
    }
  }
  # names of cols after unobserved are removed
  col_names <- names(dt)
  result <- list(
    data = dt
    , syn_control = syn_control
    , mim = if (deps_in_mim) {
      missing_matrix(dt) } else {
        missing_matrix(dt[, setdiff(col_names
                                    , syn_control$dep_cols)])
      }
    , mi_probs = raw_prob
  )
  if (plot_probs) {
    if (syn_control$pattern == "MCAR") {
      warning("prob for MCAR is uniform. plot an image of the missing indicator matrix instead")
    } else {
      result$dplot <-
        lattice::densityplot(~raw_prob
                 , xlab = paste("Probability of missingness\nassigned by"
                              , syn_control$pattern
                              , syn_control$method))
    }
  }
  return(result)
}
