#' Synthesise Likert scale data using Plumpton distributions
#'
#' @export
synth_plumpton <- function(var_names
                         , dists
                         , n = 10000) {
  if (!(exists("plumpton_dist"))) data("plumpton_dist")
  dt <- list()
  for (i in seq_along(var_names)) {
    dt[[ var_names[i] ]] <- sample(5, size = n, replace = TRUE
                                   , prob = plumpton_dist[dists[i], 1:5])
  }
  return(as.data.frame(dt))
}

#' Compare a Likert item to Plumpton distributions
#'
#' @export
compare_plumpton <- function(v, pl_dist = "all") {
  if (!(exists("plumpton_dist"))) data("plumpton_dist")

  v <- v[!(is.na(v))]

  v_stats <- c(mean(v)
               , sd(v)
               , e1071::skewness(v)
               , e1071::kurtosis(v) + 3) # e1071 gives excess kurtosis

  if (pl_dist == "all") {
    pl_stats <- t(plumpton_dist[, 6:9])
    result <- v_stats - pl_stats
    result <- rbind(result)
    return(result)
  } else {
    if (!(pl_dist %in% rownames(plumpton_dist))) {
      stop("pl_dist must be a plumpton distribution. See rownames(\"plumpton_dist\") for options")
    } else {
      pl_stats <- as.numeric(plumpton_dist[pl_dist, 6:9])
      result <- data.frame(v_stats, pl_stats, dif_stats = v_stats - pl_stats)
      rownames(result) <- c("mean", "sd", "skewness", "kurtosis")
      return(result)
    }
  }
}

#' Synthesise missing data patterns
#'
#' @export
synth_missing <- function(df, syn_control = list(pattern = "MCAR"), prob = 0.2) {
  # look up best way to implement a list control object
  if (!(syn_control$pattern %in% c("MCAR", "MAR", "MNAR"))) stop("pattern must be one of the following: MCAR, MAR, MNAR")
  if (syn_control$pattern == "MCAR") {
    dims <- c(rows = nrow(df), cols = ncol(df))
    n <- dims["rows"] * dims["cols"]
    nonresp <- matrix(rbinom(n = n, size = 1, prob = prob)
                      , ncol = dims["cols"]

                                        , nrow = dims["rows"])
    df[nonresp == 1] <- NA
    return(df)
  }
  if (syn_control$pattern == "MAR") {
    # next arg creates the char string of depend vars for fmla
    dep_cols <- sub("\\+$"
                    , ""
                    , paste(sc_pat$dep_cols, "+"
                            , collapse = " "))
    fmla <- as.formula(paste("~", dep_cols))
    pr <- princomp(fmla, data = df)
    raw_prob <- exp(pr$scores[, 1])/(1 + exp(pr$scores[, 1]))
    prob_adjust <- prob / max(raw_prob)
    prob <- raw_prob * prob_adjust
    result <- df
    for (nr_col in syn_control$nr_cols) {
      nonresp <- rbinom(n = length(df[[nr_col]])
                        , size = 1
                        , prob = prob)
      result[[nr_col]] <- ifelse(nonresp == 1
                             , NA
                             , result[[nr_col]])
    }
  }
  return(result)
}

# testing
# syn_control object
# MNAR
