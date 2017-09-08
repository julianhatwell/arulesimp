# re-do scratch to use smaller & synthetic data
rm(list = ls())
library(lattice)
library(arulesimp)
library(arules)
source("C:\\Dev\\Study\\R\\R_Themes\\MarketingTheme.R")
set.seed(1020384)

my_vars <- c("strongly_agree", "neutral_to_agree"
             , "agree_peak", "agree_flat", "multimodal"
             , "strongly_disagree", "very_strongly_disagree"
             , "neutral_peak", "neutral_to_disagree"
             , "neutral_flat")

# take a quick look at the generating parameters
data("dewinter_dist")
dewinter_dist[my_vars, ]

# gen some data
dw <- synth_dewinter(var_names = my_vars
               , dists = my_vars
               , 2000)
d_fmla_agree <- as.formula(paste("~"
          , paste(my_vars[1:5], collapse = " + ")))
d_fmla_disagree <- as.formula(paste("~"
                                 , paste(my_vars[6:10], collapse = " + ")))

densityplot(d_fmla_agree
            , data = dw
            , par.settings = MyLatticeTheme
            , scales = MyLatticeScale
            , xlab = "Selection of De Winter distributions"
            , auto.key = list(columns = 2)
            , plot.points = FALSE)

densityplot(d_fmla_disagree
            , data = dw
            , par.settings = MyLatticeTheme
            , scales = MyLatticeScale
            , xlab = "Selection of De Winter distributions"
            , auto.key = list(columns = 2)
            , plot.points = FALSE)

# synthesise some missingness
dw_mnar1 <- dw
dw_mnar1$cov1 <- rpois(2000, 3)
dw_mnar2 <- dw_mnar1
dw_mnar1 <- synth_missing(dw_mnar1
           , syn_control = missing_control(
           pattern = "MNAR"
           , method = "carpita"
           , dep_cols = "cov1"
           , unobs_cols = "cov1"
           , prob = 0.1
           )
           , plot_probs = TRUE)

dw_mnar2 <- synth_missing(dw_mnar2
          , syn_control = missing_control(
            pattern = "MNAR"
            , method = "carpita"
            , dep_cols = c("cov1", my_vars[1:2])
            , beta_0 = 0.5 # if missing here gives an error
            , betas = c(1, 2, 2)
            , nr_cols = my_vars[c(1:3, 6:7)]
            , unobs_cols = "cov1"
            , prob = 0.3
          )
          , plot_probs = TRUE)

# cov1 has been deleted
names(dw_mnar1$data)
mv1_sorted <- missing_values(dw_mnar1$data) # order of missingness, ascending
mv1_sorted
sum(complete.cases(dw_mnar1$data))
# defaulted to use all other columns
dw_mnar1$syn_control$nr_cols
dw_mnar1$dplot$par.settings <- MyLatticeTheme
dw_mnar1$dplot
dw_mnar1$data_factors <- all_factor(dw_mnar1$data)

# cov1 has been deleted
names(dw_mnar2$data)
mv2_sorted <- missing_values(dw_mnar2$data) # order of missingness, ascending
mv2_sorted
sum(complete.cases(dw_mnar2$data))
# defaulted to use all other columns
dw_mnar2$syn_control$dep_cols
dw_mnar2$dplot$par.settings <- MyLatticeTheme
dw_mnar2$dplot
dw_mnar2$data_factors <- all_factor(dw_mnar2$data)
dw_mnar2$ord_grp <- ord_grp_combine(dw_mnar2$data
                                    , stride = 2
                                    , keep_orig = TRUE)
dw_mnar2$og_factors <- all_factor(dw_mnar2$ord_grp)
dw_mnar2$ord_combi <- ord_combi_expand(dw_mnar2$data, likert_scales)
dw_mnar2$co_factors <- all_factor(dw_mnar2$ord_combi)


# step 0. convert data to transactions
dw_mnar1$data_trans <- as(dw_mnar1$data_factors, "transactions")
dw_mnar2$data_trans <- as(dw_mnar2$data_factors, "transactions")
dw_mnar1$og_trans <- as(dw_mnar1$og_factors, "transactions")
dw_mnar2$og_trans <- as(dw_mnar2$og_factors, "transactions")
dw_mnar1$co_trans <- as(dw_mnar1$co_factors, "transactions")
dw_mnar2$co_trans <- as(dw_mnar2$co_factors, "transactions")

summary(dw_mnar1$data_trans)
summary(dw_mnar1$og_trans)
summary(dw_mnar1$co_trans)

# step 1. Get the broadest possible ruleset
# step 2. Extract rules subsets
# structure is nested lists
# cars
#     |
#     target variable
#                   |
#                   antecedent
#                   consequent
#
# for each var that needs to be imputed
# this takes a bit of time because of writing to disc
# parallelise?

c_control <- cars_control(support = 0.025
  , confidence = 0.2, sort_by = "confidence"
)
og_control <- cars_control(antecedent =
    setdiff(names(dw_mnar1$ord_grp), names(mv1_sorted))
  , support = 0.2
  , confidence = 0.2
  , sort_by = "confidence"
  , maxlen = 20
)
co_control <- cars_control(support = 0.1
  , confidence = 0.2, sort_by = "confidence"
)

cars1 <- make_cars(dw_mnar1$data_trans
                   , c_control = c_control
                   , var_names = names(mv1_sorted))

og_cars1 <- make_cars(dw_mnar1$ord_grp
                   , c_control = og_control
                   , var_names = names(mv1_sorted))

co_cars1 <- make_cars(dw_mnar1$co_trans
                      , c_control = co_control
                      , var_names = names(mv1_sorted))


# how many rules per variable?
summary(sapply(cars1, length))
summary(sapply(og_cars1, length))
summary(sapply(co_cars1, length))

# default best rule
dw_mnar1$imputed <- ARImpute(cars1, dw_mnar1$data_factors)

colSums(sapply(dw_mnar1$imputed, is.na))
dw_mnar1$imputed_num <- as.data.frame(
  lapply(dw_mnar1$imputed, as.integer))

dw_mnar1$imputed_topnm <- ARImpute(cars1
                                  , dw_mnar1$data_factors
                                  , ari_control =
                                    arulesimp_control(
                                      method = "top_n_mean"
                                      , top_n = 7
                                      , use_default_classes = TRUE
                                    )) # add rounding choice

dw_mnar1$imputed_topnm
colSums(sapply(dw_mnar1$imputed_topnm, is.na))
dw_mnar1$imputed_topnm_num <- as.data.frame(
  lapply(dw_mnar1$imputed_topnm, as.integer))

dw_mnar1$imputed_topnmjv <- ARImpute(cars1
                                   , dw_mnar1$data_factors
                                   , ari_control =
                                     arulesimp_control(
                                       method = "top_n_majv"
                                       , top_n = 7
                                       , use_default_classes = TRUE
                                     ))

dw_mnar1$imputed_topnmjv
colSums(sapply(dw_mnar1$imputed_topnmjv, is.na))
dw_mnar1$imputed_topnmjv_num <- as.data.frame(
  lapply(dw_mnar1$imputed_topnmjv, as.integer))

dw_mnar1$imputed_cons_freq <- ARImpute(cars1
                                     , dw_mnar1$data_factors
                                     , ari_control =
                                       arulesimp_control(
                                         method = "consequent_frequency"
                                         , top_n = 5
                                         , use_default_classes = TRUE
                                       ))

dw_mnar1$imputed_cons_freq
colSums(sapply(dw_mnar1$imputed_cons_freq, is.na))
dw_mnar1$imputed_cons_freq_num <- as.data.frame(
  lapply(dw_mnar1$imputed_cons_freq, as.integer))

c_control$sort_by <- "laplace"
cars1_lap <- make_cars(dw_mnar1$data_trans
                   , c_control = c_control
                   , var_names = names(mv1_sorted))

dw_mnar1$imputed_laplace <- ARImpute(cars1_lap
                                     , dw_mnar1$data_factors
                                     , ari_control =
                                       arulesimp_control(
                                         method = "top_n_mean"
                                         , top_n = 10
                                         , use_default_classes = TRUE
                                       ))

dw_mnar1$imputed_laplace
colSums(sapply(dw_mnar1$imputed_laplace, is.na))
dw_mnar1$imputed_laplace_num <- as.data.frame(
  lapply(dw_mnar1$imputed_laplace, as.integer))

c_control$sort_by <- "chiSquared"
cars1_chi <- make_cars(dw_mnar1$data_trans
                       , c_control = c_control
                       , var_names = names(mv1_sorted))

dw_mnar1$imputed_chiSquared <- ARImpute(cars1_chi
                                     , dw_mnar1$data_factors
                                     , ari_control =
                                       arulesimp_control(
                                         method = "top_n_majv"
                                         , top_n = 10
                                         , use_default_classes = TRUE
                                       ))

dw_mnar1$imputed_chiSquared
colSums(sapply(dw_mnar1$imputed_chiSquared, is.na))
dw_mnar1$imputed_chiSquared_num <- as.data.frame(
  lapply(dw_mnar1$imputed_chiSquared, as.integer))


dw_mnar1$imputed_weighted_chisq <- ARImpute(cars1_chi
                                      , dw_mnar1$data_factors
                                      , ari_control =
                                       arulesimp_control(
                                         method = "consequent_frequency"
                                         , top_n = 7
                                         , weighted_chisq = TRUE
                                         , use_default_classes = TRUE
                                       ))

dw_mnar1$imputed_weighted_chisq
colSums(sapply(dw_mnar1$imputed_weighted_chisq, is.na))
dw_mnar1$imputed_weighted_chisq_num <- as.data.frame(
  lapply(dw_mnar1$imputed_weighted_chisq, as.integer))


dw_mnar1$co_imputed <- ARImpute(co_cars1
                                , dw_mnar1$co_factors)

colSums(sapply(dw_mnar1$co_imputed, is.na))

dw_mnar1$co_imputed_num <- as.data.frame(
  lapply(dw_mnar1$co_imputed, as.integer))

dw_mnar1$og_imputed <- ARImpute(og_cars1
                                , dw_mnar1$og_factors)

colSums(sapply(dw_mnar1$og_imputed, is.na))

dw_mnar1$og_imputed_num <- as.data.frame(
  lapply(dw_mnar1$og_imputed, as.integer))

dw_mnar1$og_chisq_imputed <- ARImpute(og_cars1
                                , dw_mnar1$og_factors
                                , ari_control =
                                  arulesimp_control(
                                    method = "consequent_frequency"
                                    , top_n = 5
                                    , weighted_chisq = TRUE
                                    , use_default_classes = TRUE))

colSums(sapply(dw_mnar1$og_chisq_imputed, is.na))

dw_mnar1$og_chisq_imputed_num <- as.data.frame(
  lapply(dw_mnar1$og_chisq_imputed, as.integer))



# MICE style
library(arules)
dw_mnar1$imputed_iter_best <- ARImpute_iter(dw_mnar1$data
                                            , mv1_sorted
                                            , iter_control =
                                              iteration_control(
                                                method = "none"
                                                , max_iter = 5
                                                , target_convergence = 50)
                                                # default ari and c control
)

dw_mnar1$imputed_iter_best
colSums(sapply(dw_mnar1$imputed_iter_best, is.na))
dw_mnar1$imputed_iter_best_num <- as.data.frame(
  lapply(dw_mnar1$imputed_iter_best, as.integer))


dw_mnar1$imputed_iter_chisq <- ARImpute_iter(dw_mnar1$data
                                , mv1_sorted
                                , iter_control =
                                  iteration_control(
                                    method = "propensity"
                                    , splits = 4
                                    , max_iter = 10
                                    , target_convergence = 5)
                                , ari_control =
                                  arulesimp_control(
                                    method = "consequent_frequency"
                                    , use_default_classes = TRUE
                                    , weighted_chisq = TRUE)
                                , c_control = cars_control(
                                  support = 0.025
                                  , confidence = 0.2
                                  , sort_by = "chiSquared")
)


dw_mnar1$imputed_iter_chisq
colSums(sapply(dw_mnar1$imputed_iter_chisq, is.na))
dw_mnar1$imputed_iter_chisq_num <- as.data.frame(
  lapply(dw_mnar1$imputed_iter_chisq, as.integer))


dw_mnar1$ord_grp <- ord_grp_combine(dw_mnar1$data
                                    , stride = 2
                                    , keep_orig = TRUE)
dw_mnar1$og_factors <- all_factor(dw_mnar1$ord_grp)
likert_scales <- list(pos = my_vars[1:5]
                      , neg = my_vars[6:10])
dw_mnar1$ord_combi <- ord_combi_expand(dw_mnar1$data, likert_scales)
dw_mnar1$co_factors <- all_factor(dw_mnar1$ord_combi)




dw_mnar1$imputed_iter_co_chisq <- ARImpute_iter(dw_mnar1$ord_combi
                                             , mv1_sorted
                                             , max_iter = 10
                                             , target_convergence = 5
                                             , ari_control =
                                               arulesimp_control(
                                                 method = "top_n_mean"
                                                 , top_n = 20
                                                 , use_default_classes = TRUE)
)

dw_mnar1$imputed_iter_co_chisq
colSums(sapply(dw_mnar1$imputed_iter_co_chisq, is.na))
dw_mnar1$imputed_iter_co_chisq_num <- as.data.frame(
  lapply(dw_mnar1$imputed_iter_co_chisq, as.integer))

library(lattice)
densityplot(dw$neutral_flat)
densityplot(dw_mnar1$imputed_num$neutral_flat)
densityplot(dw_mnar1$imputed_cons_freq_num$neutral_flat)
densityplot(dw_mnar1$imputed_topnm_num$neutral_flat)
densityplot(dw_mnar1$imputed_topnmjv_num$neutral_flat)
densityplot(dw_mnar1$imputed_laplace_num$neutral_flat)
densityplot(dw_mnar1$imputed_chiSquared_num$neutral_flat)
densityplot(dw_mnar1$imputed_weighted_chisq_num$neutral_flat)
densityplot(dw_mnar1$imputed_iter_best_num$neutral_flat)
densityplot(dw_mnar1$imputed_iter_chisq_num$neutral_flat)

densityplot(dw_mnar1$co_imputed_num$neutral_flat)
densityplot(dw_mnar1$og_imputed_num$neutral_flat)
densityplot(dw_mnar1$og_chisq_imputed_num$neutral_flat)

densityplot(dw$multimodal)
densityplot(dw_mnar1$imputed_num$multimodal)
densityplot(dw_mnar1$imputed_cons_freq_num$multimodal)
densityplot(dw_mnar1$imputed_topnm_num$multimodal)
densityplot(dw_mnar1$imputed_topnmjv_num$multimodal)
densityplot(dw_mnar1$imputed_laplace_num$multimodal)
densityplot(dw_mnar1$imputed_chiSquared_num$multimodal)
densityplot(dw_mnar1$imputed_weighted_chisq_num$multimodal)
densityplot(dw_mnar1$imputed_iter_best_num$multimodal)
densityplot(dw_mnar1$imputed_iter_chisq_num$multimodal)

densityplot(dw_mnar1$co_imputed_num$multimodal)
densityplot(dw_mnar1$og_imputed_num$multimodal)
densityplot(dw_mnar1$og_chisq_imputed_num$multimodal)

densityplot(dw$neutral_to_disagree)
densityplot(dw_mnar1$imputed_num$neutral_to_disagree)
densityplot(dw_mnar1$imputed_cons_freq_num$neutral_to_disagree)
densityplot(dw_mnar1$imputed_topnm_num$neutral_to_disagree)
densityplot(dw_mnar1$imputed_topnmjv_num$neutral_to_disagree)
densityplot(dw_mnar1$imputed_laplace_num$neutral_to_disagree)
densityplot(dw_mnar1$imputed_chiSquared_num$neutral_to_disagree)
densityplot(dw_mnar1$imputed_weighted_chisq_num$neutral_to_disagree)
densityplot(dw_mnar1$imputed_iter_best_num$neutral_to_disagree)
densityplot(dw_mnar1$imputed_iter_chisq_num$neutral_to_disagree)

densityplot(dw_mnar1$co_imputed_num$neutral_to_disagree)
densityplot(dw_mnar1$og_imputed_num$neutral_to_disagree)
densityplot(dw_mnar1$og_chisq_imputed_num$neutral_to_disagree)


densityplot(dw$strongly_agree)
densityplot(dw_mnar1$imputed_num$strongly_agree)
densityplot(dw_mnar1$imputed_cons_freq_num$strongly_agree)
densityplot(dw_mnar1$imputed_topnm_num$strongly_agree)
densityplot(dw_mnar1$imputed_topnmjv_num$strongly_agree)
densityplot(dw_mnar1$imputed_laplace_num$strongly_agree)
densityplot(dw_mnar1$imputed_chiSquared_num$strongly_agree)
densityplot(dw_mnar1$imputed_weighted_chisq_num$strongly_agree)
densityplot(dw_mnar1$imputed_iter_best_num$strongly_agree)
densityplot(dw_mnar1$imputed_iter_chisq_num$strongly_agree)

densityplot(dw_mnar1$co_imputed_num$strongly_agree)
densityplot(dw_mnar1$og_imputed_num$strongly_agree)
densityplot(dw_mnar1$og_chisq_imputed_num$strongly_agree)


densityplot(dw$strongly_disagree)
densityplot(dw_mnar1$imputed_num$strongly_disagree)
densityplot(dw_mnar1$imputed_cons_freq_num$strongly_disagree)
densityplot(dw_mnar1$imputed_topnm_num$strongly_disagree)
densityplot(dw_mnar1$imputed_topnmjv_num$strongly_disagree)
densityplot(dw_mnar1$imputed_laplace_num$strongly_disagree)
densityplot(dw_mnar1$imputed_chiSquared_num$strongly_disagree)
densityplot(dw_mnar1$imputed_weighted_chisq_num$strongly_disagree)
densityplot(dw_mnar1$imputed_iter_best_num$strongly_disagree)
densityplot(dw_mnar1$imputed_iter_chisq_num$strongly_disagree)

densityplot(dw_mnar1$co_imputed_num$strongly_disagree)
densityplot(dw_mnar1$og_imputed_num$very_strongly_disagree)
densityplot(dw_mnar1$og_chisq_imputed_num$very_strongly_disagree)

# with propensity



# "closing var: neutral_to_disagree rule: 5 neutral_to_agree very_strongly_disagree 2 1 row: 58 new value: 3"
# if (is.na(cars[[v]][[i]]$antecedent[1, "value"])) {
#   all_match <- TRUE # empty set match
#   warning("empty rule found. cars may not have been constructed correctly.")
#   break
# }




# step 3. Which are the rows to impute?
# convenience function, looks at the mim if given one
rows1_to_impute <- find_rows_to_impute(dw_mnar1$mim, names(mv1_sorted))
rows2_to_impute <- find_rows_to_impute(dw_mnar2$mim, names(mv2_sorted))

# step 4. do imputation
imputed1 <- ARImpute(dw_mnar1$data
                        , names(mv1_sorted)
                        , rows_to_impute = rows1_to_impute
                        , cars = cars1)






# synthesise data
# step 1. set up some items
politik <- c("pol1_strag"
             , "pol2_vstrag"
             , "pol3_multmod"
             , "pol4_neutag")
politik_dist <- c("strongly_agree"
                  , "very_strongly_agree"
                  , "multimodal"
                  , "neutral_to_agree")
Likerts <- data.frame(category = "politics"
                      , items = politik
                      , item_dist = politik_dist
                      , stringsAsFactors = FALSE)

# step 2. create the data frame of responses
synth_likerts <- synth_plumpton(Likerts$items
                                , Likerts$item_dist
                                , n = 10000)

# step 3. check an item against the known characteristics
compare_q1_strag <- compare_plumpton(synth_likerts$pol1_strag
                 , "strongly_agree")
sqrt(sum(compare_q1_strag$dif_stats^2))

# step 4. see which model population an item most closely matches
compare_q1_all <- compare_plumpton(synth_likerts$pol1_strag
                 , "all")
which.min(sqrt(apply(compare_q1_all^2, 2, sum)))

compare_Music_all <- compare_plumpton(as.vector(responses$Music)
                                   , "all")
which.min(sqrt(apply(compare_Music_all^2, 2, sum)))

# synthesising missing data
# MCAR
syn_mcar <- synth_missing(df = synth_likerts # default is MCAR with no syn_control object
              , prob = 0.2) # maximum probability from dependency


# MAR
# step 1. create a control object
sc_pat1 <- list(pattern = "MAR" # uses principle comps to create a relationship between dep cols and non-response
          , nr_cols = "pol1_strag" # columns with non-response
          , dep_cols = c("pol2_vstrag", "pol4_neutag")) # non-response depends on these

syn_mar1 <- synth_missing(df = synth_likerts
              , syn_control = sc_pat1
              , prob = 0.5) # maximum probability from dependency

sc_pat2 <- list(pattern = "MAR" # uses principle comps to create a relationship between dep cols and non-response
                , nr_cols = c("pol1_strag", "pol3_multmod") # can support multiple targets
                , dep_cols = "pol2_vstrag") # could just be one variable

syn_mar2 <- synth_missing(df = synth_likerts
              , syn_control = sc_pat2
              , prob = 0.3) # maximum probability from dependency

subrules <- rules[quality(rules)$lift > 2]
plot(subrules)


operacars <- extract_cars(cars[["Opera"]])

chemcars <- extract_cars(cars[["Chemistry"]])
rows_to_impute <- which(is.na(responses[["Chemistry"]]))

# more mice
iris.mis <- synth_missing(iris, prob = 0.25)

#Check missing values introduced in the data
summary(iris.mis)
str(iris.mis)

dw <- synth_dewinter(c("yes_yes"
                       , "no_no"
                       , "we_we")
                     , c("very_strongly_agree"
                         , "very_strongly_disagree"
                         , "multimodal")
                     , n = 100)
dw$age <- runif(100, 0, 100)
dw$weight <- rnorm(100, 100, 25)
dw$height <- rpois(100, 25)

# synth_missing(dw
# , syn_control = missing_control(pattern = "MAR"
#                      , method = "carpita"
#                      , dep_cols = c("age", "weight", "height"))
# )

dw_mis_mnar <- synth_missing(dw
  , syn_control = missing_control(
                  pattern = "MNAR"
                  , method = "wu_ranking"
                  , dep_cols = c("age", "weight", "height")
                  #, unobs_cols = c("age", "weight", "height")
                  , prob = 0.3
  )
, plot_probs = TRUE)

# to do a double round of wu:

dw_mis_mar <- synth_missing(dw
                            , syn_control = missing_control(
                              pattern = "MAR"
                              , method = "wu_ranking"
                              , nr_cols = "yes_yes"
                              , dep_cols = "age"
                              , prob = 0.3
                            )
                            , plot_probs = TRUE)
dw_mis_mar2 <- synth_missing(dw_mis_mar$data
                            , syn_control = missing_control(
                              pattern = "MAR"
                              , method = "wu_ranking"
                              , nr_cols = c("no_no", "we_we")
                              , dep_cols = c("weight", "height")
                              , prob = 0.3
                            )
                            , plot_probs = TRUE)


dw_mis_mnar <- synth_missing(dw
                             , syn_control = missing_control(
                               pattern = "MNAR"
                               , method = "carpita"
                               , dep_cols = c("we_we", "age", "weight", "height")
                               , unobs_cols = c("weight", "height")
                               , mm_cols = c("yes_yes", "no_no", "we_we")
                               , prob = 0.3
                             )
                             , plot_probs = TRUE
                             , deps_in_mim = TRUE)

dw_mis_mnar <- synth_missing(dw
                             , syn_control = missing_control(
                               pattern = "MNAR"
                               , method = "carpita"
                               , nr_cols = c("yes_yes", "no_no", "we_we")
                               , dep_cols = c("we_we", "age", "weight", "height")
                               , unobs_cols = c("weight", "height")
                               , mm_cols = c("yes_yes", "no_no", "we_we")
                               , prob = 0.3
                             )
                             , plot_probs = TRUE)

LikertImpute(dw_mis_mnar$data, dt_mim = dw_mis_mnar$mim)
LikertImpute(dw_mis_mnar$data, dt_mim = dw_mis_mnar$mim, rounding = nd_round)
LikertImpute(dw_mis_mnar$data, dt_mim = dw_mis_mnar$mim, rounding = floor)
LikertImpute(dw_mis_mnar$data, dt_mim = dw_mis_mnar$mim, rounding = ceiling)
LikertImpute(dw_mis_mnar$data, dt_mim = dw_mis_mnar$mim, rounding = round)

LikertImpute(dw_mis_mnar$data
             , dt_mim = dw_mis_mnar$mim
             , method = "CIM"
             , rounding = nd_round)
LikertImpute(dw_mis_mnar$data
             , dt_mim = dw_mis_mnar$mim
             , method = "TW"
             , rounding = nd_round)
LikertImpute(dw_mis_mnar$data
             , dt_mim = dw_mis_mnar$mim
             , method = "ICS"
             , rounding = nd_round)

# quality measures
# ordinary t-test
as.vector(t.test(rnorm(100))$conf.int)


# don't forget to quote mse ratio

# Scale Score Error requires known and imputed

# alpha confidence intervals by using bootstrapping of funciton in psych


if (ari_control$method == "best_rule") {
  for (v in var_names) {
    # another loop here over each row to impute
    for (k in seq_along(rows_to_impute[[v]])) {
      # initialize for the antecedent outer loop
      all_match <- FALSE
      num_rules <- length(cars[[v]])
      i <- 0
      # print(paste("initialised k loop", v, i, k))
      # start the antecedent outer loop
      while (
        !(all_match) &&
        i < num_rules) {
        # re-initialize for the antecedent inner loop
        i <- i + 1
        all_match <- FALSE
        # print(paste("initialised antecedent i loop", v, i, k, cars[[v]]$antecedent[[i]]))
        data_value <- as.character(dt[rows_to_impute[[v]][k]
                                      , as.character(cars[[v]][[i]]$antecedent$key)])
        condition_value <- as.character(cars[[v]][[i]]$antecedent$value)
        all_match <- identical(data_value, condition_value)
      }
      if (all_match && num_rules != 0) {
        dt[rows_to_impute[[v]][[k]], v] <- cars[[v]][[i]]$consequent
      }
    }
  }
} # end of best rule




if (grepl("^top\\_n", ari_control$method)) {
  for (v in var_names) {
    # another loop here over each row to impute
    for (k in seq_along(rows_to_impute[[v]])) {
      # initialize for the antecedent outer loop
      n_rules <- list()
      num_rules <- length(cars[[v]])
      i <- 0
      n <- 0
      while (n < ari_control$top_n &&
             i < num_rules) {
        i <- i + 1
        data_value <- as.character(dt[rows_to_impute[[v]][k]
                                      , as.character(cars[[v]][[i]]$antecedent$key)])
        condition_value <- as.character(cars[[v]][[i]]$antecedent$value)
        if (identical(data_value, condition_value)) {
          n <- n + 1
          n_rules[[n]] <- cars[[v]][[i]]
        }
      }
      if (n > 0) {
        if (ari_control$method == "top_n_mean") {
          dt[rows_to_impute[[v]][[k]], v] <- as.character(
            nd_round(mean(
              sapply(n_rules, function(rule) {
                as.numeric(rule$consequent)
              }))))
        }
        if (ari_control$method == "top_n_majv") {
          majv <- names(which.max(table(
            sapply(n_rules, function(rule) {
              as.numeric(rule$consequent)}))))
          if (length(majv > 1)) majv <- sample(majv, size = 1)
          dt[rows_to_impute[[v]][[k]], v] <- majv
        }
      }
    }
  }
} # end of top_n


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


library(arulesimp)
library(arules)
source("C:\\Dev\\Study\\R\\likertimpute\\run_experiments_header.R")
source("C:\\Dev\\Study\\R\\likertimpute\\synth_wu_data.R")

# this lavaan data model reproduces the configuration in Wu's paper
wu_data_model <- "Factor1 =~ 0.75*A1 + 0.75*A2 + 0.75*A3 + 0.75*A4 + 0.75*A5 + 0.75*A6
Factor2 =~ 0.65*B1 + 0.65*B2 + 0.65*B3 + 0.65*B4 + 0.65*B5 + 0.65*B6
Factor1 ~~ 0.3*Factor2"

non_response_cols <- paste0(rep(c("A", "B"), each = 3), 1:3)


for (i in 1:100) {

seed <- round(runif(1, 0,1) * 100000000)
set.seed(seed)

wu_data_master <- lapply(sample_sizes, synth_wu_data, model = wu_data_model)
ssize <- paste0("s", sample_sizes)
names(wu_data_master) <- ssize

wu_data <- lapply(wu_labels, function(w) {
  dt <- wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]]
  dt <- if (wu_design_matrix[w, "mpatt"] == "MCAR") {
    dt
  } else {
    cbind(dt, z1 = z1[1:wu_design_matrix[w, "ss"]]
          , z2 = z2[1:wu_design_matrix[w, "ss"]]
          , sum_z = sum_z[1:wu_design_matrix[w, "ss"]])
  }
})
names(wu_data) <- wu_labels
wu_data <- lapply(wu_labels, function(w) {
  if (wu_design_matrix[w , "mpatt"] == "MCAR") {
    synth_missing(wu_data[[w]]
                  , syn_control = missing_control(
                    pattern = "MCAR"
                    , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
                    , prob = wu_design_matrix[w, "mprop"]))
  } else {
    if (wu_design_matrix[w , "mpatt"] == "MAR") {
      synth_missing(wu_data[[w]]
                    , syn_control = missing_control(
                      # using MNAR to drop the sum_z col. Model is MAR
                      # dep_cols z1 and sum(z1, z2)
                      pattern = "MNAR"
                      , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
                      , dep_cols = c("z1", "sum_z")
                      , unobs_cols = "sum_z"
                      , method = "wu_ranking"
                      , prob = wu_design_matrix[w, "mprop"]))
    } else {
      if (wu_design_matrix[w , "mpatt"] == "MNAR") {
        synth_missing(wu_data[[w]]
                      , syn_control = missing_control(
                        pattern = "MNAR"
                        , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
                        , dep_cols = c("z1", "sum_z")
                        , unobs_cols = c("z1", "z2", "sum_z")
                        , method = "wu_ranking"
                        , prob = wu_design_matrix[w, "mprop"]))
      }
    }
  }
})
# they lose their names in the last operation
names(wu_data) <- wu_labels

# special case for MAR with covariates
mar <- wu_data[[grep("MAR", wu_labels)]]$data[, 1:12]
mn <- min(sapply(mar, as.numeric), na.rm = TRUE)
mx <- max(sapply(mar, as.numeric), na.rm = TRUE)
num_classes <- mx - (mn - 1)
mar_cov <- lapply(wu_data[[grep("MAR", wu_labels)]]$data[, 13:14], function(mc) {
  as.numeric(cut(mc, breaks = num_classes))
})
wu_data[[grep("MAR", wu_labels)]]$data[, 13:14] <- mar_cov

w <- wu_labels[2]
to_impute <- wu_data[[w]]
untreated <- wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]]

ti_data <- to_impute$data
ti_mim <- to_impute$mim
ti_factor <- all_factor(ti_data)

mv_sorted <- missing_values(ti_mim)
c_control <- cars_control(support = 0.02
                          , confidence = 0.2, sort_by = "confidence"
)
cars_conf <-
  make_cars(ti_factor
          , c_control = c_control
          , var_names = names(mv_sorted))

cars_chi <- make_cars(ti_factor
                      , c_control = c_control
                      , var_names = names(mv_sorted))

c_control$sort_by <- "laplace"
cars_lap <- make_cars(ti_factor
                      , c_control = c_control
                      , var_names = names(mv_sorted))

dt <- ti_factor
dt <- ti_data
cars <- cars_lap

ti_combi <- ord_combi_expand(ti_data[1:12]
                             , likert_scales =
                               list(A = names(ti_data[1:6])
                                    , B = names(ti_data[7:12]))
                             , keep_orig = TRUE)
ti_combi <- cbind(ti_combi, ti_data[, 13:14])
ti_combi_fac <- all_factor(ti_combi)

co_control <- cars_control(support = 0.05
                           , confidence = 0.1
                           , sort_by = "chiSquared"
)
co_cars <- make_cars(ti_combi_fac
                      , c_control = co_control
                      , var_names = names(mv_sorted))

dt <- ti_combi_fac
cars <- co_cars
var_names <- names(mv_sorted)

ARImpute(cars_chi, ti_data, var_names = names(mv_sorted))
ARImpute(co_cars, ti_combi_fac, var_names = names(mv_sorted))[1:12]


ti_og_data <- ord_grp_combine(ti_data[, 1:12]
                              , stride = 2
                              , keep_orig = TRUE)
ti_og_data <- cbind(ti_og_data, ti_data[, 13:14])
og_control <- cars_control(antecedent =
                             setdiff(names(ti_og_data)
                                     , names(mv_sorted))
                           , support = 0.2
                           , confidence = 0.2
                           , maxlen = 7
                           , sort_by = "chiSquared")

og_cars <- make_cars(ti_og_data
                      , c_control = og_control
                      , var_names = names(mv_sorted))








# how many rules per variable?
summary(sapply(cars1, length))
summary(sapply(og_cars1, length))
summary(sapply(co_cars1, length))

# default best rule
dw_mnar1$imputed <- ARImpute(cars1, dw_mnar1$data_factors)

colSums(sapply(dw_mnar1$imputed, is.na))
dw_mnar1$imputed_num <- as.data.frame(
  lapply(dw_mnar1$imputed, as.integer))

dw_mnar1$imputed_topnm <- ARImpute(cars1
                                   , dw_mnar1$data_factors
                                   , ari_control =
                                     arulesimp_control(
                                       method = "top_n_mean"
                                       , top_n = 7
                                       , use_default_classes = TRUE
                                     )) # add rounding choice

dw_mnar1$imputed_topnm
colSums(sapply(dw_mnar1$imputed_topnm, is.na))
dw_mnar1$imputed_topnm_num <- as.data.frame(
  lapply(dw_mnar1$imputed_topnm, as.integer))

dw_mnar1$imputed_topnmjv <- ARImpute(cars1
                                     , dw_mnar1$data_factors
                                     , ari_control =
                                       arulesimp_control(
                                         method = "top_n_majv"
                                         , top_n = 7
                                         , use_default_classes = TRUE
                                       ))

dw_mnar1$imputed_topnmjv
colSums(sapply(dw_mnar1$imputed_topnmjv, is.na))
dw_mnar1$imputed_topnmjv_num <- as.data.frame(
  lapply(dw_mnar1$imputed_topnmjv, as.integer))

dw_mnar1$imputed_cons_freq <- ARImpute(cars1
                                       , dw_mnar1$data_factors
                                       , ari_control =
                                         arulesimp_control(
                                           method = "consequent_frequency"
                                           , top_n = 5
                                           , use_default_classes = TRUE
                                         ))

dw_mnar1$imputed_cons_freq
colSums(sapply(dw_mnar1$imputed_cons_freq, is.na))
dw_mnar1$imputed_cons_freq_num <- as.data.frame(
  lapply(dw_mnar1$imputed_cons_freq, as.integer))

c_control$sort_by <- "laplace"
cars1_lap <- make_cars(dw_mnar1$data_trans
                       , c_control = c_control
                       , var_names = names(mv1_sorted))

dw_mnar1$imputed_laplace <- ARImpute(cars1_lap
                                     , dw_mnar1$data_factors
                                     , ari_control =
                                       arulesimp_control(
                                         method = "top_n_mean"
                                         , top_n = 10
                                         , use_default_classes = TRUE
                                       ))

dw_mnar1$imputed_laplace
colSums(sapply(dw_mnar1$imputed_laplace, is.na))
dw_mnar1$imputed_laplace_num <- as.data.frame(
  lapply(dw_mnar1$imputed_laplace, as.integer))

c_control$sort_by <- "chiSquared"
cars1_chi <- make_cars(dw_mnar1$data_trans
                       , c_control = c_control
                       , var_names = names(mv1_sorted))

dw_mnar1$imputed_chiSquared <- ARImpute(cars1_chi
                                        , dw_mnar1$data_factors
                                        , ari_control =
                                          arulesimp_control(
                                            method = "top_n_majv"
                                            , top_n = 10
                                            , use_default_classes = TRUE
                                          ))

dw_mnar1$imputed_chiSquared
colSums(sapply(dw_mnar1$imputed_chiSquared, is.na))
dw_mnar1$imputed_chiSquared_num <- as.data.frame(
  lapply(dw_mnar1$imputed_chiSquared, as.integer))


dw_mnar1$imputed_weighted_chisq <- ARImpute(cars1_chi
                                            , dw_mnar1$data_factors
                                            , ari_control =
                                              arulesimp_control(
                                                method = "consequent_frequency"
                                                , top_n = 7
                                                , weighted_chisq = TRUE
                                                , use_default_classes = TRUE
                                              ))

dw_mnar1$imputed_weighted_chisq
colSums(sapply(dw_mnar1$imputed_weighted_chisq, is.na))
dw_mnar1$imputed_weighted_chisq_num <- as.data.frame(
  lapply(dw_mnar1$imputed_weighted_chisq, as.integer))


dw_mnar1$co_imputed <- ARImpute(co_cars1
                                , dw_mnar1$co_factors)

colSums(sapply(dw_mnar1$co_imputed, is.na))

dw_mnar1$co_imputed_num <- as.data.frame(
  lapply(dw_mnar1$co_imputed, as.integer))

dw_mnar1$og_imputed <- ARImpute(og_cars1
                                , dw_mnar1$og_factors)

colSums(sapply(dw_mnar1$og_imputed, is.na))

dw_mnar1$og_imputed_num <- as.data.frame(
  lapply(dw_mnar1$og_imputed, as.integer))

dw_mnar1$og_chisq_imputed <- ARImpute(og_cars1
                                      , dw_mnar1$og_factors
                                      , ari_control =
                                        arulesimp_control(
                                          method = "consequent_frequency"
                                          , top_n = 5
                                          , weighted_chisq = TRUE
                                          , use_default_classes = TRUE))

colSums(sapply(dw_mnar1$og_chisq_imputed, is.na))

dw_mnar1$og_chisq_imputed_num <- as.data.frame(
  lapply(dw_mnar1$og_chisq_imputed, as.integer))




run_wu_ibenchmark <- function(to_impute, untreated, mi_mult = "rubin", results_only = TRUE) {
  if(!(mi_mult %in% c("rubin", "white"))) stop("mi_mult should be either \"rubin\" or \"white\"")

  ti_data <- to_impute$data
  ti_mim <- to_impute$mim
  ti_factor <- all_factor(ti_data)
  mv_sorted <- missing_values(ti_mim)

  ti_combi <- ord_combi_expand(ti_data[1:12]
                               , likert_scales =
                                 list(A = names(ti_data[1:6])
                                      , B = names(ti_data[7:12]))
                               , keep_orig = TRUE)
  if (ncol(ti_data) == 14) ti_combi <- cbind(ti_combi, ti_data[, 13:14])
  ti_combi_fac <- all_factor(ti_combi)

  c_control <- cars_control(support = 0.02
                            , confidence = 0.2
                            , sort_by = "chiSquared"
  )
  ari_control <- arulesimp_control(method = "best_rule"
                                   , use_default_classes = 1
                                   , weighted_chisq = FALSE)

  imputations <- list(
    PM = LikertImpute(
      ti_data, ti_mim
      , "PM", rounding = nd_round)
    , CIM = LikertImpute(
      ti_data, ti_mim
      , "CIM", rounding = nd_round)
    , TW = LikertImpute(
      ti_data, ti_mim
      , "TW", rounding = nd_round)
    , ICS = LikertImpute(
      ti_data, ti_mim
      , "TW", rounding = nd_round)
  )
  results <- lapply(imputations, function(rout) {

    res <- as.data.frame(sapply(rout, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })

  # multiple imputation settings
  if (mi_mult == "white") {
    mi_runs <- round(to_impute$syn_control$prob * 100)
  } else {
    if (to_impute$syn_control$prob <= 0.15) mi_runs <- 3
    if (to_impute$syn_control$prob > 0.15) mi_runs <- 5
  }

  # conduct imputations
  ari.out <- list()
  for (n in 1:mi_runs) {
    ari.out[[n]] <-
      ARImpute_iter(ti_data
                    , missing_values(ti_data)
                    , iter_control =
                      iteration_control(
                        method = "propensity"
                        , class_balance = list(method = "both")
                        , splits = 2
                        , max_iter = 5
                        , target_convergence = 36)
                    , ari_control = ari_control
                    , c_control = c_control)
  }
  isarbi.stats <- lapply(1:mi_runs, function(n) {
    wu_collect_stats(ari.out[[n]], untreated, ti_mim)
  })
  headers <- names(isarbi.stats[[1]])

  mean_params <- function(x) {
    res <- lapply(headers, function(h) {
      mean(unlist(lapply(1:mi_runs, function(n)
        x[[n]][[h]])))
    })
    names(res) <- headers
    return(res)
  }

  results$ibestdc1chip <- mean_params(isarbi.stats)

  # multiple imputation methods
  amelia.out <- amelia(ti_data, p2s = 0
                       , ords = non_response_cols
                       , m = mi_runs)
  amelia.stats <- lapply(1:mi_runs, function(n) {
    wu_collect_stats(amelia.out$imputations[[n]], untreated, ti_mim)
  })

  results$amelia <- mean_params(amelia.stats)

  mice.out <- mice(ti_data, print = FALSE
                   , m = mi_runs, meth = "pmm")
  mice.stats <- lapply(1:mi_runs, function(n) {
    wu_collect_stats(mice::complete(mice.out, n)
                     , untreated, ti_mim)
  })
  results$mice <- mean_params(mice.stats)

  if (results_only) {
    return(results)
  } else {
    imputations$ari <- ari.out
    imputations$amelia <- amelia.out$imputations
    imputations$mice <- lapply(1:mi_runs, function(n) {
      mice::complete(mice.out, n)})
    imputations$untreated <- untreated
    return(imputations)
  }
}

mi_mult = "rubin"

imputations <- list(bestrule = ARImpute(cars_conf, ti_factor
                                        , ari_control =
                                          arulesimp_control(
                                            method = "best_rule"
                                            , use_default_classes = 0))
                    , topnm3 = ARImpute(cars_conf, ti_factor
                                        , ari_control =
                                          arulesimp_control(
                                            method = "top_n_mean"
                                            , top_n = 3
                                            , use_default_classes = 0
                                            , weighted_chisq = FALSE
                                          ))
                    , topnm7 = ARImpute(cars_conf, ti_factor
                                        , ari_control =
                                          arulesimp_control(
                                            method = "top_n_mean"
                                            , top_n = 7
                                            , use_default_classes = 0
                                            , weighted_chisq = FALSE
                                          ))
                    , topnm = ARImpute(cars_conf, ti_factor
                                       , ari_control =
                                         arulesimp_control(
                                           method = "top_n_mean"
                                           , use_default_classes = 0
                                           , weighted_chisq = FALSE
                                         ))
                    , topnmjv3 = ARImpute(cars_conf, ti_factor
                                          , ari_control =
                                            arulesimp_control(
                                              method = "top_n_majv"
                                              , top_n = 3
                                              , use_default_classes = 0
                                              , weighted_chisq = FALSE
                                            ))
                    , topnmjv7 = ARImpute(cars_conf, ti_factor
                                          , ari_control =
                                            arulesimp_control(
                                              method = "top_n_majv"
                                              , top_n = 7
                                              , use_default_classes = 0
                                              , weighted_chisq = FALSE
                                            ))
                    , topnmjv = ARImpute(cars_conf, ti_factor
                                         , ari_control =
                                           arulesimp_control(
                                             method = "top_n_majv"
                                             , use_default_classes = 0
                                             , weighted_chisq = FALSE
                                           ))
                    , rhsfreq3 = ARImpute(cars_conf, ti_factor
                                          , ari_control =
                                            arulesimp_control(
                                              method = "consequent_frequency"
                                              , top_n = 3
                                              , use_default_classes = 0
                                              , weighted_chisq = FALSE
                                            ))
                    , rhsfreq7 = ARImpute(cars_conf, ti_factor
                                          , ari_control =
                                            arulesimp_control(
                                              method = "consequent_frequency"
                                              , top_n = 7
                                              , use_default_classes = 0
                                              , weighted_chisq = FALSE
                                            ))
                    , rhsfreq = ARImpute(cars_conf, ti_factor
                                         , ari_control =
                                           arulesimp_control(
                                             method = "consequent_frequency"
                                             , use_default_classes = 0
                                             , weighted_chisq = FALSE
                                           ))
)
print(i)
}
results <- lapply(imputations, function(rout) {

  res <- as.data.frame(sapply(rout, function(b) {
    if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
  }))
  res <- wu_collect_stats(res
                          , untreated = untreated
                          , mim = ti_mim)
  return(res)
})



imputations <- list(
  # topnm3_wchi = ARImpute(cars_chi, ti_factor
#                                            , ari_control =
#                                              arulesimp_control(
#                                                method = "top_n_mean"
#                                                , top_n = 3
#                                                , use_default_classes = 1
#                                                , weighted_chisq = TRUE
#                                              ))
  #
  #

  rhsfreqdc2 = ARImpute(cars_conf, ti_factor
                        , ari_control =
                          arulesimp_control(
                            method = "consequent_frequency"
                            , use_default_classes = 2
                            , weighted_chisq = FALSE
                          ))
  , ibestlapdc1 <- ARImpute_iter(ti_data
                                 , missing_values(ti_data)
                                 , iter_control =
                                   iteration_control(
                                     method = "none"
                                     , max_iter = 10
                                     , target_convergence = 0)
                                 , ari_control =
                                   arulesimp_control(
                                     method = "best_rule"
                                     , use_default_classes = 2
                                     , weighted_chisq = FALSE)
                                 , c_control = cars_control(
                                   support = 0.02
                                   , confidence = 0.2
                                   , sort_by = "laplace"))
  , ibestlapdc1p <- ARImpute_iter(ti_data
                                  , missing_values(ti_data)
                                  , iter_control =
                                    iteration_control(
                                      method = "propensity"
                                      # , class_balance =
                                      #   list(method = "both"
                                      #        , N = 1000
                                      #        , p = 0.5)
                                      , max_iter = 5
                                      , splits = 4
                                      , target_convergence = 30)
                                  , ari_control =
                                    arulesimp_control(
                                      method = "best_rule"
                                      , use_default_classes = 2
                                      , weighted_chisq = FALSE)
                                  , c_control = cars_control(
                                    support = 0.02
                                    , confidence = 0.2
                                    , sort_by = "laplace"))

  , bestrule_dc1 = ARImpute(cars_conf, ti_factor
                                            , ari_control =
                                              arulesimp_control(
                                                method = "best_rule"
                                                , use_default_classes = 1))
                    , topnm3_dc1 = ARImpute(cars_conf, ti_factor
                                            , ari_control =
                                              arulesimp_control(
                                                method = "top_n_mean"
                                                , top_n = 3
                                                , use_default_classes = 1
                                                , weighted_chisq = FALSE
                                              ))

                    , rhsfreq_dc1 = ARImpute(cars_conf, ti_factor
                                             , ari_control =
                                               arulesimp_control(
                                                 method = "consequent_frequency"
                                                 , use_default_classes = 1
                                                 , weighted_chisq = FALSE
                                               ))
                    , bestrule_dc2 = ARImpute(cars_conf, ti_factor
                                              , ari_control =
                                                arulesimp_control(
                                                  method = "best_rule"
                                                  , use_default_classes = 2))
                    , topnm3_dc2 = ARImpute(cars_conf, ti_factor
                                            , ari_control =
                                              arulesimp_control(
                                                method = "top_n_mean"
                                                , top_n = 3
                                                , use_default_classes = 2
                                                , weighted_chisq = FALSE
                                              ))

                    , rhsfreq_dc2 = ARImpute(cars_conf, ti_factor
                                             , ari_control =
                                               arulesimp_control(
                                                 method = "consequent_frequency"
                                                 , use_default_classes = 2
                                                 , weighted_chisq = FALSE
                                               ))
)
}


results <- lapply(imputations, function(rout) {

  res <- as.data.frame(sapply(rout, function(b) {
    if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
  }))
  res <- wu_collect_stats(res
                          , untreated = untreated
                          , mim = ti_mim)
  return(res)
})

cars <- cars_conf
dt <- ti_factor
