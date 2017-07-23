# re-do scratch to use smaller & synthetic data
rm(list = ls())
library(lattice)
library(arulesimp)
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


# step 0. convert data to transactions
dw_mnar1$data_trans <- as(dw_mnar1$data_factors, "transactions")
dw_mnar2$data_trans <- as(dw_mnar2$data_factors, "transactions")

summary(dw_mnar1$data_trans)

# step 1. Get the broadest possible ruleset
c_control = cars_control(support = 0.5
  , confidence = 0.5, sort_by = "confidence"
)

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

cars1 <- make_cars(dw_mnar1$data_trans
                   , c_control = c_control
                   , var_names = names(mv1_sorted))

dw_mnar1$imputed <- ARImpute(cars1, dw_mnar1$data_factors)
colSums(sapply(dw_mnar1$imputed, is.na))
dw_mnar1$imputed_num <- as.data.frame(
  lapply(dw_mnar1$imputed, as.integer))


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
