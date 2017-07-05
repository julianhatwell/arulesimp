rm(list = ls())
library(arulesimp)
library(arulesViz)
data("responses")

# check the data set to see if missing values
# are represented by anything other than NA
# fix this (default "")
resp <- char_to_na(responses)

mim_resp <- missing_matrix(resp)

# arules apriori wants logical or factor everything
resp <- all_factor(resp)

# gets a named (variable names) vector
# of number of missing values
mv_unsorted <- missing_values(resp, sorted = FALSE)
mv_sorted <- missing_values(resp)

mv_short <- mv_sorted[1:5]

# may not need this
uv_sorted <- unique_values(resp
                , which_cols = names(mv_sorted))

# step 0. convert data to transactions
resp_trans <- as(resp, "transactions")
summary(resp_trans)

# step 1. Get the broadest possible ruleset
rules <- arules::apriori(resp_trans
                 , parameter = list(
                   support = 0.15
                   , confidence = 0.3))
rules
summary(rules)

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

cars <- make_cars(rules, names(mv_sorted))

# step 3. Which are the rows to impute?
rows_to_impute <- find_rows_to_impute(responses, names(mv_sorted))

# step 4. do imputation
respimputed <- ARImpute(responses
                        , names(mv_sorted)
                        , rows_to_impute = rows_to_impute
                        , cars = cars)



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

synth_missing(dw
, syn_control = missing_control(pattern = "MAR"
                     , method = "carpita"
                     , dep_cols = c("age", "weight", "height"))
)

