rm(list = ls())
library(arulesimp)
library(arulesViz)
data("responses")

# check the data set to see if missing values
# are represented by anything other than NA
# fix this (default "")
resp <- char_to_na(responses)

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
# that relate only to the target vars
# The object will be usable in other routines
cars <- make_cars(rules, names(mv_sorted))

# for each var that needs to be imputed
# this takes a bit of time because of writing to disc
# parallelise?
# find another way!

# step 3. Which are the rows to impute?
rows_to_impute <- list()
for (var in names(mv_short)) {
  rows_to_impute[[var]] <- which(is.na(responses[[var]]))
}

for (var in mv_short) {
  # could save time to predict here
  if (length(unique(cars[[var]]$consequent)) == 1) print("only 1 outcome")
}
rows_to_impute


# initialize for the outer loop
all_match <- FALSE
num_rules <- length(operacars$lhs)
i <- 0
while (!(all_match) &
  i <= num_rules) {

  # re-initialize for the inner loop
  i <- i + 1
  all_match <- TRUE
  num_conditions <- nrow(operacars$antecedent[[i]])
  j <- 0
  while (all_match &
         j <= num_conditions) {
    j <- j + 1
    # the empty set
    if (is.na(operacars$antecedent[[i]][j, "value"])) {
      all_match <- TRUE # empty set match
      break
    }
    # otherwise test all conditions
    all_match <- identical(responses[[rows_to_impute, as.character(operacars$antecedent[[i]][j, "key"])]]
                           , operacars$antecedent[[i]][j, "value"])
  }
  if (all_match) break
}
print(operacars$consequent[[i]])

resp[rows_to_impute, "Opera"] <- operacars$consequent[[i]][2]
