rm(list = ls())
library(arulesimp)
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

uv_sorted <- unique_values(resp
                , which_cols = names(mv_sorted))

# step 0. convert data to transactions
resp <- as(resp, "transactions")
summary(resp)

# step 1. Get the broadest possible ruleset
rules <- arules::apriori(resp
                 , parameter = list(
                   support = 0.1
                   , confidence = 0.2))
rules
summary(rules)






resp <- all_factor(resp)

aresponses <- arules::apriori(resp)
