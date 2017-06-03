rm(list = ls())
responses <- read.csv("C:\\Dev\\Study\\R\\BCU_Assignment_2\\responses.csv")


resp <- char_to_na(responses)


mv_unsorted <- missing_values(responses, sorted = FALSE)
mv_sorted <- missing_values(responses)
# it turns out that the length of the survey
# is a factor determining missingness
# what of that?


resp <- all_factor(resp)
# arules apriori wants logical or factor everything
# will need some functions to clean up

aresponses <- arules::apriori(resp)
