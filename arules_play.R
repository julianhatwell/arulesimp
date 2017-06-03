# Which vars have missing?
# Sort ascending by number of missing
# Catalogue rows where missing
# Generate rules from top, exclude rows where missing

# could try using tidLists format to find missing
# if missing has its own category? 
# Perhaps this is unnecessary
# But key to match the LHS of a missing
# with the LHS of a known.

# An example of questionnaire in elem stat learn

library(arules)
data("Epub")
Epub
summary(Epub)

head(transactionInfo(Epub))

year <- strftime(as.POSIXlt(transactionInfo(Epub)[["TimeStamp"]]), "%Y")
table(year)

Epub2003 <- Epub[year == "2003"]
length(Epub2003)
image(Epub2003)
inspect(Epub2003[1:5])
as(Epub2003[1:5], "list")

EpubTidLists <- as(Epub, "tidLists")
EpubTidLists
as(EpubTidLists, "list")

# questionnaire example
data("AdultUCI")
dim(AdultUCI)
AdultUCI[1:2, ]

# not needed columns
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL

AdultUCI[[ "age"]] <- ordered(
  cut(AdultUCI[[ "age"]]
      , c(15,25,45,65,100))
  , labels = c("Young"
               , "Middle-aged"
               , "Senior"
               , "Old"))
AdultUCI[[ "hours-per-week"]] <- ordered(
  cut(AdultUCI[[ "hours-per-week"]]
      , c(0,25,40,60,168))
  , labels = c("Part-time"
               , "Full-time"
               , "Over-time"
               , "Workaholic"))
AdultUCI[[ "capital-gain"]] <- ordered(
  cut(AdultUCI[[ "capital-gain"]]
      , c(-Inf
          , 0
          , median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]] > 0])
          , Inf))
  , labels = c("None", "Low", "High"))
AdultUCI[[ "capital-loss"]] <- ordered(
  cut(AdultUCI[[ "capital-loss"]]
      , c(-Inf
          , 0
          , median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0])
          , Inf))
  , labels = c("none", "low", "high"))

# coerce to transactions
Adult <- as(AdultUCI, "transactions")
summary(Adult)

rules <- apriori(Adult
         , parameter = list(
             support = 0.01
           , confidence = 0.6))
rules
summary(rules)

rulesIncomeSmall <- subset(rules
                    , subset = rhs %in% "income=small" &
                      lift > 1.2)
rulesIncomeLarge <- subset(rules
                    , subset = rhs %in% "income=large" &
                      lift > 1.2)

inspect(head(rulesIncomeSmall
             , n = 3
             , by = "confidence"))

inspect(head(rulesIncomeLarge
             , n = 3
             , by = "confidence"))

# write out to csv
write(rulesIncomeSmall
      , file = "data.csv"
      , sep = ","
      , col.names = NA)
# wrie to PMML
write.PMML(rulesIncomeSmall
           , file = "data.xml")

# using  eclat
data("Adult")
fsets <- eclat(Adult
              , parameter = list(support = 0.05)
              , control = list(verbose=FALSE))

singleItems <- fsets[size(items(fsets)) == 1]
## Get the col numbers we have support for
singleSupport <- quality(singleItems)$support
names(singleSupport) <- unlist(LIST(items(singleItems)
                                    , decode = FALSE))
head(singleSupport, n = 5)

itemsetList <- LIST(items(fsets)
                    , decode = FALSE)
allConfidence <- quality(fsets)$support /
  sapply(itemsetList, function(x) 
    max(singleSupport[as.character(x)]))
quality(fsets) <- cbind(quality(fsets)
                        , allConfidence)

fsetsEducation <- subset(fsets
                  , subset = items %pin% "education")
inspect(sort(
  fsetsEducation[size(fsetsEducation)>1]
  , by = "allConfidence")[1 : 3])

# sampling
data("Adult")
Adult
# calculating the sample size
supp <- 0.05
epsilon <- 0.1
c <- 0.1
n <- -2 * log(c)/ (supp * epsilon^2)
n

AdultSample <- sample(Adult, n
                      , replace = TRUE)
AdultSample

# the line is the population, original dataset
itemFrequencyPlot(AdultSample
  , population = Adult
  , support = supp
  , cex.names = 0.7)

# lift = true shows deviations from the original set
# 1 is exactly proportionally represented
itemFrequencyPlot(AdultSample
  , population = Adult
  , support = supp
  , lift = TRUE
  , cex.names = 0.9)

time <- system.time(
  itemsets <- eclat(Adult
    , parameter = list(support = supp)
    , control = list(verbose = FALSE))
  )
time

timeSample <- system.time(
  itemsetsSample <- eclat(AdultSample
  , parameter = list(support = supp)
  , control = list(verbose = FALSE))
  )
timeSample

# speed up
time[1] / timeSample[1]

# compare
itemsets
itemsetsSample

match <- match(itemsets
        , itemsetsSample
        , nomatch = 0) # remove no matches
sum(match > 0) / length(itemsets)

# only itemsets near the minimum support
# were not found in the sample
summary(quality(itemsets[match == 0])$support)
summary(quality(itemsetsSample[-match])$support)

