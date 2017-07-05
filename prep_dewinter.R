dist_names <- c("very_strongly_agree"
                , "strongly_agree"
                , "agree_peak"
                , "agree_flat"
                , "neutral_to_agree"
                , "neutral_peak"
                , "neutral_flat"
                , "very_strongly_disagree"
                , "strongly_disagree"
                , "disagree_flat"
                , "neutral_to_disagree"
                , "certainly_not_disagree"
                , "multimodal"
                , "strong_multimodal")

basic_dist <- read.table("dewinterdist.txt")
names(basic_dist) <- c("dist", "pc1", "pc2", "pc3", "pc4", "pc5"
                       , "mean", "SD", "skewness", "kurtosis")
basic_dist <- basic_dist[, -1]
rownames(basic_dist) <- dist_names

dewinter_dist <- basic_dist

strag <- sample(5, size = 10000, replace = TRUE
                , prob = dewinter_dist["strongly_agree", 1:5])
dewinter_dist["strongly_agree", 6:9]
mean(strag)
sd(strag)
e1071::skewness(strag)
e1071::kurtosis(strag) + 3 # e1071 returns excess kurtosis

save(dewinter_dist
     , file = "data\\dewinter_dist.rda")
data("dewinter_dist")
