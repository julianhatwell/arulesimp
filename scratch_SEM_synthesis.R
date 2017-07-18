library(lavaan)
library(semTools)
library(semPlot)
library(psych)
library(car) # splom
library(RColorBrewer) # splom colours

wu_data_model_test <- "Factor1 =~ A1 + A2 + A3 + A4 + A5 + A6
                        Factor2 =~ B1 + B2 + B3 + B4 + B5 + B6
                        Factor1 ~~ Factor2"

wu_data_model <- "Factor1 =~ 0.7*A1 + 0.7*A2 + 0.7*A3 + 0.7*A4 + 0.7*A5 + 0.7*A6
                  Factor2 =~ 0.7*B1 + 0.7*B2 + 0.7*B3 + 0.7*B4 + 0.7*B5 + 0.7*B6
                  Factor1 ~~ 0.3*Factor2"
set.seed(10001)
wu_syn_data <- simulateData(wu_data_model, sample.nobs=3600)
print(head(wu_syn_data), digits=2)

alpha(wu_syn_data[, 1:6])

cfa_wu <- cfa(wu_data_model_test, wu_syn_data)
sem_wu <- sem(wu_data_model_test, wu_syn_data)
summary(cfa_wu, standardized = TRUE)
summary(sem_wu, fit.measures = TRUE)

# wu_sim_lik <- data.frame(lapply(wu_syn_data, function(x) {
#   cut(x, breaks=ari.env$rhemtulla_thresholds$symmetric[[3]], labels=FALSE) } ))
# describe(wu_sim_lik)

rhemtulla_thresholds <- list(
  symmetric = list()
  , moderate_asym = list()
  , severe_asym = list()
)

rhemtulla_thresholds$symmetric[[2]] <- 0.00
rhemtulla_thresholds$symmetric[[3]] <- c(-.83, .83)
rhemtulla_thresholds$symmetric[[5]] <- c(-1.50, -.50, .50, 1.50)
rhemtulla_thresholds$symmetric[[7]] <- c(-1.79, -1.07, -.36, .36, 1.07, 1.79)
rhemtulla_thresholds$moderate_asym[[2]] <- .36
rhemtulla_thresholds$moderate_asym[[3]] <- c(-.50, .76)
rhemtulla_thresholds$moderate_asym[[5]] <- c(-.70, .39, 1.16, 2.05)
rhemtulla_thresholds$moderate_asym[[7]] <- c(-1.43, -.43, .38, .94, 1.44, 2.54)
rhemtulla_thresholds$severe_asym[[2]] <- 1.04
rhemtulla_thresholds$severe_asym[[3]] <- c(.58, 1.13)
rhemtulla_thresholds$severe_asym[[5]] <- c(.05, .44, .84, 1.34)
rhemtulla_thresholds$severe_asym[[7]]<- c(-.25, .13, .47, .81, 1.18, 1.64)

scatterplotMatrix(wu_sim_lik[, c(1:3, 10:12)]
                  , diag="histogram"
                  , col=brewer.pal(3, "Paired")
                  , ellipse=TRUE)

factanal(wu_sim_lik, factors = 2)
factanal(wu_sim_lik, factors = 3)

cfa_wu_lik <- cfa(wu_data_model_test, wu_sim_lik)
sem_wu_lik <- sem(wu_data_model_test, wu_sim_lik)
summary(cfa_wu_lik, standardized = TRUE)
summary(sem_wu_lik, fit.measures = TRUE)


semPaths(cfa_wu_lik, what="est", fade=FALSE
         , residuals=FALSE
         , edge.label.cex=0.75)

# test against similar models
