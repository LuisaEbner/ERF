################################################################################
###                                                                          ###
###           EXPERT RULE FIT (ERF) for Simulated Data                       ###
###                                                                          ###
################################################################################


# Libraries

#library(pre)
#library(purrr)
#library(rlist)
#library(gbm)
#library(inTrees)
#library(randomForest)
#library(pROC)
#library(MASS)
#library(bayesm)
#library(glmnet)
#library(coefplot)
#library(purrr)
#library(rlist)
#library(tidyverse)
#library(caret)
#library(mlbench)
#library(Metrics)

# External functions
source("simulation.R")
source("erf_main.R")

#===============================================================================
#                               SIMULATION
#===============================================================================

simulation <- create_simulation(n_vars = 8, n_obs = 800,
                                mu = 0, sigma = 1, 
                                n_rule_vars = 6, 
                                n_rel_rules = 10, 
                                optional_lengths = c(1, 2, 3),
                                weights = c(1/3, 1/3, 1/3),
                                mu_beta = 0, sigma_beta = 5, 
                                mu_epsilon = 0, sigma_epsilon = 0.005)

#===============================================================================
#                                 DATA 
#===============================================================================

data <- simulation[[2]]

# train-test-split 
sets <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

#===============================================================================
#                      SIMULATED EXPERT/DOMAIN KNOWLEDGE
#===============================================================================

expert_rules <- simulation[[3]]

expert_rules

#===============================================================================
#                          EXPERT RULEFIT MODEL
#===============================================================================

erf_sim <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest, name_rules = F,
                        expert_rules = expert_rules, 
                        confirmatory_rules = expert_rules, 
                         n_imp = 10)


