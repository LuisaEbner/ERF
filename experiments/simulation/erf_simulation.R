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
source("./data simulation/simulation_main.R")
source("./ERF/erf_main.R")

#===============================================================================
#                               SIMULATION
#===============================================================================

simulation <- create_simulation(n_vars = 30, n_obs = 200,
                                mu = 0, sigma = 1, 
                                n_rule_vars = 5, 
                                n_rel_rules = 20, 
                                optional_lengths = c(1, 2, 3),
                                weights = c(1/3, 1/3, 1/3),
                                mu_beta = 0, sigma_beta = 5, 
                                mu_epsilon = 0, sigma_epsilon = 0.001)

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
conf_rules <- support_take(expert_rules, data, 0.05)
opt_rules <- setdiff(expert_rules, conf_rules)

expert_rules
conf_rules
opt_rules

#===============================================================================
#                          EXPERT RULEFIT MODEL
#===============================================================================

erf_sim <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                        confirmatory_expert_rules = expert_rules, minsup = 0.01, 
                        ensemble = "both")


#===============================================================================
#                        3. MODEL COMPARISONS
#===============================================================================

# a) ExpertRuleFit  w.o. expert knowledge
rf_sim <- ExpertRuleFit(X, y, Xtest, ytest)

# Complexity
rf_sim$NTerms 
rf_sim$AvgRuleLength 

# Accuracy 
rf_sim$AUC
rf_sim$ClassErr

# Important features
rf_sim$ImportantFeatures

#-------------------------------------------------------------------------------

# b) ERF Alternative: Prediction Rule Ensembles (pre package)

#pre(formula, data, family = "binomial", use.grad = TRUE,
#    tree.unbiased = TRUE, type = "both", sampfrac = 0.5, maxdepth = 3L,
#    learnrate = .01, confirmatory = NULL, mtry = Inf, ntrees = 500,
#    tree.control, removeduplicates = TRUE, removecomplements = TRUE,
#    winsfrac = 0.025, normalize = TRUE, standardize = FALSE,
#    ordinal = TRUE, nfolds = 10L, verbose = FALSE, par.init = FALSE,
#    par.final = FALSE, ...)

# Input
train <- cbind.data.frame(X, y)
test <- cbind.data.frame(Xtest, ytest)

# Model
pre_sim <- pre(y ~ ., data = train , family = "binomial", 
                    type = "rules", ntrees = 250)

# Complexity
rel_coefs <- coef(pre_sim)$coefficient[coef(pre_sim)$coefficient != 0] 
nterms <- length(rel_coefs) 

rules <- coef(pre_sim)$description
avgrulelength <- average_rule_length(rules) 

# Accuracy
preds <- predict(pre_sim, newdata = test, type = "class")
auc <-  auc(ytest, as.integer(preds)) 
ce <-   ce(ytest, as.integer(preds)) 

# Important features 
impfeatures <- rules[1:10] 

#===============================================================================

