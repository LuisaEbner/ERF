################################################################################
################################################################################

#            COMPARISON OF MODEL RESULTS  - SIMULATED DATA                     #

################################################################################
################################################################################

# Libraries
# install.packages('xrf')
# install.packages('xgboost')
# install.packages('pre')
# install.packages('RCurl')
# install.packages('ROCR')

library(dplyr)
library(pre)
library(glmnet)
library(xgboost)
library(RCurl)
library(xrf)
library(pROC)
library(ROCit)
library(ROCR)
library(mice)

# external functions
source("erf_main.R")
source("simulation.R")

# seed
set.seed(179)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                       RULEFIT IMPLEMENTATION OPTIONS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# a) ExpertRuleFit

#ExpertRuleFit(X=NULL, y=NULL, Xtest=NULL, ytest=NULL,
#              name_rules = T, expert_rules = NULL, confirmatory_rules = NULL,
#              name_lins = T, linterms=NULL, confirmatory_lins = NULL,
#              ntree=250, ensemble= "GBM", mix=0.5, L=4, S=6, minsup=.025, 
#              intercept=T, corelim = 1, 
#              alpha = 1, nfolds = 10, type.measure = "class",
#              s = "lambda.min", dfmax = 500, pmax = 500, standardize = F, 
#              print_output = T) {


# b) PRE

# see: https://github.com/marjoleinF/pre

#pre(formula, data, family = "binomial", use.grad = TRUE,
#    tree.unbiased = TRUE, type = "both", sampfrac = 0.5, maxdepth = 3L,
#    learnrate = .01, confirmatory = NULL, mtry = Inf, ntrees = 500,
#    tree.control, removeduplicates = TRUE, removecomplements = TRUE,
#    winsfrac = 0.025, normalize = TRUE, standardize = FALSE,
#    ordinal = TRUE, nfolds = 10L, verbose = FALSE, par.init = FALSE,
#    par.final = FALSE, ...)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                            SIMULATION DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

simulation <- create_simulation(n_vars = 100, n_obs = 1000,
                                n_rule_vars = 20, 
                                n_rel_rules = 10)
data_sim <- simulation[[2]]

# train-test-split for ERF
sets_sim <- create_X_y_Xtest_ytest(data_sim, 0.7, pos_class = 1)
X_sim <- sets_sim[[1]]
y_sim <- sets_sim[[2]]
Xtest_sim <- sets_sim[[3]]
ytest_sim <- sets_sim[[4]]

# train-test-split for XRF and PRE
train_data_sim <- cbind.data.frame(X_sim, y_sim)
test_data_sim <- cbind.data.frame(cbind(Xtest_sim, ytest_sim))

# expert knowledge (rules only)
expert_rules_sim <- simulation[[3]]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# a) ExpertRuleFit 

# Model w.o. expert knowledge
rf_sim <- ExpertRuleFit(X_sim, y_sim, Xtest_sim, ytest_sim, name_rules = F, print_output = T)

# Model Complexity
rf_sim$Nterms # nterms
rf_sim$AvgRuleLength # average rule length

# Predictive Accuray 
rf_sim$AUC # AUC
rf_sim$ClassErr # Classification Error

# Most important features
rf_sim$ImpTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Model (with optional expert knowledge)
erf_sim <- ExpertRuleFit(X_sim, y_sim, Xtest_sim, ytest_sim, name_rules = F,
                         expert_rules = expert_rules_sim, print_output = T)


# Model Complexity
erf_sim$Nterms # nterms
erf_sim$AvgRuleLength # average rule length

# Predictive Accuray 
erf_sim$AUC # AUC
erf_sim$ClassErr # Classification Error

# Most important features 
erf_sim$ImpTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Model (with confirmatory expert knowledge)
erf_sim_c <- ExpertRuleFit(X_sim, y_sim, Xtest_sim, ytest_sim, name_rules = F,
                         expert_rules = expert_rules_sim, 
                         confirmatory_rules = expert_rules_sim, 
                         print_output = T)
# Model Complexity
erf_sim_c$Nterms # nterms
erf_sim_c$AvgRuleLength # average rule length

# Predictive Accuray 
erf_sim_c$AUC # AUC
erf_sim_c$ClassErr # Classification Error

# Most important features
erf_sim_c$ImpTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# b) PRE

# Model
pre_sim <- pre(y_sim ~ ., data = train_data_sim, family = "binomial", type = "rules", 
               ntrees = 250)
pre_sim

# Model Complexity
pre_sim_coefs <- coef(pre_sim)
pre_sim_rel_coefs <- pre_sim_coefs$coefficient[pre_sim_coefs$coefficient != 0] #(non-zero) Coefficients

pre_sim_nterms <- length(pre_sim_rel_coefs)-1 # nterms

pre_sim_rules <- pre_sim_coefs$description[1:pre_sim_nterms]
pre_sim_avgrulelength <- average_rule_length(pre_sim_rules) #average rule length

# Predictive Accuracy
pre_sim_preds <- predict(pre_sim, newdata = test_data_sim, type = "class")
pre_sim_actual <- ytest_sim
auc <-  auc(ytest_sim, as.integer(pre_sim_preds)) # AUC
ce <-   ce(ytest_sim, as.integer(pre_sim_preds)) # Classification Error

# Most important features 
pre_sim_impterms <- pre_sim_rules[1:10]
pre_sim_impterms


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

