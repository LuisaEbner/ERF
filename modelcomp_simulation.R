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

################################################################################

# RULEFIT IMPLEMENTATION OPTIONS

# a) ExpertRuleFit

#ExpertRuleFit(X=NULL, y=NULL, Xtest=NULL, ytest=NULL,
#              name_rules = T, expert_rules = NULL, confirmatory_rules = NULL,
#              name_lins = T, linterms=NULL, confirmatory_lins = NULL,
#              ntree=250, ensemble= "GBM", mix=0.5, L=4, S=6, minsup=.025, 
#              intercept=T, corelim = 1, 
#              alpha = 1, nfolds = 10, type.measure = "class",
#              s = "lambda.min", dfmax = 500, pmax = 500, standardize = F, 
#              print_output = T) {

# b) XRF


#xrf(object, data, family = "binomial", xgb_control = list(nrounds = 250, max_depth = 3),
#  glm_control = list(type.measure = "auc", nfolds = 10), sparse = TRUE,
#  prefit_xgb = NULL, deoverlap = FALSE, ...)



# c) PRE

#pre(formula, data, family = "binomial", use.grad = TRUE,
#    tree.unbiased = TRUE, type = "both", sampfrac = 0.5, maxdepth = 3L,
#    learnrate = .01, confirmatory = NULL, mtry = Inf, ntrees = 500,
#    tree.control, removeduplicates = TRUE, removecomplements = TRUE,
#    winsfrac = 0.025, normalize = TRUE, standardize = FALSE,
#    ordinal = TRUE, nfolds = 10L, verbose = FALSE, par.init = FALSE,
#    par.final = FALSE, ...)


################################################################################

# Comparisons for simulated Data

# 1. Simulated Data
source("simulation.R")
source("erf_main.R")

set.seed(179)
simulation <- create_simulation(n_vars = 100, n_obs = 1000,
                                n_rule_vars = 10, 
                                n_rel_rules = 20)
data_sim <- simulation[[2]]
sets_sim <- create_X_y_Xtest_ytest(data_sim, 0.7, pos_class = 1)
# data for ERF
X_sim <- sets_sim[[1]]
y_sim <- sets_sim[[2]]
Xtest_sim <- sets_sim[[3]]
ytest_sim <- sets_sim[[4]]
# data for XRF and PRE
train_data_sim <- cbind.data.frame(X_sim, y_sim)
test_data_sim <- cbind.data.frame(cbind(Xtest_sim, ytest_sim))
# expert knowledge
expert_rules_sim <- simulation[[3]]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# a) ExpertRuleFit (w.o. expert knowledge)

# Model
rf_sim <- ExpertRuleFit(X_sim, y_sim, Xtest_sim, ytest_sim, print_output = T)

# Model Complexity

# nterms
rf_sim$Nterms
# average rule length
rf_sim$AvgRuleLength

# Predictive Accuray 

# AUC
rf_sim$AUC
# Classification Error
rf_sim$ClassErr

# Most important variables 
rf_sim$ImpTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# b) ExpertRuleFit 

# Model (with optional expert knowledge)
erf_sim <- ExpertRuleFit(X_sim, y_sim, Xtest_sim, ytest_sim, name_rules = F,
                         expert_rules = expert_rules_sim, print_output = T)

# Model (with confirmatory expert knowledge)
erf_sim_c <- ExpertRuleFit(X_sim, y_sim, Xtest_sim, ytest_sim, name_rules = F,
                         expert_rules = expert_rules_sim, 
                         confirmatory_rules = expert_rules_sim, 
                         print_output = T)
# Model Complexity

# nterms
erf_sim$Nterms
# average rule length
erf_sim$AvgRuleLength

# Predictive Accuray 

# AUC
erf_sim$AUC
# Classification Error
erf_sim$ClassErr

# Most important variables 
erf_sim$ImpTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# b) pre

# Model
pre_sim <- pre(y_sim ~ ., data = train_data_sim, family = "binomial", type = "rules", 
               ntrees = 250)
pre_sim

# Model Complexity
# (non-zero) Coefficients
pre_sim_coefs <- coef(pre_sim)
pre_sim_rel_coefs <- pre_sim_coefs$coefficient[pre_sim_coefs$coefficient != 0]
pre_sim_rel_coefs
# a) nterms
pre_sim_nterms <- length(pre_sim_rel_coefs)
pre_sim_nterms
# b) rule lengths
pre_sim_rules <- pre_sim_coefs$description[1:pre_sim_nterms]
pre_sim_avgrulelength <- average_rule_length(pre_sim_rules)
pre_sim_avgrulelength

# Predictive Accuracy
pre_sim_preds <- predict(pre_sim, newdata = test_data_sim, type = "class")
pre_sim_actual <- ytest_sim

# AUC
auc <-  auc(ytest_sim, as.integer(pre_sim_preds))
auc
# Classification Error
ce <-   ce(ytest_sim, as.integer(pre_sim_preds))
ce
# cross validated prediction error
sim.cv <- cvpre(pre_sim)
sim.cv$accuracy

# Most important features 
pre_sim_impterms <- pre_sim_rules[1:10]
pre_sim_impterms


################################################################################
#                              COMPARISONS                                     #
################################################################################

# 1. Predictions (percentage of equal predictions)
# 2. ClassErr, AUC, Sens, Spez
# 3. nterms
# 4. average rule length
# 5. Most important variables
# 6. Most important terms

################################################################################




