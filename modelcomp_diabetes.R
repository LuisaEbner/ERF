################################################################################
################################################################################

#            COMPARISON OF MODEL RESULTS  - DIABETES DATA                      #

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


# c) PRE

#pre(formula, data, family = "binomial", use.grad = TRUE,
#    tree.unbiased = TRUE, type = "both", sampfrac = 0.5, maxdepth = 3L,
#    learnrate = .01, confirmatory = NULL, mtry = Inf, ntrees = 500,
#    tree.control, removeduplicates = TRUE, removecomplements = TRUE,
#    winsfrac = 0.025, normalize = TRUE, standardize = FALSE,
#    ordinal = TRUE, nfolds = 10L, verbose = FALSE, par.init = FALSE,
#    par.final = FALSE, ...)


################################################################################

# 2. Diabetes Data

source("erf_main.R")
source("erf_diabetes_dataprep.R")

set.seed(12456)
# data
data_diab <- read.csv(file = 'diabetes.csv', header = T)
data_diab <- prepare_diabetes_data(data_diab)


# train-test-split for ERF
sets_diab <- create_X_y_Xtest_ytest(data_diab, 0.7, pos_class = 1)
X_diab <- sets_diab[[1]]
y_diab <- sets_diab[[2]]
Xtest_diab <- sets_diab[[3]]
ytest_diab <- sets_diab[[4]]

# train-test-split for XRF and PRE
train_data_diab <- cbind.data.frame(X_diab, y_diab)
test_data_diab <- cbind.data.frame(Xtest_diab, ytest_diab)

# Expert knowledge
# rules
expert_rules_diab <- c("Glucose>=80 & Glucose<=135", 
                       "Glucose>=135 & Glucose<=160", "Glucose>160",
                       "BloodPressure<100",
                       "BloodPressure>=100 & BloodPressure<=107",
                       "BloodPressure>107",
                       "BMI>=19 & BMI<=24", "BMI>=24 & BMI<=26", "BMI>26",
                       "BMI>=25", "Age>=45",
                       "Glucose>=144 & Glucose<=199")
# variables
linterms_diab <- c("Age", "BMI", "Glucose", "Insulin")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# a) ExpertRuleFit (w.o. expert knowledge)

# 1. Model
rf_diab <- ExpertRuleFit(X_diab, y_diab, Xtest_diab, ytest_diab, print_output = F)

# 2. Model Complexity
rf_diab$Nterms #nterms
rf_diab$AvgRuleLength # average rule length

# 3. Predictive Accuray 
rf_diab$AUC # AUC
rf_diab$ClassErr # Classification Error

# 4. Most important variables 
rf_diab$ImpTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# b) ExpertRuleFit (with expert_knowledge)

# 1.1 Model (with optional expert knowledge)
erf_diab <- ExpertRuleFit(X_diab, y_diab, Xtest_diab, ytest_diab, name_rules = T,
                         expert_rules = expert_rules_diab, name_lins = T,
                         linterms = colnames(X_diab), print_output = F)

# 1.2 Model (with confirmatory expert knowledge)
erf_diab_c <- ExpertRuleFit(X_diab, y_diab, Xtest_diab, ytest_diab, name_rules = T,
                           expert_rules = expert_rules_diab, 
                           confirmatory_rules = expert_rules_diab, 
                           name_lins = T, linterms = colnames(X_diab), 
                           confirmatory_lins = linterms_diab,
                           print_output = T)

# 2. Model Complexity
erf_diab$Nterms #nterms
erf_diab_c$Nterms 

erf_diab$AvgRuleLength # average rule length
erf_diab_c$AvgRuleLength

# 3. Predictive Accuray 
erf_diab$AUC # AUC
erf_diab_c$AUC 

erf_diab$ClassErr # Classification Error
erf_diab_c$ClassErr

# 4. Most important variables 
erf_diab$ImpTerms
erf_diab_c$ImpTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# c) Prediction Rule Ensembles (PRE)

# 1. Model
pre_diab <- pre(y_diab ~ ., data = train_data_diab, family = "binomial", 
                type = "both", ntrees = 250)

pre_diab
# 2. Model Complexity
pre_diab_coefs <- coef(pre_diab)
pre_diab_rel_coefs <- pre_diab_coefs$coefficient[pre_diab_coefs$coefficient != 0] # (non-zero) Coefficients
pre_diab_nterms <- length(pre_diab_rel_coefs) #nterms
pre_diab_nterms
pre_diab_rules <- pre_diab_coefs$description[1:pre_diab_nterms]
pre_diab_avgrulelength <- average_rule_length(pre_diab_rules) # average rule lengths
pre_diab_avgrulelength 
# 3. Predictive Accuracy
pre_diab_preds <- predict(pre_diab, newdata = test_data_diab, type = "class")
pre_diab_auc <-  auc(ytest_diab, as.integer(pre_diab_preds)) #AUC
pre_diab_auc
pre_diab_ce <-   ce(ytest_diab, as.integer(pre_diab_preds)) # Classification Error
pre_diab_ce

# 4. Most important features 
pre_diab_impterms <- pre_diab_rules[1:10] 
pre_diab_impterms
