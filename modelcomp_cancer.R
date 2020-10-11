################################################################################
################################################################################

#         COMPARISON OF MODEL RESULTS  - CERVICAL CANCER DATA                  #

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

# Cervical Cancer Data

source("erf_main.R")
source("erf_cancer_dataprep.R")

# data
data_cancer <- read.csv(file = 'risk_factors_cervical_cancer.csv', header = T)
data_cancer <- prepare_cervicalcancer_data(data = data_cancer, 
                                           del_maj_missing = T,
                                           add_NA_features = F, 
                                           impute_missing = T,
                                           target = "Biopsy", balance = T)

# train-test-split for ERF
sets_cancer <- create_X_y_Xtest_ytest(data_cancer, 0.7, pos_class = 1)
X_cancer <- sets_cancer[[1]]
y_cancer <- sets_cancer[[2]]
Xtest_cancer <- sets_cancer[[3]]
ytest_cancer <- sets_cancer[[4]]

# train-test-split for PRE
train_data_cancer <- cbind.data.frame(X_cancer, y_cancer)
test_data_cancer <- cbind.data.frame(Xtest_cancer, ytest_cancer)

# Expert knowledge

# rules
expert_rules_cancer <- c("First.sexual.intercourse<18",
                       "Number.of.sexual.partners>2",
                       "Smokes==1 & Smokes..years.>1",
                       "Hormonal.Contraceptives==1 & Hormonal.Contraceptives..years.>5",
                       "Num.of.pregnancies>=3",
                       "Age<=20 & Num.of.pregnancies>=1",
                       "IUD==0", "STDs.HPV==1",
                       "Smokes==1 & Smokes..packs.year.>=15",
                       "First.sexual.intercourse<14",
                       "Number.of.sexual.partners>4",
                       "STDs.genital.herpes==1", 
                       "Hormonal.Contraceptives==1 & Hormonal.Contraceptives..years.>=5 & Hormonal.Contraceptives..years.<=9",
                       "Hormonal.Contraceptives==1 & Hormonal.Contraceptives..years.>=10",
                       "Num.of.pregnancies>1")
# variables
linterms_cancer <- c("Number.of.sexual.partners", "Age", "Num.of.pregnancies", 
                   "First.sexual.intercourse", "Hormonal.Contraceptives..years.")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# a) ExpertRuleFit (w.o. expert knowledge)

# 1. Model
rf_cancer <- ExpertRuleFit(X_cancer, y_cancer, Xtest_cancer, ytest_cancer, 
                           print_output = F)

# 2. Model Complexity
rf_cancer$Nterms #nterms
rf_cancer$AvgRuleLength # average rule length

# 3. Predictive Accuray 
rf_cancer$AUC # AUC
rf_cancer$ClassErr # Classification Error

# 4. Most important variables 
rf_cancer$ImpTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# b) ExpertRuleFit (with expert_knowledge)

# 1.1 Model (with optional expert knowledge)
erf_cancer <- ExpertRuleFit(X_cancer, y_cancer, Xtest_cancer, ytest_cancer, 
                            name_rules = F, expert_rules = expert_rules_cancer,
                            print_output = T)

# 1.2 Model (with confirmatory expert knowledge)
erf_cancer_c <- ExpertRuleFit(X_cancer, y_cancer, Xtest_cancer, ytest_cancer,
                              name_rules = F, expert_rules = expert_rules_cancer, 
                              confirmatory_rules = expert_rules_cancer, 
                              print_output = T)

# 2. Model Complexity
erf_cancer$Nterms #nterms
erf_cancer_c$Nterms 

erf_cancer$AvgRuleLength # average rule length
erf_cancer_c$AvgRuleLength

# 3. Predictive Accuray 
erf_cancer$AUC # AUC
erf_cancer_c$AUC 

erf_cancer$ClassErr # Classification Error
erf_cancer_c$ClassErr

# 4. Most important variables 
erf_cancer$ImpTerms
erf_cancer_c$ImpTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# c) Prediction Rule Ensembles (PRE)

# 1. Model
pre_cancer <- pre(y_cancer ~ ., data = train_data_cancer, family = "binomial", 
                type = "rules", ntrees = 250)

# 2. Model Complexity
pre_cancer_rel_coefs <- coef(pre_cancer)$coefficient[coef(pre_cancer)$coefficient != 0] # (non-zero) Coefficients
pre_cancer_nterms <- length(pre_cancer_rel_coefs) #nterms

pre_cancer_rules <- pre_cancer_coefs$description[1:pre_cancer_nterms]
pre_cancer_avgrulelength <- average_rule_length(pre_cancer_rules) # average rule lengths

# 3. Predictive Accuracy
pre_cancer_preds <- predict(pre_cancer, newdata = test_data_cancer, type = "class")
pre_cancer_auc <-  auc(ytest_cancer, as.integer(pre_cancer_preds)) #AUC
pre_cancer_ce <-   ce(ytest_cancer, as.integer(pre_cancer_preds)) # Classification Error

# 4. Most important features 
pre_cancer_impterms <- pre_cancer_rules[1:10] 

