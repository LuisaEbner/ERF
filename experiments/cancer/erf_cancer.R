################################################################################
################################################################################
###                                                                          ###
###           EXPERT RULE FIT (ERF) for Cervical Cancer                      ###
###                                                                          ###
################################################################################
################################################################################

# Libraries
# library(dplyr)
# library(pre)
# library(glmnet)
# library(xgboost)
# library(RCurl)
# library(xrf)
# library(pROC)
# library(ROCit)
# library(ROCR)
# library(mice)

# External functions
source("./experiments/cancer/erf_cancer_dataprep.R")
source("./ERF/erf_main.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Information sources

# 1. Training data
# 2. Expert Knowledge (FDK+HEK)

# ERF Model

# 1. Model input
# 2. Model application
# 3. Model Comparisons

#===============================================================================
#                             INFORMATION SOURCES
#===============================================================================

# 1. Training Data = Cervical Cancer (Risk Factors)

data <- read.csv(file = './data sets/risk_factors_cervical_cancer.csv', header = T)
data <- prepare_cervicalcancer_data(data = data, balance = F)


# 2. Expert Knowledge

source("./expert knowledge/cancer/EK_cancer.R")

#===============================================================================
#                              2 ERF MODEL
#===============================================================================

# a) Train/Test sets

sets <- createERFsets(data, 0.7)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

# b) Expert Rules

rules <- c(fdk_rules1, fdk_rules2)
conf_rules <- support_take(rules, data, 0.05)
opt_rules <- setdiff(rules, conf_rules)


# c) Expert Linear Terms

conf_linear <- fdk_linear

#===============================================================================
#                         2.2. Model Application
#===============================================================================

erf_cancer <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                              confirmatory_expert_rules = conf_rules, 
                              optional_expert_rules = opt_rules, 
                              confirmatory_linear_terms = conf_linear,
                              print_output = T)


erf_cancer2 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                              optional_expert_rules = opt_rules, 
                              confirmatory_linear_terms = conf_linear,
                              optional_penalty = 0.9,
                              print_output = T)


erf_cancer3 <- ExpertRuleFit(X, y, Xtest, ytest, print_output = T)


# ERF Alternative: Prediction Rule Ensembles (pre package)

train <- cbind.data.frame(X, y)
test <- cbind.data.frame(Xtest, ytest)

# Model
pre_cancer <- pre(y ~ ., data = train , family = "binomial", 
                  type = "rules", ntrees = 250)

pre_cancer

#===============================================================================




