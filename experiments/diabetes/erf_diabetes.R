################################################################################
################################################################################
###                                                                          ###
###                 EXPERT RULE FIT (ERF) for Diabetes                       ###
###                                                                          ###
################################################################################
################################################################################

# External functions
source("./experiments/diabetes/erf_diabetes_dataprep.R")
source("./ERF/erf_main.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Information Sources

# 1.1. Training data
# 1.2. Expert knowledge

# 2. ERF Model

# 2.1. Model input
# 2.2. Model application

# 3. Model Comparisons

#===============================================================================
#                             1.1. TRAINING DATA
#===============================================================================


## Pima Indian Diabetes dataset available from UCI ML Repository.
## http://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes
data <- read.csv(file = './data sets/diabetes.csv', header = T)

# Data Preprocessing (see file 'erf_diabetes_dataprep.R')
data <- prepare_diabetes_data(data)


#===============================================================================
#                           1.2. EXPERT KNOWLEDGE
#===============================================================================

source("./expert knowledge/diabetes/EK_diabetes.R")

#===============================================================================
#                              2 ERF MODEL
#===============================================================================


# a) Training data

train.index <- createDataPartition(data$y, p = 0.7, list = FALSE)
train <- data[ train.index,]
test  <- data[-train.index,]
X <- train[, -ncol(train)]
y <- train$y
Xtest <- test[, -ncol(test)]
ytest <- test$y


# b) Expert Rules
rules <- c(fdk_rules1, fdk_rules2, hek_rules)
conf_rules <- support_take(rules, data, 0.05)
opt_rules <- setdiff(rules, conf_rules)


# c) Expert Linear Terms

# contained in both linear EK formulations
conf_linear <- c("Age", "BMI", "DPF")

# contained only in the FDK
opt_linear <- c("BP", "Glucose")


#===============================================================================
#                         2.2. Model Application
#===============================================================================

erf_diabetes <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                              confirmatory_expert_rules = conf_rules, 
                              optional_expert_rules = opt_rules, 
                              optional_linear_terms= opt_linear,
                              confirmatory_linear_terms = conf_linear,
                              expert_only = T, print_output = T)


#===============================================================================
#                        3. MODEL COMPARISONS
#===============================================================================

erf1 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                      optional_expert_rules = opt_rules, 
                      confirmatory_expert_rules = conf_rules,  
                      optional_linear_terms=opt_linear,
                      confirmatory_linear_terms = conf_linear, 
                      print_output = F)

erf2 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                      print_output = F)

erf3 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                      confirmatory_expert_rules = conf_rules,
                      print_output = F)

pre1 <- pre_for_comparison(train, test)

# performance comparison
performance_comparison <- modelcomp(erf1 = erf1, erf2 = erf2 , erf3 = erf3, 
                                    pre1 = pre1, pre2 = NULL, pre3 = NULL)


# cross validated performance
CV_erf(data = data, optional_expert_rules = optional_rules, 
       confirmatory_expert_rules = confirmatory_rules,  
       optional_linear_terms=optional_linears,
       confirmatory_linear_terms = confirmatory_linears)


