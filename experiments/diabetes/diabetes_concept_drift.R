################################################################################
#                                                                              #
#         Predicting Diabetes: Experiments on the concept drift                #   
#                                                                              #
################################################################################


# External functions
source("./experiments/diabetes/erf_diabetes_dataprep.R")
source("./ERF/erf_main.R")


# 1. DATA SET

# Pima Indian Diabetes (UCI ML Repository)
data <- read.csv(file = './data sets/diabetes.csv', header = T)

# Data Preprocessing 
data <- prepare_diabetes_data(data)

# 2. EXPERT KNOWLEDGE

source("./expert knowledge/diabetes/EK_diabetes.R")

# 3. ERF MODEL

# a) Training data

train.index <- createDataPartition(data$y, p = 0.7, list = FALSE)
train <- data[ train.index,]
test  <- data[-train.index,]
X <- train[, -ncol(train)]
y <- train$y
Xtest <- test[, -ncol(test)]
ytest <- test$y


# b) Expert Rules
conf_dk_rules1 <- support_take(dk_rules1, data, 0.05)
opt_dk_rules1 <- setdiff(dk_rules1, conf_dk_rules1)

conf_dk_rules2 <- support_take(dk_rules2, data, 0.05)
opt_dk_rules2 <- setdiff(dk_rules2, conf_dk_rules2)

optional_rules <- c(opt_dk_rules1, opt_dk_rules2)


optional_rules
confirmatory_rules
confirmatory_rules <- c(conf_dk_rules1, conf_dk_rules2)


# c) Expert Linear Terms
optional_linears <- c("BP", "Glucose")
confirmatory_linears <- c("Age", "BMI", "DPF")

#===============================================================================
#                         2.2. Model Application
#===============================================================================

erf_diabetes <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                              optional_expert_rules = optional_rules, 
                              confirmatory_expert_rules = confirmatory_rules,  
                              optional_linear_terms=optional_linears,
                              confirmatory_linear_terms = confirmatory_linears,
                              expert_only = T, print_output = T)


erf_diabetes$NTerms
erf_diabetes$AvgRuleLength
erf_diabetes$AUC
erf_diabetes$ClassErr
erf_diabetes$PropEKImp
erf_diabetes$PropEK
erf_diabetes$PropOptionalEK

cv_measures <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr", "PropEKImp", "PropEK", "PropOptionalEK")
res <- matrix(0, 10, length(cv_measures))
for(k in cv_measures){
  res[1, k] <- erf_diabetes[[k]]
}

res[1, 1]
#===============================================================================
#                        3. MODEL COMPARISONS
#===============================================================================

erf1 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                      optional_expert_rules = optional_rules, 
                      confirmatory_expert_rules = confirmatory_rules,  
                      optional_linear_terms=optional_linears,
                      confirmatory_linear_terms = confirmatory_linears, 
                      print_output = F)

erf2 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                      print_output = F, s = "lambda.1se")

erf3 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                      confirmatory_expert_rules = confirmatory_rules,
                      print_output = F, s = "lambda.1se")

pre1 <- pre_for_comparison(train, test)

performance_comparison <- modelcomp(erf1 = erf1, erf2 = erf2 , erf3 = erf3, 
                                    pre1 = pre1, pre2 = NULL, pre3 = NULL)


cross_validated <- CV_erf(data = data, optional_expert_rules = optional_rules, 
                          confirmatory_expert_rules = confirmatory_rules,  
                          optional_linear_terms=optional_linears,
                          confirmatory_linear_terms = confirmatory_linears)


