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
# 1.2. Factual domain knowledge from medical guidelines
# 1.3. Heuristic expert knowledge

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
#                        1.2. FACTUAL DOMAIN KNOWLEDGE
#===============================================================================

# 1. Rules extracted from the 'Diabetes Risk Test' within the
#    'Standards of Medical Care in Diabetes - 2019' 
#    published by the American Diabetes Association

dk_rules1 <-  c("Age<=39 & BP<=80 & BMI<25",
                "Age>=40 & Age<=49 & BP<=80 & BMI<25", 
                "Age>=50 & Age<=59 & BP<=80 & BMI<25", 
                "Age>=60 & BP<=80 & BMI<25", 
                "Age<=39 & BP>=81 & BMI<25", 
                "Age>=40 & Age<=49 & BP>=81 & BMI<25", 
                "Age>=50 & Age<=59 & BP>=81 & BMI<25", 
                "Age>=60 & BP>=81 & BMI<25", 
                "Age<=39 & BP<=80 & BMI>=25 & BMI<=30",
                "Age>=40 & Age<=49 & BP<=80 & BMI>=25 & BMI<=30", 
                "Age>=50 & Age<=59 & BP<=80 & BMI>=25 & BMI<=30", 
                "Age>=60 & BP<=80 & BMI>=25 & BMI<=30", 
                "Age<=39 & BP>=81 & BMI>=25 & BMI<=30", 
                "Age>=40 & Age<=49 & BP>=81 & BMI>=25 & BMI<=30", 
                "Age>=50 & Age<=59 & BP>=81 & BMI>=25 & BMI<=30", 
                "Age<=39 & BP<=80 & BMI>=31 & BMI<=40", 
                "Age>=40 & Age<=49 & BP<=80 & BMI>=31 & BMI<=40", 
                "Age>=50 & Age<=59 & BP<=80 & BMI>=31 & BMI<=40", 
                "Age<=39 & BP>=81 & BMI>=31 & BMI<=40", 
                "Age>=40 & Age<=49 & BP>=81 & BMI>=31 & BMI<=40", 
                "Age<=39 & BP<=80 & BMI>40", 
                "Age>=40 & Age<=49 & BP<=80 & BMI>40",
                "Age>=60 & BP>=81 & BMI>=25 & BMI<=30",
                "Age>=60 & BP<=80 & BMI>=31 & BMI<=40",
                "Age>=50 & Age<=59 & BP>=81 & BMI>=31 & BMI<=40",
                "Age>=60 & BP>=81 & BMI>=31 & BMI<=40", 
                "Age>=50 & Age<=59 & BP<=80 & BMI>40", 
                "Age>=60 & BP<=80 & BMI>40", 
                "Age<=39 & BP>=81 & BMI>40", 
                "Age>=40 & Age<=49 & BP>=81 &  BMI>40", 
                "Age>=50 & Age<=59 &  BP>=81 & BMI>40", 
                "Age>=60 & BP>=81 & BMI>40")


# 2. Rules Extracted from Chapter 3, Therapeutic Goals of the
#    Nationale Versorgungs-Leitlinie Diabetes Mellitus Typ 2

dk_rules2 <- c("Glucose<=100", 
               "Glucose>100 & Glucose<=110", 
               "Glucose>110",
               "BP<=85",
               "BP>85 & BP<=90",
               "BP>90",
               "BMI<=24",
               "BMI<24 & BMI<=26", 
               "BMI>26", 
               "Glucose>110 & BP>90",
               "Glucose>110 & BMI>26",
               "BP>90 & BMI>26",
               "Glucose>110 & BP>90 & BMI>26")


# Linear terms extracted from both guidelines

opt_dk_lins12 <- c("BP", "Glucose")
conf_dk_lins12 <- c("Age", "BMI", "DPF")

#===============================================================================
#                         HEURISTIC EXPERT KNOWLEDGE
#===============================================================================

exp_interview_rules <- c("Age<=42 & BP<=80 & BMI<=29",
                         "Age>=45 & BP>=90 & Glucose>=125",
                         "Age<=31 & BP>=90 & BMI>=38",
                         "Age>=55 & BP<=80 & BMI<=29",
                         "Age>=60 & Glucose>=130 & BMI>=35", 
                         "Age>=60 & BP>=90 & BMI>=37",
                         "Age>=45 & BP>=90 & BMI>=35 & Glucose>=130",
                         "Age>=55 & BP<=90 & BMI<=30 & Glucose>=130",
                         "Age<=60 & BP<=90 & BMI<=30 & Glucose<=100")

exp_interview_lins <- c("BMI", "Age", "BP", "Glucose")

#===============================================================================
#                               2. ERF MODEL
#===============================================================================
#                             2.1. MODEL INPUT
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
#===============================================================================

# Cross Validation:

CV_erf <- function(data, cv_folds = 10, seed = 1432, intercept=T,
                   optional_expert_rules = NULL, confirmatory_expert_rules = NULL,  
                   optional_linear_terms=NULL, confirmatory_linear_terms = NULL,
                   expert_only = F, optional_penalty = 1,
                   ntree=250, ensemble = "GBM", mix=0.5, L=3, S=6, minsup=.025,
                   corelim = 1, alpha = 1, s = "lambda.1se", 
                   n_imp = 10){
  cv_measures <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr", "PropEKImp", "PropEK", "PropOptionalEK")
  n_measures <- length(cv_measures)
  res <- matrix(0, cv_folds, n_measures)
  
  set.seed(seed)
  
  ids = sample(1:nrow(data))
  fold = rep(1:10, length.out = nrow(data))
  target_col = ncol(data)
  y = as.factor(data[, target_col])
  x = data[,-target_col]
  for(i in 1:cv_folds){
    xtrain = x[ids[fold != i], ]
    ytrain = as.factor(y[ids[fold != i]])
    xtest = x[ids[fold == i], ]
    ytest = as.factor(y[ids[fold == i]])
    model <- ExpertRuleFit(X=xtrain, y = ytrain, Xtest = xtest, ytest = ytest, intercept = intercept,
                           optional_expert_rules = optional_expert_rules, 
                           confirmatory_expert_rules = confirmatory_expert_rules,
                           optional_linear_terms = optional_linear_terms, 
                           confirmatory_linear_terms = confirmatory_linear_terms,
                           expert_only = expert_only, optional_penalty = optional_penalty,
                           ntree = ntree, ensemble = ensemble, mix = mix, L = L, S = S, minsup = minsup, 
                           corelim = corelim, alpha = alpha, s = s, n_imp = n_imp,
                           print_output = F)
    
    for(k in 1:length(cv_measures)){
      res[i, k] <- model[[cv_measures[k]]]
    }
  }
  
  #print(res)
  cv_res <- colMeans(res)
  
  #print(cv_res)
  
  out = list(NTerms = cv_res[1], AvgRuleLength = cv_res[2], AUC = cv_res[3], 
             ClassErr = cv_res[4], PropEKImp = cv_res[5], PropEK = cv_res[6],
             PropOptionalEK = cv_res[7])
  
  out
  
}

# Example:

CV_erf(data = data, optional_expert_rules = optional_rules, 
       confirmatory_expert_rules = confirmatory_rules,  
       optional_linear_terms=optional_linears,
       confirmatory_linear_terms = confirmatory_linears)


