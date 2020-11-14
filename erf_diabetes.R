################################################################################
################################################################################
###                                                                          ###
###                 EXPERT RULE FIT (ERF) for Diabetes                       ###
###                                                                          ###
################################################################################
################################################################################

# External functions
source("erf_diabetes_dataprep.R")
source("erf_main.R")

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
data <- read.csv(file = 'diabetes.csv', header = T)

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

# interview information following

#===============================================================================
#                               2. ERF MODEL
#===============================================================================
#                             2.1. MODEL INPUT
#===============================================================================

# a) Training data
sets <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

# b) Expert Rules
conf_dk_rules1 <- support_take(dk_rules1, data, 0.05)
opt_dk_rules1 <- setdiff(dk_rules1, conf_dk_rules1)

conf_dk_rules2 <- support_take(dk_rules2, data, 0.05)
opt_dk_rules2 <- setdiff(dk_rules2, conf_dk_rules2)

optional_rules <- c(opt_dk_rules1, opt_dk_rules2)
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
                              corelim = 0.9)

#===============================================================================
#                        3. MODEL COMPARISONS
#===============================================================================

# a) ExpertRuleFit  w.o. expert knowledge
rf_diabetes <- ExpertRuleFit(X, y, Xtest, ytest, print_output = F)

# Complexity
rf_diabetes$NTerms 
rf_diabetes$AvgRuleLength 

# Accuracy 
rf_diabetes$AUC
rf_diabetes$ClassErr

# Important features
rf_diabetes$ImportantFeatures

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
pre_diabetes <- pre(y ~ ., data = train , family = "binomial", 
                  type = "rules", ntrees = 250)

# Complexity
rel_coefs <- coef(pre_diabetes)$coefficient[coef(pre_diabetes)$coefficient != 0] 
nterms <- length(rel_coefs) 

rules <- rel_coefs$description[1:nterms]
avgrulelength <- average_rule_length(rules) 

# Accuracy
preds <- predict(pre_diabetes, newdata = test, type = "class")
auc <-  auc(ytest, as.integer(preds)) 
ce <-   ce(ytest, as.integer(preds)) 

# Important features 
impfeatures <- rules[1:10] 

#===============================================================================



