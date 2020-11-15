################################################################################
################################################################################
###                                                                          ###
###           EXPERT RULE FIT (ERF) for Cervical Cancer                      ###
###                                                                          ###
################################################################################
################################################################################

# Libraries
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

# External functions
source("erf_cancer_dataprep.R")
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

# Cervical Cancer (Risk Factors) dataset available from the UCI ML Repository
data <- read.csv(file = 'risk_factors_cervical_cancer.csv', header = T)

# Preprocessing (see 'erf_cancer_dataprep.R')
data <- prepare_cervicalcancer_data(data = data, balance = T)


#===============================================================================
#                        1.2. FACTUAL DOMAIN KNOWLEDGE
#===============================================================================

# a) Rules extracted from Chapter 3.4, Risk Factors and Disease Development
#      of the German oncology guideline: 'S3-Leitlinie Diagnostik und Therapie 
#      Zervixkarzinom' (LOKZ) 

# Guideline text
# 1. Infection with the human papilloma virus (mainly type 16 + 18)
# 2. Smoking (>15 cigarettes per day)
# 3. Immuno-compromised patients (HIV, medication)
# 4. Early start of sexual activity (< 14 years of age)
# 5. Frequently changing sexual partners (> 4 in 10 years)
# 6. Other infections (e.g. genital herpes, chlamydia etc.) 
# 7. Long-term use oral contraceptives (> 5 years)


# Corresponding expert rules
dk_rules1 <- c("STDs.HPV == 1", # STDs.HPV == 1
               "Smoking ==1 ", #Smoking == 1
               "STDs.HIV == 1",
               "STDs.Hepatitis.B == 1",
               "First.sexual.intercourse<14",
               "(((Age - First.sexual.intercourse)*Number.of.sexual.partners)/10)>4",
               "(STDs.genital.herpes+STDs.molluscum.contagiosum+
               STDs.pelvic.inflammatory.disease+STDs.syphilis)>=1", 
               "Hormonal.Contraceptives..years.>5")



# b) Rules Extracted from 'The American Cancer Society Guidelines for the
#    Prevention and Early Detection of Cervical Cancer' (ACS-CC) 

# Guideline text
# 1. Becoming sexually active at a young age (especially < 18)
# 2. Young age at first full-term pregnancy (< 20)
# 3. Having multiple full-term pregnancies (3 or more)
# 4. Intrauterine device use (risk lowering factor)


# Corresponding expert rules
dk_rules2 <- c("First.sexual.intercourse<18",
               "Age<20 & Num.of.pregnancies>=1",
               "Num.of.pregnancies>=3",
               "Intra.uterine.device == 1")


# c) Linear Terms extracted from both guidelines

# Guideline text
# 1. number of full-term pregnancies (LOZK)
# 2. number of sexual partners (ACS-CC)

# Corresponding linear terms
dk_lins <- c( "Num.of.pregnancies", "Number.of.sexual.partners",
              "Age",  "First.sexual.intercourse", 
              "Hormonal.Contraceptives..years.") 


#===============================================================================
#                        1.3. HEURISTIC EXPERT KNOWLEDGE
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
conf_dk_rules1
opt_dk_rules1 <- setdiff(dk_rules1, conf_dk_rules1)

conf_dk_rules2 <- support_take(dk_rules2, data, 0.05)
conf_dk_rules2
opt_dk_rules2 <- setdiff(dk_rules2, conf_dk_rules2)

optional_rules <- c(opt_dk_rules1, opt_dk_rules2)
confirmatory_rules <- c(conf_dk_rules1, conf_dk_rules2)

# c) Expert Linear Terms
optional_linears <- dk_lins

#===============================================================================
#                         2.2. Model Application
#===============================================================================


erf_cancer <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                              optional_expert_rules = optional_rules, 
                              confirmatory_expert_rules = confirmatory_rules,  
                              optional_linear_terms=optional_linears,
                              corelim = 0.9)


#===============================================================================
#                        3. MODEL COMPARISONS
#===============================================================================

# a) ExpertRuleFit  w.o. expert knowledge
rf_cancer <- ExpertRuleFit(X, y, Xtest, ytest, print_output = F)

# Complexity
rf_cancer$NTerms 
rf_cancer$AvgRuleLength 

# Accuracy 
rf_cancer$AUC
rf_cancer$ClassErr

# Important features
rf_cancer$ImportantFeatures

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
pre_cancer <- pre(y ~ ., data = train , family = "binomial", 
                  type = "rules", ntrees = 250)

# Complexity
rel_coefs <- coef(pre_cancer)$coefficient[coef(pre_cancer)$coefficient != 0] 
nterms <- length(rel_coefs) 

rules <- coef(pre_cancer)$description
avgrulelength <- average_rule_length(rules) 

# Accuracy
preds <- predict(pre_cancer, newdata = test, type = "class")
auc <-  auc(ytest, as.integer(preds)) 
ce <-   ce(ytest, as.integer(preds)) 

# Important features 
impfeatures <- rules[1:10] 

#===============================================================================




