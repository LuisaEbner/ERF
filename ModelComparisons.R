################################################################################
################################################################################

#                        COMPARISON OF MODEL RESULTS                           #

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

source("erf_main.R")
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

#' @name modelcomp
#' @description compares measures of model complexity, predictive accuracy, and expert knowledge usage across different ERF specifications and the PRE model
#' @param data initial, full dataset
#' @param train_frac fraction of training data in train-test-split
#' @param see all other params describe in function ExpertRuleFit
#' @return table of measures for comparison

modelcomp <- function(data, train_frac = 0.7, name_rules = T, expert_rules,
                      name_lins = T, 
                      linterms = NULL, type = "both"){
  
  # train-test-split for ERF
  sets <- create_X_y_Xtest_ytest(data, train_frac, pos_class = 1)
  X <- sets[[1]]
  y <- sets[[2]]
  Xtest <- sets[[3]]
  ytest <- sets[[4]]
  
  # train-test-split for XRF and PRE
  train <- cbind.data.frame(X, y)
  test <- cbind.data.frame(Xtest, ytest)
  
  # 1. ERF without expert knowledge
  rf <- ExpertRuleFit(X, y, Xtest, ytest, name_rules = name_rules, print_output = F)
  
  print("ERF without expert knowledge - check")
  
  # 2. ERF with optional expert knowledge
  erf_opt <- ExpertRuleFit(X, y, Xtest, ytest, name_rules = name_rules,
                           expert_rules = expert_rules, name_lins = name_lins,
                           linterms = linterms, print_output = F)
  
  print("ERF with optional expert knowledge - check")
  
  # 3. ERF with confirmatory expert knowledge
  erf_conf <- ExpertRuleFit(X, y, Xtest, ytest, name_rules = name_rules,
                            expert_rules = expert_rules, 
                            confirmatory_rules = expert_rules, 
                            name_lins = name_lins, linterms = linterms, 
                            confirmatory_lins = linterms,
                            print_output = F)
  
  print("ERF with confirmatory expert knowledge - check")
  
  # 4. PRE
  pre <- pre(y ~ ., data = train, family = "binomial", type = type, 
             ntrees = 250)
  
  print("PRE - check")
  
  # Model Measures
  pre_coefs <- coef(pre)
  pre_rel_coefs <- pre_coefs$coefficient[pre_coefs$coefficient != 0] 
  pre_nterms <- length(pre_rel_coefs) #nterms
  pre_rules <- pre_coefs$description[1:pre_nterms]
  pre_avgrl <- average_rule_length(pre_rules) # average rule lengths
  pre_preds <- predict(pre, newdata = test, type = "class")
  pre_auc <-  auc(ytest, as.integer(pre_preds)) #AUC
  pre_ce <-   ce(ytest, as.integer(pre_preds)) # Classification Error
  pre_impterms <- pre_rules[1:10] # important terms
  
  # Model Measures
  comp_table <- data.frame(ERF_no <- c(rf$Nterms, rf$AvgRuleLength, rf$AUC,
                                       rf$ClassErr, NA, NA),
                           ERF_opt <- c(erf_opt$Nterms, erf_opt$AvgRuleLength,
                                        erf_opt$AUC, erf_opt$ClassErr, 
                                        erf_opt$PropEK, erf_opt$PropEKImp),
                           ERF_conf <- c(erf_conf$Nterms, erf_conf$AvgRuleLength,
                                         erf_conf$AUC, erf_conf$ClassErr, 
                                         erf_conf$PropEK, erf_opt$PropEKImp),
                           PRE_no <- c(pre_nterms, pre_avgrl, pre_auc,
                                       pre_ce, NA, NA))
  rownames(comp_table) = c("number of terms", "average rule length", 
                           "AUC", "Class. Err.", "% ExpKnow", "ExpKnow/ImpTerms")
  colnames(comp_table) = c("ERF w.o. ExpKnow", "ERF, ExpKnow optional", 
                           "ERF, ExpKnow confirmatory", "PRE")
  
  # Important Terms
  comp_imps <- list(ERF_no_ExpKnow = rf$ImpTerms,
                          ERF_opt_ExpKnow = erf_opt$ImpTerms,
                          ERF_conf_ExpKnow = erf_conf$ImpTerms,
                          PRE_no_ExpKnow = pre_impterms)
  
  
  #print(comp_table)
  #print(comp_imps)
  
  out <- list(ModelMeasures = comp_table, ImportantTerms = comp_imps)
  out
}


################################################################################

# Example 1: Diabetes
source("erf_diabetes_dataprep.R")
set.seed(19)
# Data
data_diab <- read.csv(file = 'diabetes.csv', header = T)
data_diab <- prepare_diabetes_data(data_diab)

# Expert Knowledge
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

comp_diab <- modelcomp(data_diab, expert_rules = expert_rules_diab, 
                       linterms = linterms_diab)

comp_diab$ModelMeasures
comp_diab$ImportantTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example 2: Simulation

source("simulation.R")
set.seed(179)

simulation <- create_simulation(n_vars = 20, n_obs = 700,
                                n_rule_vars = 5, 
                                n_rel_rules = 10, mu_epsilon = 0.05)
# data
data_sim <- simulation[[2]]

# expert knowledge
expert_rules_sim <- simulation[[3]]

comp_sim <- modelcomp(data_sim, name_rules = F, expert_rules = expert_rules_sim, 
                      type = "rules")

comp_sim$ModelMeasures
comp_sim$ImportantTerms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example 3: Cervical cancer

source("erf_cancer_dataprep.R")

# data
data_cancer <- read.csv(file = 'risk_factors_cervical_cancer.csv', header = T)
data_cancer <- prepare_cervicalcancer_data(data = data_cancer, 
                                           del_maj_missing = T,
                                           add_NA_features = F, 
                                           impute_missing = T,
                                           target = "Biopsy", balance = T)

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


comp_cancer <- modelcomp(data_cancer, expert_rules = expert_rules_cancer, 
                       linterms = linterms_cancer)

comp_cancer$ModelMeasures
comp_cancer$ImportantTerms$PRE_no_ExpKnow

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~