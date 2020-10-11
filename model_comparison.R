################################################################################
################################################################################

#                     COMPARISON OF MODEL RESULTS                              #

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

# DATASETS

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



# 2. Diabetes Data

source("erf_diabetes_dataprep.R")
data_diab <- read.csv(file = 'diabetes.csv', header = T)
data_diab <- prepare_diabetes_data(data_diab)
sets_diab <- create_X_y_Xtest_ytest(data_diab, 0.7, pos_class = 1)
# data for ERF
X_diab <- sets_diab[[1]]
y_diab <- sets_diab[[2]]
Xtest_diab <- sets_diab[[3]]
ytest_diab <- sets_diab[[4]]
# data for XRF and PRE
train_data_diab <- cbind.data.frame(X_diab, y_diab)
test_data_diab <- cbind.data.frame(Xtest_diab, ytest_diab)
# Expert knowledge
expert_rules_diab <- c("Glucose>=80 & Glucose<=135", 
                       "Glucose>=135 & Glucose<=160", "Glucose>160",
                       "BloodPressure<100",
                       "BloodPressure>=100 & BloodPressure<=107",
                       "BloodPressure>107",
                       "BMI>=19 & BMI<=24", "BMI>=24 & BMI<=26", "BMI>26",
                       "BMI>=25", "BloodPressure>=107", "Age>=45",
                       "Glucose>=144 & Glucose<=199")
linterms_diab <- c("Age", "BMI", "Glucose", "Insulin")



# 3. Cervical Cancer Data

source("erf_cancer_dataprep.R")
data_canc <- read.csv(file = 'risk_factors_cervical_cancer.csv', header = T)
data_canc <- prepare_cervicalcancer_data(data = data_canc, del_maj_missing = T,
                                    add_NA_features = F, impute_missing = T,
                                    target = "Biopsy", balance = T)
sets_canc <- create_X_y_Xtest_ytest(data_canc, 0.7, pos_class = 1)
# data for ERF
X_canc <- sets_canc[[1]]
y_canc <- sets_canc[[2]]
Xtest_canc <- sets_canc[[3]]
ytest_canc <- sets_canc[[4]]
# data for XRF and PRE
train_data_canc <- cbind.data.frame(X_canc, y_canc)
test_data_canc <- cbind.data.frame(Xtest_canc, ytest_canc)
# Expert knowledge
expert_rules_canc <- c("First.sexual.intercourse<18",
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
linterms_canc <- c("Number.of.sexual.partners", "Age", "Num.of.pregnancies", 
                  "First.sexual.intercourse", "Hormonal.Contraceptives..years.")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. applied to simulated data

# a) ExpertRuleFit (w.o. expert knowledge)

# Model
erf_sim <- ExpertRuleFit(X_sim, y_sim, Xtest_sim, ytest_sim, print_output = T)


#erf_sim$Coefficients

# Model Complexity
erf_sim$Nterms
# missing: rule length

# Predictions

# AUC
erf_sim$AUC
# Classification Error
erf_sim$ClassErr

# Most important variables (anhand der coefficients?)
erf_sim$Imps
# most important terms


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


# variable importances
imps_sim <- importance(pre_sim, round = 4)
imps_sim$varimps
# base learner importances
imps_sim$baseimps

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. applied to diabetes data

# a) ExpertRuleFit (w.o. expert knowledge)

# Model
erf_diab <- ExpertRuleFit(X_diab, y_diab, Xtest_diab, ytest_diab, print_output = F)
erf_diab$Model
erf_diab$Features
erf_diab$Coefficients

# Model Complexity
erf_diab$Nterms
# missing: rule length

# Predictions (als output hinzufügen) Accuracy evtl anhanf von preds prüfen

# Predictive Accuracy (measure überprüfen), evtl Spezifität, Sensitivität
erf_diab$AUC
erf_diab$ClassErr

# Most important variables (anhand der coefficients?)

# most important terms


# b) xrf (eXtreme RuleFit) 

# Model
xrf_diab <- xrf(y ~ ., train_data_diab, family = 'binomial', 
                xgb_control = list(nrounds = 50, max_depth = 3), 
                deoverlap = TRUE)
xrf_coefs_diab <- coef(xrf_diab, lambda = 'lambda.min')
xrf_coefs_diab %>%filter(xrf_coefs_diab != 0) %>%arrange(term)

# Model Complexity
# nterms
# rule lengths


# Predictions
predict(xrf_diab, newdata = test_data_diab , type = "class")

# Predictive Accuracy

# Importances
# Important variables
# Important terms

# c) pre

# Model
pre_diab <- pre(y ~ ., data = train_data_diab, family = "binomial", type = "rules", 
                ntrees = 250)
pre_diab
pre_coefs_diab <- coef(pre_diab)
pre_coefs_diab

# Model Complexity
# nterms
# rule lengths

# Predictions
predict(pre_diab, newdata = test_data_diab, type = "class")

# Predictive Accuracy
diab.cv <- cvpre(pre_diab)
diab.cv$accuracy


# Importance
# variable importances
imps_diab <- importance(pre_diab, round = 4)
imps_diab$varimps
# base learner importances
imps_diab$baseimps


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. appied to cervical cancer data

# a) ExpertRuleFit (w.o. expert knowledge)

# Model
erf_canc <- ExpertRuleFit(X_canc, y_canc, Xtest_canc, ytest_canc, print_output = F)
erf_canc$Model
erf_canc$Features
erf_canc$Coefficients

# Model Complexity
erf_canc$Nterms
# missing: rule length

# Predictions (als output hinzufügen) Accuracy evtl anhanf von preds prüfen

# Predictive Accuracy (measure überprüfen), evtl Spezifität, Sensitivität
erf_canc$AUC
erf_canc$ClassErr

# Most important variables (anhand der coefficients?)

# most important terms


# b) xrf (eXtreme RuleFit) 

# Model
xrf_canc <- xrf(y ~ ., train_data_canc, family = 'binomial', 
               xgb_control = list(nrounds = 50, max_depth = 3), 
               deoverlap = TRUE)
xrf_coefs_canc <- coef(xrf_canc, lambda = 'lambda.min')
xrf_coefs_canc %>%filter(xrf_coefs_canc != 0) %>%arrange(term)

# Model Complexity
# nterms
# rule lengths


# Predictions
predict(xrf_canc, newdata = test_data_canc , type = "class")

# Predictive Accuracy

# Importances
# Important variables
# Important terms

# c) pre

# Model
pre_canc <- pre(y ~ ., data = train_data_canc, family = "binomial", type = "rules", 
               ntrees = 250)
pre_canc
pre_coefs_canc <- coef(pre_canc)
pre_coefs_canc

# Model Complexity
# nterms
# rule lengths

# Predictions
predict(pre_canc, newdata = test_data_canc, type = "class")

# Predictive Accuracy
canc.cv <- cvpre(pre_canc)
canc.cv$accuracy


# Importance
# variable importances
imps_canc <- importance(pre_canc, round = 4)
imps_canc$varimps
# base learner importances
imps_canc$baseimps

################################################################################
#                              COMPARISONS                                     #
################################################################################

# 1. Predictions
# 2. ClassErr, AUC, Sens, Spez
# 3. nterms
# 4. average rule length
# 5. Most important variables
# 6. Most important terms

################################################################################




