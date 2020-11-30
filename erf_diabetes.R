################################################################################
################################################################################
###                                                                          ###
###                 EXPERT RULE FIT (ERF) for Diabetes                       ###
###                                                                          ###
################################################################################
################################################################################

# External functions
setwd("C:/Users/ebner/Documents/MasterArbeit/ExpertRuleFit")

source("erf_diabetes_dataprep.R")
source("erf_main.R")


# Information Sources

# 1. Training data
# 2. Factual domain knowledge from medical guidelines
# 3. Heuristic expert knowledge

#===============================================================================
#                             1.1. TRAINING DATA
#===============================================================================


## Pima Indian Diabetes dataset available from UCI ML Repository.
## http://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes
data <- read.csv(file = 'diabetes.csv', header = T)

# Data Preprocessing (see file 'erf_diabetes_dataprep.R')
data <- prepare_diabetes_data(data)


#===============================================================================
#                           DOMAIN KNOWLEDGE
#===============================================================================

# 1. Rules extracted from the 'Diabetes Risk Test' within the
#    'Standards of Medical Care in Diabetes - 2019' 
#    published by the American Diabetes Association

opt_exp_rules1 <-  c("Age<=39 & BP<=80 & BMI<25",
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
                    "Age>=40 & Age<=49 & BP<=80 & BMI>40")

conf_exp_rules1 <- c("Age>=60 & BP>=81 & BMI>=25 & BMI<=30",
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


opt_exp_rules2 <- c("Glucose<=100", 
                   "Glucose>100 & Glucose<=110", 
                   "Glucose>110",
                   "BP<=85",
                   "BP>85 & BP<=90",
                   "BP>90",
                   "BMI<=24",
                   "BMI<24 & BMI<=26", 
                   "BMI>26")

conf_exp_rules2 <- c("Glucose>110 & BP>90",
                    "Glucose>110 & BMI>26",
                    "BP>90 & BMI>26",
                    "Glucose>110 & BP>90 & BMI>26")


# Linear terms extracted from both guidelines

opt_exp_lins12 <- c("BP", "Glucose")

conf_exp_lins12 <- c("Age", "BMI", "DPF")

#===============================================================================
#                         HEURISTIC EXPERT KNOWLEDGE
#===============================================================================


#===============================================================================
#                          EXPERT RULEFIT MODEL
#===============================================================================

# Data Input
sets <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

# Expert Knowledge Input
opt_exp_r <- c(opt_exp_rules1, opt_exp_rules2)
opt_exp_r
conf_exp_r <- c(conf_exp_rules1, conf_exp_rules2)

opt_exp_lin <- opt_exp_lins12
conf_exp_lin <- conf_exp_lins12

erf_diabetes <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                              optional_expert_rules = opt_exp_r, 
                              confirmatory_expert_rules = conf_exp_r,  
                              optional_linear_terms=opt_exp_lin,
                              confirmatory_linear_terms = conf_exp_lin)







