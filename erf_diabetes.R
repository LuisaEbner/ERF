################################################################################
################################################################################
###                                                                          ###
###                 EXPERT RULE FIT (ERF) for Diabetes                       ###
###                                                                          ###
################################################################################
################################################################################

# External functions
# setwd("C:/Users/ebner/Documents/MasterArbeit/ExpertRuleFit")

source("erf_diabetes_dataprep.R")
source("erf_main.R")


# 1. Knowledge Sources

# 1.1. Training Data
# 1.2. Factual Domain Knowledge from Clinical Practice Guidelines

#===============================================================================
#                             1.1. TRAINING DATA
#===============================================================================

# DESCRIPTION

## dataset obtained from the University of California at Irvine (UCI) 
## Machine Learning Repository.
## http://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes

# Goal: Predict the presence of diabetes type 2 given a number of risk factors

## Patients: 768 females; >= 21 years of age; of Pima Indian heritage.

## 8 numeric attributes (= risk factors):

# 1. Number of times pregnant
# 2. Plasma glucose concentration at 2 hours in an oral glucose tolerance test
# 3. Diastolic blood pressure (mm Hg)
# 4. Triceps skin fold thickness (mm)
# 5. 2-Hour serum insulin (Uh/ml)
# 6. Body mass index (Weight in kg / (Height in in))
# 7. Diabetes Pedigree Function:
#      provides a synthesis of the diabetes mellitus history in
#      relatives and the genetic relationship of those relatives to the
#      subject. The DPF uses information from parents, grandparents,
#      full and half siblings, full and half aunts and uncles, and first
#      cousins. It provides a measure of the expected genetic influence of
#      affected and unaffected relatives on the subject's eventual diabetes
#      risk.
# 8. Age (years)

# 1 binary target = diabetes test result
# class Distribution: 500x0 ("tested negative"), 268x1 ("tested positive")

data <- read.csv(file = 'diabetes.csv', header = T)

# Data Preprocessing (see file 'erf_diabetes_dataprep.R')
data <- prepare_diabetes_data(data)


#===============================================================================
#                           DOMAIN KNOWLEDGE
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

conf_dk_rules1 <- c("Age>=60 & BP>=81 & BMI>=25 & BMI<=30",
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

conf_dk_rules2 <- c("Glucose>110 & BP>90",
                    "Glucose>110 & BMI>26",
                    "BP>90 & BMI>26",
                    "Glucose>110 & BP>90 & BMI>26")


# Linear terms extracted from both guidelines

linterms <- c("BP", "Glucose", "Age", "BMI", "DPF")

conf_linterms <- c("Age", "BMI", "DPF")

#===============================================================================
#                         HEURISTIC EXPERT KNOWLEDGE
#===============================================================================

ek_rules1 <- c()

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
optional_expert_rules <- c(opt_dk_rules1, opt_dk_rules2)
confirmatory_expert_rules <- c(conf_dk_rules1, conf_dk_rules2)

optional_linterms <- opt_dk_linterms
confirmatory_linterms <- conf_dk_linterms



# Model
erf_diabetes <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest)




