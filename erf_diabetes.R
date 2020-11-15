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

erf1 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                      optional_expert_rules = optional_rules, 
                      confirmatory_expert_rules = confirmatory_rules,  
                      optional_linear_terms=optional_linears,
                      confirmatory_linear_terms = confirmatory_linears, 
                      print_output = F)

erf2 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                      optional_expert_rules = confirmatory_rules, 
                      print_output = F)

erf3 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                      confirmatory_expert_rules = confirmatory_rules,
                      print_output = F)

pre1 <- pre_for_comparison(train, test)

# funktioniert aktuell nicht
# pre2 <- pre_for_comparison(train, test, conf = confirmatory_rules)


performance_comparison <- modelcomp(erf1 = erf1, erf2 = erf2 , erf3 = erf3, 
                                    pre1 = pre1, pre2 = NULL, pre3 = NULL)
#===============================================================================



