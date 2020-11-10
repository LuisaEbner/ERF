################################################################################
################################################################################
###                                                                          ###
###           EXPERT RULE FIT (ERF) for Cervical Cancer                      ###
###                                                                          ###
################################################################################
################################################################################

# External functions
source("erf_cancer_dataprep.R")
source("erf_main.R")

# 1. Information Sources

# 1.1. Training data
# 1.2. Factual domain knowledge from medical guidelines
# 1.3. Heuristic expert knowledge

# 2. ERF Model

#===============================================================================
#                             1.1. Training data
#===============================================================================

# Cervical Cancer (Risk Factors) dataset available from the UCI ML Repository
data <- read.csv(file = 'risk_factors_cervical_cancer.csv', header = T)

# Preprocessing (see 'erf_cancer_dataprep.R')
data <- prepare_cervicalcancer_data(data = data, balance = T)


#===============================================================================
#            1.2. Factual domain knowledge from medical guidelines
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
dk_rules1 <- c("STDs.HPV1 > 0.5", # STDs.HPV == 1
               "Smoking1 > 0.5", #Smoking == 1
               "(STDs.HIV+STDs.Hepatitis.B)>=1",
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
               "Intra.uterine.device1 > 0.5") # Intra.uterine.device == 1


# c) Linear Terms extracted from both guidelines

# Guideline text
# 1. number of full-term pregnancies (LOZK)
# 2. number of sexual partners (ACS-CC)

# Corresponding linear terms
dk_lins <- c( "Num.of.pregnancies", "Number.of.sexual.partners") 


#===============================================================================
#                        1.3. HEURISTIC EXPERT KNOWLEDGE
#===============================================================================

# Data Input
sets <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

# EK Input

# (optional) expert rules
expert_rules <- c(dk_rules1, dk_rules2)
conf_rules <- dk_rules1

# expert liner terms
expert_linear_terms <- dk_lins
conf_linear_terms <- c("Num.of.pregnancies")


#===============================================================================
#                               2. ERF MODEL
#===============================================================================

erf_cancer <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                            expert_rules = expert_rules, 
                            confirmatory_rules = conf_rules,
                            linterms = expert_linear_terms, 
                            confirmatory_lins = conf_linear_terms, 
                            print_output = T)





