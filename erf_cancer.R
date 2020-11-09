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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Knowledge Sources

# 1.1. Training Data
# 1.2. Factual Domain Knowledge from Medical Guidelines
# 1.3. Heuristic Expert Knowledge

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                             1.1. TRAINING DATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# UCI ML Repository: Cervical Cancer (Risk Factors)

data <- read.csv(file = 'risk_factors_cervical_cancer.csv', header = T)

data <- prepare_cervicalcancer_data(data = data, del_maj_missing = T,
                                    add_NA_features = F, impute_missing = T,
                                    target = "Risk1", balance = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           1.2. FACTUAL DOMAIN KNOWLEDGE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.2.1. Rules extracted from Chapter 3.4, Risk Factors and Disease Development
#        of the German oncology guideline 'S3-Leitlinie Diagnostik und Therapie 
#        Zervixkarzinom' (LOZK) published in 2014 

# 1. Infection with the human papilloma virus (mainly Type 16 + 18)
# 2. Smoking (>15 cigarettes oer day)
# 3. Immuno-compromised patients (HIV, medication)
# 4. Early start of sexual activity (< 14 years of age)
# 5. Frequently changing sexual partners (> 4 in 10 years)
# 6. Other infections (e.g. genital herpes, chlamydia etc.) 
# 7. Long-term use oral contraceptives (> 5 years)


dk_rules1 <- c("STDs.HPV==1",
               "Smokes==1",
               "STDs.HIV + STDs.Hepatitis.B + STDs.AIDS>=1",
               "First.sexual.intercourse<14",
               "(((Age - First.sexual.intercourse)*Number.of.sexual.partners)/10)>4",
               "STDs.genital.herpes + STDs.cervical.condylomatosis + 
                STDs.condylomatosis + STDs.molluscum.contagiosum + 
                STDs.pelvic.inflammatory.disease +
                STDs.syphilis + STDs.vaginal.condylomatosis + 
                STDs.vulvo.perineal.condylomatosis >=1", 
               "Hormonal.Contraceptives..years.>5")



# 1.2.2. Rules Extracted from 'The American Cancer Society Guidelines for the
#        Prevention and Early Detection of Cervical Cancer' (ACS-CC) 

# 1. Becoming sexually active at a young age (especially < 18)
# 2. Young age at first full-term pregnancy (< 20)
# 3. Having multiple full-term pregnancies (3 or more)
# 4. Intrauterine device use (risk lowering factor)


dk_rules2 <- c("First.sexual.intercourse<18",
               "Age<20 & Num.of.pregnancies>=1",
               "Num.of.pregnancies>=3",
               "IUD==1")


# Linear Terms extracted from both guidelines

# 1. number of full-term pregnancies (LOZK)
# 2. number of sexual partners (ACS-CC)

dk_lins <- c( "Num.of.pregnancies", "Number.of.sexual.partners") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                        1.3. HEURISTIC EXPERT KNOWLEDGE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ek_rules <- c()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                           EXPERT-RULEFIT INPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Data

# train-test-split
sets <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

# 2. Expert Knowledge

# 2.1. (optional) expert rules
expert_rules <- c(dk_rules1, dk_rules2, ek_rules) 

# 2.2. (optional) expert lins

# 2.3. confirmatory expert rules

# 2.4. confirmatory expert lins

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                          EXPERT-RULEFIT MODEL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

erf_cancer <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                            expert_rules = expert_rules, 
                            linterms = linterms, confirmatory_lins = linterms,
                            print_output = F)

erf_cancer$Model
erf_cancer$Nterms
erf_cancer$AvgRuleLength
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



