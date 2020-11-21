################################################################################
#                                                                              #
#     Expert Knowledge on the Risk Factors and Diagnosis of Diabetes           # 
#                                                                              #
################################################################################

# 1. Factual Domain Knowledge
# 2. Heuristic Expert Knowledge


#===============================================================================
#                        1. FACTUAL DOMAIN KNOWLEDGE
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
fdk_rules1 <- c("STDs.HPV == 1", # STDs.HPV == 1
               "Smoking ==1 ", #Smoking == 1
               "STDs.HIV == 1",
               "STDs.Hepatitis.B == 1",
               "First.sexual.intercourse<14",
               "(((Age - First.sexual.intercourse)*Number.of.sexual.partners)/10)>4",
               "Hormonal.Contraceptives..years.>5")

#"any(c(STDs.genital.herpes, STDs.molluscum.contagiosum)), 
#STDs.pelvic.inflammatory.disease, STDs.syphilis)>0)", 


# b) Rules Extracted from 'The American Cancer Society Guidelines for the
#    Prevention and Early Detection of Cervical Cancer' (ACS-CC) 

# Guideline text
# 1. Becoming sexually active at a young age (especially < 18)
# 2. Young age at first full-term pregnancy (< 20)
# 3. Having multiple full-term pregnancies (3 or more)
# 4. Intrauterine device use (risk lowering factor)


# Corresponding expert rules
fdk_rules2 <- c("First.sexual.intercourse<18",
               "Age<20 & Num.of.pregnancies>=1",
               "Num.of.pregnancies>=3",
               "Intra.uterine.device == 1")


# c) Linear Terms extracted from both guidelines

# Guideline text
# 1. number of full-term pregnancies (LOZK)
# 2. number of sexual partners (ACS-CC)

# Corresponding linear terms
fdk_linear <- c( "Num.of.pregnancies", "Number.of.sexual.partners",
                 "Age",  "First.sexual.intercourse", 
                 "Hormonal.Contraceptives..years.") 


#===============================================================================
#                        2. HEURISTIC EXPERT KNOWLEDGE
#===============================================================================

# interview information following
