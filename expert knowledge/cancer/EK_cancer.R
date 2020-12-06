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
fdk_rules1 <- c("STDs.HPV == 1",
               "Smoking ==1 ", 
               "STDs.HIV == 1",
               "STDs.Hepatitis.B == 1",
               "First.sexual.intercourse<14",
               "(((Age - First.sexual.intercourse)*Number.of.sexual.partners)/10)>4",
               "Hormonal.Contraceptives..years.>5")


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

# interview with Dr. med. Volker Meltzer

# a) Linear Terms

# 1. number.of.sexual.partners
# 2. First.sexual.intercourse

ek_linear <- c("Number.of.sexual.partners", "First.sexual.intercourse")

# b) Rules

# 0. HIV, Smoking, increased
# 1. HPV, high
# 2. Number.of.sexual.partners high (>5) + Smoking, increased
# 3. Number.of.sexual.partners high (>5) + Smoking + First.sexual.intercourse < 14
# 4. STDs.condylomatis, increased
# 5. STDs.genital.herpes, low
# 6. IUD, low
# 7. Number.of.sexual.partners high (>5) + Smoking + inflammatorydisease, high

ek_rules <- c("STDs.HPV == 1",
              "Smoking == 1 ", 
              "STDs.HIV == 1",
              "Number.of.sexual.partners > 5 & Smoking == 1",
              "STDs.condylomatis == 1",
              "STDs.genital.herpes == 1",
              "Intra.uterine.device == 1",
              "Number.of.sexual.partners > 5 & Somking == 1 & STDs.pelvic.inflammatory.disease ==1")
