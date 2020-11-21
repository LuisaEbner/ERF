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

# 1.1. Rules extracted from the 'Diabetes Risk Test' within the
#      'Standards of Medical Care in Diabetes - 2019' 
#      published by the American Diabetes Association

fdk_rules1 <-  c("Age<=39 & BP<=80 & BMI<25",
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


# 1.2. Rules Extracted from Chapter 3, Therapeutic Goals of the
#      Nationale Versorgungs-Leitlinie Diabetes Mellitus Typ 2

fdk_rules2 <- c("Glucose<=100", 
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

fdk_linear <- c("BP", "Glucose", "Age", "BMI", "DPF")

#===============================================================================
#                        2. HEURISTIC EXPERT KNOWLEDGE
#===============================================================================

hek_rules <- c("Age<=42 & BP<=80 & BMI<=29",
                         "Age>=45 & BP>=90 & Glucose>=125",
                         "Age<=31 & BP>=90 & BMI>=38",
                         "Age>=55 & BP<=80 & BMI<=29",
                         "Age>=60 & Glucose>=130 & BMI>=35", 
                         "Age>=60 & BP>=90 & BMI>=37",
                         "Age>=45 & BP>=90 & BMI>=35 & Glucose>=130",
                         "Age>=55 & BP<=90 & BMI<=30 & Glucose>=130",
                         "Age<=60 & BP<=90 & BMI<=30 & Glucose<=100")

hek_linear <- c("BMI", "Age", "DPF")
