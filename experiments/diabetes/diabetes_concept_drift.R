################################################################################
#                                                                              #
#         Predicting Diabetes: Experiments on the concept drift                #   
#                                                                              #
################################################################################

# Libraries
library(ggplot2)
library(plyr)
library(dplyr)


# External functions
source("./experiments/diabetes/erf_diabetes_dataprep.R")
source("./ERF/erf_main.R")


#===============================================================================
#                          EXPLORATORY ANALYSIS
#===============================================================================

data <- read.csv(file = './data sets/diabetes.csv', header = T)
data <- prepare_diabetes_data(data)
data$Age.greater.35 <- factor((as.numeric(data$Age > 35)), levels = c("0","1"))
data$overweight <- factor((as.numeric(data$BMI > 30)), levels = c("0","1"))

# 1. Overlayed histograms according to normal- and overweight population

# a) Pregnancies
mu_preg_w <- ddply(data, "overweight", summarise, grp.mean=mean(Pregnancies))
p1  <- ggplot(data, aes(x=Pregnancies, fill = overweight, color=overweight)) +
             geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 1)+
             geom_density(alpha=0.4)+
             geom_vline(data=mu_preg_w, aes(xintercept=grp.mean, color=overweight), linetype="dashed")+
             theme(legend.position="right") +
             scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
             scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
             labs(title="Pregnancies plot grouped by being overweight (BMI > 30)",x="Pregnancies", y = "Density")+
             theme(plot.title = element_text(size=15),
                          legend.text=element_text(size=11),
                          text = element_text(size = 14), 
                          axis.text = element_text(size = 12)) 
p1
pdf("Pregnancies_overweight.pdf")
p1
dev.off()


# b) Blood Pressure
mu_bp_w <- ddply(data, "overweight", summarise, grp.mean=mean(BP))
p1  <- ggplot(data, aes(x=BP, fill = overweight, color=overweight)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 5)+
  geom_density(alpha=0.4)+
  geom_vline(data=mu_bp_w, aes(xintercept=grp.mean, color=overweight), linetype="dashed")+
  theme(legend.position="right") +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  labs(title="Blood pressure plot grouped by being overweight (BMI > 30)",x="Blood pressure", y = "Density")+
  theme(plot.title = element_text(size=15),
        legend.text=element_text(size=11),
        text = element_text(size = 14), 
        axis.text = element_text(size = 12)) 
p1
pdf("BP_overweight.pdf")
p1
dev.off()


# a) Glucose
mu_glucose_w <- ddply(data, "overweight", summarise, grp.mean=mean(Glucose))
p1  <- ggplot(data, aes(x=Glucose, fill = overweight, color=overweight)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 5)+
  geom_density(alpha=0.4)+
  geom_vline(data=mu_glucose_w, aes(xintercept=grp.mean, color=overweight), linetype="dashed")+
  theme(legend.position="right") +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
  labs(title="Glucose plot grouped by being overweight (BMI > 30)",x="Glucose level", y = "Density")+
  theme(plot.title = element_text(size=15),
        legend.text=element_text(size=11),
        text = element_text(size = 14), 
        axis.text = element_text(size = 12)) 
p1
pdf("Glucose_overweight.pdf")
p1
dev.off()


# b) Diabetes
mu_y_w <- ddply(data, "overweight", summarise, grp.mean=mean(as.numeric(y)))
p2 <-  ggplot(data, aes(x=as.numeric(y), fill = overweight, color=overweight)) +
             geom_histogram(aes(y=..density..), position="identity", alpha=0.4, binwidth = 1)+
             geom_vline(data=mu_y_w, aes(xintercept=grp.mean, color=overweight), linetype="dashed")+
             theme(legend.position="right") +
             scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
             scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
             theme(axis.text.x = element_blank()) +
             labs(title="Diabetes grouped by being overweight (BMI > 30)",x="Diabetes(1 = no, 2 = yes)", y = "Density")+
             theme(plot.title = element_text(size=15),
                   legend.text=element_text(size=11),
                   text = element_text(size = 14), 
                   axis.text = element_text(size = 12)) 
p2
pdf("y_overweight.pdf")
p2
dev.off()

#-------------------------------------------------------------------------------

# 2. Overlayed histograms according to Age (being "age.greater.35" (>35) or of advanced age)

# a) Pregnancies
mu_preg_a <- ddply(data, "Age.greater.35", summarise, grp.mean=mean(Pregnancies))
p3 <-ggplot(data, aes(x=Pregnancies, fill = Age.greater.35, color=Age.greater.35)) +
            geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 1)+
            geom_density(alpha=0.4)+
            geom_vline(data=mu_preg_a, aes(xintercept=grp.mean, color=Age.greater.35), linetype="dashed")+
            theme(legend.position="right") +
            scale_color_brewer(palette="Dark2")+
            scale_fill_brewer(palette="Dark2") +
            labs(title="Pregnancies plot grouped by Age (> 35)",x="Number of Pregnancies", y = "Density")+
            theme(plot.title = element_text(size=15),
                  legend.text=element_text(size=11),
                  text = element_text(size = 14), 
                  axis.text = element_text(size = 12)) 
p3
pdf("Pregnancies_age.pdf")
p3
dev.off()


# b) Glucose
mu_glucose_a <- ddply(data, "Age.greater.35", summarise, grp.mean=mean(Glucose))
p4  <- ggplot(data, aes(x=Glucose, fill = Age.greater.35, color=Age.greater.35)) +
              geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 5)+
              geom_density(alpha=0.4)+
              geom_vline(data=mu_glucose_a, aes(xintercept=grp.mean, color=Age.greater.35), linetype="dashed")+
              theme(legend.position="right") +
              scale_color_brewer(palette="Dark2")+
              scale_fill_brewer(palette="Dark2") +
              labs(title="Glucose plot grouped by Age (> 35)",x="Glucose level", y = "Density")+
              theme(plot.title = element_text(size=15),
                    legend.text=element_text(size=11),
                    text = element_text(size = 14), 
                    axis.text = element_text(size = 12)) 
              
p4
pdf("Glucose_age.pdf")
p4
dev.off()


# c) Blood Pressure
mu_bp_a <- ddply(data, "Age.greater.35", summarise, grp.mean=mean(BP))
p5 <-ggplot(data, aes(x=BP, fill = Age.greater.35, color=Age.greater.35)) +
            geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 5)+
            geom_density(alpha=0.4)+
            geom_vline(data=mu_bp_a, aes(xintercept=grp.mean, color=Age.greater.35), linetype="dashed")+
            theme(legend.position="right") +
            scale_color_brewer(palette="Dark2")+
            scale_fill_brewer(palette="Dark2")+
            labs(title="Blood pressure plot grouped by Age (> 35)",x="Blood Pressure", y = "Density")+
            theme(plot.title = element_text(size=15),
                  legend.text=element_text(size=11),
                  text = element_text(size = 14), 
                  axis.text = element_text(size = 12)) 

p5
pdf("BP_age.pdf")
p5
dev.off()

# d) Diabetes
mu_y_a <- ddply(data, "Age.greater.35", summarise, grp.mean=mean(as.numeric(y)))
p6 <-ggplot(data, aes(x=as.numeric(y), fill = Age.greater.35, color=Age.greater.35)) +
             geom_histogram(aes(y=..density..), position="identity", alpha=0.4, binwidth = 1)+
             geom_vline(data=mu_y_a, aes(xintercept=grp.mean, color=Age.greater.35), linetype="dashed")+
             theme(legend.position="right") +
             scale_color_brewer(palette="Dark2") +
             scale_fill_brewer(palette="Dark2") +
             labs(title="Histogram plot grouped by Age (> 35)",x="Diabetes (1 = no, 2 = yes)", y = "Density")+
             theme(plot.title = element_text(size=15),
                    legend.text=element_text(size=11),
                    text = element_text(size = 14), 
                    axis.text = element_text(size = 12)) 
p6
pdf("y_age.pdf")
p6
dev.off()

#===============================================================================
#             MODEL COMPARISON: GENERALIZABILITY/CONCEPT DRIFT
#===============================================================================

#===============================================================================
#         Concept Drift 1: younger (<=35) vs. older population
#===============================================================================

# Data 
data <- read.csv(file = './data sets/diabetes.csv', header = T)
data <- prepare_diabetes_data(data)

set.seed(234567)
age_drift <- concept_drift_split(data, "Age > 35")
X_age_drift <- age_drift[[1]]
y_age_drift <- age_drift[[2]]
Xtest_age_drift <- age_drift[[3]]
ytest_age_drift <- age_drift[[4]]
train_age_drift <- cbind(X_age_drift, y_age_drift)
test_age_drift <- cbind(Xtest_age_drift, ytest_age_drift)

names(train_age_drift)[names(train_age_drift) == 'y_age_drift'] <- 'y'
names(test_age_drift)[names(test_age_drift) == 'ytest_age_drift'] <- 'y'


# Expert Knowledge
source("./expert knowledge/diabetes/EK_diabetes.R")
# Rules
rules <- c(fdk_rules1, fdk_rules2, hek_rules)

age_rules <- c("Age<=39 & BP<=80 & BMI<25",
               "Age<=39 & BP>=81 & BMI>=25 & BMI<=30",
               "Age<=39 & BP<=80 & BMI>=31 & BMI<=40",
               "Age<=39 & BP>=81 & BMI>=31 & BMI<=40",
               "Age<=42 & BP<=80 & BMI<=29", 
               "Age<=39 & BP>=81 & BMI<25",
               "Age<=39 & BP>=81 & BMI>40", 
               "Age<=31 & BP>=90 & BMI>=38",
               "Age<=39 & BP<=80 & BMI>40",
               "Age<=39 & BP<=80 & BMI>=25 & BMI<=30")

sup_rules <- support_take(rules, data, 0.05)
add_rules <- setdiff(sup_rules, age_rules)



conf_rules <- c(age_rules, add_rules)
opt_rules <- setdiff(rules, conf_rules)

#  Linear Terms
conf_linear <- c("Age", "BMI", "DPF")
opt_linear <- c("BP", "Glucose")


# Models

erf_age <- ExpertRuleFit(X = X_age_drift, y = y_age_drift, Xtest = Xtest_age_drift,
                         ytest = ytest_age_drift, confirmatory_expert_rules = conf_rules, 
                         optional_expert_rules = opt_rules, 
                         optional_linear_terms= opt_linear,
                         confirmatory_linear_terms = conf_linear, print_output = F)

erf_prio_age <- ExpertRuleFit(X = X_age_drift, y = y_age_drift, Xtest = Xtest_age_drift,
                              ytest = ytest_age_drift, confirmatory_expert_rules = conf_rules, 
                              optional_expert_rules = opt_rules, 
                              optional_linear_terms= opt_linear,
                              confirmatory_linear_terms = conf_linear, 
                              optional_penalty = 0.5, print_output = F)

erf_only_age <- ExpertRuleFit(X = X_age_drift, y = y_age_drift, Xtest = Xtest_age_drift,
                              ytest = ytest_age_drift, confirmatory_expert_rules = conf_rules, 
                              optional_expert_rules = opt_rules, 
                              optional_linear_terms= opt_linear,
                              confirmatory_linear_terms = conf_linear, expert_only = T,
                              print_output = F)

rf_age <- ExpertRuleFit(X = X_age_drift, y = y_age_drift, Xtest = Xtest_age_drift,
                        ytest = ytest_age_drift, print_output = F)

pre_age <- pre_for_comparison(train = train_age_drift, test = test_age_drift)

age_drift_performance <- data.frame(AUC = c(erf_age$AUC, erf_prio_age$AUC, erf_only_age$AUC, rf_age$AUC, pre_age$AUC),
                                    ClassErr = c(erf_age$ClassErr, erf_prio_age$ClassErr, erf_only_age$ClassErr, rf_age$ClassErr, pre_age$ClassErr),
                                    NTerms = c(erf_age$NTerms, erf_prio_age$NTerms, erf_only_age$NTerms, rf_age$NTerms, pre_age$NTerms),
                                    AvgRuleLength = c(erf_age$AvgRuleLength, erf_prio_age$AvgRuleLength, erf_only_age$AvgRuleLength, rf_age$AvgRuleLength, pre_age$AvgRuleLength), 
                                    Model = c("ERF", "ERF EK prio", "ERF EK only", "RuleFit", "PRE"))

age_drift_performance


# Plots

# 1. AUC Plot
p1 <- ggplot(age_drift_performance, aes(x = Model, y=AUC, color = Model)) +
  geom_point(size = 4) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c", "#4C9900")) + 
  labs(title = "AUC in a concept drift setting (train: Age >35)", x ="Model", y = "AUC") + 
  scale_y_continuous(limits = c(0.5, 1)) +
  theme_minimal() + theme(plot.title = element_text(size=15),
                          legend.text=element_text(size=11),
                          text = element_text(size = 14), 
                          axis.text = element_text(size = 12)) 

p1
pdf("AUC_Age35_conceptdrift_diabetes.pdf")
p1
dev.off()

# 1. Classification Error Plot
p2 <- ggplot(age_drift_performance, aes(x = Model, y = ClassErr, color = Model)) +
  geom_point(size = 4) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c", "#4C9900")) + 
  labs(title = "Classication Error in a concept drift setting (train: Age>35)", x ="Model", y = "Classification Error") + 
  scale_y_continuous(limits = c(0, 0.5)) + theme(plot.title = element_text(size=15),
                                                    legend.text=element_text(size=11),
                                                    text = element_text(size = 14), 
                                                    axis.text = element_text(size = 12)) +
  theme_minimal() 

p2
pdf("CE_Age35_conceptdrift_diabetes.pdf")
p2
dev.off()

# 1. NTerms Plot
p3 <- ggplot(age_drift_performance, aes(x = Model, y=NTerms, color = Model)) +
  geom_point(size = 4) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c", "#37004D")) + 
  labs(title = "Model complexity in a concept drift setting (train: Age>35)", x ="Model", y = "Number of terms") + 
  theme_minimal() + scale_y_continuous(limits = c(0, 40)) +
  theme(plot.title = element_text(size=15),
                      legend.text=element_text(size=11),
                      text = element_text(size = 14), 
                      axis.text = element_text(size = 12))

p3
pdf("NTerms_Age35_conceptdrift_diabetes.pdf")
p3
dev.off()

# 1. AvgRuleLength Plot
p4 <- ggplot(age_drift_performance, aes(x = Model, y=AvgRuleLength, color = Model)) +
  geom_point(size = 4) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c", "#37004D")) + 
  labs(title = "Model complexity in a concept drift setting (train: Age>35)", x ="Model", y = "Average rule length") + 
  scale_y_continuous(limits = c(0, 4)) + theme(plot.title = element_text(size=15),
                                               legend.text=element_text(size=11),
                                               text = element_text(size = 14), 
                                               axis.text = element_text(size = 12)) 

p4
pdf("AvgRuleLength_Age35_conceptdrift_diabetes.pdf")
p4
dev.off()

#===============================================================================
#         Concept Drift 2: normal weight vs. overweight population
#===============================================================================

# Data
weight_drift <- concept_drift_split(data, "BMI > 30")
X_weight_drift <- weight_drift[[1]]
y_weight_drift <- weight_drift[[2]]
Xtest_weight_drift <- weight_drift[[3]]
ytest_weight_drift <- weight_drift[[4]]
train_weight_drift <- cbind(X_weight_drift, y_weight_drift)
test_weight_drift <- cbind(Xtest_weight_drift, ytest_weight_drift)

names(train_weight_drift)[names(train_weight_drift) == 'y_weight_drift'] <- 'y'
names(test_weight_drift)[names(test_weight_drift) == 'ytest_weight_drift'] <- 'y'

# EK

# Expert Knowledge
source("./expert knowledge/diabetes/EK_diabetes.R")
# Rules
rules <- c(fdk_rules1, fdk_rules2, hek_rules)
rules

weight_rules <- c( "Age<=39 & BP<=80 & BMI<25", "Age>=40 & Age<=49 & BP<=80 & BMI<25",           
                   "Age>=50 & Age<=59 & BP<=80 & BMI<25",          
                   "Age>=60 & BP<=80 & BMI<25", "Age<=39 & BP>=81 & BMI<25",
                   "Age>=40 & Age<=49 & BP>=81 & BMI<25",          
                   "Age>=50 & Age<=59 & BP>=81 & BMI<25",
                   "Age>=60 & BP>=81 & BMI<25", "Age<=39 & BP<=80 & BMI>=25 & BMI<=30",          
                   "Age>=40 & Age<=49 & BP<=80 & BMI>=25 & BMI<=30",
                   "Age>=50 & Age<=59 & BP<=80 & BMI>=25 & BMI<=30",
                   "Age>=60 & BP<=80 & BMI>=25 & BMI<=30",         
                   "Age<=39 & BP>=81 & BMI>=25 & BMI<=30",
                   "Age>=40 & Age<=49 & BP>=81 & BMI>=25 & BMI<=30",
                   "Age>=50 & Age<=59 & BP>=81 & BMI>=25 & BMI<=30",
                   "BMI<=24", "BMI>24 & BMI<=26", 
                   "Age<=60 & BP<=90 & BMI<=30 & Glucose<=100",
                   "Age>=55 & BP<=90 & BMI<=30 & Glucose>=130",
                   "Age<=42 & BP<=80 & BMI<=29",
                   "Age>=55 & BP<=80 & BMI<=29")

sup_rules <- support_take(rules, data, 0.05)
add_rules <- setdiff(sup_rules, weight_rules)



conf_rules <- c(weight_rules, add_rules)
opt_rules <- setdiff(rules, conf_rules)

#  Linear Terms
conf_linear <- c("Age", "BMI", "DPF")
opt_linear <- c("BP", "Glucose")


# Models
erf_weight <- ExpertRuleFit(X = X_weight_drift, y = y_weight_drift, Xtest = Xtest_weight_drift,
                         ytest = ytest_weight_drift, confirmatory_expert_rules = conf_rules, 
                         optional_expert_rules = opt_rules, 
                         optional_linear_terms= opt_linear,
                         confirmatory_linear_terms = conf_linear, print_output = F)

erf_conf_weight <- ExpertRuleFit(X = X_weight_drift, y = y_weight_drift, Xtest = Xtest_weight_drift,
                            ytest = ytest_weight_drift, confirmatory_expert_rules = c(conf_rules, opt_rules), 
                            confirmatory_linear_terms = c(conf_linear, opt_linear), print_output = F)

erf_prio_weight <- ExpertRuleFit(X = X_weight_drift, y = y_weight_drift, Xtest = Xtest_weight_drift,
                              ytest = ytest_weight_drift, confirmatory_expert_rules = conf_rules, 
                              optional_expert_rules = opt_rules, 
                              optional_linear_terms= opt_linear,
                              confirmatory_linear_terms = conf_linear, 
                              optional_penalty = 0.5, print_output = F)

erf_only_weight <- ExpertRuleFit(X = X_weight_drift, y = y_weight_drift, Xtest = Xtest_weight_drift,
                              ytest = ytest_weight_drift, confirmatory_expert_rules = conf_rules, 
                              optional_expert_rules = opt_rules, 
                              optional_linear_terms= opt_linear,
                              confirmatory_linear_terms = conf_linear, expert_only = T,
                              print_output = F)

rf_weight <- ExpertRuleFit(X = X_weight_drift, y = y_weight_drift, Xtest = Xtest_weight_drift,
                        ytest = ytest_weight_drift, print_output = F)


pre_weight <- pre_for_comparison(train = train_weight_drift, test = test_weight_drift)

weight_drift_performance <- data.frame(AUC = c(erf_weight$AUC, erf_conf_weight$AUC, erf_prio_weight$AUC, erf_only_weight$AUC, rf_weight$AUC, pre_weight$AUC),
                                    ClassErr = c(erf_weight$ClassErr, erf_conf_weight$ClassErr, erf_prio_weight$ClassErr, erf_only_weight$ClassErr, rf_weight$ClassErr, pre_weight$ClassErr),
                                    NTerms = c(erf_weight$NTerms, erf_conf_weight$NTerms, erf_prio_weight$NTerms, erf_only_weight$NTerms, rf_weight$NTerms, pre_weight$NTerms),
                                    AvgRuleLength = c(erf_weight$AvgRuleLength, erf_conf_weight$AvgRuleLength, erf_prio_weight$AvgRuleLength, erf_only_weight$AvgRuleLength, rf_weight$AvgRuleLength, pre_weight$AvgRuleLength), 
                                    Model = c("ERF", "ERF conf", "ERF prio", "ERF only", "RuleFit", "PRE"))

weight_drift_performance



# Plots

# 1. AUC Plot
p5 <- ggplot(weight_drift_performance, aes(x = Model, y=AUC, color = Model)) +
             geom_point(size = 4) +
             scale_color_manual(values=c("#999999", "#4C9900", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
             labs(title = "AUC in a concept drift setting (trained: BMI>30)", x ="Model", y = "AUC") + 
             scale_y_continuous(limits = c(0.5, 1)) + theme(plot.title = element_text(size=15),
                                                                          legend.text=element_text(size=11),
                                                                          text = element_text(size = 14), 
                                                                          axis.text = element_text(size = 12))

p5
pdf("AUC_BMI30_conceptdrift_diabetes.pdf")
p5
dev.off()

# 1. Classification Error Plot
p6 <- ggplot(weight_drift_performance, aes(x = Model, y = ClassErr, color = Model)) +
      geom_point(size = 4) +
      scale_color_manual(values=c("#999999", "#4C9900", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
      labs(title = "Classication Error in a concept drift setting (train: BMI>30)", x ="Model", y = "Classification Error") + 
      scale_y_continuous(limits = c(0, 0.5)) + theme(plot.title = element_text(size=15),
                                                        legend.text=element_text(size=11),
                                                        text = element_text(size = 14), 
                                                        axis.text = element_text(size = 12))

p6
pdf("CE_BMI30_conceptdrift_diabetes.pdf")
p6
dev.off()

# 1. NTerms Plot
p7 <- ggplot(weight_drift_performance, aes(x = Model, y=NTerms, color = Model)) +
      geom_point(size = 4) +
      scale_color_manual(values=c("#999999", "#4C9900", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
      labs(title = "Model complexity in a concept drift setting (train: BMI>30)", x ="Model", y = "Number of terms") + 
      scale_y_continuous(limits = c(0, 40)) +
      theme(plot.title = element_text(size=15),
          legend.text=element_text(size=11),
          text = element_text(size = 14), 
          axis.text = element_text(size = 12))

p7
pdf("NTerms_BMI30_conceptdrift_diabetes.pdf")
p7
dev.off()

# 1. AvgRuleLength Plot
p8 <- ggplot(weight_drift_performance, aes(x = Model, y=AvgRuleLength, color = Model)) +
      geom_point(size = 4) +
      scale_color_manual(values=c("#999999", "#4C9900", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
      labs(title = "Model complexity in a concept drift setting (train: BMI>30)", x ="Model", y = "Average rule length") + 
      scale_y_continuous(limits = c(0, 4))  + theme(plot.title = element_text(size=15),
                                                    legend.text=element_text(size=11),
                                                    text = element_text(size = 14), 
                                                    axis.text = element_text(size = 12))

p8
pdf("AvgRuleLength_BMI30_conceptdrift_diabetes.pdf")
p8
dev.off()
