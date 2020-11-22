################################################################################
#                                                                              #
#         Predicting Diabetes: Experiments on the concept drift                #   
#                                                                              #
################################################################################

# Libraries
# library(ggplot2)
# library(plyr)
# library(dplyr)


# External functions
source("./experiments/diabetes/erf_diabetes_dataprep.R")
source("./ERF/erf_main.R")

#===============================================================================
#                                 DATA
#===============================================================================

# Pima Indian Diabetes (UCI ML Repository)
data <- read.csv(file = './data sets/diabetes.csv', header = T)
data <- prepare_diabetes_data(data)


#===============================================================================
#                          EXPLORATORY ANALYSIS
#===============================================================================

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
             theme_classic()
p1
pdf("Pregnancies_overweight.pdf")
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
             theme_classic()
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
            theme_classic()
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
              theme_classic()
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
            theme_classic()

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
             theme_classic()
p6
pdf("y_age.pdf")
p6
dev.off()


# Concept Drift 1: younger vs. older population

data <- read.csv(file = './data sets/diabetes.csv', header = T)
data <- prepare_diabetes_data(data)

set.seed(234567)
age_drift <- concept_drift_split(data, "Age <= 35")
X_age_drift <- age_drift[[1]]
y_age_drift <- age_drift[[2]]
Xtest_age_drift <- age_drift[[3]]
ytest_age_drift <- age_drift[[4]]

# Concept Drift 2: normal weight vs. overweight population
weight_drift <- concept_drift_split(data, "BMI > 30")
X_weight_drift <- weight_drift[[1]]
y_weight_drift <- weight_drift[[2]]
Xtest_weight_drift <- weight_drift[[3]]
ytest_weight_drift <- weight_drift[[4]]

# Expert Knowledge

source("./expert knowledge/diabetes/EK_diabetes.R")

# Rules
rules <- c(fdk_rules1, fdk_rules2, hek_rules)
conf_rules <- support_take(rules, data, 0.05)
opt_rules <- setdiff(rules, conf_rules)

#  Linear Terms
conf_linear <- c("Age", "BMI", "DPF")
opt_linear <- c("BP", "Glucose")



#===============================================================================
#             MODEL COMPARISON: GENERALIZABILITY/CONCEPT DRIFT
#===============================================================================

# Experiments on the concept drift when splitting population into younger and older patients

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

pre_age 

#===============================================================================

# Experiments on the concept drift when splitting population into normal weight and overweight patients

erf_weight <- ExpertRuleFit(X = X_weight_drift, y = y_weight_drift, Xtest = Xtest_weight_drift,
                         ytest = ytest_weight_drift, confirmatory_expert_rules = conf_rules, 
                         optional_expert_rules = opt_rules, 
                         optional_linear_terms= opt_linear,
                         confirmatory_linear_terms = conf_linear, print_output = F)

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

pre_weight 
