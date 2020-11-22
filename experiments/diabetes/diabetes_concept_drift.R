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
mu_preg_a <- ddply(data, "age.greater.35", summarise, grp.mean=mean(Pregnancies))
p3 <-ggplot(data, aes(x=Pregnancies, fill = age.greater.35, color=age.greater.35)) +
            geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 1)+
            geom_density(alpha=0.4)+
            geom_vline(data=mu_preg_a, aes(xintercept=grp.mean, color=age.greater.35), linetype="dashed")+
            theme(legend.position="right") +
            scale_color_brewer(palette="Dark2")+
            scale_fill_brewer(palette="Dark2") +
            labs(title="Histogram plot grouped by Age (> 35)",x="Number of Pregnancies", y = "Density")+
            theme_classic()
p3
pdf("Pregnancies_age.pdf")
p3
dev.off()


# b) Glucose
mu_glucose_a <- ddply(data, "age.greater.35", summarise, grp.mean=mean(Glucose))
p_glucose_a  <- ggplot(data, aes(x=Glucose, fill = age.greater.35, color=age.greater.35)) +
                      geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 5)+
                      geom_density(alpha=0.4)+
                      geom_vline(data=mu_glucose_a, aes(xintercept=grp.mean, color=age.greater.35), linetype="dashed")+
                      theme(legend.position="right") +
                      scale_color_brewer(palette="Dark2")+
                      scale_fill_brewer(palette="Dark2") +
                      labs(title="Histogram plot grouped by Age (<= 35)",x="Glucose level", y = "Density")+
                      theme_classic()
p_glucose_a


# c) Blood Pressure
mu_bp_a <- ddply(data, "age.greater.35", summarise, grp.mean=mean(BP))
p_bp_a <-ggplot(data, aes(x=BP, fill = age.greater.35, color=age.greater.35)) +
                geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 5)+
                geom_density(alpha=0.4)+
                geom_vline(data=mu_bp_a, aes(xintercept=grp.mean, color=age.greater.35), linetype="dashed")+
                theme(legend.position="right") +
                scale_color_brewer(palette="Dark2")+
                scale_fill_brewer(palette="Dark2")+
                labs(title="Histogram plot grouped by Age (<= 35)",x="Blood Pressure", y = "Density")+
                theme_classic()
p_bp_a

# d) Diabetes
mu_y_a <- ddply(data, "age.greater.35", summarise, grp.mean=mean(as.numeric(y)))
p_y_a <-ggplot(data, aes(x=as.numeric(y), fill = age.greater.35, color=age.greater.35)) +
               geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 1)+
               geom_vline(data=mu_y_a, aes(xintercept=grp.mean, color=age.greater.35), linetype="dashed")+
               theme(legend.position="right") +
               scale_color_brewer(palette="Dark2") +
               scale_fill_brewer(palette="Dark2") +
               labs(title="Histogram plot grouped by Age (<= 35)",x="Diabetes (1 = yes, 0 = no)", y = "Density")+
               theme_classic()
p_y_a



# Concept Drift 1: younger vs. older population
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


#===============================================================================
#             MODEL COMPARISON: GENERALIZABILITY/CONCEPT DRIFT
#===============================================================================


