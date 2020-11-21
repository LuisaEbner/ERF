################################################################################
#                                                                              #
#         Predicting Diabetes: Experiments on the concept drift                #   
#                                                                              #
################################################################################

# Libraries
library(ggplot2)
library(plyr)


# External functions
source("./experiments/diabetes/erf_diabetes_dataprep.R")
source("./ERF/erf_main.R")

#===============================================================================
#                                 DATA
#===============================================================================

# Pima Indian Diabetes (UCI ML Repository)
data <- read.csv(file = './data sets/diabetes.csv', header = T)


# Data Preprocessing 
data <- prepare_diabetes_data(data)

#===============================================================================
#                          EXPLORATORY ANALYSIS
#===============================================================================

data$age.greater.35 <- factor((as.numeric(data$Age > 35)), levels = c("0","1"))
data$overweight <- factor((as.numeric(data$BMI > 30)), levels = c("0","1"))

# 1. Overlayed histograms according to normal- and overweight population

# a) Pregnancies
mu_preg_w <- ddply(data, "overweight", summarise, grp.mean=mean(Pregnancies))
p_preg_w  <- ggplot(data, aes(x=Pregnancies, fill = overweight, color=overweight)) +
                   geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 1)+
                   geom_density(alpha=0.4)+
                   geom_vline(data=mu_preg_w, aes(xintercept=grp.mean, color=overweight), linetype="dashed")+
                   theme(legend.position="right") +
                   scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
                   scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
                   labs(title="Histogram plot grouped by overweight (BMI > 30)",x="DPF", y = "Density")+
                   theme_classic()
p_preg_w

# b) Diabetes
mu_y_w <- ddply(data, "overweight", summarise, grp.mean=mean(as.numeric(y)))
p_y_w <-  ggplot(data, aes(x=as.numeric(y), fill = overweight, color=overweight)) +
                 geom_histogram(aes(y=..density..), position="identity", alpha=0.4, binwidth = 1)+
                 geom_vline(data=mu_y_w, aes(xintercept=grp.mean, color=overweight), linetype="dashed")+
                 theme(legend.position="right") +
                 scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
                 scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) + 
                 labs(title="Histogram plot grouped by Age (<= 35)",x="Diabetes (1 = yes, 0 = no)", y = "Density")+
                 theme_classic()
p_y_w

#-------------------------------------------------------------------------------

# 2. Overlayed histograms according to Age (being "age.greater.35" (>35) or of advanced age)

# a) Pregnancies
mu_preg_a <- ddply(data, "age.greater.35", summarise, grp.mean=mean(Pregnancies))
p_preg_a <-ggplot(data, aes(x=Pregnancies, fill = age.greater.35, color=age.greater.35)) +
                  geom_histogram(aes(y=..density..), position="identity", alpha=0.3, binwidth = 1)+
                  geom_density(alpha=0.4)+
                  geom_vline(data=mu_preg_a, aes(xintercept=grp.mean, color=age.greater.35), linetype="dashed")+
                  theme(legend.position="right") +
                  scale_color_brewer(palette="Dark2")+
                  scale_fill_brewer(palette="Dark2") +
                  labs(title="Histogram plot grouped by Age (<= 35)",x="Number of Pregnancies", y = "Density")+
                  theme_classic()
p_preg_a


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

#===============================================================================
#                         STRATIFIED SAMPLING
#===============================================================================

library(dplyr)


concept_drift_split <- function(data, condition, train_frac = 0.7, cond = 0.8){
  n_train = floor(train_frac*nrow(data))
  data_cond = data %>% filter(eval(str2expression(condition)))
  n_cond = nrow(data_cond)
  data_rest = setdiff(data, data_cond)
  frac_cond = n_cond/n_train
  cat(sprintf(" %#.4f of the examples apply to the specified condition. \n", n_cond/nrow(data)))
  cat(sprintf("\n"))
  
  if (frac_cond < cond){
    train <- data_cond
    n_extra <- n_train - nrow(train)
    extra <- data_rest %>% sample_n(n_extra)
    train <- rbind(train, extra)
  } else{
    train <- data_cond %>% sample_n(frac_cond*n_cond)
    cat(sprintf(" %d examples from the %d examples defined as training set apply to the specified condition. \n", nrow(train), n_train))
    n_extra <- n_train - nrow(train)
    extra <- data_rest %>% sample_n(n_extra)
    train <- rbind(train, extra)
  }
  
  test <- setdiff(data, train)
  
  X <- train[, -ncol(train)]
  Xtest <- test[, -ncol(test)]
  
  y <- train[, ncol(train)]
  ytest <- test[, ncol(test)]

  
  out <- list(X, y, Xtest, ytest)
  out
  
}

# Example
# drift <- concept_drift_split(data, "Age <= 35")
# X <- drift[[1]]
# y <- drift[[2]]
# Xtest <- drift[[3]]
# ytest <- drift[[4]]


# 2. EXPERT KNOWLEDGE

source("./expert knowledge/diabetes/EK_diabetes.R")



