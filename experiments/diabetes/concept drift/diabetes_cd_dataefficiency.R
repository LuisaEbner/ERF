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

# Data
data <- read.csv(file = './data sets/diabetes.csv', header = T)
data <- prepare_diabetes_data(data)

# Data sample
set.seed(2594)
data_600 <- sample_n(data, 600)
data_400 <- sample_n(data, 400)
data_200 <- sample_n(data, 200)


# Expert Knowledge
source("./expert knowledge/diabetes/EK_diabetes.R")
rules <- c(fdk_rules1, fdk_rules2, hek_rules)

conf_rules <- support_take(rules, data, 0.03)
opt_rules <- setdiff(rules, conf_rules)

#  Linear Terms
conf_lins <- c("Age", "BMI", "DPF")
opt_lins <- c("BP", "Glucose")

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

# 80-20 Train-test-split
# 90-10 vs. 10-90 young old split

# Recommendation 1: Age <= 30 vs. Age > 30
# Recommendation 2: BMI <= 30 vs. BMI > 30

createERF_drift_sets <- function(data, warning_size = 200,
                                 drift_condition = NULL, 
                                 conf_rules = NULL, opt_rules = NULL, 
                                 conf_lins = NULL, opt_lins = NULL,
                                 train_frac = 0.7, drift_frac = 0.95, folds = 10){
  
  p <- list()
  
  for(i in 1:folds){
    
    data_cond <- data %>% filter(eval(str2expression(drift_condition)))
    data_anticond <- setdiff(data, data_cond)
    
    n_cond <- nrow(data_cond)
    n_anticond <- nrow(data_anticond)
    
    # function input checks
    if(n_cond < warning_size){
      stop("Warning: Too few examples match the specified condition.")
    }
    
    # create train set
    n_train_cond <- ceiling(train_frac*drift_frac*n_cond)
    train_cond <- sample_n(data_cond, n_train_cond)
    
    n_train_anticond <- ceiling(train_frac*(1-drift_frac)*n_anticond)
    train_anticond <- sample_n(data_anticond, n_train_anticond)
    
    # create test set
    n_test_cond <- ceiling((1-train_frac)*(1-drift_frac)*n_cond)
    test_cond <- sample_n(setdiff(data_cond, train_cond), n_test_cond)
    
    n_test_cond <- ceiling((1-train_frac)*(1-drift_frac)*n_cond)
    test_cond <- sample_n(setdiff(data_cond, train_cond), n_test_cond)
    
    n_test_anticond <- ceiling((1-train_frac)*(drift_frac)*n_anticond)
    test_anticond <- sample_n(setdiff(data_anticond, train_anticond), n_test_anticond)
    
    train <- rbind(train_cond, train_anticond)
    test <- rbind(test_cond, test_anticond)
    
    
    X <- train[, -ncol(train)]
    Xtest <- test[, -ncol(test)]
    y <- train[, ncol(train)]
    ytest <- test[, ncol(test)]
    
    
    erf <- ExpertRuleFit(X = X, y = y, Xtest = Xtest, ytest = ytest, 
                  confirmatory_expert_rules = conf_rules, 
                  optional_expert_rules = opt_rules, 
                  optional_linear_terms= opt_lins,
                  confirmatory_linear_terms = conf_lins, print_output = F)
    
    erf_prio <- ExpertRuleFit(X = X, y = y, Xtest = Xtest, ytest = ytest, 
                         confirmatory_expert_rules = conf_rules, 
                         optional_expert_rules = opt_rules, 
                         optional_linear_terms= opt_lins,
                         confirmatory_linear_terms = conf_lins, 
                         optional_penalty = 0.5, print_output = F)
    
    erf_only <- ExpertRuleFit(X = X, y = y, Xtest = Xtest, ytest = ytest, 
                         confirmatory_expert_rules = conf_rules, 
                         confirmatory_linear_terms = conf_lins, 
                         expert_only = T, print_output = F)
    
    rf <- ExpertRuleFit(X = X, y = y, Xtest = Xtest, ytest = ytest, 
                        print_output = F)
    
    pre <- pre_for_comparison(train = train, test = test)
    
    
    performance <- data.frame(ERF = c(erf$AUC, erf$ClassErr, erf$NTerms, erf$AvgRuleLength, erf$PropEKImp, erf$PropEK),
                              ERF_prio = c(erf_prio$AUC, erf_prio$ClassErr, erf_prio$NTerms, erf_prio$AvgRuleLength, erf_prio$PropEKImp, erf_prio$PropEK),
                              ERF_only = c(erf_only$AUC, erf_only$ClassErr, erf_only$NTerms, erf_only$AvgRuleLength, erf_only$PropEKImp, erf_only$PropEK),
                              RuleFit = c(rf$AUC, rf$ClassErr, rf$NTerms, rf$AvgRuleLength, rf$PropEKImp, rf$PropEK),
                              PRE = c(pre$AUC, pre$ClassErr, pre$NTerms, pre$AvgRuleLength, 0, 0))
    row.names(performance) <- c("AUC", "ClassErr", "NTerms", "AvgRuleLength", "PropEKImp", "PropEK")
    
    
    print(performance)
    p[[i]] <- performance
    
    
  }
  
  cv_perf <- apply(abind::abind(p[[1]], p[[2]], p[[3]], p[[4]],p[[5]], p[[6]],
                                p[[7]], p[[8]],p[[9]], p[[10]], along = 3), 1:2, mean) 
  cv_perf

}
  



pf <- createERF_drift_sets(data = data, drift_condition = "Age > 30", 
                                  conf_rules = conf_rules, opt_rules = opt_rules, 
                                  conf_lins = conf_lins, opt_lins = opt_lins)
  

p6 <- createERF_drift_sets(data = data_600, warning_size = 100,
                                 drift_condition = "Age > 30", 
                                 conf_rules = conf_rules, opt_rules = opt_rules, 
                                 conf_lins = conf_lins, opt_lins = opt_lins)


p4 <- createERF_drift_sets(data = data_400,  warning_size = 50,
                                 drift_condition = "Age > 30", 
                                 conf_rules = conf_rules, opt_rules = opt_rules, 
                                 conf_lins = conf_lins, opt_lins = opt_lins)


p2 <- createERF_drift_sets(data = data_200, warning_size = 10,
                                 drift_condition = "Age > 30", 
                                  conf_rules = conf_rules, opt_rules = opt_rules, 
                                  conf_lins = conf_lins, opt_lins = opt_lins)


AUC <- unlist(c(pf[1, ], p6[1, ], p4[1, ], p2[1, ]), use.names = F)
ClassAcc <- unlist(c((1-pf[2, ]), (1-p6[2, ]), (1-p4[2, ]), (1-p2[2, ])), use.names = F)
NTerms <- unlist(c(pf[3, ], p6[3, ], p4[3, ], p2[3, ]), use.names = F)
AvgRuleLength <- unlist(c(pf[4, ], p6[4, ], p4[4, ], p2[4, ]), use.names = F)
PropEKImp <- unlist(c(pf[5, ], p6[5, ], p4[5, ], p2[5, ]), use.names = F)
PropEK <- unlist(c(pf[6, ], p6[6, ], p4[6, ], p2[6, ]), use.names = F)

comparison_df <- data.frame(AUC = AUC,  ClassAcc = ClassAcc, NTerms = NTerms, AvgRuleLength = AvgRuleLength, 
                            PropEKImp = PropEKImp, PropEK = PropEK, 
                            Sample = c(rep("full (768)", 5), rep("600", 5),
                                      rep("400", 5), rep("200", 5)),
                            Model = rep(c("ERF", "ERF prio", "ERF only", "RuleFit", "PRE"),4))

comparison_df

save.image(file = "diabetes_conceptdrift_Age_lambdamin.RData")
#load("diabetes_conceptdrift_Age_lambdamin.RData")



# 1. NTerms Plot
b1 <- ggplot(comparison_df, 
             aes(x = Sample, y=NTerms, color = Model, group = Model)) +
  geom_point(size = 3) + geom_line(size = 1.3) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
  labs(title = "Model complexity in a concept drift setting", x ="dataset size", y = "Number of terms") + 
  theme_minimal() + theme(plot.title = element_text(size=15),
                          legend.text=element_text(size=11),
                          text = element_text(size = 14), 
                          axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 80)) +
  theme(legend.position = "right") 

b1
pdf("NTerms_conceptdrift_Age.pdf")
b1
dev.off()


# 2. Average Rule Length Plot

b2 <- ggplot(comparison_df, 
             aes(x = Sample, y=AvgRuleLength, color = Model, group = Model)) +
  geom_point(size = 3) + geom_line(size = 1.3) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
  labs(title = "Rule complexity in a concept drift setting", x ="dataset size", y = "Average rule length") + 
  theme_minimal() + theme(plot.title = element_text(size=12),
                          legend.text=element_text(size=11),
                          text = element_text(size = 14), 
                          axis.text = element_text(size = 15)) +
  scale_y_continuous(limits = c(1, 4)) +
  theme(legend.position = "right") 

b2
pdf("Avgrulelength_conceptdrift_Age.pdf")
b2
dev.off()

# 3. AUC Plot

b3 <- ggplot(comparison_df, 
             aes(x = Sample, y=AUC, color = Model, group = Model)) +
  geom_point(size = 3) + geom_line(size = 1.3) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
  labs(title = "Predictive performance in a concept drift setting", x ="dataset size", y = "AUC") + 
  theme_minimal() + theme(plot.title = element_text(size=15),
                          legend.text=element_text(size=11),
                          text = element_text(size = 14), 
                          axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0.5, 1)) +
  theme(legend.position = "right") 

b3
pdf("AUC_conceptdrift_Age.pdf")
b3
dev.off()

# 4. Classification Error Plot

b4 <- ggplot(comparison_df, 
             aes(x = Sample, y=ClassAcc, color = Model, group = Model)) +
  geom_point(size = 3) + geom_line(size = 1.3) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
  labs(title = "Predictive performance in a concept drift setting", x ="dataset size", y = "Classification Accuracy") + 
  theme_minimal() + theme(plot.title = element_text(size=15),
                          legend.text=element_text(size=11),
                          text = element_text(size = 14), 
                          axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0.5,1)) +
  theme(legend.position = "right") 

b4
pdf("ClassErr_conceptdrift_Age.pdf")
b4
dev.off()


# 5. Proportion of Expert Knowledge among the most important terms

b5 <- ggplot(comparison_df, 
             aes(x = Sample, y=PropEKImp, color = Model, group = Model)) +
  geom_point(size = 3) + geom_line(size = 1.3) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
  labs(title = "Proportion of EK in a concept drift setting", x ="dataset size", y = "Proportion of EK among the 10 most important terms") + 
  theme_minimal() + theme(plot.title = element_text(size=15),
                          legend.text=element_text(size=11),
                          text = element_text(size = 14), 
                          axis.text = element_text(size = 12)) +
  theme(legend.position = "right") 

b5
pdf("PropEKImp_conceptdrift_Age.pdf")
b5
dev.off()

# 6. Proportion of Expert Knowledge in the final model

b6 <- ggplot(comparison_df, 
             aes(x = Sample, y=PropEK, color = Model, group = Model)) +
  geom_point(size = 3) + geom_line(size = 1.3) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
  labs(title = "Proportion of EK in a concept drift setting", x ="dataset size", y = "Proportion of EK") + 
  theme_minimal() + theme(plot.title = element_text(size=15),
                          legend.text=element_text(size=11),
                          text = element_text(size = 14), 
                          axis.text = element_text(size = 12)) +
  theme(legend.position = "right") 

b6
pdf("PropEK_conceptdrift_Age.pdf")
b6
dev.off()
