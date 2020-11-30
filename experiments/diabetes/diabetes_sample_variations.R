################################################################################
#                                                                              #
#             Can Expert Knowledge increase Data Efficiency?                   #
#                                                                              #
#                   Experiments on Predicting Diabetes                         #
#                                                                              #
################################################################################ 

source("./experiments/diabetes/erf_diabetes_dataprep.R")
source("./ERF/erf_main.R")


# 1. Training Data 
data <- read.csv(file = './data sets/diabetes.csv', header = T)
data <- prepare_diabetes_data(data)

# 2. Expert Knowledge
source("./expert knowledge/diabetes/EK_diabetes.R")

# Rules
rules <- c(fdk_rules1, fdk_rules2, hek_rules)
conf_rules <- support_take(rules, data, 0.05)
opt_rules <- setdiff(rules, conf_rules)

#  Linear Terms
conf_linear <- c("Age", "BMI", "DPF")
opt_linear <- c("BP", "Glucose")


#===============================================================================
#                 EXPERIMENT: VARYING DATASET SIZES
#===============================================================================

# apply the same ERF, ERF with EK prioritized, ERF with EK only, RF,
# PRE Model with 786 (all), 600, 400, 200 observations

set.seed(2394)
data_600 <- sample_n(data, 600)
data_400 <- sample_n(data, 400)
data_200 <- sample_n(data, 200)

# ERF Model 1 (use full data set)
erf_full <- CV_erf(data = data, confirmatory_expert_rules = conf_rules, 
                   optional_expert_rules = opt_rules, 
                   optional_linear_terms= opt_linear,
                   confirmatory_linear_terms = conf_linear)


# ERF Model 2 (600 obs)
erf_600 <- CV_erf(data = data_600, confirmatory_expert_rules = conf_rules, 
                   optional_expert_rules = opt_rules, 
                   optional_linear_terms= opt_linear,
                   confirmatory_linear_terms = conf_linear)


# ERF Model 3 (400 obs)
erf_400 <- CV_erf(data = data_400, confirmatory_expert_rules = conf_rules, 
                   optional_expert_rules = opt_rules, 
                   optional_linear_terms= opt_linear,
                   confirmatory_linear_terms = conf_linear)


# ERF Model 4 (200 obs)
erf_200 <- CV_erf(data = data_200, confirmatory_expert_rules = conf_rules, 
                   optional_expert_rules = opt_rules, 
                   optional_linear_terms= opt_linear,
                   confirmatory_linear_terms = conf_linear)


# create dataframe to plot 
erf_vec_200 <- unlist(erf_200, use.names=FALSE)
erf_vec_400 <- unlist(erf_400, use.names=FALSE)
erf_vec_600 <- unlist(erf_600, use.names=FALSE)
erf_vec_full <- unlist(erf_full, use.names=FALSE)

erf_performance <- data.frame(erf_full = erf_vec_full, erf_600 = erf_vec_600, 
                              erf_400 = erf_vec_400, erf_200 = erf_vec_200)
row.names(erf_performance) <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr", 
                                "PropEKImp", "PropEK", "PropOptionalEK")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ERF with EK Priority 1 (use full data set)
erf_prio_full <- CV_erf(data = data, confirmatory_expert_rules = conf_rules, 
                   optional_expert_rules = opt_rules, 
                   optional_linear_terms= opt_linear,
                   confirmatory_linear_terms = conf_linear, 
                   optional_penalty = 0.5)


# ERF with EK Priority 2 (600 obs)
erf_prio_600 <- CV_erf(data = data_600, confirmatory_expert_rules = conf_rules, 
                  optional_expert_rules = opt_rules, 
                  optional_linear_terms= opt_linear,
                  confirmatory_linear_terms = conf_linear,
                  optional_penalty = 0.5)


# ERF with EK Priority 3 (400 obs)
erf_prio_400 <- CV_erf(data = data_400, confirmatory_expert_rules = conf_rules, 
                  optional_expert_rules = opt_rules, 
                  optional_linear_terms= opt_linear,
                  confirmatory_linear_terms = conf_linear,
                  optional_penalty = 0.5)


# ERF with EK Priority (200 obs)
erf_prio_200 <- CV_erf(data = data_200, confirmatory_expert_rules = conf_rules, 
                  optional_expert_rules = opt_rules, 
                  optional_linear_terms= opt_linear,
                  confirmatory_linear_terms = conf_linear,
                  optional_penalty = 0.5)


# create dataframe to plot 
erf_prio_vec_200 <- unlist(erf_prio_200, use.names=FALSE)
erf_prio_vec_400 <- unlist(erf_prio_400, use.names=FALSE)
erf_prio_vec_600 <- unlist(erf_prio_600, use.names=FALSE)
erf_prio_vec_full <- unlist(erf_prio_full, use.names=FALSE)

erf_prio_performance <- data.frame(erf_prio_full = erf_prio_vec_full, 
                                   erf_prio_600 = erf_prio_vec_600, 
                                   erf_prio_400 = erf_prio_vec_400, 
                                   erf_prio_200 = erf_prio_vec_200)
row.names(erf_prio_performance) <- c("NTerms", "AvgRuleLength", "AUC",
                                     "ClassErr", "PropEKImp", "PropEK",
                                     "PropOptionalEK")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ERF EK only 1 (use full data set)
erf_only_full <- CV_erf(data = data, confirmatory_expert_rules = conf_rules, 
                   optional_expert_rules = opt_rules, 
                   optional_linear_terms= opt_linear,
                   confirmatory_linear_terms = conf_linear, expert_only = T)


# ERF EK only 2 (600 obs)
erf_only_600 <- CV_erf(data = data_600, confirmatory_expert_rules = conf_rules, 
                  optional_expert_rules = opt_rules, 
                  optional_linear_terms= opt_linear,
                  confirmatory_linear_terms = conf_linear, expert_only = T)


# ERF EK only 3 (400 obs)
erf_only_400 <- CV_erf(data = data_400, confirmatory_expert_rules = conf_rules, 
                  optional_expert_rules = opt_rules, 
                  optional_linear_terms= opt_linear,
                  confirmatory_linear_terms = conf_linear, expert_only = T)


# ERF EK only 4 (200 obs)
erf_only_200 <- CV_erf(data = data_200, confirmatory_expert_rules = conf_rules, 
                  optional_expert_rules = opt_rules, 
                  optional_linear_terms= opt_linear,
                  confirmatory_linear_terms = conf_linear, expert_only = T)


# create dataframe to plot 
erf_only_vec_200 <- unlist(erf_only_200, use.names=FALSE)
erf_only_vec_400 <- unlist(erf_only_400, use.names=FALSE)
erf_only_vec_600 <- unlist(erf_only_600, use.names=FALSE)
erf_only_vec_full <- unlist(erf_only_full, use.names=FALSE)

erf_only_performance <- data.frame(erf_only_full = erf_only_vec_full,
                              erf_only_600 = erf_only_vec_600, 
                              erf_only_400 = erf_only_vec_400, 
                              erf_only_200 = erf_only_vec_200)
row.names(erf_only_performance) <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr",
                                     "PropEKImp", "PropEK", "PropOptionalEK")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# RuleFit

# RF Model 1 (use full data set)
rf_full <- CV_erf(data = data)

# ERF Model 2 (600 obs)
rf_600 <- CV_erf(data = data_600)

# ERF Model 3 (400 obs)
rf_400 <- CV_erf(data = data_400)


# ERF Model 4 (200 obs)
rf_200 <- CV_erf(data = data_200)

# create dataframe to plot 
rf_vec_200 <- unlist(rf_200, use.names=FALSE)
rf_vec_400 <- unlist(rf_400, use.names=FALSE)
rf_vec_600 <- unlist(rf_600, use.names=FALSE)
rf_vec_full <- unlist(rf_full, use.names=FALSE)

rf_performance <- data.frame(rf_full = rf_vec_full, rf_600 = rf_vec_600, rf_400 = rf_vec_400, rf_200 = rf_vec_200)
row.names(rf_performance) <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr", "PropEKImp", "PropEK", "PropOptionalEK")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prediction Rule Ensembles


# PRE Model 1 (use full data set)
pre_full <- CV_pre(data)

# PRE Model 2 (600 obs)
pre_600 <- CV_pre(data_600)

# PRE Model 3 (400 obs)
pre_400 <- CV_pre(data_400)


# PRE Model 4 (200 obs)
pre_200 <- CV_pre(data_200)


# create dataframe to plot 
pre_vec_200 <- unlist(pre_200, use.names=FALSE)
pre_vec_400 <- unlist(pre_400, use.names=FALSE)
pre_vec_600 <- unlist(pre_600, use.names=FALSE)
pre_vec_full <- unlist(pre_full, use.names=FALSE)

pre_performance <- data.frame(pre_full = c(pre_vec_full, 0,0,0), pre_600 = c(pre_vec_600,0,0,0), 
                              pre_400 = c(pre_vec_400,0,0,0), pre_200 = c(pre_vec_200,0,0,0))
row.names(pre_performance) <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr", "PropEKImp", "PropEK", "PropOptionalEK")

#===============================================================================

# Comparison Plots

erf_performance
erf_prio_performance
erf_only_performance
rf_performance
pre_performance

# NTerms Plot

NTerms <- unlist(c(erf_performance[1, ], erf_prio_performance[1, ], erf_only_performance[1, ], rf_performance[1, ], pre_performance[1, ]), use.names = F)
AvgRuleLength <- unlist(c(erf_performance[2, ], erf_prio_performance[2, ], erf_only_performance[2, ], rf_performance[2, ], pre_performance[2, ]), use.names = F)
AUC <- unlist(c(erf_performance[3, ], erf_prio_performance[3, ], erf_only_performance[3, ], rf_performance[3, ], pre_performance[3, ]), use.names = F)
ClassErr <- unlist(c(erf_performance[4, ], erf_prio_performance[4, ], erf_only_performance[4, ], rf_performance[4, ], pre_performance[4, ]), use.names = F)
PropEKImp <- unlist(c(erf_performance[5, ], erf_prio_performance[5, ], erf_only_performance[5, ], rf_performance[5, ], pre_performance[5, ]), use.names = F)
PropEK <- unlist(c(erf_performance[6, ], erf_prio_performance[6, ], erf_only_performance[6, ], rf_performance[6, ], pre_performance[6, ]), use.names = F)
PropOptionalEK <- unlist(c(erf_performance[7, ], erf_prio_performance[7, ], erf_only_performance[7, ], rf_performance[7, ], pre_performance[7, ]), use.names = F)

comparison_df <- data.frame(NTerms = NTerms, AvgRuleLength = AvgRuleLength, AUC = AUC, ClassErr = ClassErr,
                            PropEKImp = PropEKImp, PropEK = PropEK, PropOptionalEK = PropOptionalEK,
                            Model = c(rep("ERF", 4), rep("ERF prio", 4), rep("ERF only", 4), rep("RuleFit", 4),rep("PRE", 4)), 
                            Sample = rep(c("full (768)", "600", "400", "200"),5))

comparison_df

# 1. NTerms Plot
p1 <- ggplot(comparison_df, 
       aes(x = Sample, y=NTerms, color = Model, group = Model)) +
       geom_point(size = 3) + geom_line(size = 1) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
       scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
       labs(title = "Model complexity for varying dataset sizes", x ="dataset size", y = "Number of terms") + 
       theme_minimal() + theme(plot.title = element_text(size=15),
                               legend.text=element_text(size=11),
                               text = element_text(size = 14), 
                               axis.text = element_text(size = 12)) +
       scale_y_continuous(limits = c(0, 40)) +
       theme(legend.position = "right") 

p1
pdf("NTerms_samplesize_diabetes.pdf")
p1
dev.off()

# 2. Average Rule Length Plot

p2 <- ggplot(comparison_df, 
        aes(x = Sample, y=AvgRuleLength, color = Model, group = Model)) +
        geom_point(size = 3) + geom_line(size = 1) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
        labs(title = "Rule complexity for varying dataset sizes", x ="dataset size", y = "Average rule length") + 
        theme_minimal() + theme(plot.title = element_text(size=15),
                                legend.text=element_text(size=11),
                                text = element_text(size = 14), 
                                axis.text = element_text(size = 12)) +
        scale_y_continuous(limits = c(1, 3)) +
        theme(legend.position = "right") 

p2
pdf("Avgrulelength_samplesize_diabetes.pdf")
p2
dev.off()

# 3. AUC Plot

p3 <- ggplot(comparison_df, 
       aes(x = Sample, y=AUC, color = Model, group = Model)) +
        geom_point(size = 3) + geom_line(size = 1) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
        labs(title = "Predictive performance for varying dataset sizes", x ="dataset size", y = "AUC") + 
        theme_minimal() + theme(plot.title = element_text(size=15),
                                legend.text=element_text(size=11),
                                text = element_text(size = 14), 
                                axis.text = element_text(size = 12)) +
        scale_y_continuous(limits = c(0.5, 1)) +
        theme(legend.position = "right") 

p3
pdf("AUC_samplesize_diabetes.pdf")
p3
dev.off()

# 4. Classification Error Plot

p4 <- ggplot(comparison_df, 
       aes(x = Sample, y=ClassErr, color = Model, group = Model)) +
      geom_point(size = 3) + geom_line(size = 1) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
      scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
      labs(title = "Predictive performance for varying dataset sizes", x ="dataset size", y = "Classification Error") + 
      theme_minimal() + theme(plot.title = element_text(size=15),
                              legend.text=element_text(size=11),
                              text = element_text(size = 14), 
                              axis.text = element_text(size = 12)) +
      scale_y_continuous(limits = c(0, 0.5)) +
      theme(legend.position = "right") 

p4
pdf("ClassErr_samplesize_diabetes.pdf")
p4
dev.off()


# 5. Proportion of Expert Knowledge among the most important terms

p5 <- ggplot(comparison_df, 
             aes(x = Sample, y=PropEKImp, color = Model, group = Model)) +
             geom_point(size = 3) + geom_line(size = 1) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
             scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
             labs(title = "Proportion of EK for varying dataset sizes", x ="dataset size", y = "Proportion of EK among the 10 most important terms") + 
             theme_minimal() + theme(plot.title = element_text(size=15),
                                     legend.text=element_text(size=11),
                                     text = element_text(size = 14), 
                                     axis.text = element_text(size = 12)) +
             theme(legend.position = "right") 

p5
pdf("PropEKImp_samplesize_diabetes.pdf")
p5
dev.off()

# 6. Proportion of Expert Knowledge in the final model

p6 <- ggplot(comparison_df, 
        aes(x = Sample, y=PropEK, color = Model, group = Model)) +
        geom_point(size = 3) + geom_line(size = 1) + scale_x_discrete(limits=c("full (768)","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
        labs(title = "Proportion of EK for varying dataset sizes", x ="dataset size", y = "Proportion of EK") + 
        theme_minimal() + theme(plot.title = element_text(size=15),
                                legend.text=element_text(size=11),
                                text = element_text(size = 14), 
                                axis.text = element_text(size = 12)) +
        theme(legend.position = "right") 

p6
pdf("PropEK_samplesize_diabetes.pdf")
p6
dev.off()

