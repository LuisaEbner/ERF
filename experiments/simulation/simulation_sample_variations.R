################################################################################
#                                                                              #
#             Can Expert Knowledge increase Data Efficiency?                   #
#                                                                              #
#                   Experiments on Predicting Diabetes                         #
#                                                                              #
################################################################################ 

# Libraries
# library(gridExtra)

# External functions
source("./data simulation/simulation_main.R")
source("./ERF/erf_main.R")

# todo: try lambda.min

sample_variations <- function(conf_support = 0.01, sample_sizes = c(600, 400, 200), 
                              cv_folds = 1, optional_penalty = 0.5){
        simulation <- create_simulation(n_vars = 30, n_obs = 1000,
                                        mu = 0, sigma = 1, 
                                        n_rule_vars = 10, 
                                        n_rel_rules = 20, 
                                        optional_lengths = c(1, 2, 3),
                                        weights = c(1/3, 1/3, 1/3),
                                        mu_beta = 0, sigma_beta = 5, 
                                        mu_epsilon = 0, sigma_epsilon = 0.01)
        data <- simulation[[2]]
        
        data_600 <- sample_n(data, sample_sizes[1])
        data_400 <- sample_n(data, sample_sizes[2])
        data_200 <- sample_n(data, sample_sizes[3])
        
        expert_rules <- simulation[[3]]
        conf_rules <- expert_rules[1:17]
        opt_rules <- setdiff(expert_rules, conf_rules)
        
        # ERF
        erf_full <- CV_erf(data = data, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                           optional_expert_rules = opt_rules)

        erf_600 <- CV_erf(data = data_600, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                          optional_expert_rules = opt_rules)

        erf_400 <- CV_erf(data = data_400, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                          optional_expert_rules = opt_rules)

        erf_200 <- CV_erf(data = data_200, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                          optional_expert_rules = opt_rules)
        
        erf_vec_200 <- unlist(erf_200, use.names=FALSE)
        erf_vec_400 <- unlist(erf_400, use.names=FALSE)
        erf_vec_600 <- unlist(erf_600, use.names=FALSE)
        erf_vec_full <- unlist(erf_full, use.names=FALSE)
        erf_performance <- data.frame(erf_full = erf_vec_full, erf_600 = erf_vec_600,
                                      erf_400 = erf_vec_400, erf_200 = erf_vec_200)
        row.names(erf_performance) <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr",
                                        "PropEKImp", "PropEK", "PropOptionalEK")
        
        print("erf done")
        
        # ERF with EK Priority
        erf_prio_full <- CV_erf(data = data, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                                optional_expert_rules = opt_rules, optional_penalty = optional_penalty)
        erf_prio_600 <- CV_erf(data = data_600, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                               optional_expert_rules = opt_rules, optional_penalty = optional_penalty)
        erf_prio_400 <- CV_erf(data = data_400, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                               optional_expert_rules = opt_rules,  optional_penalty = optional_penalty)
        erf_prio_200 <- CV_erf(data = data_200, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                               optional_expert_rules = opt_rules,  optional_penalty = optional_penalty)
        
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
        
        print("erf prio done")
        
        # ERF with EK only
        erf_only_full <- CV_erf(data = data, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                                optional_expert_rules = opt_rules, expert_only = T)

        erf_only_600 <- CV_erf(data = data_600, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                               optional_expert_rules = opt_rules, expert_only = T)

        erf_only_400 <- CV_erf(data = data_400, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                               optional_expert_rules = opt_rules, expert_only = T)
        
        erf_only_200 <- CV_erf(data = data_200, cv_folds = cv_folds, confirmatory_expert_rules = conf_rules, 
                               optional_expert_rules = opt_rules, expert_only = T)
        
        erf_only_vec_200 <- unlist(erf_only_200, use.names=FALSE)
        erf_only_vec_400 <- unlist(erf_only_400, use.names=FALSE)
        erf_only_vec_600 <- unlist(erf_only_600, use.names=FALSE)
        erf_only_vec_full <- unlist(erf_only_full, use.names=FALSE)
        erf_only_performance <- data.frame(erf_only_full = erf_only_vec_full,
                                           erf_only_600 = erf_only_vec_600, 
                                           erf_only_400 = erf_only_vec_400, 
                                           erf_only_200 = erf_only_vec_200)
        row.names(erf_only_performance) <- c("NTerms", "AvgRuleLength", "AUC",
                                             "ClassErr","PropEKImp", "PropEK",
                                             "PropOptionalEK")
        
        print("erf only done")
        
        # ERF without EK
        rf_full <- CV_erf(data = data, cv_folds = cv_folds)
        rf_600 <- CV_erf(data = data_600, cv_folds = cv_folds)
        rf_400 <- CV_erf(data = data_400, cv_folds = cv_folds)
        rf_200 <- CV_erf(data = data_200, cv_folds = cv_folds)
        
        rf_vec_200 <- unlist(rf_200, use.names=FALSE)
        rf_vec_400 <- unlist(rf_400, use.names=FALSE)
        rf_vec_600 <- unlist(rf_600, use.names=FALSE)
        rf_vec_full <- unlist(rf_full, use.names=FALSE)
        rf_performance <- data.frame(rf_full = rf_vec_full, rf_600 = rf_vec_600, 
                                     rf_400 = rf_vec_400, rf_200 = rf_vec_200)
        row.names(rf_performance) <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr",
                                       "PropEKImp", "PropEK", "PropOptionalEK")
        
        print("rf done")
        
        # PRE 
        pre_full <- CV_pre(data, cv_folds = cv_folds)
        pre_600 <- CV_pre(data_600, cv_folds = cv_folds)
        pre_400 <- CV_pre(data_400, cv_folds = cv_folds)
        pre_200 <- CV_pre(data_200, cv_folds = cv_folds)
        
        pre_vec_200 <- unlist(pre_200, use.names=FALSE)
        pre_vec_400 <- unlist(pre_400, use.names=FALSE)
        pre_vec_600 <- unlist(pre_600, use.names=FALSE)
        pre_vec_full <- unlist(pre_full, use.names=FALSE)
        pre_performance <- data.frame(pre_full = c(pre_vec_full, 0, 0, 0),
                                      pre_600 = c(pre_vec_600, 0, 0, 0),
                                      pre_400 = c(pre_vec_400, 0, 0, 0), 
                                      pre_200 = c(pre_vec_200, 0, 0, 0))
        row.names(pre_performance) <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr",
                                        "PropEKImp", "PropEK", "PropOptionalEK")
        
        print("pre done")
        
        out <- list(erf_performance,
                    erf_prio_performance,
                    erf_only_performance,
                    rf_performance,
                    pre_performance)
        
        out
        
}

x1 <- sample_variations()
x2 <- sample_variations()
x3 <- sample_variations()
x4 <- sample_variations()
x5 <- sample_variations()

res_list <- list(x1, x2, x3, x4, x5)

cv_sim <- function(res_list){
        cv_res_list <- list()
        for(i in 1:length(res_list[[1]])){
                cv_res_list[[i]] <- apply(abind::abind(res_list[[1]][[i]], 
                                                       res_list[[2]][[i]],
                                                       res_list[[3]][[i]],
                                                       res_list[[4]][[i]],
                                                       res_list[[5]][[i]],
                                                       along = 3), 1:2, mean)  
        }
        cv_res_list
}


final <- cv_sim(res_list)

final
names(final) <- c("erf_performance", "erf_prio_performance", "erf_only_performance", "rf_performance", "pre_performance")

final$erf_performance
# Probe Expert only



# NTerms Plot

NTerms <- unlist(c(final$erf_performance[1, ], final$erf_prio_performance[1, ], final$erf_only_performance[1, ], final$rf_performance[1, ], final$pre_performance[1, ]), use.names = F)
AvgRuleLength <- unlist(c(final$erf_performance[2, ], final$erf_prio_performance[2, ], final$erf_only_performance[2, ], final$rf_performance[2, ], final$pre_performance[2, ]), use.names = F)
AUC <- unlist(c(final$erf_performance[3, ], final$erf_prio_performance[3, ], final$erf_only_performance[3, ], final$rf_performance[3, ], final$pre_performance[3, ]), use.names = F)
ClassAcc <- unlist(c((1-final$erf_performance[4, ]), (1-final$erf_prio_performance[4, ]), (1-final$erf_only_performance[4, ]), (1-final$rf_performance[4, ]), (1-final$pre_performance[4, ])), use.names = F)
PropEKImp <- unlist(c(final$erf_performance[5, ], final$erf_prio_performance[5, ], final$erf_only_performance[5, ], final$rf_performance[5, ], final$pre_performance[5, ]), use.names = F)
PropEK <- unlist(c(final$erf_performance[6, ], final$erf_prio_performance[6, ], final$erf_only_performance[6, ], final$rf_performance[6, ], final$pre_performance[6, ]), use.names = F)
PropOptionalEK <- unlist(c(final$erf_performance[7, ], final$erf_prio_performance[7, ], final$erf_only_performance[7, ], final$rf_performance[7, ], final$pre_performance[7, ]), use.names = F)

comparison_df <- data.frame(NTerms = NTerms, AvgRuleLength = AvgRuleLength, AUC = AUC, ClassAcc = ClassAcc,
                            PropEKImp = PropEKImp, PropEK = PropEK, PropOptionalEK = PropOptionalEK,
                            Model = c(rep("ERF", 4), rep("ERF EK Prio", 4), rep("ERF EK only", 4), rep("RuleFit", 4),rep("PRE", 4)), 
                            Sample = rep(c("full (1000)", "600", "400", "200"),5))

comparison_df

# 1. NTerms Plot
p1 <- ggplot(comparison_df, 
        aes(x = Sample, y=NTerms, color = Model, group = Model)) +
        geom_point(size = 3) + geom_line(size = 1.5) + scale_x_discrete(limits=c("full (1000)","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
        scale_y_continuous(limits = c(0, 60)) +
        labs(title = "Model complexity for varying dataset sizes", x ="dataset size", y = "Number of terms") + 
        theme_minimal() + theme(plot.title = element_text(size=15),
                                             legend.text=element_text(size=11),
                                             text = element_text(size = 14), 
                                             axis.text = element_text(size = 12), 
                                             legend.position = "right")

p1
pdf("NTerms_samplesize_simulation.pdf")
p1
dev.off()

# 2. Average Rule Length Plot

p2 <- ggplot(comparison_df, 
        aes(x = Sample, y=AvgRuleLength, color = Model, group = Model)) +
        geom_point(size = 3) + geom_line(size = 1.5) + scale_x_discrete(limits=c("full (1000)","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
        labs(title = "Rule complexity for varying dataset sizes", x ="dataset size", y = "Average rule length") + 
        theme_minimal() + scale_y_continuous(limits = c(0, 3)) + theme(plot.title = element_text(size=15),
                                                     legend.text=element_text(size=11),
                                                     text = element_text(size = 14), 
                                                     axis.text = element_text(size = 12),
                                                     legend.position = "right") 

p2
pdf("rulelength_samplesize_simulation.pdf")
p2
dev.off()



# 3. AUC Plot

p3 <- ggplot(comparison_df, 
        aes(x = Sample, y=AUC, color = Model, group = Model)) +
        geom_point(size = 3) + geom_line(size = 1.5) + scale_x_discrete(limits=c("full (1000)","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
        labs(title = "Predictive performance for varying dataset sizes", x ="dataset size", y = "AUC") + 
        theme_minimal() + scale_y_continuous(limits = c(0.5, 1)) + theme(plot.title = element_text(size=15),
                                                                         legend.text=element_text(size=11),
                                                                         text = element_text(size = 14), 
                                                                         axis.text = element_text(size = 12),
                                                                         legend.position = "right") 

p3
pdf("AUC_samplesize_simulation.pdf")
p3
dev.off()


# 4. Classification Accuracy Plot

p4 <- ggplot(comparison_df, 
        aes(x = Sample, y= ClassAcc, color = Model, group = Model)) +
        geom_point(size = 4) + geom_line(size = 1.5) + scale_x_discrete(limits=c("full (1000)","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
        labs(title = "Predictive performance for varying dataset sizes", x ="dataset size", y = "Classification Accuracy") + 
        theme_minimal() + scale_y_continuous(limits = c(0.5, 1)) + theme(plot.title = element_text(size=15),
                                                                         legend.text=element_text(size=11),
                                                                         text = element_text(size = 14), 
                                                                         axis.text = element_text(size = 12),
                                                                         legend.position = "right") 
p4
pdf("CalssAcc_samplesize_simulation.pdf")
p4
dev.off()

# 5. Proportion of Expert Knowledge among the most important terms

p5 <- ggplot(comparison_df, 
        aes(x = Sample, y=PropEKImp, color = Model, group = Model)) +
        geom_point(size = 3) + geom_line(size = 1.5) + scale_x_discrete(limits=c("full (1000)","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
        labs(title = "Proportion of EK for varying dataset sizes", x ="dataset size", y = "Proportion of EK among the 10 most important terms") + 
        scale_y_continuous(limits = c(0, 1)) + theme(plot.title = element_text(size=15),
                                                     legend.text=element_text(size=11),
                                                     text = element_text(size = 14), 
                                                     axis.text = element_text(size = 12), 
                                                     legend.position = "right")

p5
pdf("PropEKImp_samplesize_simulation.pdf")
p5
dev.off()


# 6. Proportion of Expert Knowlegde in the final model

p6 <- ggplot(comparison_df, 
        aes(x = Sample, y=PropEK, color = Model, group = Model)) +
        geom_point(size = 3) + geom_line(size = 1.5) + scale_x_discrete(limits=c("full (1000)","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
        labs(title = "Proportion of EK for varying dataset sizes", x ="dataset size", y = "Proportion of EK") + 
        theme_minimal() + theme(plot.title = element_text(size=15),
                                legend.text=element_text(size=11),
                                text = element_text(size = 14), 
                                axis.text = element_text(size = 12),
                                legend.position = "right") 
p6
pdf("PropEK_samplesize_simulation.pdf")
p6
dev.off()


# 7. Proportion of Expert Knowlegde in the final model

p7 <- ggplot(comparison_df, 
             aes(x = Sample, y=PropOptionalEK, color = Model, group = Model)) +
            geom_point(size = 3) + geom_line(size = 1.5) + scale_x_discrete(limits=c("full (1000)","600","400", "200")) +
            scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
            labs(title = "Proportion of EK for varying dataset sizes", x ="dataset size.", y = "Proportion of optional EK") + 
            theme_minimal() + theme(plot.title = element_text(size=15),
                                    legend.text=element_text(size=11),
                                    text = element_text(size = 14), 
                                    axis.text = element_text(size = 12),
                                    legend.position = "right") 

p7
pdf("PropOptionalEK_samplesize_simulation.pdf")
p7
dev.off()

