################################################################################
#                                                                              #
#             Can Expert Knowledge increase Data Efficiency?                   #
#                                                                              #
#                   Experiments with simulated data                            #
#                                                                              #
################################################################################ 

# Libraries
# library(gridExtra)

# External functions
source("./data simulation/simulation_main.R")
source("./ERF/erf_main.R")

#' @name sample_variations
#' @description simulates datasets of varying sizes, simulates corresponding EK and then trains different model settings using cross validation. Eventually, the results are combined in one data.frame.
sample_variations <- function(sample_sizes = c(600, 400, 200), seed = 202,
                              cv_folds = 5, optional_penalty = 0.5){
        simulation <- create_simulation(n_vars = 50, n_obs = 1000,
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
        conf_rules <- support_take(expert_rules, data, 0.06)
        print(length(conf_rules))
        
        opt_rules <- setdiff(expert_rules, conf_rules)
        
        # ERF
        erf_full <- CV_erf(data = data, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
                           optional_expert_rules = opt_rules)

        erf_600 <- CV_erf(data = data_600, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
                          optional_expert_rules = opt_rules)

        erf_400 <- CV_erf(data = data_400, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
                          optional_expert_rules = opt_rules)

        erf_200 <- CV_erf(data = data_200, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
                          optional_expert_rules = opt_rules)
        
        erf_vec_200 <- unlist(erf_200, use.names=FALSE)
        erf_vec_400 <- unlist(erf_400, use.names=FALSE)
        erf_vec_600 <- unlist(erf_600, use.names=FALSE)
        erf_vec_full <- unlist(erf_full, use.names=FALSE)
        erf_performance <- data.frame(erf_full = erf_vec_full, erf_600 = erf_vec_600,
                                      erf_400 = erf_vec_400, erf_200 = erf_vec_200)
        row.names(erf_performance) <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr",
                                        "PropEKImp", "PropEK", "PropOptionalEK")
        
        print(erf_performance)
        
        # ERF with EK Priority
        erf_prio_full <- CV_erf(data = data, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
                                optional_expert_rules = opt_rules, optional_penalty = optional_penalty)
        erf_prio_600 <- CV_erf(data = data_600, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
                               optional_expert_rules = opt_rules, optional_penalty = optional_penalty)
        erf_prio_400 <- CV_erf(data = data_400, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
                               optional_expert_rules = opt_rules,  optional_penalty = optional_penalty)
        erf_prio_200 <- CV_erf(data = data_200, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
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
        
        print(erf_prio_performance)
        
        # ERF with EK only
        erf_only_full <- CV_erf(data = data, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
                                optional_expert_rules = opt_rules, expert_only = T)

        erf_only_600 <- CV_erf(data = data_600, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
                               optional_expert_rules = opt_rules, expert_only = T)

        erf_only_400 <- CV_erf(data = data_400, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
                               optional_expert_rules = opt_rules, expert_only = T)
        
        erf_only_200 <- CV_erf(data = data_200, cv_folds = cv_folds, seed = seed, confirmatory_expert_rules = conf_rules, 
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
        
        print(erf_only_performance)
        
        # ERF without EK
        rf_full <- CV_erf(data = data, cv_folds = cv_folds, seed = seed)
        rf_600 <- CV_erf(data = data_600, cv_folds = cv_folds, seed = seed)
        rf_400 <- CV_erf(data = data_400, cv_folds = cv_folds, seed = seed)
        rf_200 <- CV_erf(data = data_200, cv_folds = cv_folds, seed = seed)
        
        rf_vec_200 <- unlist(rf_200, use.names=FALSE)
        rf_vec_400 <- unlist(rf_400, use.names=FALSE)
        rf_vec_600 <- unlist(rf_600, use.names=FALSE)
        rf_vec_full <- unlist(rf_full, use.names=FALSE)
        rf_performance <- data.frame(rf_full = rf_vec_full, rf_600 = rf_vec_600, 
                                     rf_400 = rf_vec_400, rf_200 = rf_vec_200)
        row.names(rf_performance) <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr",
                                       "PropEKImp", "PropEK", "PropOptionalEK")
        
        print(rf_performance)
        
        # PRE 
        pre_full <- CV_pre(data, cv_folds = cv_folds, seed = seed)
        pre_600 <- CV_pre(data_600, cv_folds = cv_folds, seed = seed)
        pre_400 <- CV_pre(data_400, cv_folds = cv_folds, seed = seed)
        pre_200 <- CV_pre(data_200, cv_folds = cv_folds, seed = seed)
        
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
        
        print(pre_performance)
        
        out <- list(erf_performance,
                    erf_prio_performance,
                    erf_only_performance,
                    rf_performance,
                    pre_performance)
        
        out
        
}

x1 <- sample_variations()
x45 <- sample_variations(seed = 208)
x78 <- sample_variations(seed = 569)
x95 <- sample_variations(seed = 47)
x14 <- sample_variations(seed = 2345)
x35 <- sample_variations(seed = 58694)
x18 <- sample_variations(seed = 149)
x50 <- sample_variations(seed = 10000)
x130 <- sample_variations(seed = 3402)
x121 <- sample_variations(seed = 121295)

res_list <- list(x1, x45, x78, x95, x14, x35, x18, x50, x130, x121)

#save.image(file = "simulation_sample_variations.RData")

cv_sim <- function(res_list){
        cv_res_list <- list()
        for(i in 1:length(res_list[[1]])){
                cv_res_list[[i]] <- apply(abind::abind(res_list[[1]][[i]], 
                                                       res_list[[2]][[i]],
                                                       res_list[[3]][[i]],
                                                       res_list[[4]][[i]],
                                                       res_list[[5]][[i]],
                                                       res_list[[6]][[i]],
                                                       res_list[[7]][[i]],
                                                       res_list[[8]][[i]],
                                                       res_list[[9]][[i]],
                                                       res_list[[10]][[i]],
                                                       along = 3), 1:2, mean)  
        }
        cv_res_list
}


final <- cv_sim(res_list)
final
names(final) <- c("erf_performance", "erf_prio_performance", "erf_only_performance", "rf_performance", "pre_performance")




# Dataframe for comparison plots
NTerms <- unlist(c(final$erf_performance[1, ], final$erf_prio_performance[1, ], final$erf_only_performance[1, ], final$rf_performance[1, ], final$pre_performance[1, ]), use.names = F)
AvgRuleLength <- unlist(c(final$erf_performance[2, ], final$erf_prio_performance[2, ], final$erf_only_performance[2, ], final$rf_performance[2, ], final$pre_performance[2, ]), use.names = F)
AUC <- unlist(c(final$erf_performance[3, ], final$erf_prio_performance[3, ], final$erf_only_performance[3, ], final$rf_performance[3, ], final$pre_performance[3, ]), use.names = F)
ClassAcc <- unlist(c((1-final$erf_performance[4, ]), (1-final$erf_prio_performance[4, ]), (1-final$erf_only_performance[4, ]), (1-final$rf_performance[4, ]), (1-final$pre_performance[4, ])), use.names = F)
PropEKImp <- unlist(c(final$erf_performance[5, ], final$erf_prio_performance[5, ], final$erf_only_performance[5, ], final$rf_performance[5, ], final$pre_performance[5, ]), use.names = F)
PropEK <- unlist(c(final$erf_performance[6, ], final$erf_prio_performance[6, ], final$erf_only_performance[6, ], final$rf_performance[6, ], final$pre_performance[6, ]), use.names = F)
PropOptionalEK <- unlist(c(final$erf_performance[7, ], final$erf_prio_performance[7, ], final$erf_only_performance[7, ], final$rf_performance[7, ], final$pre_performance[7, ]), use.names = F)

comparison_df <- data.frame(NTerms = NTerms, AvgRuleLength = AvgRuleLength, AUC = AUC, ClassAcc = ClassAcc,
                            PropEKImp = PropEKImp, PropEK = PropEK, PropOptionalEK = PropOptionalEK,
                            Model = c(rep("ERF", 4), rep("ERF, EK prio", 4), rep("ERF, EK only", 4), rep("RuleFit", 4),rep("PRE", 4)), 
                            Sample = rep(c("1000", "600", "400", "200"),5))

comparison_df



# 1. NTerms Plot
p1 <- ggplot(comparison_df, 
        aes(x = Sample, y=NTerms, color = Model, group = Model)) +
        geom_point(size = 3.2) + geom_line(size = 1) + scale_x_discrete(limits=c("1000","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
        scale_y_continuous(limits = c(0, 100)) +
        labs(x ="dataset size", y = "Number of terms") + 
        theme_minimal() + theme(plot.title = element_text(size=14),
                                             legend.text=element_text(size=14),
                                             text = element_text(size = 14), 
                                             axis.text = element_text(size = 14),
                                             legend.position = "right")

p1
pdf("NTerms_de_simulation.pdf")
p1
dev.off()

# 2. Average Rule Length Plot
p2 <- ggplot(comparison_df, 
        aes(x = Sample, y=AvgRuleLength, color = Model, group = Model)) +
        geom_point(size = 3.2) + geom_line(size = 1) + scale_x_discrete(limits=c("1000","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
        labs(x ="dataset size", y = "Average rule length") + 
        theme_minimal() + scale_y_continuous(limits = c(1, 3)) + theme(plot.title = element_text(size=14),
                                                     legend.text=element_text(size=14),
                                                     text = element_text(size = 14), 
                                                     axis.text = element_text(size = 14),
                                                     legend.position = "right") 

p2
pdf("rulelength_de_simulation.pdf")
p2
dev.off()



# 3. AUC Plot
p3 <- ggplot(comparison_df, 
        aes(x = Sample, y=AUC, color = Model, group = Model)) +
        geom_point(size = 3.2) + geom_line(size = 1) + scale_x_discrete(limits=c("1000","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
        labs(x ="dataset size", y = "AUC") + 
        theme_minimal() + scale_y_continuous(limits = c(0.5, 1)) + theme(plot.title = element_text(size=14),
                                                                         legend.text=element_text(size=14),
                                                                         text = element_text(size = 14), 
                                                                         axis.text = element_text(size = 14),
                                                                         legend.position = "right") 

p3
pdf("AUC_de_simulation.pdf")
p3
dev.off()


# 4. Classification Accuracy Plot
p4 <- ggplot(comparison_df, 
        aes(x = Sample, y= ClassAcc, color = Model, group = Model)) +
        geom_point(size = 3.2) + geom_line(size = 1) + scale_x_discrete(limits=c("1000","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
        labs(x ="dataset size", y = "Classification Accuracy") + 
        theme_minimal() + scale_y_continuous(limits = c(0.5, 1)) + theme(plot.title = element_text(size=14),
                                                                         legend.text=element_text(size=14),
                                                                         text = element_text(size = 14), 
                                                                         axis.text = element_text(size = 14),
                                                                         legend.position = "right") 
p4
pdf("ClassAcc_de_simulation.pdf")
p4
dev.off()

# 5. Proportion of Expert Knowledge among the most important terms
p5 <- ggplot(comparison_df, 
        aes(x = Sample, y=PropEKImp, color = Model, group = Model)) +
        geom_point(size = 3.2) + geom_line(size = 1) + scale_x_discrete(limits=c("1000","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#995d30", "#0a565c")) + 
        labs(x ="dataset size", y = "Proportion of EK among the 10 most important terms") + 
        theme_minimal() +
        scale_y_continuous(limits = c(0, 1)) + theme(plot.title = element_text(size=14),
                                                     legend.text=element_text(size=14),
                                                     text = element_text(size = 14), 
                                                     axis.text = element_text(size = 14), 
                                                     legend.position = "right")

p5
pdf("PropEKImp_de_simulation.pdf")
p5
dev.off()


# 6. Proportion of Expert Knowlegde overall
p6 <- ggplot(comparison_df, 
        aes(x = Sample, y=PropEK, color = Model, group = Model)) +
        geom_point(size = 3.2) + geom_line(size = 1) + scale_x_discrete(limits=c("1000","600","400", "200")) +
        scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","#995d30", "#0a565c")) + 
        labs(x ="dataset size", y = "Proportion of EK") + 
        theme_minimal() + theme(plot.title = element_text(size=14),
                                legend.text=element_text(size=14),
                                text = element_text(size = 14), 
                                axis.text = element_text(size = 14),
                                legend.position = "right") 
p6
pdf("PropEK_de_simulation.pdf")
p6
dev.off()

#save.image(file = "simulation_dataefficiency.RData")

#===============================================================================
#                         RESULTS VARIATION
#===============================================================================

#(x1, x45, x78, x95, x14, x35, x18, x50, x130, 121)

# erf
erf_x1 <- t(x1[[1]])
erf_x2 <- t(x45[[1]])
erf_x3 <- t(x78[[1]])
erf_x4 <- t(x95[[1]])
erf_x5 <- t(x14[[1]])
erf_x6 <- t(x35[[1]])
erf_x7 <- t(x18[[1]])
erf_x8 <- t(x50[[1]])
erf_x9 <- t(x130[[1]])
erf_x10 <- t(x121[[1]])

erfs <- rbind(erf_x1, erf_x2, erf_x3, erf_x4, erf_x5, erf_x6, erf_x7, erf_x8, 
              erf_x9, erf_x10)
erfs <- cbind.data.frame(erfs, Sample = c(rep(c("full(768)", "600", "400", "200"), 10)),
                         Model = c(rep(c("ERF"), 40)))


# erf prio
erf_prio_x1 <- t(x1[[2]])
erf_prio_x2 <- t(x45[[2]])
erf_prio_x3 <- t(x78[[2]])
erf_prio_x4 <- t(x95[[2]])
erf_prio_x5 <- t(x14[[2]])
erf_prio_x6 <- t(x35[[2]])
erf_prio_x7 <- t(x18[[2]])
erf_prio_x8 <- t(x50[[2]])
erf_prio_x9 <- t(x130[[2]])
erf_prio_x10 <- t(x121[[2]])

erf_prios <- rbind(erf_prio_x1, erf_prio_x2, erf_prio_x3, erf_prio_x4, 
                   erf_prio_x5, erf_prio_x6, erf_prio_x7, erf_prio_x8, 
                   erf_prio_x9, erf_prio_x10)
erf_prios <- cbind.data.frame(erf_prios, 
                              Sample = c(rep(c("full(768)", "600", "400", "200"), 10)),
                              Model = c(rep(c("ERF, EK prio"), 40)))


# erf only
erf_only_x1 <- t(x1[[3]])
erf_only_x2 <- t(x45[[3]])
erf_only_x3 <- t(x78[[3]])
erf_only_x4 <- t(x95[[3]])
erf_only_x5 <- t(x14[[3]])
erf_only_x6 <- t(x35[[3]])
erf_only_x7 <- t(x18[[3]])
erf_only_x8 <- t(x50[[3]])
erf_only_x9 <- t(x130[[3]])
erf_only_x10 <- t(x121[[3]])


erf_onlys <- rbind(erf_only_x1, erf_only_x2, erf_only_x3, erf_only_x4,
                   erf_only_x5, erf_only_x6, erf_only_x7, erf_only_x8, 
                   erf_only_x9, erf_only_x10)
erf_onlys <- cbind.data.frame(erf_onlys, 
                              Sample = c(rep(c("full(768)", "600", "400", "200"), 10)), 
                              Model = c(rep(c("ERF, EK only"), 40)))


# rf
rf_x1 <- t(x1[[4]])
rf_x2 <- t(x45[[4]])
rf_x3 <- t(x78[[4]])
rf_x4 <- t(x95[[4]])
rf_x5 <- t(x14[[4]])
rf_x6 <- t(x35[[4]])
rf_x7 <- t(x18[[4]])
rf_x8 <- t(x50[[4]])
rf_x9 <- t(x130[[4]])
rf_x10 <- t(x121[[4]])

rfs <- rbind(rf_x1, rf_x2, rf_x3, rf_x4, rf_x5, rf_x6, rf_x7, rf_x8,
             rf_x9, rf_x10)
rfs <- cbind.data.frame(rfs, 
                        Sample = c(rep(c("full(768)", "600", "400", "200"), 10)), 
                        Model = c(rep(c("RuleFit"), 40)))


# pre
pre_x1 <- t(x1[[5]])
pre_x2 <- t(x45[[5]])
pre_x3 <- t(x78[[5]])
pre_x4 <- t(x95[[5]])
pre_x5 <- t(x14[[5]])
pre_x6 <- t(x35[[5]])
pre_x7 <- t(x18[[5]])
pre_x8 <- t(x50[[5]])
pre_x9 <- t(x130[[5]])
pre_x10 <- t(x121[[5]])


pres <- rbind(pre_x1, pre_x2, pre_x3, pre_x4, pre_x5, pre_x6, pre_x7,
              pre_x8, pre_x9, pre_x10)
pres <- cbind.data.frame(pres, Sample = c(rep(c("full(768)", "600", "400", "200"), 10)),
                         Model = c(rep(c("PRE"), 40)))


variation_frame <- rbind(erfs, erf_prios, erf_onlys, rfs, pres)
rownames(variation_frame) <- NULL
variation_frame
variation_frame$Sample = factor(variation_frame$Sample, levels=c("full(768)","600","400", "200"))
variation_frame$ClassAcc <- 1-variation_frame$ClassErr


# 1. AUC
variation_plot0 <- ggplot(variation_frame, aes(x=Model, y=AUC, fill=Sample)) + 
        geom_boxplot() + facet_wrap(~Model, scale="free") +
        labs(fill = "Dataset size") +  scale_fill_manual(values=c("bisque4","bisque3","bisque2", "beige")) + 
        scale_y_continuous(limits = c(0.5, 1)) + theme(legend.text=element_text(size=11),
                                                         text = element_text(size = 11), 
                                                         axis.text = element_text(size = 11))

variation_plot0
pdf("AUC_var_simulation.pdf")
variation_plot0
dev.off()

library(ggplot2)

# 1. Class Accuracy
variation_plot1 <- ggplot(variation_frame, aes(x=Model, y=ClassAcc, fill=Sample)) + 
        geom_boxplot() + facet_wrap(~Model, scale="free") +
        labs(fill = "Dataset size") + scale_fill_manual(values=c("bisque4","bisque3","bisque2", "beige"))+ 
        scale_y_continuous(limits = c(0.5, 1)) +
        labs(y = "Classification Accuracy") + theme(legend.text=element_text(size=11),
                                                    text = element_text(size = 11), 
                                                    axis.text = element_text(size = 11))
variation_plot1
pdf("CA_var_simulation.pdf")
variation_plot1
dev.off()

# 3. Number of terms
variation_plot2 <- ggplot(variation_frame, aes(x=Model, y=NTerms, fill=Sample)) + 
        geom_boxplot() + facet_wrap(~Model, scale="free") +
        labs(fill = "Dataset size") +  scale_fill_manual(values=c("bisque4","bisque3","bisque2", "beige"))+
        scale_y_continuous(limits = c(0,100))+ 
        labs(y = "Number of terms") + theme(legend.text=element_text(size=11),
                                            text = element_text(size = 11), 
                                            axis.text = element_text(size = 11))

variation_plot2
pdf("NTerms_var_simulation.pdf")
variation_plot2
dev.off()

# 1. Average rule length
variation_plot3 <- ggplot(variation_frame, aes(x=Model, y=AvgRuleLength, fill=Sample)) + 
        geom_boxplot() + facet_wrap(~Model, scale="free") +
        labs(fill = "Dataset size") +  scale_fill_manual(values=c("bisque4","bisque3","bisque2", "beige"))+
        scale_y_continuous(limits = c(0,3))+ 
        labs(y = "Average rule length") + theme(legend.text=element_text(size=11),
                                                text = element_text(size = 11), 
                                                axis.text = element_text(size = 11))
variation_plot3
pdf("rulelength_var_simulation.pdf")
variation_plot3
dev.off()

# 1. PropEKImp
variation_plot4 <- ggplot(variation_frame, aes(x=Model, y=PropEKImp, fill=Sample)) + 
        geom_boxplot() + facet_wrap(~Model, scale="free") +
        labs(fill = "Dataset size") +  scale_fill_manual(values=c("bisque4","bisque3","bisque2", "beige"))+
        scale_y_continuous(limits = c(0,1))+ 
        labs(y = "Proportion of EK among the 10 most important terms") + theme(legend.text=element_text(size=11),
                                                                               text = element_text(size = 11), 
                                                                               axis.text = element_text(size = 11))

variation_plot4
pdf("PropEKImp_var_simulation.pdf")
variation_plot4
dev.off()

# 1. Class Accuracy
variation_plot5 <- ggplot(variation_frame, aes(x=Model, y=PropEK, fill=Sample)) + 
        geom_boxplot() + facet_wrap(~Model, scale="free") +
        labs(fill = "Dataset size") +  scale_fill_manual(values=c("bisque4","bisque3","bisque2", "beige"))+
        scale_y_continuous(limits = c(0,1))+ 
        labs(y = "Proportion of EK overall") + theme(legend.text=element_text(size=11),
                                                     text = element_text(size = 11), 
                                                     axis.text = element_text(size = 11))
variation_plot5
pdf("PropEK_var_simulation.pdf")
variation_plot5
dev.off()


save.image(file = "simulation_dataefficiency_variation.RData")
