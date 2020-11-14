################################################################################
#                                                                              #
#      HOW MUCH VARIANCE IS EXPLAINED BY THE EXPERT KNOWLEDGE?                 #
#                                                                              #
#                 EXPERIMENTS WITH SIMULATED DATA                              #
#                                                                              #
################################################################################


# external functions
source("simulation.R")
source("erf_main.R")

#===============================================================================
#                               SIMULATION
#===============================================================================

simulation <- create_simulation(n_vars = 100, n_obs = 1000,
                                mu = 0, sigma = 1, 
                                n_rule_vars = 15, 
                                n_rel_rules = 10, 
                                optional_lengths = c(1, 2, 3, 4),
                                weights = c(1/3, 1/4, 1/4, 1/6),
                                mu_beta = 0, sigma_beta = 7, 
                                mu_epsilon = 0, sigma_epsilon = 0.05)

#===============================================================================
#                                 DATA 
#===============================================================================

data <- simulation[[2]]

# train-test-split 
sets <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

#===============================================================================
#                      SIMULATED EXPERT/DOMAIN KNOWLEDGE
#===============================================================================

expert_rules <- simulation[[3]]
expert_rules
#===============================================================================
#                   GLM-ANOVA-ANALYSIS on EXPERT KNOWLEDGE
#===============================================================================

X_erf <- createX(X, expert_rules, t = 0)
data_erf <- cbind.data.frame(X_erf[[1]], y)
colnames(data_erf)[1:(length(data_erf)-1)] <- expert_rules
colnames(data_erf)

fit <- glm(y ~., data = data_erf, family = "binomial")
summary(fit)

anova_expknow <- anova(fit)
anova_expknow

