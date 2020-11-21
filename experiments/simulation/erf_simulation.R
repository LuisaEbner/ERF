################################################################################
###                                                                          ###
###           EXPERT RULE FIT (ERF) for Simulated Data                       ###
###                                                                          ###
################################################################################


# External functions
source("./data simulation/simulation_main.R")
source("./ERF/erf_main.R")

#===============================================================================
#                               SIMULATION
#===============================================================================

simulation <- create_simulation(n_vars = 30, n_obs = 1000,
                                mu = 0, sigma = 1, 
                                n_rule_vars = 10, 
                                n_rel_rules = 20, 
                                optional_lengths = c(1, 2, 3),
                                weights = c(1/3, 1/3, 1/3),
                                mu_beta = 0, sigma_beta = 5, 
                                mu_epsilon = 0, sigma_epsilon = 0.01)

#===============================================================================
#                                 DATA 
#===============================================================================

data <- simulation[[2]]

# train-test-split 
sets <- createERFsets(data, 0.7)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

#===============================================================================
#                      SIMULATED EXPERT/DOMAIN KNOWLEDGE
#===============================================================================

expert_rules <- simulation[[3]]
conf_rules <- support_take(expert_rules, data, 0.05)
opt_rules <- setdiff(expert_rules, conf_rules)

# expert_rules
# conf_rules
# opt_rules

#===============================================================================
#                              ERF MODEL
#===============================================================================

erf_sim <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                         confirmatory_expert_rules = conf_rules,  
                         optional_expert_rules = opt_rules, optional_penalty = 0.2)

erf_sim2 <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest)


# Alternative: Prediction Rule Ensembles (pre package)

# Input
train <- cbind.data.frame(X, y)
test <- cbind.data.frame(Xtest, ytest)

# Model
pre_sim <- pre(y ~ ., data = train , family = "binomial", 
                    type = "rules", ntrees = 250)

