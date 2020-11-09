################################################################################
#
#                           ERF DATA SIMULATION
#
################################################################################

# Libraries
library(pre)
library(purrr)
library(rlist)

# source external functions
source('simulation_auxiliaries.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function

#' @name create_simulation
#' @description creates a data set including expert knowledge to apply the Expert-RuleFit model to
#' @param n_vars number of input variables to be generated from a (standard) normal distribution, default = 100
#' @param n_obs number of observations/examples, default = 2000
#' @param mu single number or vector of length n_vars defining the mean of the normal distribution used to generate input variables, default = 0
#' @param sigma standard deviation of normal distribution from which input variable values are sampled, default = 1
#' @param n_rule_vars number of input variables to be sampled for definition of relevant predictor rules, default = 20
#' @param n_rel_rules number of relevant predictor rules, default = 10
#' @param optional_lengths vector ranging from 1 to the max. complexity of relevant predictor rules, default = c(1, 2, 3, 4)
#' @param weights weight vector defining the sampling probability of certain rule lengths, default = c(1/3, 1/4, 1/4, 1/6)
#' @param mu_beta single number or vector of length n_vars defining the mean of the normal distribution(s) from which the betas are sampled, default = 0
#' @param sigma_beta standard deviation of normal distributionfrom which the betas are sampled, default = 5
#' @param mu_epsilon single number or vector of length n_obs defining the mean of the normal distribution from which random noise is sampled, default = 0
#' @param sigma_epsilon standard deviation of normal distribution from which random noise is sampled, default = 0.025
#' @return a list of the following components:
#'   \item{sim_relevant_data}{dataframe including only the relevant predictor variables and the target y}
##'  \item{sim_full_data}{dataframe including all input variables(all rules and linear terms) and the target y (used to apply ERF)}
##'  \item{expert_rules}{list, first element includes specified expert rules, second element includes relevant variables/linear terms}

create_simulation <- function(n_vars = 100, n_obs = 2000,
                              mu = 0, sigma = 1, 
                              n_rule_vars = 20, 
                              n_rel_rules = 10, 
                              optional_lengths = c(1, 2),
                              weights = c(1/2, 1/2),
                              mu_beta = 0, sigma_beta = 5, 
                              mu_epsilon = 0, sigma_epsilon = 0.25){
  
  # 1. Generate input variables x_1,...,x_nvars, 
  #    each with n_obs values sampled from N(mu,sigma)
  X <- sim_data(n_vars, n_obs, mu, sigma)
  
  # 2. Define the Rules
  # 2.1. Sample input variables and define them as relevant rule components
  rule_vars <- sample_rule_vars(X, n_rule_vars)

  
  # 2.2 Sample rule lengths using optional rule lenghts incl. weights
  rule_lengths <- sample_rule_lengths(n_rel_rules, optional_lengths,
                                      weights)

  
  # 2.3. Sample input variables per rule
  var_list_rules <- sample_vars_per_rule(n_rel_rules, rule_vars, rule_lengths)

  
  # 2.4. Sample inequality signs per rule
  sign_list_rules <- sample_signs_per_rule(n_rel_rules, rule_lengths)

  
  # 2.5. Define condition values per rule, each as a random sample
  #      from N(mu,sigma)
  value_list_rules <- sample_values_per_rule(n_rel_rules, 
                                             rule_lengths, mu, sigma)

  
  # 2.6. Concatenate the variables, signs and values as condition strings
  conditions <- define_conditions(n_rel_rules, rule_lengths,
                                  var_list_rules, sign_list_rules,
                                  value_list_rules)
  

  
  # 3. Define all relevant predictors (rules only)
  rule_preds <- define_rules(n_rel_rules, rule_lengths, conditions)

  
  # 4. createX as relevant to the outcome variable
  dt <- createX(X, rule_preds, t = 0.025)

  
  # 5. Sample the betas 
  betas <- sample_betas(rule_preds, mu_beta, sigma_beta)
  # print(betas)
  
  # 6. Sample random noise 
  epsilon <- sample_epsilon(n_obs, mu_epsilon, sigma_epsilon)
  # print(epsilon)
  
  # 7. Calculate the outcome probabilities while adding random noise
  dt_y <- as.data.frame(dt[1])
  linear_predictor <- calc_linear_predictor(dt_y, betas)
  y1_prob <- round(1/(1 + exp(-(linear_predictor))),3)

  # 8. Sample binary class outcome from Bernoulli distirbution with prob = y1_prob
  y <- sample_y(y1_prob)
  
  # 9. Define dataset with all relevant predictor variables, y1_prob and y
  sim_relevant_data = cbind(dt_y, y1_prob, y)
  sim_relevant_data$y <- factor(sim_relevant_data$y)

  # 10. Define full dataset inkl. y
  sim_full_data <- cbind(X, y)
  sim_full_data$y <- factor(sim_full_data$y)
  expert_rules <- rule_preds
    
  out = list(sim_relevant_data, sim_full_data, expert_rules)
  
  out
}

#===============================================================================

# Example
# simulation <- create_simulation(n_vars = 40, n_obs = 2000,
#                                 mu = 0, sigma = 1, 
#                                 n_rule_vars = 15, 
#                                 n_rel_rules = 10, 
#                                 optional_lengths = c(1, 2, 3, 4),
#                                 weights = c(1/3, 1/4, 1/4, 1/6),
#                                 mu_beta = 0, sigma_beta = 5, 
#                                 mu_epsilon = 0, sigma_epsilon = 0.1)


# Dataset of relevant predictors + y 
# rel_predictor_data <- simulation[[1]]


# Data to apply Expert-RuleFit to
# data <- simulation[[2]]

# Expert knowledge
# expert_rules <- simulation[[3]]
# expert_rules


