################################################################################
#
#                 (ERF) DATA SIMULATION - AUXILIARY FUNCTIONS
#
################################################################################

# Libraries
# library(purrr)
# library(rlist)


# Functions

# 1. sim_data
# 2. sample_rule_vars
# 3. sample_rule_lengths
# 4. sample_vars_per_rule
# 5. sample_signs_per_rule
# 6. sample_values_per_rule
# 7. define_conditions
# 9. define_rules
# 10. sample_betas
# 11. sample_epsilon
# 12. calc_linear_predictor
# 13. sample_y
# 14. adapt_names

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Generate input data

#' @name sim_data
#' @description Generates random input variables, each with nobs values sampled from N(mu,sigma)
#' @param n_vars number of input variables to be generated 
#' @param n_obs number of observations per variable
#' @param mu single number or vector of length n_vars defining the mean of the normal distribution used to generate input variables, default = 0
#' @param sigma standard deviation of normal distribution from which input variable values are sampled, default = 1
#' @return X, a dataframe of random variable values with n_vars columns and n_obs rows

sim_data <- function(n_vars, n_obs, mu, sigma){
  data <- data.frame(matrix(NA, nrow = n_obs, ncol = n_vars))
  for (j in 1:n_vars){
    if(length(mu) == n_vars & length(sigma) == 1){
      col = rnorm(n_obs, mu[j], sigma)
    } else if(length(mu) == 1 & length(sigma) == n_vars){
      col = rnorm(n_obs, mu, sigma[j])
    } else if(length(mu) == n_vars & length(sigma) == n_vars){
      col = rnorm(n_obs, mu[j], sigma[j])
    } else if(length(mu) == 1 & length(sigma) == 1){
      col = rnorm(n_obs, mu, sigma)
    } else {
      print("Wrong Input: mu and sigma need to be either an integer or a vector 
            of the same length as the number of input attributes to be generated.")
    }
    data[,j] <- col
  }
  data
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2. Define rules as relevant predictors

#' @name sample_rule_vars 
#' @description Samples input variables and defines them as the components of relevant predictor rules
#' @param X X, a dataframe of random variable values with n_vars columns and n_obs rows
#' @param n_rule_vars number of input variables to be sampled, default = 20
#' @return vector of strings including the variable names 

sample_rule_vars <- function(X, n_rule_vars){
  colnumbers <- as.vector(1: ncol(X))
  rule_vars_numbers <- sample(colnumbers, size = n_rule_vars, replace = FALSE)
  rule_vars <- c()
  for (i in 1:length(rule_vars_numbers)){
    rule_vars[i] <- paste("X[,", rule_vars_numbers[i], "]", sep = "")
  }
  rule_vars
}

# rule_vars <- sample_rule_vars(X, n_rule_vars)
# rule_vars

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name sample_rule_lengths
#' @description samples rule lengths ( = number of variables to be included in one rule)
#' @param n_rel_rules number of relevant rules (integer)
#' @param optional_lengths vector ranging from 1 to the max. complexity of relevant predictor rules, default = c(1, 2, 3, 4)
#' @param weights weight vector of same length as 'optional lengths', defining the sampling probability of certain rule lengths, default = c(1/3, 1/4, 1/4, 1/6)
#' @return vector of integers, indicating rule lengths of relevant rules

sample_rule_lengths <- function(n_rel_rules, optional_lengths, weights){
  rule_lengths <- sample(optional_lengths, size = n_rel_rules, prob = weights,
                         replace = TRUE)
  rule_lengths
}

# rule_lengths <- sample_rule_lengths(n_rel_rules, optional_lengths, weights)
# rule_lengths

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name sample_vars_per_rule
#' @description Defines variables per rule according to defined rule lengths
#' @param n_rel_rules number of relevant rules (integer)
#' @param rule_vars vector of strings including the relevant variable names 
#' @param rule_lengths vector of rule lengths
#' @return vector of strings = variables per rule

sample_vars_per_rule <- function(n_rel_rules, rule_vars, rule_lengths){
  var_list <- vector(mode = "list", length = n_rel_rules)
  for (i in 1:n_rel_rules){
    var_list[[i]] <- sample(x = rule_vars, size = rule_lengths[i], 
                            replace = FALSE)
  }
  var_list
}

# var_list_rules <- sample_vars_per_rule(n_rel_rules, rule_vars, rule_lengths)
# var_list_rules

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name sample_signs_per_rule
#' @description Defines inequality signs per rule, sampling options are <=, >=
#' @param n_rel_rules number of relevant rules (integer)
#' @param rule_lengths vector of rule lengths 
#' @return vector of inequality sign strings

sample_signs_per_rule <- function(n_rel_rules, rule_lengths){
  signs <- c(">=", "<=")
  sign_list <- vector(mode = "list", length = n_rel_rules)
  for (i in 1:n_rel_rules){
    sign_list[[i]] <- sample(x = signs, size = rule_lengths[i], replace = TRUE)
  }
  sign_list
}

# sign_list_rules <- sample_signs_per_rule(n_rel_rules, rule_lengths)
# sign_list_rules

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name sample_values_per_rule
#' @description Defines condition values per rule as random samples from same N(mu, sigma) as X was sampled before
#' @param n_rel_rules number of relevant rules (integer)
#' @param rule_lengths vector of rule lengths
#' @param mu single number or vector of length n_vars defining the mean of the normal distribution used to generate input variables
#' @param sigma standard deviation of normal distribution from which input variable values are sampled
#' @return vector of strings, including split point values for prediction rule

sample_values_per_rule <- function(n_rel_rules, rule_lengths, mu, sigma){
  value_list <- vector(mode = "list", length = n_rel_rules)
  for (i in 1:n_rel_rules){
    value_list[[i]] <- as.character(round(rnorm(rule_lengths[i], mu, sigma),4))
  }
  value_list
}

# value_list_rules <- sample_values_per_rule(n_rel_rules, rule_lengths, mu, sigma)
# value_list_rules

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name define_conditions
#' @description Concatenate the variables, signs and values as a vector of strings
#' @param n_rel_rules number of relevant rules (integer)
#' @param rule_lengths vector of rule lengths
#' @param var_list_rules  vector of variables per rule-strings
#' @param sign_list_rules vector of inequality sign strings
#' @param value_list_rules vector of split point value strings
#' @return vector of strings including all variable-sign-value conditions

define_conditions <- function(n_rel_rules, rule_lengths,
                              var_list_rules, sign_list_rules,
                              value_list_rules){
  all_conditions <- vector(mode = "list", length = n_rel_rules)
  
  for (i in 1:n_rel_rules){
    conditions_per_rule <- c()
    for (j in 1:rule_lengths[i]){
      conditions_per_rule[j] <- paste(var_list_rules[[i]][j],
                                      sign_list_rules[[i]][j],
                                      value_list_rules[[i]][j], sep = "")
    }
    all_conditions[[i]] <- conditions_per_rule
  }
  all_conditions
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name define_rules
#' @description combines conditions to rules according to defined rule lengths
#' @param n_rel_rules number of relevant rules (integer)
#' @param rule_lengths vector of rule lengths
#' @param conditions vector of strings = variable-sign-value conditions
#' @return vector of strings including prediction rules

define_rules <- function(n_rel_rules, rule_lengths, conditions){
  all_rules <- vector(mode = "list", length = n_rel_rules)
  
  for (i in 1:n_rel_rules){
    if(rule_lengths[i] == 1){
      rule <- conditions[[i]][1]
    } else if (rule_lengths[i] == 2){
      rule <- paste(conditions[[i]][1], "&", conditions[[i]][2], sep = " ")
    } else if (rule_lengths[i] == 3){
      rule <- paste(conditions[[i]][1], "&", conditions[[i]][2], "&",
                    conditions[[i]][3], sep = " ")
    } else if (rule_lengths[i] == 4){
      rule <- paste(conditions[[i]][1], "&", conditions[[i]][2], "&",
                    conditions[[i]][3], "&", conditions[[i]][4],  sep = " ")
    }
    all_rules[i] <- rule
  }
  unlist(all_rules)
}



#===============================================================================

# 4. create model 

#' @name sample_betas
#' @description Samples beta coefficient values (in easiest case, all betas are +1)
#' @param all_predictors vector of strings including relevant rules and relevant linear terms
#' @param mu_beta single number or vector of length n_vars defining the mean of the normal distribution(s) from which the betas are sampled
#' @param sigma_beta standard deviation of normal distribution from which the betas are sampled
#' @return vector of numeric values as the coefficient values of all relevant predictors (rules + linear terms)

sample_betas <- function(all_predictors, mu_beta, sigma_beta){
  betas <- rnorm(length(all_predictors), mu_beta, sigma_beta)
  betas
}


#===============================================================================

#' @name sample_epsilon
#' @description simulate random noise sampled from a normal distribution
#' @param n_obs  number of observations/examples
#' @param mu_epsilon single number or vector of length n_obs defining the mean of the normal distribution from which random noise is sampled, default = 0
#' @param sigma_epsilon standard deviation of normal distribution from which random noise is sampled, default = 0.025
#' @return vector of numeric values, indicating random noise at each observation


sample_epsilon <- function(n_obs, mu_epsilon, sigma_epsilon){
  epsilon <- rnorm(n_obs, mu_epsilon, sigma_epsilon)
  epsilon
}

#===============================================================================

# 5. Define y

#' @name calc_linear_predictor
#' @descriptions  calculates the linear predictor per observation
#' @param X a dataframe of  random variable values with n_vars columns and n_obs rows
#' @param betas vector of numeric values as the coefficient values of all relevant predictors
#' @return vector of numeric values as calculated from linear predictor for every observation


calc_linear_predictor <- function(dt_y, betas){
  linear_predictor = c()
  for (i in 1:nrow(dt_y)){
    help = 0
    for(j in 1:ncol(dt_y)){
      help = help + betas[j]*dt_y[i,j]
    }
    linear_predictor[i] = help
  }
  linear_predictor
}

#===============================================================================

#' @name sample_y
#' @description Calculates outcome classes as a binary response
#' @param y1_prob vector of numeric values indicating positive class probabilities
#' @return vector of numeric class values (binary)

sample_y <- function(y1_prob){
  y <- rep(0, length(y1_prob))
  for(i in 1:length(y1_prob)){
    y[i] <- as.numeric(rbernoulli(1, y1_prob[i]))
  }
  y
}


#===============================================================================

# 6. Create simulation dataset

#' @name adapt_names
#' @description changes the column names of all input attributes to "X1., x2.,..." and the target to "y".
adapt_names <- function(data){
  X <- data[, -ncol(data)]
  pos_names <- colnames(X)
  classic_names <- c()
  for (i in 1:ncol(X)){
    classic_names[i] <- paste("X",i,".", sep = "")
  }
  colnames(data) <- c(classic_names, "y")
  return(data)
}
