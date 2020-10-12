################################################################################
################################################################################
#                                                                              #
#                   ExpertRulefit - Auxiliary Functions                        #
#                                                                              #                                                                              #
################################################################################
################################################################################

# Library 
library(caret)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Functions

#' @name create_X_y_Xtest_ytest
#' @description takes a dataset including a binary target column and turns it into training and test data for X and y, separately. Converts y to make it applicable to glmnet. When train_frac = 1, no test data is created.
#' @param data: dataframe object including predictors and binary target
#' @param train_frac: value between 0 and 1 to specify the fraction of training samples to be sampled
#' @param pos_class: string or value of the positive class value of y, eg. 1,"pos","TRUE","yes" etc.
#' @return list object, including X, Xtest, y, ytest


create_X_y_Xtest_ytest <- function(data, train_frac, pos_class = 1,
                                   target_name = NULL, type_missing = NULL){
  
  # rename the target column
  if (is.null(target_name) == F){
    names(data)[names(data) == target_name] <- "y"
  } else{
    names(data)[ncol(data)] <- "y"
  }
  
  data$y <- factor(data$y)
  
  if(is.null(type_missing) == F){
    data[data == type_missing] <- NA
  }
  
  data <- data[complete.cases(data), ]
  
  # train-test-split
  set.seed(45)
  sample <- sample.int(n = nrow(data),
                       size = floor(train_frac*nrow(data)), replace = F)
  train <- data[sample, ]
  test  <- data[-sample, ]
  
  # convert dataframe to matrix format, remove target column
  X <- model.matrix(y ~., train)[,-1]
  Xtest <- model.matrix(y~.,test)[,-1]
  
  # Convert the target column of the training data to a 0-1-coded factor
  y <- factor(ifelse(train$y == pos_class, 1, 0))
  ytest <- factor(ifelse(test$y == pos_class, 1, 0))
  
  out = list(X, y, Xtest, ytest)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'@name names_to_positions
#'@description replaces the name of a variable in an expert rule with its position to become readable for the function createX
#'@param X data frame of input variables
#'@param name_rules vector of strings, containing expert rules with variable names as in the original dataset
#'@return the same vector as name_rules, just with the original variable names replaced by their position in the dataset

names_to_positions <- function(X, name_rules){
  names <- colnames(X)
  positions <- c()
  for (i in 1:ncol(X)){
    positions[i] <- paste("X[,",i, "]", sep = "")
  }
  
  bool <- T
  iteration <- 0
  
  pos_rules <- c()
  for (j in 1:length(name_rules)){
    for (k in 1:length(names)){
      if(grepl(names[k],name_rules[j], fixed = T)){
        pos_rules[j] <- gsub(names[k], positions[k], name_rules[j], fixed = T)
      }
    }
  }
  
  
  while(bool){
    bool <- F
    for (j in 1:length(pos_rules)){
      for (k in 1:length(names)){
        if(grepl(names[k],pos_rules[j], fixed = T)){
          bool <- T
          pos_rules[j] <- gsub(names[k], positions[k], pos_rules[j], fixed = T)
        }
      }
    }
  }
  pos_rules
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'@name positions_to_names
#'@description replaces the position of a variable (X[,xy]) in an expert rule with its original variable name to become readable for the human user
#'@param X data frame of input variables
#'@param pos_rules vector of strings, containing expert rules with variable names as (X[,xy])
#'@return the same vector as pos_rules, just with the original variable names instead of positions


positions_to_names <- function(X, pos_rules){
  names <- colnames(X)
  positions <- c()
  for (i in 1:ncol(X)){
    positions[i] <- paste("X[,",i, "]", sep = "")
  }
  
  bool <- T
  iteration <- 0
  
  name_rules <- c()
  for (j in 1:length(pos_rules)){
    for (k in 1:length(positions)){
      if(grepl(positions[k],pos_rules[j], fixed = T)){
        name_rules[j] <- gsub(positions[k], names[k], pos_rules[j], fixed = T)
      }
    }
  }
  
  
  while(bool){
    bool <- F
    for (j in 1:length(name_rules)){
      for (k in 1:length(positions)){
        if(grepl(positions[k],name_rules[j], fixed = T)){
          bool <- T
          name_rules[j] <- gsub(positions[k], names[k], name_rules[j], fixed = T)
        }
      }
    }
  }
  name_rules
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'@name names_to_numbers
#'@description replaces the name of a variable in a number indicating its column position 
#'@param X data frame of input variables
#'@param name_rules vector of variable names as the names of relevant linear terms
#'@return a vector of numbers, as the variable's column positions in the dataset

names_to_numbers <- function(X, variable_names){
  names <- colnames(X)
  positions <- 1:ncol(X)
  
  num_variables <- c()
  for (j in 1:length(variable_names)){
    for (k in 1:length(names)){
      if(names[k] == variable_names[j]){
        num_variables[j] <- gsub(names[k], positions[k], variable_names[j], fixed = T)
      }
    }
  }
  as.numeric(num_variables)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name regularized_regression
#' @description performs regularized regression as described in stage 2 of the ERF model
#' @param X: matrix of predictor variables (training set)
#' @param y: vector of target values as a binary, 0-1-encoded factor (training set)
#' @param type_measure "auc", "mse", "mae", "class"
#' @param Xtest optional matrix of predictor variables (test set)
#' @param ytest optional vector of target values, 0-1-encoded factor (test set)
#' @param nfolds: number of cross validation folds
#' @param s: string, either "lambda.min" or "lambda.1se", where "lambda.min" indicates the value of lambda that gives minimum mean cross-validated error, whereas  "lambda.1se": value of lambda which gives the most regularized model such that error is within one standard error of the minimum. 
#' @param confirmatory_cols: vector of integers indicating the columns of the input variables that should not be penalized (confirmatory model terms)
#' @param alpha: elastic net mixing parameter. Allowed values include: 1 for lasso regression, 0 for ridge regression, any value between 0 and 1 for elastic net regression.
#' @param dfmax:	Limits the maximum number of variables in the model. Useful for very large nvars, if a partial path is desired.
#' @param pmax:	Limits the maximum number of variables ever to be nonzero
#' @param standardize Logical flag for X variable standardization, prior to fitting the model sequence. The coefficients are always returned on the original scale.
#' @param n number of most important terms to select, default = 10
#' @param print_output logical value, indicating whether a model output should be printed
#' @return if print_output == T: console output including: nfolds, s, lambda, number of terms in the final model, 
#' mean_cv_error, the final model as a dataframe of variable names and coefficients, Xtest, a confusion matrix, the AUC and the Classification error
#' else if print_output == F: list with the following elements: Results = dataframe of variable names and coefficient,  
#' n_terms = number of terms in the final model, lambda,  mean cv error, Confusion matrix, AUC and classification error.


regularized_regression <- function(X, y, Xtest = NULL, ytest = NULL,
                                   name_rules = T,
                                   type_measure = "class",
                                   nfolds = 10,
                                   s = "lambda.min",
                                   confirmatory_cols =NULL, 
                                   alpha = 1,
                                   pmax, dfmax, standardize, 
                                   n = 5,
                                   print_output = T){
  
  set.seed(123) 
  # define penalties according to confirmatory columns
  if(length(confirmatory_cols) == 0){
    p.fac = rep(1, ncol(X))
  }else{
    p.fac = rep(1, ncol(X))
    p.fac[confirmatory_cols] <- 0
  }
  
  # # find best lambda via cross validation
  cvfit <- cv.glmnet(as.matrix(X), y, family = "binomial", alpha = alpha, 
                     type_measure = "class", nfolds = 10, pmax = pmax, 
                     dfmax = dfmax, standardize = standardize, 
                     penalty.factor = p.fac)
  
  if (s == "lambda.min"){
    lambda <- cvfit$lambda.min
  } else{
    lambda <- cvfit$lambda.1se
  }
  
  fit <- glmnet(as.matrix(X), y, family = "binomial", alpha = alpha, pmax = pmax, 
                dfmax = dfmax, standardize = standardize, penalty.factor = p.fac)
  
  # variable coefficients
  coefs <- coef(fit, s=lambda);
  # non-zero coefficients
  coefs[which(coefs != 0 ) ] 
  # non-zero coefficient feature names
  coefs@Dimnames[[1]][which(coefs != 0 ) ]  
  # number of non-zero coefficients
  n_terms = length(coefs[which(coefs != 0 ) ] - 1)
  
  Results <- data.frame( features = coefs@Dimnames[[1]][ which(coefs != 0 ) ], 
                         coefficients    = coefs       [ which(coefs != 0 ) ]
  )
  
  
  # Average Rule length
  avgrl <- average_rule_length(Results$features)
  
  # Variable Importance
  important_terms <- imp_terms(Results, n)

  if (is.null(Xtest) == T) {
    result <- list(Results = Results, nfolds = nfolds, s = s, n_terms = n_terms,
                   lambda =lambda, PenFac = p.fac, AvgRuleLength = avgrl, 
                   ImpTerms = important_terms)
    
  } else{
    pred_prob <- predict(fit, newx = as.matrix(Xtest), s = lambda, type = "response")
    pred_class <- predict(fit, newx = as.matrix(Xtest), s = lambda, type = "class")
    predictions <- as.numeric(pred_class)
    conf_mat <- table(pred = pred_class,true = ytest)
    auc <-  auc(ytest, as.integer(pred_class))
    ce <-   ce(ytest, as.integer(pred_class))
    
    result <- list(Results = Results, nfolds = nfolds, s = s,
                   n_terms = n_terms, lambda =lambda, 
                   Conf_Mat = conf_mat, AUC = auc, CE = ce, PenFac = p.fac,
                   Predictions = predictions, AvgRuleLength = avgrl,
                   ImpTerms = important_terms)
    }
  result
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title regr_output
#' @description  prints all relevant glmnet model information as received from the function 'regularized_regression'
#' @param see function description 'regularized_regression'
#' @return print statements


regr_output <- function(X, Xtest, name_rules, regmodel){
  cat(sprintf("Final ensemble with CV (k= %d) error with s = %s\n", 
              regmodel$nfolds, regmodel$s))
  cat(sprintf("\n"))
  cat(sprintf("Lambda = %f \n", regmodel$lambda))
  cat(sprintf("Number of terms = %d \n", regmodel$n_terms))
  cat(sprintf("Average rule length = %#.4f \n", regmodel$AvgRuleLength))
  cat(sprintf("\n"))
  cat(sprintf("Regularized Logistic Regression Model: \n"))
  cat(sprintf("\n"))
  if(name_rules == T){
    regmodel$Results$features <- positions_to_names(X, regmodel$Results$features)
  } 
  print(regmodel$Results)
  cat(sprintf("\n"))
  cat(sprintf(" Most important Features (acc. to coefs):\n"))
  cat(sprintf("\n"))
  if(name_rules == T){
    regmodel$ImpTerms <- positions_to_names(X, regmodel$ImpTerms)
  }
  print(regmodel$ImpTerms)
  cat(sprintf("\n"))
  if (is.null(Xtest) == F){
    cat(sprintf("Confusion matrix: \n"))
    cat(sprintf("\n"))
    print(regmodel$Conf_Mat)
    cat(sprintf("\n"))
    cat(sprintf("AUC = %#.4f \n\n", regmodel$AUC))
    cat(sprintf("Classification error = %#.4f \n", regmodel$CE))
    cat(sprintf("\n"))
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title expert_occurences
#' @description calculates the proportion of expert rules that entered the final ERF model
#' @param expert_rules vector of rule strings, including all of the external expert knowledge in form of rules
#' @param model_rules vector of strings, including all rules and variables included in the final Expert-Rulefit model (after regularized regression)
#' @return value between 0 and 1, indicating the proportion of expert rules and expert linear terms that entered the final ERF model
#'                    

expert_occurences <- function(expert_rules, confirmatory_lins, model_features){
  ek <- c(expert_rules, confirmatory_lins)
  n_ek <- length(ek)
  # counter for expert rules in the final model
  ek_in = 0
  
  if(n_ek > 0){
    for (i in 1:n_ek){
      if(ek[i] %in% model_features){
        ek_in = ek_in + 1
      }
    }
    prop_ek <- ek_in/n_ek
  } else{
    prop_ek <- 0
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title expert_output
#' @description  prints the expert rules and the proportion of expert knowledge that entered the final ERF model 
#' @param expert_rules vector of rule strings, including all of the external expert knowledge in form of rules
#' @param prop_ek proportion of expert rules and variables that entered the final ERF model
#' @return print statements

expert_output <- function(X, name_rules = T, name_lins = T, expert_rules,
                          removed_expertrules,
                          confirmatory_lins, prop_ek){
  cat(sprintf("All Expert Rules: \n"))
  cat(sprintf("\n"))
  if (!(is.null(expert_rules))){
    if(name_rules == T){
      print(positions_to_names(X, expert_rules))
    } else{
      print(expert_rules)
    }
  } else {
    print("None.")
  }
  
  cat(sprintf("\n"))
  cat(sprintf("Expert Rules removed due to low support or correlation withother rules in the model: \n"))
  cat(sprintf("\n"))
  if(!(is.null(removed_expertrules))){
    if(name_rules == T){
      print(positions_to_names(X, removed_expertrules))
    }else{
      print(removed_expertrules)
    }
  } else {
    print("None.")
  }
  
  cat(sprintf("\n"))
  cat(sprintf("Expert Linear terms: \n"))
  if(!(is.null(confirmatory_lins))){
    if(name_lins == T){
      print(positions_to_names(X, confirmatory_lins))
    } else{
      print(confirmatory_lins)
    }
  } else {
    print("None.")
  }

  cat(sprintf("\n"))
  cat(sprintf("Proportion of expert knowledge in the final model:  %#.4f \n", prop_ek))
  
} 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name avergage_rule_length
#' @description calculates the average rule lengths of the rules in the final model
#' @param rules rule strings = features of the final model
#' @return numeric value indicating the average rule length of rules in the final model

library(stringr)

average_rule_length <- function(rules){
  rule_lengths <- c()
  for(i in 1:length(rules)){
    rule_lengths[i] <- str_count(rules[i], "&") + 1
  }
  avg_length <- mean(rule_lengths)
  avg_length
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name imp_terms
#' @description selects the n model terms with the greatest model coefficients
#' @param model output of ExpertRuleFit$Model
#' @param n integer indicting number of terms to select
#' @return string vector indicating most important terms

imp_terms <- function(model, n){
  largest_coefs <- sort(model[,2], decreasing = T)[1:n]
  largest_pos <- c()
  for(i in 1: length(largest_coefs)){
    largest_pos[i] <- which(model[,2] == largest_coefs[i])
  }
  imp_terms <- model[,1][largest_pos]
  imp_terms
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name translate_out
#' @description used at the end of the main function ExpertRuleFit to convert X[,xy] into the original variable names of the dataset X

translate_out <- function(X, expert_rules, removed_expertrules, 
                          confirmatory_terms, out){
  
  colnames(out$Train) <- positions_to_names(X, colnames(out$Train))
  colnames(out$Test) <- positions_to_names(X, colnames(out$Test))
  out$Features <- positions_to_names(X, out$Features)
  out$ImpTerms <- positions_to_names(X, out$ImpTerms)
  if(!(is.null(expert_rules))){
    out$ExpertRules <- positions_to_names(X, out$ExpertRules) 
  }
  if(!(is.null(removed_expertrules))){
    out$RemovedExpertRules <- positions_to_names(X, out$RemovedExpertRules) 
  }
  if(!(is.null(confirmatory_terms))){
    out$ConfTerms <- positions_to_names(X, out$ConfTerms) 
  }
  out
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

