################################################################################
################################################################################
#                                                                              #
#                   Expert Rulefit - Auxiliary Functions                       #
#                                                                              #                                                                              #
################################################################################
################################################################################

# Libraries
library(caret)
library(stringr)
library(dplyr)


# Functions
# 1. create_X_y_Xtest_ytest
# 2. names_to_positions
# 3. positions_to_names
# 4. names_to_numbers
# 5. regularized_regression
# 6. regr_output
# 7. expert_output
# 8. avergage_rule_length
# 9. imp_terms
# 10. translate_out

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name create_X_y_Xtest_ytest
#' @description turns a data set into a valid set of inputs for the main function ExpertRuleFit.
#' @param data: data frame including predictor attributes and binary target attribute
#' @param train_frac: specifies the fraction of training sample
#' @param pos_class: specifies the datasets positive target value, eg. 1,"pos","yes" 
#' @return list object of length 4 including the valid set of intputs as X, Xtest, y, ytest


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
  
  # split training- and test data
  set.seed(45)
  train.index <- createDataPartition(data$y, p = train_frac, list = FALSE)
  train <- data[ train.index,]
  test  <- data[-train.index,]
  
  # convert data frame into matrix, remove target attribute
  X <- model.matrix(y ~., train)[,-1]
  Xtest <- model.matrix(y~.,test)[,-1]
  
  # Convert target attribute into 0-1-coded factor
  y <- factor(ifelse(train$y == pos_class, 1, 0))
  ytest <- factor(ifelse(test$y == pos_class, 1, 0))
  
  out = list(X, y, Xtest, ytest)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'@name names_to_positions
#'@description replaces the name of predictor attributes with their column position.
#'@param X data frame of predictor attributes
#'@param name_rules vector of expert rules using the original attribute names
#'@return vector of expert rules using X[,column position] referring to predictor attributes

names_to_positions <- function(X, name_rules){
  names <- colnames(X)
  positions <- c()
  for (i in 1:ncol(X)){
    positions[i] <- paste("X[,",i, "]", sep = "")
  }
  
  bool <- T
  iteration <- 0
  
  pos_rules <- c()
  
  if(length(name_rules) != 0){
    for (j in 1:length(name_rules)){
      for (k in 1:length(names)){
        if(length(name_rules[j]) > 0){
          if(grepl(names[k],name_rules[j], fixed = T)){
            pos_rules[j] <- gsub(names[k], positions[k], name_rules[j], fixed = T)
          }
        }
      }
    }
    
    
    while(bool){
      bool <- F
      for (j in 1:length(pos_rules)){
        for (k in 1:length(names)){
          if(length(pos_rules[j]) > 0){
            if(grepl(names[k],pos_rules[j], fixed = T)){
              bool <- T
              pos_rules[j] <- gsub(names[k], positions[k], pos_rules[j], fixed = T)
            }
          }
        }
      }
    }
    
  }
  pos_rules
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'@name positions_to_names
#'@description  replaces the column positions of predictor attributes with their names.
#'@param X data frame of predictor attributes
#'@param pos_rules vector of expert rules using X[,column position] referring to predictor attributes
#'@return vector of expert rules using the original attribute names


positions_to_names <- function(X, pos_rules){
  names <- colnames(X)
  positions <- c()
  for (i in 1:ncol(X)){
    positions[i] <- paste("X[,",i, "]", sep = "")
  }
  
  bool <- T
  iteration <- 0
  
  name_rules <- c()
  
  if(length(pos_rules) != 0){
    for (j in 1:length(pos_rules)){
      for (k in 1:length(positions)){
        if(length(pos_rules[j]) > 0){
          if(grepl(positions[k], pos_rules[j], fixed = T)){
            name_rules[j] <- gsub(positions[k], names[k], pos_rules[j], fixed = T)
          }
        }
      }
    }
    
    while(bool){
      bool <- F
      for (j in 1:length(name_rules)){
        for (k in 1:length(positions)){
          if(length(name_rules[j]) > 0){
            if(grepl(positions[k],name_rules[j], fixed = T)){
              bool <- T
              name_rules[j] <- gsub(positions[k], names[k], name_rules[j], fixed = T)
            }
          }
        }
      }
    }
    
  }
  
  name_rules
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'@name names_to_numbers
#'@description replaces the names of predictor attributes with numbers indicating their column positions.
#'@param X data frame of predictor attributes
#'@param variable_names vector of attribute names 
#'@return a vector of numbers corresponding to the attributes' column position

names_to_numbers <- function(X, variable_names){
  names <- colnames(X)
  positions <- 1:ncol(X)
  
  num_variables <- c()
  
  if(length(variable_names) != 0){
    for (j in 1:length(variable_names)){
      for (k in 1:length(names)){
        if(names[k] == variable_names[j]){
          num_variables[j] <- gsub(names[k], positions[k], variable_names[j], fixed = T)
        }
      }
    }
    
  }
  as.numeric(num_variables)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name regularized_regression
#' @description performs regularized regression as described in Stage 3 of the ERF model.
#' @param X: data frame of predictor attributes (training set)
#' @param y: vector of the target attribute (training set)
#' @param Xtest optional data frame of predictor attributes (test set)
#' @param ytest optional vector of the target attribute (test set)
#' @param s: string, either "lambda.min" or "lambda.1se". "lambda.min" searches the value of lambda that gives minimum mean cross-validated error, "lambda.1se" searches the most regularized model such that error is within one standard error of the minimum. 
#' @param confirmatory_cols: vector of numbers indicating the column positions of attributes to be spared from penalization.
#' @param alpha: see function glmnet
#' @param standardize see function glmnet
#' @param n number of most important terms to print as part of the ERF model output
#' @param print_output logical value, indicating whether model output should be printed
#' @return list of the following elements:
#'   \item{Results}{returns a data frame, which lists the base classifiers in the final model together with their corresponding coefficients.}
#'   \item{n_terms}{returns the number of base classifiers in the final model as an indicator of model complexity.}
#'   \item{Conf_Mat}{returns a confusion matrix indicating predictive performance on the test set.}
#'   \item{AUC}{returns the Area Under the Curve. Values close to 1 indicate high predictive accuracy whereas values close to 0.5 indicate an uninformative classifier.}
#'   \item{CE}{returns the classification error evaluated on the test set.}
#'   \item{PenFac}{returns the penalty factors.}
#'   \item{Predictions}{returns a binary vector of target predictions for the observations in the test set.}
#'   \item{AvgRuleLength{returns the average lengths of all rules included in the final prediction model.}
#'   \item{ImpTerms}{returns a character vector including the n_imp most important base classifiers according to the absolute value of their respective coefficients.}

regularized_regression <- function(X, y, Xtest = NULL, ytest = NULL,
                                   s = "lambda.min",
                                   confirmatory_cols =NULL, 
                                   alpha = 1, standardize = F, 
                                   n = 5, print_output = T){
  
  
  # define penalties according to confirmatory columns
  if(length(confirmatory_cols) == 0){
    p.fac = rep(1, ncol(X))
  }else{
    p.fac = rep(1, ncol(X))
    p.fac[confirmatory_cols] <- 0
  }
  
  # find best lambda via cross validation
  cvfit <- cv.glmnet(as.matrix(X), y, family = "binomial", 
                     alpha = alpha, 
                     standardize = standardize, 
                     penalty.factor = p.fac)
  
  if (s == "lambda.min"){
    lambda <- cvfit$lambda.min
  } else{
    lambda <- cvfit$lambda.1se
  }
  
  fit <- glmnet(as.matrix(X), y, family = "binomial", alpha = alpha,
                standardize = standardize, penalty.factor = p.fac)
  
  # attribute coefficients
  coefs <- coef(fit, s=lambda)
  
  
  # number of non-zero coefficients (= number of terms/features)
  n_terms = length(coefs[which(coefs != 0 ) ] - 1)
  
  Results <- data.frame( features = coefs@Dimnames[[1]][which(coefs != 0 ) ], 
                         coefficients    = coefs       [ which(coefs != 0 ) ]
  )
  
  #Results$features <- positions_to_names(X, Results$features)
  
  # Average rule length
  avgrl <- average_rule_length(Results$features)
  
  
  # Feature importance
  important_terms <- imp_terms(Results, n)
  
  if (is.null(Xtest) == T) {
    result <- list(Results = Results, s = s, NTerms = n_terms,
                   lambda =lambda, PenaltyFactor = p.fac, AvgRuleLength = avgrl, 
                   ImpFeatures = important_terms)
    
  } else{
    pred_prob <- predict(fit, newx = as.matrix(Xtest), s = lambda, type = "response")
    pred_class <- predict(fit, newx = as.matrix(Xtest), s = lambda, type = "class")
    predictions <- as.numeric(pred_class)
    conf_mat <- table(pred = pred_class,true = ytest)
    auc <-  auc(ytest, as.integer(pred_class))
    ce <-   ce(ytest, as.integer(pred_class))
    
    result <- list(Results = Results, s = s,
                   NTerms = n_terms, lambda =lambda, 
                   ConfusionMatrix = conf_mat, AUC = auc, CE = ce, PenaltyFactor = p.fac,
                   Predictions = predictions, AvgRuleLength = avgrl,
                   ImpFeatures = important_terms)
    
  }
  result
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title regr_output
#' @description  prints the function output of 'regularized regression'.


regr_output <- function(X, Xtest, regmodel){
  cat(sprintf("Final Expert RuleFit ensemble - Model & Results Overview"))
  cat(sprintf("\n"))
  cat(sprintf("\n"))
  cat(sprintf("Lambda: %f \n", regmodel$lambda))
  cat(sprintf("Number of terms: %d \n", regmodel$NTerms))
  cat(sprintf("Average rule length: %#.4f \n", regmodel$AvgRuleLength))
  cat(sprintf("\n"))
  if (is.null(Xtest) == F){
    cat(sprintf("Confusion matrix: \n"))
    cat(sprintf("\n"))
    print(regmodel$ConfusionMatrix)
    cat(sprintf("\n"))
    cat(sprintf("AUC: %#.4f \n\n", regmodel$AUC))
    cat(sprintf("Classification error: %#.4f \n", regmodel$CE))
    cat(sprintf("\n"))
  }
  cat(sprintf("Regularized logistic regression model: \n"))
  cat(sprintf("\n"))
  regmodel$Results$features <- positions_to_names(X, regmodel$Results$features)
  print(regmodel$Results)
  cat(sprintf("\n"))
  cat(sprintf("Most important features:\n"))
  cat(sprintf("\n"))
  regmodel$ImpFeatures <- positions_to_names(X, regmodel$ImpFeatures)
  print(regmodel$ImpFeatures)
  cat(sprintf("\n"))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title expert_output
#' @description  prints an information summary on expert knowledge that entered the final ERF model.


expert_output <- function(opt_ek_imp, corr_ek_imp, conf_ek_imp, n_imp, 
                          prop_ek_imp, all_opt_ek, all_conf_ek, removed_ek, 
                          prop_opt_ek, prop_all_ek){
  
  cat(sprintf("\n"))
  cat(sprintf("a) Among them optional expert knowledge (EK): \n"))
  if(!(is.null(opt_ek_imp))| !(is.null(corr_ek_imp))){
    print(opt_ek_imp, corr_ek_imp)
  } else {
    print("None.")
  }
  
  cat(sprintf("\n"))
  cat(sprintf("b) Among them confirmatory EK: \n"))
  if(!(is.null(conf_ek_imp))){
    print(conf_ek_imp)
  } else {
    print("None.")
  }
  
  cat(sprintf("\n"))
  cat(sprintf("-> Proportion of EK among the %#.0f most important features :  %#.4f \n", n_imp, prop_ek_imp))
  
  cat(sprintf("\n"))
  cat(sprintf("All EK in the final model: \n"))
  cat(sprintf("a) Optional: \n"))
  cat(sprintf("\n"))
  if (!(is.null(all_opt_ek))){
    print(all_opt_ek) 
  } else {
    print("None.")
  }
  cat(sprintf("\n"))
  cat(sprintf("b) Confirmatory: \n"))
  cat(sprintf("\n"))
  if (!(is.null(all_conf_ek))){
    print(all_conf_ek) 
  } else {
    print("None.")
  }
  cat(sprintf("\n"))
  
  
  cat(sprintf("\n"))
  cat(sprintf("EK removed due to too low/high support: \n"))
  cat(sprintf("\n"))
  if(!(is.null(removed_ek))){
    print(removed_ek)
  } else {
    print("None.")
  }
  
  
  cat(sprintf("\n"))
  cat(sprintf("Proportion of optional EK in the final model:  %#.4f \n", prop_opt_ek))
  
  cat(sprintf("\n"))
  cat(sprintf("Proportion of EK in the final model (overall):  %#.4f \n", prop_all_ek))
  
} 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name avergage_rule_length
#' @description calculates the average rule lengths of the rules in the final model.

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
#' @description selects the n model terms with the greatest model coefficients indicating most important rules and linear terms.

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



contains <- function(ek, final_terms){
  ek_contained <- c()
  if(length(ek)>0){
    for (i in 1:length(ek)){
      if(ek[i] %in% final_terms){
        ek_contained <- c(ek_contained, ek[i])
      }
    }
  }
  ek_contained
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

not_contained <- function(string_vec1, string_vec2){
  uncommon_vec <- c()
  if(length(string_vec1)>0){
    for (i in 1:length(string_vec1)){
      if(!(string_vec1[i] %in% string_vec2)){
        uncommon_vec <- c(uncommon_vec, string_vec1[i])
      }
    }
  }
  uncommon_vec
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

support_remove <- function(rules, data, minsup){
  sup_vals <- c()
  if(length(rules) > 0){
    for(i in 1:length(rules)){
      sup_vals <- c(sup_vals, nrow(data %>% filter(eval(str2expression(rules[i]))))/nrow(data))
    }
  sup_frame <- data.frame(rules = rules, support_values = sup_vals)
  sup_frame1 <- sup_frame %>% filter(support_values <= minsup)
  sup_frame2 <- sup_frame %>% filter(support_values >= (1-minsup))
  out <- (rbind.data.frame(sup_frame1, sup_frame2))$rules
  } else {
    out = "None."
  }
  out
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

support_take <- function(rules, data, minsup){
  sup_vals <- c()
  if(length(rules) > 0){
    for(i in 1:length(rules)){
      sup_vals <- c(sup_vals, nrow(data %>% filter(eval(str2expression(rules[i]))))/nrow(data))
    }
  }
  sup_frame <- data.frame(rules = rules, support_values = sup_vals)
  sup_frame <- sup_frame %>% filter((support_values > minsup) & support_values < (1-minsup))
  out <- sup_frame$rules
  out
}


