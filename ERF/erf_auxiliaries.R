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
library(glmnet)
library(xgboost)
library(RCurl)
library(xrf)
library(pROC)
library(ROCit)
library(ROCR)
library(mice)
library(cvAUC)

# Functions 
# 1. create_X_y_Xtest_ytest
# 2. names_to_positions
# 3. positions_to_names
# 4. names_to_numbers
# 5. avergage_rule_length
# 6. imp_terms
# 7. regularized_regression
# 8. regression_output
# 9. expert_output
# 10. contains
# 11. support_remove
# 12. support_take
# 13. modelcomp
# 14. CV_erf
# 15. concept_drift_split

# + Functions adopted from the MSc. Thesis of Malte Nalenz

# 1. take1
# 2. genrulesRF
# 3. genrulesGBM
# 4. createX
# 5. createXtest

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name create_X_y_Xtest_ytest
#' @description turns a data set into a valid set of inputs for the main function ExpertRuleFit.
#' @param data: data frame including predictor attributes and binary target attribute
#' @param train_frac: specifies the fraction of training sample
#' @param pos_class: specifies the datasets positive target value, eg. 1,"pos","yes" 
#' @return list object of length 4 including the valid set of intputs as X, Xtest, y, ytest


createERFsets <- function(data, train_frac, n_obs = NULL, pos_class = 1, target_name = NULL, type_missing = NULL){
  
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
  
  # down-size the data set 
  if(!(is.null(n_obs))){
    if(n_obs > nrow(data)){
      stop("n_obs needs to be smaller or equal to the number of observations in the original dataset.")
    } else {
      data <- sample_n(data, n_obs)
    }
  }
  
  #print(data)
  
  # split training- and test data
  set.seed(45)
  train.index <- createDataPartition(data$y, p = train_frac, list = FALSE)
  train <- data[ train.index,]
  test  <- data[-train.index,]
  
  # create dataframe of input attributes
  if(train_frac > 0 & train_frac < 1){
    X <- train[, -ncol(train)]
    Xtest <- test[, -ncol(test)]
    
    y <- factor(ifelse(train$y == pos_class, 1, 0))
    ytest <- factor(ifelse(test$y == pos_class, 1, 0))
  } else if (train_frac == 1){
    X <- train[, -ncol(train)]
    Xtest <- NULL
    y <- factor(ifelse(train$y == pos_class, 1, 0))
    ytest <- NULL
  } else{
    stop("The fraction of training examples must be greater than 0 and at least 1.")
  }
  
  out = list(X, y, Xtest, ytest)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'@name names_to_positions
#'@description replaces the name of predictor attributes with their column position.
#'@param X data frame of predictor attributes
#'@param name_strings vector of strings with rules or linear terms using the original attribute names
#'@return vector of strings with rules or linear terms using X[,column position] referring to predictor attributes

names_to_positions <- function(X, name_strings){
  names <- colnames(X)
  positions <- c()
  for (i in 1:ncol(X)){
    positions[i] <- paste("X[,",i, "]", sep = "")
  }
  
  bool <- T
  iteration <- 0
  
  pos_strings <- c()
  
  if(length(name_strings) > 0){
    for (j in 1:length(name_strings)){
      for (k in 1:length(names)){
        if(length(name_strings[j]) > 0){
          if(grepl(names[k],name_strings[j], fixed = T)){
            pos_strings[j] <- gsub(names[k], positions[k], name_strings[j], fixed = T)
          }
        }
      }
    }
    
    
    while(bool){
      bool <- F
      for (j in 1:length(pos_strings)){
        for (k in 1:length(names)){
          if(length(pos_strings[j]) > 0){
            if(grepl(names[k],pos_strings[j], fixed = T)){
              bool <- T
              pos_strings[j] <- gsub(names[k], positions[k], pos_strings[j], fixed = T)
            }
          }
        }
      }
    }
    
  }
  pos_strings
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'@name positions_to_names
#'@description  replaces the column positions of predictor attributes with their names.
#'@param X data frame of predictor attributes
#'@param pos_strings vector strings with rules or linear terms using the X[,column position] referring to predictor attributes
#'@return vector of strings with rules or linear terms using the original attribute names


positions_to_names <- function(X, pos_strings){
  names <- colnames(X)
  positions <- c()
  for (i in 1:ncol(X)){
    positions[i] <- paste("X[,",i, "]", sep = "")
  }
  
  bool <- T
  iteration <- 0
  
  name_strings <- c()
  
  if(length(pos_strings) != 0){
    for (j in 1:length(pos_strings)){
      for (k in 1:length(positions)){
        if(length(pos_strings[j]) > 0){
          if(grepl(positions[k], pos_strings[j], fixed = T)){
            name_strings[j] <- gsub(positions[k], names[k], pos_strings[j], fixed = T)
          }
        }
      }
    }
    
    while(bool){
      bool <- F
      for (j in 1:length(name_strings)){
        for (k in 1:length(positions)){
          if(length(name_strings[j]) > 0){
            if(grepl(positions[k],name_strings[j], fixed = T)){
              bool <- T
              name_strings[j] <- gsub(positions[k], names[k], name_strings[j], fixed = T)
            }
          }
        }
      }
    }
    
  }
  
  name_strings
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#'@name names_to_numbers
#'@description replaces the names of predictor attributes with numbers indicating their column positions.
#'@param X data frame of predictor attributes
#'@param attribute_names vector of attribute names 
#'@return a vector of numbers corresponding to the attributes' column position

names_to_numbers <- function(X, attribute_names){
  names <- colnames(X)
  positions <- 1:ncol(X)
  
  num_attributes <- c()
  
  if(length(attribute_names) != 0){
    for (j in 1:length(attribute_names)){
      for (k in 1:length(names)){
        if(names[k] == attribute_names[j]){
          num_attributes[j] <- gsub(names[k], positions[k], attribute_names[j], fixed = T)
        }
      }
    }
    
  }
  as.numeric(num_attributes)
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
  nt <- length(model$features)
  if(n < nt){
    largest_coefs <- sort(model[,2], decreasing = T)[1:n]
  } else{
    largest_coefs <- sort(model[,2], decreasing = T)[1:nt]
  }
  
  largest_pos <- c()
  if(length(largest_coefs) > 0){
    for(i in 1: length(largest_coefs)){
      largest_pos[i] <- which(model[,2] == largest_coefs[i])
    } 
  }

  imp_terms <- model[,1][largest_pos]
  imp_terms
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
                                   optional_cols = NULL,
                                   optional_penalty = 1,
                                   alpha = 1, standardize = F, 
                                   n = 5, print_output = T){
  
  
  # define penalties according to confirmatory and optional columns
  if(length(confirmatory_cols) > 0 & length(optional_cols) > 0){
    p.fac = rep(1, ncol(X))
    p.fac[confirmatory_cols] <- 0
    p.fac[optional_cols] <- optional_penalty
  } else if(length(confirmatory_cols) >0 & length(optional_cols) == 0){
    p.fac = rep(1, ncol(X))
    p.fac[confirmatory_cols] <- 0
  } else if(length(confirmatory_cols) == 0 & length(optional_cols) > 0){
    p.fac = rep(1, ncol(X))
    p.fac[optional_cols] <- optional_penalty
  } else{
    p.fac = rep(1, ncol(X))
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
  n_terms = length(coefs[which(coefs != 0 ) ]) -1
  
  Results <- data.frame( features = coefs@Dimnames[[1]][which(coefs != 0 ) ], 
                         coefficients    = coefs       [ which(coefs != 0 ) ]
  )
  
  #Results$features <- positions_to_names(X, Results$features)
  
  # Average rule length
  avgrulelen <- average_rule_length(Results$features)
  
  
  # Feature importance
  important_terms <- imp_terms(Results, n)
  
  if (is.null(Xtest) == T) {
    result <- list(Results = Results, s = s, NTerms = n_terms,
                   lambda =lambda, PenaltyFactor = p.fac, AvgRuleLength = avgrulelen, 
                   ImpFeatures = important_terms)
    
  } else{
    pred_prob <- predict(fit, newx = as.matrix(Xtest), s = lambda, type = "response")
    pred_class <- predict(fit, newx = as.matrix(Xtest), s = lambda, type = "class")
    predictions <- as.numeric(pred_class)
    conf_mat <- table(pred = pred_class,true = ytest)
    auc <-  auc(ytest, pred_prob)
    ce <-   ce(ytest, as.integer(pred_class))
    
    result <- list(Results = Results, s = s,
                   NTerms = n_terms, lambda =lambda, 
                   ConfusionMatrix = conf_mat, AUC = auc, CE = ce, PenaltyFactor = p.fac,
                   Predictions = predictions, AvgRuleLength = avgrulelen,
                   ImpFeatures = important_terms)
    
  }
  result
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title regression_output
#' @description  prints the function output of 'regularized regression'.


regression_output <- function(X, Xtest, regmodel){
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

expert_output <- function(X = X, opt_ek_imp = opt_ek_imp, conf_ek_imp = conf_ek_imp,
                          n_imp = n_imp, prop_ek_imp = prop_ek_imp,
                          opt_ek_in = opt_ek_in, conf_ek_in = conf_ek_in, 
                          removed_as_unsup = removed_as_unsup,
                          removed_as_corr = removed_as_corr,
                          prop_opt_ek = prop_opt_ek, prop_all_ek = prop_all_ek){
  
  cat(sprintf("\n"))
  cat(sprintf("a) Among them optional expert knowledge (EK): \n"))
  if(length(opt_ek_imp)>0){
    opt_ek_imp <- positions_to_names(X, opt_ek_imp)
    print(opt_ek_imp)
  } else {
    print("None.")
  }
  
  cat(sprintf("\n"))
  cat(sprintf("b) Among them confirmatory EK: \n"))
  if(length(conf_ek_imp)>0){
    conf_ek_imp <- positions_to_names(X, conf_ek_imp)
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
  if (length(opt_ek_in)>0){
    opt_ek_in <- positions_to_names(X, opt_ek_in)
    print(opt_ek_in)
  } else {
    print("None.")
  }
  cat(sprintf("\n"))
  cat(sprintf("b) Confirmatory: \n"))
  cat(sprintf("\n"))
  if (length(conf_ek_in)>0){
    conf_ek_in <- positions_to_names(X, conf_ek_in)
    print(conf_ek_in)
  } else {
    print("None.")
  }
  cat(sprintf("\n"))
  
  
  cat(sprintf("\n"))
  cat(sprintf("EK removed due to too low/high support: \n"))
  cat(sprintf("\n"))
  if(length(removed_as_unsup) > 0){
    removed_as_unsup <- positions_to_names(X, removed_as_unsup)
    print(removed_as_unsup)
  } else {
    print("None.")
  }
  
  cat(sprintf("\n"))
  cat(sprintf("EK removed due to too high correlation: \n"))
  cat(sprintf("\n"))
  if(length(removed_as_corr) > 0){
    removed_as_corr <- positions_to_names(X, removed_as_corr)
    print(removed_as_corr)
  } else {
    print("None.")
  }
  
  cat(sprintf("\n"))
  cat(sprintf("Proportion of optional EK in the final model:  %#.4f \n", prop_opt_ek))
  
  cat(sprintf("\n"))
  cat(sprintf("Proportion of EK in the final model (overall):  %#.4f \n", prop_all_ek))
  
} 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title contains
#' @description selects and returns the common subset of 2 string vectors, one of which is a subset of the other


contains <- function(subset, superset){
  common_set <- c()
  if(length(subset)>0){
    for (i in 1:length(subset)){
      if(subset[i] %in% superset){
        common_set <- c(common_set, subset[i])
      }
    }
  }
  common_set
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title support_remove
#' @description selects and returns a subset of 'rules' to be deleted due to too low of too high support ('minsup') on the 'data'.

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

#' @title support_take
#' @description selects and returns a subset of 'rules' within the specified support range on the 'data'.


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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pre_for_comparison <- function(data, train_frac = 0.7, ntrees = 250, n_imp = 10, 
                               conf = NULL){
  
  train.index <- createDataPartition(data$y, p = train_frac, list = FALSE)
  train <- data[ train.index,]
  test  <- data[-train.index,]
  
  if(is.null(conf)){
    pre <- pre(y ~ ., data = train, family = "binomial", ntrees = ntrees)
  } else {
    pre <- pre(y ~ ., data = train, family = "binomial", ntrees = ntrees, 
               confirmatory = conf)
  }

  coefficients <- coef(pre)$coefficient[coef(pre)$coefficient != 0] 
  nterms <- length(coefficients) -1
  rules <- coef(pre)$description[1:length(coefficients)]
  if(length(coefficients) == length(rules)){
    model <-  data.frame(features = rules, coefficients = coefficients)
  } else{
    model <- list(coefficients = coefficients, rules = rules)
  }
  lambda.1se <- pre$glmnet.fit$lambda.1se
  lambda.min <- pre$glmnet.fit$lambda.min
  arl <- average_rule_length(rules) 
  predictions <- as.numeric(as.vector(predict(pre, newdata = test, type = "class")))
  auc <-  auc(test$y, predictions) 
  ce <-   ce(test$y, predictions) 
  conf_mat <- table(pred = predictions, true = test$y)
  impfeatures <- rules[1:n_imp]
  
  if(!(is.null(conf))){
    confek <- conf
  } else{
    confek <- NULL
  }
  
  impek <- contains(confek, impfeatures)
  prop_impek <- length(impek)/length(impfeatures)
  prop_ek <- length(confek)/nterms

  
  out = list(NTerms = nterms,
             AvgRuleLength = arl,
             AUC = auc, 
             ClassErr = ce,
             PropEKImp = prop_impek,
             PropEK = prop_ek,
             Train = train, 
             Test = test,
             Model = model, 
             Features = rules, 
             Coefficients = coefficients, 
             Predictions = predictions,
             ConfusionMatrix = conf_mat, 
             ImportantFeatures = impfeatures,
             ConfirmatoryEK = confek,
             ImportantEK = impek)
  
  out
}

# Example
# source("erf_diabetes_dataprep.R")
# source("erf_main.R")
# data <- read.csv(file = 'diabetes.csv', header = T)
# data <- prepare_diabetes_data(data)
# train.index <- createDataPartition(data$y, p = 0.7, list = FALSE)
# train <- data[ train.index,]
# test  <- data[-train.index,]

# pre_for_comparison(train, test)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' ERF Alternative: Prediction Rule Ensembles (pre package)

#pre(formula, data, family = "binomial", use.grad = TRUE,
#    tree.unbiased = TRUE, type = "both", sampfrac = 0.5, maxdepth = 3L,
#    learnrate = .01, confirmatory = NULL, mtry = Inf, ntrees = 500,
#    tree.control, removeduplicates = TRUE, removecomplements = TRUE,
#    winsfrac = 0.025, normalize = TRUE, standardize = FALSE,
#    ordinal = TRUE, nfolds = 10L, verbose = FALSE, par.init = FALSE,
#    par.final = FALSE, ...)

#' @name modelcomp
#' @description compares measures of model complexity, predictive accuracy, and expert knowledge usage across different ERF specifications and the PRE model
#' @param data initial, full data set
#' @param train_frac fraction of training data in train-test-split
#' @param see function ExpertRuleFit
#' @return table of measures for comparison

modelcomp <- function(erf1, erf2 = NULL , erf3 = NULL, 
                      pre1 = NULL, pre2 = NULL, pre3 = NULL, print_output = T){
  
  comp_table <- data.frame(ERF_1 <- c(erf1$NTerms, erf1$AvgRuleLength, erf1$AUC,
                                      erf1$ClassErr, erf1$PropEKImp, erf1$PropEK))
  colnames_table <- c("ERF1")

  if(!(is.null(erf2))){
    ct2 <- data.frame(ERF_2 <- c(erf2$NTerms, erf2$AvgRuleLength, erf2$AUC,
                                 erf2$ClassErr, erf2$PropEKImp, erf2$PropEK))
    comp_table <- cbind(comp_table, ct2)
    colnames_table <- c(colnames_table, "ERF2")
  }
  
  if(!(is.null(erf3))){
    ct3 <- data.frame(ERF_2 <- c(erf3$NTerms, erf3$AvgRuleLength, erf3$AUC,
                                 erf3$ClassErr, erf3$PropEKImp, erf3$PropEK))
    comp_table <- cbind(comp_table, ct3)
    colnames_table <- c(colnames_table, "ERF3")
  }
  
  if(!(is.null(pre1))){
    ct4 <- data.frame(PRE_1 <- c(pre1$NTerms, pre1$AvgRuleLength, pre1$AUC,
                                 pre1$ClassErr, pre1$PropEKImp, pre1$PropEK))
    comp_table <- cbind(comp_table, ct4)
    colnames_table <- c(colnames_table, "PRE1")
  }
  
  if(!(is.null(pre2))){
    ct5 <- data.frame(PRE_2 <- c(pre2$NTerms, pre2$AvgRuleLength, pre2$AUC,
                                 pre2$ClassErr, pre2$PropEKImp, pre2$PropEK))
    comp_table <- cbind(comp_table, ct5)
    colnames_table <- c(colnames_table, "PRE2")
  }
  
  if(!(is.null(pre3))){
    ct6 <- data.frame(PRE_3 <- c(pre3$NTerms, pre3$AvgRuleLength, pre3$AUC,
                                 pre3$ClassErr, pre3$PropEKImp, pre3$PropEK))
    comp_table <- cbind(comp_table, ct6)
    colnames_table <- c(colnames_table, "PRE3")
  }
  
  
  rownames(comp_table) = c("# terms", "avg. rule length", "AUC", "Class. Err.",
                           "EK/# terms", "EK/# imp. terms")
  colnames(comp_table) = colnames_table
  

  if(print_output == T){
    print(comp_table)
  }

  out <- comp_table
  out
  
}

################################################################################

# Auxiliary functions implemented by my supervisor Malte Nalenz in the 
# for his own MSc. Thesis:

# 1. take1
# 2. genrulesRF
# 3. genrulesGBM
# 4. createX
# 5. createXtest

take1 = function(len) {
  draw = c()
  for(i in 1:(len/2)){
    draw[[i]] = sample(1:2)
  }
  
  keep  = which(unlist(draw) == 1)
  keep
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @importFrom randomForest randomForest
#' @importFrom randomForest combine
#' @import inTrees

genrulesRF = function(X, y, nt,S ,L){
  N      = dim(X)[1]
  sf     = min(1, (11*sqrt(N)+1)/N)
  mn     = L*2
  ns     = S
  forest = randomForest(x = X, y=y, sampsize = sf*N, replace=F, ntree =nt, maxnodes=mn, nodesize = ns)
  
  treelist = RF2List(forest)
  rules    = lapply(1:L, function(d)as.character(extractRules(treeList=treelist, X=X, ntree=nt, maxdepth=d)))
  rules    = unique(unlist(rules))
  rules    = rules[take1(length(rules))]
  rulesmat = matrix(rules)
  colnames(rulesmat) = "condition"
  metric   = getRuleMetric(rulesmat,X,y)
  pruned   = pruneRule(metric, X, y, 0.025, typeDecay = 1)
  unique(pruned[,4])
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @import gbm
#' @import inTrees


genrulesGBM = function(X, y, nt, S, L) {
  N = dim(X)[1]
  sf = min(1, (11*sqrt(N)+1)/N)
  ns = S
  dist = ifelse(is.numeric(y), "gaussian", "bernoulli")
  if (is.numeric(y)==F){
    y = as.numeric(y)-1
  }
  model1 = gbm.fit(x = X, y=y, bag.fraction = sf, n.trees =nt, interaction.depth = L
                   , shrinkage = 0.01, distribution = dist, verbose = F, n.minobsinnode = ns)
  
  treelist = GBM2List(model1, X)
  rules    = lapply(1:L, function(d)as.character(extractRules(treeList=treelist, X=X, ntree=nt, maxdepth=d)))
  rules    = unique(unlist(rules))
  rules    = rules[take1(length(rules))]
  rulesmat = matrix(rules)
  colnames(rulesmat) = "condition"
  metric = getRuleMetric(rulesmat,X,y)
  pruned = pruneRule(metric, X, y, 0.025, typeDecay = 1)
  unique(pruned[,4])
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


createX = function(X, rules, t, corelim=1){
  Xr = matrix(0, nrow=dim(X)[1], ncol=length(rules))
  for (i in 1:length(rules)){
    Xr[eval(parse(text = rules[i])),i] = 1
  }
  
  Nr = dim(Xr)[2]
  ind = 1:Nr
  if(dim(X)[1]<200){
    t= 0.05
  }
  
  sup  = apply(Xr, 2, mean)
  elim = which((sup<t)|(sup>(1-t)))
  
  if(length(elim)>0){
    ind = ind[-elim]
  }
  
  cMat      = abs(cor(Xr[,ind])) >= (corelim)
  whichKeep = which(rowSums(lower.tri(cMat) * cMat) == 0)
  
  ind = ind[whichKeep]
  Xr = Xr[,ind]
  rules = rules[ind]
  list(data.matrix(Xr), rules)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

createXtest = function(X, rules) {
  Xr = matrix(0, nrow=dim(X)[1], ncol=length(rules))
  for (i in 1:length(rules)){
    Xr[eval(parse(text = rules[i])),i] = 1
  }
  data.matrix(Xr)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cross Validation:

CV_erf <- function(data, cv_folds = 10, seed = 1432, intercept=T,
                   optional_expert_rules = NULL, confirmatory_expert_rules = NULL,  
                   optional_linear_terms=NULL, confirmatory_linear_terms = NULL,
                   expert_only = F, optional_penalty = 1,
                   ntree=250, ensemble = "GBM", mix=0.5, L=3, S=6, minsup=.025,
                   corelim = 1, alpha = 1, s = "lambda.1se", 
                   n_imp = 10){
  cv_measures <- c("NTerms", "AvgRuleLength", "AUC", "ClassErr", "PropEKImp", "PropEK", "PropOptionalEK")
  n_measures <- length(cv_measures)
  res <- matrix(0, cv_folds, n_measures)
  
  set.seed(seed)
  
  ids = sample(1:nrow(data))
  fold = rep(1:10, length.out = nrow(data))
  target_col = ncol(data)
  y = as.factor(data[, target_col])
  x = data[,-target_col]
  for(i in 1:cv_folds){
    xtrain = x[ids[fold != i], ]
    ytrain = as.factor(y[ids[fold != i]])
    xtest = x[ids[fold == i], ]
    ytest = as.factor(y[ids[fold == i]])
    model <- ExpertRuleFit(X=xtrain, y = ytrain, Xtest = xtest, ytest = ytest, intercept = intercept,
                           optional_expert_rules = optional_expert_rules, 
                           confirmatory_expert_rules = confirmatory_expert_rules,
                           optional_linear_terms = optional_linear_terms, 
                           confirmatory_linear_terms = confirmatory_linear_terms,
                           expert_only = expert_only, optional_penalty = optional_penalty,
                           ntree = ntree, ensemble = ensemble, mix = mix, L = L, S = S, minsup = minsup, 
                           corelim = corelim, alpha = alpha, s = s, n_imp = n_imp,
                           print_output = F)
    
    for(k in 1:length(cv_measures)){
      res[i, k] <- model[[cv_measures[k]]]
    }
  }
  
  #print(res)
  cv_res <- colMeans(res)
  
  #print(cv_res)
  
  out = list(NTerms = cv_res[1], AvgRuleLength = cv_res[2], AUC = cv_res[3], 
             ClassErr = cv_res[4], PropEKImp = cv_res[5], PropEK = cv_res[6],
             PropOptionalEK = cv_res[7])
  
  out
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# creates artificial concept drift

concept_drift_split <- function(data, condition, train_frac = 0.7, cond = 0.8){
  n_train = floor(train_frac*nrow(data))
  data_cond = data %>% filter(eval(str2expression(condition)))
  n_cond = nrow(data_cond)
  data_rest = setdiff(data, data_cond)
  frac_cond = n_cond/n_train
  cat(sprintf(" %#.4f of the examples apply to the specified condition. \n", n_cond/nrow(data)))
  cat(sprintf("\n"))
  
  if (frac_cond < cond){
    train <- data_cond
    n_extra <- n_train - nrow(train)
    extra <- data_rest %>% sample_n(n_extra)
    train <- rbind(train, extra)
  } else{
    train <- data_cond %>% sample_n(frac_cond*n_cond)
    cat(sprintf(" %d examples from the %d examples defined as training set apply to the specified condition. \n", nrow(train), n_train))
    n_extra <- n_train - nrow(train)
    extra <- data_rest %>% sample_n(n_extra)
    train <- rbind(train, extra)
  }
  
  test <- setdiff(data, train)
  
  X <- train[, -ncol(train)]
  Xtest <- test[, -ncol(test)]
  
  y <- train[, ncol(train)]
  ytest <- test[, ncol(test)]
  
  
  out <- list(X, y, Xtest, ytest)
  out
  
}


