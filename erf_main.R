################################################################################
################################################################################
###                                                                          ###
###                         EXPERT RULE FIT (ERF)                            ###
###                                                                          ###
################################################################################
################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Libraries~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(gbm)
library(inTrees)
library(randomForest)
library(pROC)
library(MASS)
library(bayesm)
library(glmnet)
library(coefplot)
library(purrr)
library(rlist)
library(tidyverse)
library(caret)
library(mlbench)
library(Metrics)

#~~~~~~~~~~~~~~~~~~~~~~~~~~External Functions~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# External functions implemented by Luisa Ebner
source("simulation.R")
source("erf_auxiliaries.R")

# External functions implemented by Malte Nalenz
source("take1.R")
source("genrulesgbm.R")
source("genrulesrf.R")
source("createX.R")
source("create_test.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title ExpertRuleFit
#' @description fits the Expert RuleFit model described in the MSc. Thesis "Complementing Prediction Rule Ensembles with Expert Knowledge" based on the work of Malte Nalenz for his model HorseRuleFit 
#' @param X specifies a matrix containing the predictor attributes
#' @param y specifies a vector containing the binary response attribute.
#' @param Xtest specifies a matrix containing the predictor attributes of the test set.
#' @param ytest specifies a vector containing the binary response attribute.
#' @param intercept specifies whether to include an intercept (highly recommended).
#' @param optional_expert_rules specifies a character vector of expert-derived rules to be included to  the  set  of  data-generated  rules  as  candidate  base  classifiers  for  the  final model.
#' @param confirmatory_expert_rules specifies a character vector of expert-derived rules to certainly be included as base classifiers in the final model. No penalty will be applied to the respective coefficients, which will yield a non-zero coefficient for the rule in the final model.
#' @param expert_linear_terms specifies a character vector of expert-derived predictor attributes to be included as candidate base learners in the final model.
#' @param confirmatory_linear_terms specifies a character vector of expert-derived predictor attributes to certainly be included as base learners in the final model. No penalty will be applied to the respective coefficients, which will yield a non-zero coefficient for the linear term in the final model.
#' @param name_rules specifies  whether  the  original  column  names  are  used  to  specify  the expert rules.   If set to FALSE, attributes are assumed to be specified as X[,1],...,X[,p]
#' @param name_lins specifies  whether  the  original  column  names  are  used  to  specify  liner terms.   If set to FALSE, attributes are assumed to be specified as X[,1],...,X[,p]
#' @param ntree specifies the number of trees in the ensemble step from which data rules are extracted.
#' @param ensemble specifies whether gradient boosting ("GBM"), random forest ("RF") or a mixture of both ("both") shall be employed to generate the tree ensemble.
#' @param mix specifies that mix*ntree trees are generated via random forest and (1-mix)*ntree trees via gradient boosting whenever ensemble is set to "both".
#' @param L controls the complexity of the generated rules whereby higher values lead to more complex rules.
#' @param S controls the minimum number of observations in the tree growing process.
#' @param minsup specifies the minimum value of support such that rules with support < minsup are removed. Higher values can be used to prevent overfitting.
#' @param corelim specifies the minimum value of correlation where correlated rules are cleaned.
#' @param alpha specifies the elastic-net mixing parameter with values between 0 and 1, where 1 represents the lasso penalty, and 0 the ridge penalty.
#' @param s see function glmnet
#' @param standardize see function glmnet
#' @param n_imp specifies the number of features (rules + linear terms) to be printed as most important features.
#' @param print_output controls whether the elements of the function output are additionally printed to the console.
#' @return An object of class ExpertRuleFit, which is a list of the following components:
#'   \item{Train}{returns a large data frame as the result of the Combined Rule Generation within the ERF model applied to the training data. The same contains as columns an intercept, standardized expert linear terms, data rules and expert rules.  Only the data rules are mandatory, the remaining elements are optional.}
##'  \item{Model}{returns a data frame, which lists the base classifiers in the final model together with their corresponding coefficients.}
##'  \item{Features}{returns a character vector including first all linear terms and then allrules as base classifiers in the final model.}
##'  \item{Coefficients}{returns a vector including the coefficients of all base classifiersin the final prediction model.}
##'  \item{Nterms}{returns the number of base classifiers in the final model as an indicator of model complexity.}
##'  \item{AvgRuleLength}{returns the average lengths of all rules included in the final prediction model.}
##'  \item{ImpTerms}{returns a character vector including the n_imp most important base classifiers according to the absolute value of their respective coefficients.}
##'  \item{ExpertRules}{returns the character vector received from the input argument optional_expert_rules.}
##'  \item{RemovedExpertRules}{returns a character vector including those rules within the expert_rules that were removed due to too low support or too high correlation with other expert- or data rules.}
##'  \item{ConfTerms}{returns  a  character  vector  including  all  confirmatory  model  elements as a combination of all elements from the input confirmatory_expert_rules and the input confirmatory_linear_terms.}
##'  \item{PropEK}{returns a numeric value between 0 and 1 indicating the proportion of expert rules and expert linear terms included in the final model.}
##'  \item{PropEKImp}{returns a numeric value between 0 and 1 indicating the proportion of expert rules and expert linear terms included within the n_imp most important features of the final model.}
##'  If Xtest is not NULL, additional list elements are:
##'  \item{Test}{returns a large data frame as the result of Combined Rule Generation Stage within the ERF model applied to the test data.  The same contains as columns an intercept, standardized expert linear terms, data rules and expert rules. Only the data rules are mandatory, the remaining elements are optional.}
##'  \item{Predictions}{returns a binary vector of target predictions for the observations in the test set.}
##'  \item{ConfusionMatrix}{returns a confusion matrix indicating predictive performance on the test set.}
##'  \item{AUC}{returns the Area Under the Curve. Values close to 1 indicate high predictive accuracy whereas values close to 0.5 indicate an uninformative classifier.}
##'  \item{ClassErr}{returns the classification error evaluated on the test set.}


ExpertRuleFit = function(X=NULL, y=NULL, Xtest=NULL, ytest=NULL, intercept=T,
                         name_rules = T, expert_rules = NULL, confirmatory_rules = NULL,
                         name_lins = T, linterms=NULL, confirmatory_lins = NULL,
                         ntree=250, ensemble= "GBM", mix=0.5, L=3, S=6, minsup=.025, corelim = 1, 
                         alpha = 1, s = "lambda.min", standardize = F,
                         n_imp = 10, print_output = T) {
  
  
  # function input checks
  if((is.matrix(X)|is.data.frame(X))==F){
    stop("X must be a matrix or data frame.")
  }
  
  if((!is.factor(y))){
    stop("y is not a (binary) factor. Currently only (binary) classification is supported.")
  }
  
  if(!(is.null(Xtest))){
    if(dim(X)[2]!= dim(Xtest)[2]){
      stop("The dimensionality between X and Xtest differ.")
    }
  }
  
  if(is.null(ytest)==F){
    if(mode(y)!=mode(ytest)){
      stop("The mode of y and ytest differs.")
    }
  }
  
  if(!(is.null(expert_rules))){
    if(name_rules == T){
      expert_rules <- names_to_positions(X, expert_rules)
    }
  } 
  
  if(!(is.null(confirmatory_rules))){
    if(name_rules == T){
      confirmatory_rules <- names_to_positions(X, confirmatory_rules)
    }
    if(!(all(confirmatory_rules %in% expert_rules))){
      stop("confirmatory_rules needs to be a subset of expert_rules.")
    }
  } 
  
  if(!(is.null(linterms))){
    if(name_lins == T){
      linterms <- names_to_numbers(X, linterms)
    }
    for(l in 1:length(linterms)){
      if(is.numeric(X[,linterms[l]])==F){
        stop(sprintf("Variable %i is not numeric and can not be included as
                     linear term. Please check the variable.",l))
      }
    }
  }
  
  
  if(!(is.null(confirmatory_lins))){
    if(name_lins == T){
      confirmatory_lins <- names_to_numbers(X, confirmatory_lins)
    } 
    if(!(all(confirmatory_lins %in% linterms))){
      stop("confirmatory_lins needs to be a subset of linterms.")
    }
    confirmatory_lins <- paste("X[,",confirmatory_lins, "]", sep = "") 
  }
  
  
  if(ntree<2){
    stop("Too few trees are chosen for ExpertRuleFit.")
  }
  
  
  if((mix<0)|(mix>=1)){
    stop("invalid choice for mix, please chose a value between 0 and 1.")
  }
  
  if(L<2){
    stop("Parameter L needs to be >=2.")
  }
  
  if(S<1){
    stop("Parameter S needs to be >=1.")
  }
  
  if((minsup<0)|(minsup>=1)){
    stop("invalid choice for minimum support, please chose a 
         value between 0 and 1.")
  }
  
  
  if(is.logical(intercept)==F){
    stop("Invalid intercept choice. Must be TRUE or FALSE.")
  }
  
  
  if((alpha<0)|(alpha>1)){
    stop("invalid choice for alpha, please chose a value between 0 and 1.")
  }
  
  
  if(is.logical(print_output)==F){
    stop("Invalid choice regarding output print. Must be TRUE or FALSE.")
  }
  
  
  # tree ensemble -> rule ensemble generation
  N = length(y)
  if (ensemble == "RF") {
    capture.output(rulesf <- genrulesRF(X, y, nt=ntree, S=S, L=L))
  } else if (ensemble == "GBM") {
    capture.output(rulesf <- genrulesGBM(X, y, nt=ntree,S=S, L=L))
  } else if (ensemble == "both"){
    capture.output(rules1 <- genrulesRF(X, y, nt=round(ntree*mix),
                                        S=S, L=L))
    capture.output(rules2 <- genrulesGBM(X, y, nt=round(ntree*(1-mix)),
                                         S=S, L=L))
    rulesf = c(rules1, rules2)
  } else {
    print("invalid Tree ensemble choice")
  }
  
  # add expert rules to rule ensemble if present
  if(!(is.null(expert_rules))){
    rulesf <- c(rulesf, expert_rules)
  }
  
  
  # create data matrix with rules as columns
  dt = createX(X = X, rules = rulesf, t = minsup, corelim = corelim)
  Xr = dt[[1]]
  
  # initial set of rules
  rulesFin = dt[[2]]
  
  # get the expert rules, that were removed from createX due to low support or high correlation
  if (!(is.null(expert_rules))){
    removed_expertrules <- c()
    for (i in 1:length(expert_rules)){
      if(!(expert_rules[i] %in% rulesFin)){
        removed_expertrules <- c(removed_expertrules, expert_rules[i])
      }
    }
    removed_expertrules
  } else{
    removed_expertrules <- NULL
  }
  
  # standardize linear terms 
  sdl=0
  mul=0
  
  if(length(linterms)>1){
    mul = apply(X[,linterms], 2, mean)
    sdl = apply(X[,linterms], 2, sd)
    for(l in 1:length(linterms)){
      X[,linterms[l]] = (X[,linterms[l]]-mul[l])/sdl[l]
    }
  } else if(length(linterms)==1){
    mul = mean(X[,linterms])
    sdl = sd(X[,linterms])
    X[,linterms] = (X[,linterms] - mul)/sdl
  }
  
  # add linear terms and intercept (optional) to rule matrix Xt
  if(is.null(linterms)){
    if(intercept==TRUE){
      Xt = as.data.frame(cbind(rep(1, times= dim(Xr)[1]),Xr))
    } else{
      Xt = as.data.frame(Xr)
    }
  } else{
    if(intercept==TRUE){
      Xt = as.data.frame(cbind(rep(1, times=dim(X)[1]), X[,linterms], Xr))
    } else{
      Xt = as.data.frame(cbind(X[,linterms], Xr))
    }
  } 
  
  
  # change column names: intercept = X0, linear terms = X1,...Xp, rules as specified conditions
  if((intercept == TRUE) & (!(is.null(linterms)))){
    colnames(Xt)[1] <- "Intercept"
    colnames(Xt)[2:(length(linterms)+1)] <- paste("X[,",linterms, "]", sep = "") 
    colnames(Xt)[(length(linterms)+2): ncol(Xt)] <- rulesFin
  } else if ((intercept == TRUE) & (is.null(linterms))){
    colnames(Xt)[1] <- "Intercept"
    colnames(Xt)[2: ncol(Xt)] <- rulesFin
  } else if ((intercept == FALSE) & (!(is.null(linterms)))){
    colnames(Xt)[1:length(linterms)] <- paste("X[,",linterms, "]", sep = "")
    colnames(Xt)[(length(linterms)+1): ncol(Xt)] <- rulesFin
  } else{ 
    colnames(Xt) <- rulesFin
  }
  
  
  
  # get the column indices of the confirmatory terms
  if((!(is.null(confirmatory_rules))) & (!(is.null(confirmatory_lins)))){
    confirmatory_terms <- c(confirmatory_rules, confirmatory_lins)
  } else if ((is.null(confirmatory_rules)) & (!(is.null(confirmatory_lins)))){
    confirmatory_terms <- confirmatory_lins
  } else if ((!(is.null(confirmatory_rules))) & (is.null(confirmatory_lins))){
    confirmatory_terms <- confirmatory_rules
  } else {
    confirmatory_terms <- NULL
  }
  
  if(!(is.null(confirmatory_terms))){
    confirmatory_cols <- c()
    for(i in 1: length(confirmatory_terms)){
      if(confirmatory_terms[i] %in% colnames(Xt)){
        confirmatory_cols <- c(confirmatory_cols, which(colnames(Xt) == confirmatory_terms[i]))
      }
    }
  } else{
    confirmatory_cols <- NULL
  }
  
  
  if(is.null(Xtest) == T){
    regmodel = regularized_regression(X=Xt, y=y, Xtest = NULL, ytest =NULL,
                                      s = s,
                                      confirmatory_cols = confirmatory_cols,
                                      alpha = alpha, standardize = standardize, n = n_imp,
                                      print_output = print_output)
    
    model_features <- regmodel$Results$features
    imp_features <- regmodel$ImpTerms
    prop_ek <- expert_occurences(expert_rules, confirmatory_lins, model_features)
    prop_ek_imp <- expert_occurences(expert_rules, confirmatory_lins, imp_features)
    
    
    if(print_output == T){
      reg_info <- regr_output(X, Xtest, name_rules, regmodel)
      exp_info <- expert_output(X, name_rules, name_lins,
                                expert_rules, removed_expertrules, 
                                confirmatory_lins, prop_ek)
      output <- list(reg_info, exp_info)
      
      
      out = c(Train = Xt , Model = regmodel$Results, 
              Features = regmodel$Results$features, 
              Coefficients = regmodel$Results$coefficients, 
              Nterms = regmodel$n_terms,
              AvgRuleLength = regmodel$AvgRuleLength,
              ImpTerms <- regmodel$ImpTerms,
              ExpertRules = expert_rules, 
              ConfTerms = confirmatory_terms,
              Removed_ExpertRules = removed_expertrules,  PropEK = prop_ek, 
              PropEKImp = prop_ek_imp)
    } else{
      
      out = c(Train = Xt , Model = regmodel$Results, 
              Features = regmodel$Results$features, 
              Coefficients = regmodel$Results$coefficients, 
              Nterms = regmodel$n_terms,
              AvgRuleLength = regmodel$AvgRuleLength,
              ImpTerms = regmodel$ImpTerms,
              ExpertRules = expert_rules, 
              ConfTerms = confirmatory_terms,
              Removed_ExpertRules = removed_expertrules,  PropEK = prop_ek,
              PropEKImp = prop_ek_imp)
    }
    
    
  }else{
    
    #create rules.
    Xrt = createXtest(Xtest, rulesFin)
    
    ##preparing test data set. Standardize linear terms Xtest
    if(!(is.null(linterms))){
      for(l in 1:length(linterms)){
        Xtest[,linterms[l]] = (Xtest[,linterms[l]]-mul[l])/sdl[l]
      }
    }
    
    #combine to data frame
    if(is.null(linterms)){
      if(intercept==TRUE) {
        X_test = as.data.frame(cbind(rep(1, times = dim(Xrt)[1]), Xrt))
      }else{X_test = Xrt}
    } else {
      if(intercept==TRUE) {
        X_test = as.data.frame(cbind(rep(1, times = dim(Xrt)[1]), Xtest[,linterms], Xrt))
      }else{
        X_test = as.data.frame(cbind(Xtest[,linterms], Xrt))
      }
    }
    
    # adapt column names
    if((intercept == TRUE) & (!(is.null(linterms)))){
      colnames(X_test)[1] <- "X0"
      colnames(X_test)[2:(length(linterms)+1)] <- paste("X", linterms, sep = "")
      colnames(X_test)[(length(linterms)+2): ncol(X_test)] <- rulesFin
    } else if ((intercept == TRUE) & (is.null(linterms))){
      colnames(X_test)[1] <- "X0"
      colnames(X_test)[2: ncol(X_test)] <- rulesFin
    } else if (intercept == FALSE & (!(is.null(linterms)))){
      colnames(X_test)[1:length(linterms)] <- paste("X", linterms, sep = "")
      colnames(X_test)[(length(linterms)+1): ncol(X_test)] <- rulesFin
    } else{      
      colnames(X_test) <- rulesFin
    }
    
    
    # add prediction and error to model output
    regmodel = regularized_regression(X = Xt, y = y, Xtest = X_test,
                                      ytest = ytest, 
                                      s = s,
                                      confirmatory_cols = confirmatory_cols,
                                      alpha = alpha,
                                      standardize = standardize, n = n_imp,
                                      print_output = print_output)
    
    model_features <- regmodel$Results$features
    imp_features <- regmodel$ImpTerms
    prop_ek <- expert_occurences(expert_rules, confirmatory_lins, model_features)
    prop_ek_imp <- expert_occurences(expert_rules, confirmatory_lins, imp_features)
    
    
    if(print_output == T){
      reg_info <- regr_output(X, Xtest, name_rules, regmodel)
      exp_info <- expert_output(X, name_rules, name_lins, expert_rules, 
                                removed_expertrules, confirmatory_lins, prop_ek)
      output <- list(reg_info, exp_info)
      
      out = list(Train = Xt, Test = X_test, Model = regmodel$Results, 
                 Features = regmodel$Results$features, 
                 Coefficients = regmodel$Results$coefficients, 
                 Nterms = regmodel$n_terms,
                 Predictions = regmodel$Predictions,
                 AvgRuleLength = regmodel$AvgRuleLength,
                 ImpTerms = regmodel$ImpTerms,
                 ConfusionMatrix = regmodel$Conf_Mat, AUC = regmodel$AUC, 
                 ClassErr = regmodel$CE, ExpertRules = expert_rules, 
                 ConfTerms = confirmatory_terms,
                 RemovedExpertRules = removed_expertrules,
                 PropEK = prop_ek, PropEKImp = prop_ek_imp)
    } else{
      
      out = list(Train = Xt, Test = X_test, Model = regmodel$Results, 
                 Features = regmodel$Results$features, 
                 Coefficients = regmodel$Results$coefficients, 
                 Nterms = regmodel$n_terms,
                 Predictions = regmodel$Predictions,
                 AvgRuleLength = regmodel$AvgRuleLength,
                 ImpTerms = regmodel$ImpTerms,
                 ConfusionMatrix = regmodel$Conf_Mat, AUC = regmodel$AUC, 
                 ClassErr = regmodel$CE, ExpertRules = expert_rules, 
                 ConfTerms = confirmatory_terms,
                 RemovedExpertRules = removed_expertrules,
                 PropEK = prop_ek, PropEKImp = prop_ek_imp)
      
    }
  }
  
  if(name_rules == T){
    out <- translate_out(X, expert_rules, removed_expertrules, 
                         confirmatory_terms, out)
  }
  
  class(out) = "ExpertRulemodel"
  
  out
}