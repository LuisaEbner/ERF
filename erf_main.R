################################################################################
################################################################################
###                                                                          ###
###                         EXPERT RULE FIT (ERF)                            ###
###                                                                          ###
################################################################################
################################################################################

# Libraries
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


# External functions implemented by Luisa Ebner
source("simulation.R")
source("erf_auxiliaries.R")

# External functions implemented by Malte Nalenz
source("take1.R")
source("genrulesgbm.R")
source("genrulesrf.R")
source("createX.R")
source("create_test.R")

# THE EXPERT RULEFIT IMPLEMENTATION

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
##'  \item{ImportantFeatures}{returns a character vector including the n_imp most important ensemble learners (features) according to the absolute value of their respective coefficients.}
##'  \item{ImportantEK}{returns a character vector including the optional and confirmatory expert knowledge among the ImportantFeatures.}
##'  \item{PropEKImp}{returns the proportion (real value) of EK among the ImportantFeatures.}
##'  \item{OptionalEK}{returns a character vector including all of the optional expert rules and expert linear terms including data rules that are perfectly correlated with optional expert rules.}
##'  \item{ConfirmatoryEK}{returns  a  character  vector  including  all  confirmatory expert rules and linear terms included in the model.}
##'  \item{RemovedEK}{returns a character vector including the expert rules that were removed due to too low or too high support on the dataset.}
##'  \item{PropOptionalEK}{returns the proportion (real value) of optional expert rules and optional expert linear terms (including perfectly correlated data rules) in the final model.}
##'  \item{PropEK}{returns the proportion (real value) of all expert rules and expert linear terms (optional + confirmatory) the final model.}
##'  If Xtest is not NULL, additional list elements are:
##'  \item{Test}{returns a large data frame as the result of Combined Rule Generation Stage within the ERF model applied to the test data.  The same contains as columns an intercept, standardized expert linear terms, data rules and expert rules. Only the data rules are mandatory, the remaining elements are optional.}
##'  \item{Predictions}{returns a binary vector of target predictions for the observations in the test set.}
##'  \item{ConfusionMatrix}{returns a confusion matrix indicating predictive performance on the test set.}
##'  \item{AUC}{returns the Area Under the Curve. Values close to 1 indicate high predictive accuracy whereas values close to 0.5 indicate an uninformative classifier.}
##'  \item{ClassErr}{returns the classification error evaluated on the test set.}


ExpertRuleFit = function(X=NULL, y=NULL, Xtest=NULL, ytest=NULL, intercept=T,
                         optional_expert_rules = NULL, confirmatory_expert_rules = NULL,  
                         optional_linear_terms=NULL, confirmatory_linear_terms = NULL,
                         ntree=250, ensemble= "GBM", mix=0.5, L=3, S=6, minsup=.025, corelim = 1, 
                         alpha = 1, s = "lambda.min", standardize = F,
                         n_imp = 10, print_output = T) {
  
  # combine optional and confirmatory EK 
  all_expert_rules <- c(optional_expert_rules, confirmatory_expert_rules)
  all_linear_terms <- c(optional_linear_terms, confirmatory_linear_terms)
  
  
  # function input checks
  if((is.matrix(X)|is.data.frame(X))==F){
    stop("X must be a matrix or data frame.")
  }
  
  if((!is.factor(y))){
    stop("y is not a (binary) factor. Currently only (binary) classification is supported.")
  }
  
  if(!(is.null(Xtest))){
    if(dim(X)[2]!= dim(Xtest)[2]){
      stop("The dimensionality between X and Xtest differs.")
    }
  }
  
  if(is.null(ytest)==F){
    if(mode(y)!=mode(ytest)){
      stop("The mode of y and ytest differs.")
    }
  }
  
  if(!(is.null(all_expert_rules))){
      all_expert_rules <- names_to_positions(X, all_expert_rules)
  } 
  
  if(!(is.null(optional_expert_rules))){
      optional_expert_rules <- names_to_positions(X, optional_expert_rules)
  } 
  
  if(!(is.null(confirmatory_expert_rules))){
      confirmatory_expert_rules <- names_to_positions(X, confirmatory_expert_rules)
  } 
  
  if(!(is.null(all_linear_terms))){
    all_linear_terms <- names_to_numbers(X, all_linear_terms)
    for(l in 1:length(all_linear_terms)){
      if(is.numeric(X[,all_linear_terms[l]])==F){
        stop(sprintf("Variable %i is not numeric and can not be included as
                     linear term. Please check the variable.",l))
      }
    }
  }
  
  if(!(is.null(optional_linear_terms))){
    optional_linear_terms <- names_to_numbers(X, optional_linear_terms)
    optional_linear_terms <- paste("X[,",optional_linear_terms, "]", sep = "") 
  }
  
  
  
  if(!(is.null(confirmatory_linear_terms))){
    confirmatory_linear_terms <- names_to_numbers(X, confirmatory_linear_terms)
    confirmatory_linear_terms <- paste("X[,",confirmatory_linear_terms, "]", sep = "") 
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
  
  
  # tree ensemble -> rule ensemble
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
  if(!(is.null(all_expert_rules))){
    rulesf <- c(rulesf, all_expert_rules)
  }
  
  
  # create data matrix with rules as columns
  dt = createX(X = X, rules = rulesf, t = minsup, corelim = corelim)
  Xr = dt[[1]]
  
  # initial set of rules
  rulesFin = dt[[2]]
  
  # get the expert rules, that were removed from createX due to too low/high 
  # support or high correlation
  
  if (!(is.null(all_expert_rules))){
    removed_expertrules <- c()
    for (i in 1:length(all_expert_rules)){
      if(!(all_expert_rules[i] %in% rulesFin)){
        removed_expertrules <- c(removed_expertrules, all_expert_rules[i])
      }
    }
    removed_expertrules
  } else{
    removed_expertrules <- NULL
  }
  
  
  # standardize linear terms 
  sdl=0
  mul=0
  
  if(length(all_linear_terms)>1){
    mul = apply(X[,all_linear_terms], 2, mean)
    sdl = apply(X[,all_linear_terms], 2, sd)
    for(l in 1:length(all_linear_terms)){
      X[,all_linear_terms[l]] = (X[,all_linear_terms[l]]-mul[l])/sdl[l]
    }
  } else if(length(all_linear_terms)==1){
    mul = mean(X[,all_linear_terms])
    sdl = sd(X[,all_linear_terms])
    X[,all_linear_terms] = (X[,all_linear_terms] - mul)/sdl
  }
  
  # add linear terms and intercept (optional) to rule matrix Xt
  if(is.null(all_linear_terms)){
    if(intercept==TRUE){
      Xt = as.data.frame(cbind(rep(1, times= dim(Xr)[1]),Xr))
    } else{
      Xt = as.data.frame(Xr)
    }
  } else{
    if(intercept==TRUE){
      Xt = as.data.frame(cbind(rep(1, times=dim(X)[1]), X[,all_linear_terms], Xr))
    } else{
      Xt = as.data.frame(cbind(X[,all_linear_terms], Xr))
    }
  } 
  
  
  # change column names: intercept = X0, linear terms = X1,...Xp, rules as specified conditions
  if((intercept == TRUE) & (!(is.null(all_linear_terms)))){
    colnames(Xt)[1] <- "Intercept"
    colnames(Xt)[2:(length(all_linear_terms)+1)] <- paste("X[,",all_linear_terms, "]", sep = "") 
    colnames(Xt)[(length(all_linear_terms)+2): ncol(Xt)] <- rulesFin
  } else if ((intercept == TRUE) & (is.null(all_linear_terms))){
    colnames(Xt)[1] <- "Intercept"
    colnames(Xt)[2: ncol(Xt)] <- rulesFin
  } else if ((intercept == FALSE) & (!(is.null(all_linear_terms)))){
    colnames(Xt)[1:length(all_linear_terms)] <- paste("X[,",all_linear_terms, "]", sep = "")
    colnames(Xt)[(length(all_linear_terms)+1): ncol(Xt)] <- rulesFin
  } else{ 
    colnames(Xt) <- rulesFin
  }
  
  
  
  # get the column indices of the confirmatory terms
  if((!(is.null(confirmatory_expert_rules))) & (!(is.null(confirmatory_linear_terms)))){
    confirmatory_terms <- c(confirmatory_expert_rules, confirmatory_linear_terms)
  } else if ((is.null(confirmatory_expert_rules)) & (!(is.null(confirmatory_linear_terms)))){
    confirmatory_terms <- confirmatory_linear_terms
  } else if ((!(is.null(confirmatory_expert_rules))) & (is.null(confirmatory_linear_terms))){
    confirmatory_terms <- confirmatory_expert_rules
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
                                      alpha = alpha, standardize = standardize,
                                      n = n_imp,
                                      print_output = print_output)
    
    # EK INFO
    
    # all ensemble members (rules + linear terms)
    model_features <- regmodel$Results$features
    
    # the n_imp most important ensemble members (=features)
    imp_features <- regmodel$ImpFeatures
    
    # optional EK among the most important features
    opt_ek <- c(optional_expert_rules, optional_linear_terms)
    opt_ek_imp <- contains(opt_ek, imp_features)
    opt_ek_imp <- positions_to_names(X, opt_ek_imp)
    
    # Expert rules removed due to too low/high support on data
    optional_expert_rules_names <- positions_to_names(X, optional_expert_rules)
    unsup_expert_rules <- support_remove(optional_expert_rules_names, rbind.data.frame(X,y), minsup)
    removed_expertrules_names <- positions_to_names(X, removed_expertrules)
    
    # Expert rules with perfect/very high correlation with data rules among most imp. features
    corr_expert_rules <- setdiff(removed_expertrules_names, unsup_expert_rules)
    imp_features_names <- positions_to_names(X, imp_features)
    corr_ek_imp <- contains(corr_expert_rules, imp_features_names)
    
    # confirmatory EK among the most important features
    conf_ek <- c(confirmatory_expert_rules, confirmatory_linear_terms)
    conf_ek_imp <- contains(confirmatory_expert_rules, imp_features)
    conf_ek_imp <- positions_to_names(X, conf_ek_imp)

    # proportion of EK among most imp. features
    prop_ek_imp <- length(c(opt_ek_imp, corr_ek_imp, conf_ek_imp))/n_imp
    
    # optional EK in the final model
    all_opt_ek <- contains(opt_ek, model_features)
    all_opt_ek <- c(all_opt_ek, corr_expert_rules)
    all_opt_ek <- positions_to_names(X, all_opt_ek)
    
    # confirmatory EK in the final model
    all_conf_ek <- contains(conf_ek, model_features)
    all_conf_ek <- positions_to_names(X, all_conf_ek)
    
    # all EK in the final model
    all_ek_in <- c(all_opt_ek, all_conf_ek)
    
    # EK removed due to too low/high support
    removed_ek <- unsup_expert_rules
    
    # proportion of optional EK/EK among all EK/all features
    prop_opt_ek <- length(all_opt_ek)/(regmodel$NTerms)
    prop_all_ek <- length(all_ek_in)/(regmodel$NTerms)
    
    
    if(print_output == T){
      reg_info <- regression_output(X, Xtest, regmodel)
      exp_info <- expert_output(opt_ek_imp, corr_ek_imp, conf_ek_imp, n_imp, 
                                prop_ek_imp, all_opt_ek, all_conf_ek, removed_ek, 
                                prop_opt_ek, prop_all_ek)
      
      output <- list(reg_info, exp_info)
    }
    
    
    out = c(Train = Xt , 
            Model = regmodel$Results, 
            Features = regmodel$Results$features, 
            Coefficients = regmodel$Results$coefficients, 
            NTerms = regmodel$NTerms,
            AvgRuleLength = regmodel$AvgRuleLength,
            ImportantFeatures = regmodel$ImpFeatures,
            ImportantEK = c(opt_ek_imp, corr_ek_imp, conf_ek_imp),
            PropEKImp = prop_ek_imp,
            OptionalEK = all_opt_ek,
            ConfirmatoryEK = all_conf_ek,
            RemovedEK = removed_ek, 
            PropOptionalEK = prop_opt_ek, 
            PropEK = prop_all_ek)
    
    # give training data original column names
    colnames(out$Train) <- positions_to_names(X, colnames(out$Train))
    
    
  }else{
    
    #create rules.
    Xrt = createXtest(Xtest, rulesFin)
    
    ##preparing test data set. Standardize linear terms Xtest
    if(!(is.null(all_linear_terms))){
      for(l in 1:length(all_linear_terms)){
        Xtest[,all_linear_terms[l]] = (Xtest[,all_linear_terms[l]]-mul[l])/sdl[l]
      }
    }
    
    #combine to data frame
    if(is.null(all_linear_terms)){
      if(intercept==TRUE) {
        X_test = as.data.frame(cbind(rep(1, times = dim(Xrt)[1]), Xrt))
      }else{X_test = Xrt}
    } else {
      if(intercept==TRUE) {
        X_test = as.data.frame(cbind(rep(1, times = dim(Xrt)[1]), Xtest[,all_linear_terms], Xrt))
      }else{
        X_test = as.data.frame(cbind(Xtest[,all_linear_terms], Xrt))
      }
    }
    
    # adapt column names
    if((intercept == TRUE) & (!(is.null(all_linear_terms)))){
      colnames(X_test)[1] <- "X0"
      colnames(X_test)[2:(length(all_linear_terms)+1)] <- paste("X", all_linear_terms, sep = "")
      colnames(X_test)[(length(all_linear_terms)+2): ncol(X_test)] <- rulesFin
    } else if ((intercept == TRUE) & (is.null(all_linear_terms))){
      colnames(X_test)[1] <- "X0"
      colnames(X_test)[2: ncol(X_test)] <- rulesFin
    } else if (intercept == FALSE & (!(is.null(all_linear_terms)))){
      colnames(X_test)[1:length(all_linear_terms)] <- paste("X", all_linear_terms, sep = "")
      colnames(X_test)[(length(all_linear_terms)+1): ncol(X_test)] <- rulesFin
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
    
    
    # EK INFO
    
    # all ensemble members (rules + linear terms)
    model_features <- regmodel$Results$features
    
    # the n_imp most important ensemble members (=features)
    imp_features <- regmodel$ImpFeatures
    
    # optional EK among the most important features
    opt_ek <- c(optional_expert_rules, optional_linear_terms)
    opt_ek_imp <- contains(opt_ek, imp_features)
    opt_ek_imp <- positions_to_names(X, opt_ek_imp)
    
    # Expert rules removed due to too low/high support on data
    optional_expert_rules_names <- positions_to_names(X, optional_expert_rules)
    unsup_expert_rules <- support_remove(optional_expert_rules_names, rbind.data.frame(X,y), minsup)
    removed_expertrules_names <- positions_to_names(X, removed_expertrules)
    
    # Expert rules with perfect/very high correlation with data rules among most imp. features
    corr_expert_rules <- setdiff(removed_expertrules_names, unsup_expert_rules)
    imp_features_names <- positions_to_names(X, imp_features)
    corr_ek_imp <- contains(corr_expert_rules, imp_features_names)
    
    # confirmatory EK among the most important features
    conf_ek <- c(confirmatory_expert_rules, confirmatory_linear_terms)
    conf_ek_imp <- contains(confirmatory_expert_rules, imp_features)
    conf_ek_imp <- positions_to_names(X, conf_ek_imp)
    
    # proportion of EK among most imp. features
    prop_ek_imp <- length(c(opt_ek_imp, corr_ek_imp, conf_ek_imp))/n_imp
    
    # optional EK in the final model
    all_opt_ek <- contains(opt_ek, model_features)
    all_opt_ek <- c(all_opt_ek, corr_expert_rules)
    all_opt_ek <- positions_to_names(X, all_opt_ek)
    
    # confirmatory EK in the final model
    all_conf_ek <- contains(conf_ek, model_features)
    all_conf_ek <- positions_to_names(X, all_conf_ek)
    
    # all EK in the final model
    all_ek_in <- c(all_opt_ek, all_conf_ek)
    
    # EK removed due to too low/high support
    removed_ek <- unsup_expert_rules
    
    # proportion of optional EK/EK among all EK/all features
    prop_opt_ek <- length(all_opt_ek)/(regmodel$NTerms)
    prop_all_ek <- length(all_ek_in)/(regmodel$NTerms)
    
    
    if(print_output == T){
      reg_info <- regression_output(X, Xtest, regmodel)
      exp_info <- expert_output(opt_ek_imp, corr_ek_imp, conf_ek_imp, n_imp, 
                                prop_ek_imp, all_opt_ek, all_conf_ek, removed_ek, 
                                prop_opt_ek, prop_all_ek)
      output <- list(reg_info, exp_info)
      
    }
    
    
    out = c(Train = Xt , 
            Test = X_test,
            Model = regmodel$Results, 
            Features = regmodel$Results$features, 
            Coefficients = regmodel$Results$coefficients, 
            NTerms = regmodel$NTerms,
            Predictions = regmodel$Predictions,
            AvgRuleLength = regmodel$AvgRuleLength,
            ConfusionMatrix = regmodel$ConfusionMatrix, 
            AUC = regmodel$AUC, 
            ClassErr = regmodel$CE,
            ImportantFeatures = regmodel$ImpFeatures,
            ImportantEK = c(opt_ek_imp, corr_ek_imp, conf_ek_imp),
            PropEKImp = prop_ek_imp,
            OptionalEK = all_opt_ek,
            ConfirmatoryEK = all_conf_ek,
            RemovedEK = removed_ek, 
            PropOptionalEK = prop_opt_ek, 
            PropEK = prop_all_ek)
   
    # change column names back to original names 
    colnames(out$Train) <- positions_to_names(X, colnames(out$Train))
    colnames(out$Test) <- positions_to_names(X, colnames(out$Test))
    
  }
  

  
  class(out) = "ExpertRulemodel"
  
  out
}