################################################################################
#                                                                              #
#       EXPERT RULE FIT for Cervical Cancer - Training Data Preparation        #
#                                                                              #
################################################################################

# Libraries
# library(mice)
# library(DMwR)


# Functions
# 1. find_cols
# 2. fix_cols
# 3. prepare_cervicalcancer_data


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name find_cols
#' @description identifies all columns with missing values encoded as "?"
#' @param X data frame including missing values
#' @return columns with missing values

find_cols = function(X){
  cols = c()
  for (i in 1:ncol(X)){
    if ((sum(X[,i] == "?")) > 0){
      cols = c(cols,i)
    }  
  }
  return(cols)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name fix_columns
#' @description replaces "?" by -1
#' @param X data frame with missing values encoded as "?"
#' @param cols columns with missing values
#' @return data frame with missing values encoded as -1

fix_columns = function(X,cols) {
  for (j in 1:length(cols)) {
    X[,cols[j]] = as.character(X[,cols[j]])
    X[which(X[,cols[j]] == "?"),cols[j]] = -1
    X[,cols[j]] = as.numeric(X[,cols[j]])
  }
  return(X)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @name prepare_cervicalcancer_data
#' @description Preparation of the dataset Cervical Cancer (Risk Factors) for ERF
#' @param data UCI dataset Cervical Cancer (Risk Factors)
#' @param del_maj_missing logical value. If set to TRUE, observations with more than 50 percent missing values (ca. 100. obs) are deleted 
#' @param add_NA_features logical value, if set to TRUE, 3 variables concerning the presence of missing values for the number of sexual partners, the age at first sexual intercourse and the number of pregnancies are added to the dataset
#' @param impute_missing logical value indicating whether to impute missing values. Strongly recommended. Only 59 complete cases in the data set.
#' @param imp_methods a single string, or a vector of strings with length = #columns to impute, specifying the imputation method to be used for each column in data. If specified as a single string, the same method will be used for all blocks. Common options: "pmm" = Predictive mean matching (default), "midastouch"	=	Weighted predictive mean matching, "sample" = 	Random sample from observed values, "cart"	=	Classification and regression trees and "rf" = 	Random forest imputations
#' @param target string value defining the target column. Options are: "Biopsy", "Schiller", "Hinselmann", "Citology", "Risk1" (at least 1 of the 4 tests is positive) and "Risk2" (at least 2 of the 4 tests are positive).
#' @param balance logical value. If set to TRUE, handles, the SMOTE method is applied to balance the target
#' @param perc_over a number that drives the decision of how many extra cases from the minority class are generated (known as over-sampling).
#' @param perc_under a number that drives the decision of how many extra cases from the majority classes are selected for each case generated from the minority class (known as under-sampling)

prepare_cervicalcancer_data <- function(data, del_maj_missing = T,  
                                        add_NA_features = F, impute_missing = T,
                                        imp_method = "pmm",
                                        target = "Biopsy", balance = F,
                                        perc_over = 400,
                                        perc_under = 200){
  
  # identify colums with "?" indicating missing values and change them to -1
  cols_to_fix = find_cols(data)
  data = fix_columns(data,cols_to_fix)
  
  
  # delete observations with more than 50% missing 
  if(del_maj_missing == T){
    data[data == -1] <- NA
    data <- data[which(rowMeans(!is.na(data)) > 0.5), ]
  } else{
    data[data == -1] <- NA
  }
  
  # replace stds-missing values with 0s
  cols.stds <- c("STDs", "STDs..number.", "STDs.condylomatosis", 
                 "STDs.cervical.condylomatosis", "STDs.vaginal.condylomatosis",
                 "STDs.vulvo.perineal.condylomatosis", "STDs.syphilis",
                 "STDs.pelvic.inflammatory.disease", "STDs.genital.herpes",
                 "STDs.molluscum.contagiosum", "STDs.AIDS", "STDs.HIV", 
                 "STDs.Hepatitis.B","STDs.HPV", "STDs..Time.since.first.diagnosis",
                 "STDs..Time.since.last.diagnosis")
  data[cols.stds][is.na(data[cols.stds])] <- 0
  data[is.na(data)] <- -1
  
  # add features concerning presence of missing values
  if(add_NA_features == T){
    data$partners_unknown <- as.numeric(data$Number.of.sexual.partners == -1)
    data$partners_unknown = factor(data$partners_unknown, levels=c("0","1"))
    
    data$intercourse_unknown <- as.numeric(data$First.sexual.intercourse == -1)
    data$intercourse_unknown = factor(data$intercourse_unknown,
                                      levels=c("0","1"))
    
    data$pregnancies_unknown <- as.numeric(data$Num.of.pregnancies == -1)
    data$pregnancies_unknown = factor(data$pregnancies_unknown, 
                                      levels=c("0","1"))
  }
  
  # impute missing values with multiple imputation
  if(impute_missing == T){
    data[data == -1] <- NA
    cols.impute <- c("Number.of.sexual.partners", "Num.of.pregnancies",
                     "First.sexual.intercourse",
                     "Smokes", "Smokes..years.", "Smokes..packs.year.",
                     "Hormonal.Contraceptives", "Hormonal.Contraceptives..years.",
                     "IUD", "IUD..years.")
    imputed = mice(data[, cols.impute], method = imp_method, print =  FALSE)
    data[,cols.impute] <- mice::complete(imputed)
  }
  
  # turn variables with integer/factor values into integers/factors
  cols.int <- c("Age", "Number.of.sexual.partners","First.sexual.intercourse",
                "Num.of.pregnancies", "STDs..number.", "STDs..Number.of.diagnosis",
                "STDs..Time.since.first.diagnosis", "STDs..Time.since.last.diagnosis")
  
  cols.fact <- c("Smokes", "Hormonal.Contraceptives",
                 "IUD", "STDs", "STDs.condylomatosis", 
                 "STDs.cervical.condylomatosis",
                 "STDs.vaginal.condylomatosis",
                 "STDs.vulvo.perineal.condylomatosis", 
                 "STDs.syphilis",
                 "STDs.pelvic.inflammatory.disease",
                 "STDs.genital.herpes",
                 "STDs.molluscum.contagiosum",
                 "STDs.AIDS", "STDs.HIV", 
                 "STDs.Hepatitis.B","STDs.HPV", "Dx.Cancer","Dx.CIN" , 
                 "Dx.HPV", "Dx")
  
  data[cols.int] <- lapply(data[cols.int], as.integer)
  data[cols.fact] <- lapply(data[cols.fact], as.factor)
  
  
  # delete smokes packs year (seemingly wrong), AIDS, cervical condylomatis (only 1 level)
  data = subset(data, select= -c(Smokes..packs.year., STDs.AIDS, STDs.cervical.condylomatosis))
  
  # round numeric attributes to 2 digits
  cols.num <- c("Smokes..years.", "Hormonal.Contraceptives..years.", "IUD..years.",
               "STDs..Time.since.first.diagnosis", "STDs..Time.since.last.diagnosis")
  data[cols.num] <- sapply(data[cols.num], as.numeric)
  
  data$Smokes..years. <- round(data$Smokes..years., digits=2)
  data$Hormonal.Contraceptives..years. <- round(data$Smokes..years., digits=2)
  data$IUD..years. <- round(data$Smokes..years., digits=2)
  data$STDs..Time.since.first.diagnosis <- round(data$STDs..Time.since.first.diagnosis, digits = 1)
  data$STDs..Time.since.last.diagnosis <- round(data$STDs..Time.since.last.diagnosis, digits = 1)
  
  
  # define the target
  if(target == "Biopsy"){
    data$Class = factor(data$Biopsy, levels=c("0","1"))
    data = subset(data, select= -c(Biopsy, Hinselmann, Schiller, Citology))
  } else if(target == "Schiller"){
    data$Class = factor(data$Schiller, levels=c("0","1"))
    data = subset(data, select= -c(Schiller, Biopsy, Hinselmann, Citology))
  } else if(target == "Hinselmann"){
    data$Class = factor(data$Hinselmann, levels=c("0","1"))
    data = subset(data, select= -c(Hinselmann, Biopsy, Schiller, Citology))
  }else if(target == "Citology"){
    data$Class = factor(data$Citology, levels=c("0","1"))
    data = subset(data, select= -c(Citology, Biopsy, Hinselmann, Schiller))
  }else if(target == "Risk1"){
    data$Class = as.numeric(((data$Hinselmann + data$Schiller +
                                data$Citology + data$Biopsy) > 0))
    data$Class = factor(data$Class, levels=c("0","1"))
    data = subset(data, select= -c(Biopsy, Hinselmann, Schiller, Citology))
  }else if(target == "Risk2"){
    data$Class = as.numeric(((data$Hinselmann + data$Schiller +
                                data$Citology + data$Biopsy) > 1))
    data$Class = factor(data$Class, levels=c("0","1"))
    data = subset(data, select= -c(Biopsy, Hinselmann, Schiller, Citology))
  } else{
    print("Invalid target choice.")
  }
  
  # balance the data
  if(balance == T){
    balanced <- SMOTE(Class ~ ., data, perc.over = perc_over, 
                      perc.under = perc_under)
    balanced[cols.int] <-  round(balanced[cols.int], digits = 0)
    balanced[cols.int] <-  sapply(balanced[cols.int], as.integer)
    data <- balanced
  }
  
  # change column names to be unique to ERF 
  colnames(data)[which(names(data) == "Smokes")] <- "Smoking"
  colnames(data)[which(names(data) == "Hormonal.Contraceptives")] <- "Hormonal.contaceptives"
  colnames(data)[which(names(data) == "IUD")] <- "Intra.uterine.device"
  colnames(data)[which(names(data) == "STDs")] <- "Sexually.transmitted.diseases"
  colnames(data)[which(names(data) == "Dx")] <- "DX"
  names(data)[names(data) == 'Class'] <- 'y'
  
  
  return(data)
}

