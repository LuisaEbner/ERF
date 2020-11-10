################################################################################
################################################################################
###                                                                          ###
###          Training Data Preparation - Pima Indian Diabetes                ###
###                                                                          ###
################################################################################
################################################################################

# Libraries
library(mice)

# Function

#' @name prepare_diabetes_data
#' @description Preparation of the Pima Indians Diabetes data set for ERF
#' @param data original Pima Indian Diabetes data (see UCI ML Repo)
#' @param imp_method a single string, or a vector of strings, specifying the imputation method to be used for the attributes 'SkinThickness' and 'Insulin'. If specified as a single string, the same method will be used for all blocks. Common choices: "pmm" = Predictive mean matching, sample" = Random sample from observed values, "cart" =	Classification and regression trees, "rf" = 	Random forest imputations (default).
#' @return prepared Pima Indian Diabetes data

prepare_diabetes_data <- function(data, imp_method = "rf"){
  
  # Shorten some attribute names
  names(data)[names(data) == "BloodPressure"] <- "BP"
  names(data)[names(data) == "DiabetesPedigreeFunction"] <- "DPF"
  
  
  # Impute missing data (= 0 except for the attributes 'Pregnancies' & 'Outcome') 
  cols_missing <- colnames(data)[!colnames(data) %in% c("Pregnancies",
                                                        "Outcome")]
  missing <- data[cols_missing] == 0
  data[cols_missing][missing] <- NA
  
  # a) Impute with median where few values are missing per column
  data$Glucose[is.na(data$Glucose)] <- median(data$Glucose,na.rm = T)
  data$BP[is.na(data$BP)] <- median(data$BP, na.rm = T)
  data$BMI[is.na(data$BMI)] <- median(data$BMI,na.rm = T)
  
  # b) Predict missing values where many values are missing
  mice_imp <- mice(data[, c("SkinThickness","Insulin")], method=imp_method, print = F) 
  mice_complete <- mice::complete(mice_imp)
  data$SkinThickness <- mice_complete$SkinThickness
  data$Insulin <- mice_complete$Insulin
  
  return(data)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# EXAMPLE
# data <- read.csv(file = 'diabetes.csv', header = T)
# data <- prepare_diabetes_data(data)
