---
output:
  md_document:
    variant: markdown_github
---


# ExpertRuleFit: Complementing Rule Ensembles with Expert Knowledge

This repository includes the implementation of the ExpertRuleFit model proposed in the Master Thesis **Expert RuleFit - Complementing Rule Ensembles with Expert Knowledge**. 
Expert RuleFit is a novel machine learning model for binary classification that allows for a specified integration of expert knowledge in form of rules and linear terms, which may be specified as confirmatory or optional respecitvely. 

ERF is based on the RuleFit method proposed by Friedman and Popescu in 2008.
Within RuleFit, a large initial ensemble of candidate rules is generated from a boosted tree ensemble. Subsequently, the same are applied together with linear terms as base classifiers in a L1-regularized regression to specify model coefficients for the final ensemble and remove unimportant base classifiers.
Competitive, state-of-the-art performance at a comparatively high level of interpretability promote the use of RuleFit in the context of medical classification tasks, such as diagnosis. Limitations arise from overfitting and high model complexity.

For the purpose of accurate, yet interpretable decision support in medical classification
tasks, ERF modifies and extends RuleFit with the following adjustments:

1) The implementation is completely **R** based, allowing better access to results and more control over parameters used to generate the prediction model.
2) The initial tree ensembles may be generated mixing random forest and gradient boosting instead of gradient boosting only.
2) Regularization is formulated according to the elastic net instead of Lasso only, such that flexible combinations of Ridge an Lasso are built-in alternatives. Glmnet is used for implementation.
3) ERF applies adaptive regularization, where adaptive weigths allow for a customized penalization of optional expert knowledge (EK) and an expemtion from penalization for confirmatory EK.
4) ERF may be specified as to contain only expert rules and -linear terms to assess the predictive accuracy of EK alone together with respective ciefficients as measures of the degree and direction of predictive influence.

The exploitation of expert knowledge/reasoning and training examples as complementary information sources makes ER 

1) more generalizable (robust to concept drifts),
2) data-efficient (robust to small training sets) and
3) interpretable (decreased model complexity, increased human involvement, increased classifier acceptance)

The basic usage and default settings of the ExpertRuleFit function are as follows:

```{r, results = FALSE}

createERFsets(data, train_frac)
set.seed(42)
diabetes.erf <- ExpertRuleFit(X, y, Xtest = NULL, ytest = NULL, intercept=T,
                              optional_expert_rules = NULL, confirmatory_expert_rules = NULL,
                              optional_linear_terms = NULL, confirmatory_linear_terms = NULL,
                              optional_penalty = NULL, ntree=250, ensemble= "GBM", mix=0.5, L=3, S=6,
                              minsup=.025, corelim = 1, n_imp = 10, print_output = T, ...)
```

\texttt{X} and \texttt{y} are the only mandatory arguments, the remaining arguments are optional.
A detailed description of the parameters can be found in the main file 'erf_main' in the folder ERF.

To get a first impression of how the function `ExpertRuleFit()` works, a short introductory example is provided regarding the prediction of Diabetes. An extensive description of the fitting procedure and example analyses with more extensive explanations can be found in the folders **ERF** and **experiments**, as well as the respective MSc. thesis.

## Example: Predicting Diabetes

The **Diabetes data** stored as *diabetes.csv* and was loaded from the UCI Machine Learning Repository.
The dataset was preprocessed to be applicable to the ExpertRuleFit model in the file *erf_diabetes_dataprep.R*.

**Data.** The Pima Indian Diabetes (PID) data set loaded from the UCI Machine Learning Repository
results from a survey taken out by the National Institute of Diabetes and Digestive and Kidney Diseases. Recorded information regards a 768 adult women sampled from the Pima Indian population in Arizona.
The binary target *diabetes* was diagnosed according to the WHO criteria regarding glucose
concentrations in different medical test settings. Eight numeric attributes were included as significant risk factors for an onset of
diabetes. There are: *Pregnancies, Glucose, BP, SkinThickness, Insulin, BMI, DPF* and *Age*.

```{r}

source("./experiments/diabetes/erf_diabetes_dataprep.R")
source("./ERF/erf_main.R")

data <- read.csv(file = './data sets/diabetes.csv', header = T)
data <- prepare_diabetes_data(data)

head(data)

```

**Expert Knowledge** Factual medical expert knowledge may be extracted from textbooks, 
clinical practice guidelines and expert interviews/assessments. For this example we use EK from the medical guideline *Standards of Medical Care in Diabetes - 2019 (SMCD)* published by the American Diabetes Association as well as the German guideline *Nationale Versorgungs-Leitlinie - Diabetes mellitus Typ 2 (NVLDM)*

```{r, results = FALSE}

# confirmatory expert rules (1)
guideline_rules1 <- c("Age<=39 & BP<=80 & BMI<25",
                                  "Age<=39 & BP<=80 & BMI>=25 & BMI<=30",
                                  "Age<=39 & BP<=80 & BMI>=31 & BMI<=40",         
                                  "Age>=40 & Age<=49 & BP<=80 & BMI>=31 & BMI<=40",
                                  "Age<=39 & BP>=81 & BMI>=31 & BMI<=40",
                                  "Age<=39 & BP<=80 & BMI>40",
                                  "Glucose<=100", "Glucose>100 & Glucose<=110",
                                  "Glucose>110", "BP<=85", "BP>85 & BP<=90", 
                                  "BMI<=24", "BMI<24 & BMI<=26", "BMI>26",
                                  "Glucose>110 & BMI>26")
                                  
# optional expert rules                              
guideline_rules2 <- c("Age>=50 & Age<=59 & BP>=81 & BMI>=25 & BMI<=30",
                              "Age>=50 & Age<=59 & BP<=80 & BMI>=31 & BMI<=40",
                              "Age>=60 & BP<=80 & BMI>=31 & BMI<=40",
                              "Age>=50 & Age<=59 &  BP>=81 & BMI>40", "BP>90"                                         
                              "Glucose>110 & BP>90", "BP>90 & BMI>26")

# optional expert linear terms                            
guideline_terms <-  c("BP", Glucose")

```

Further expert knowledge stems from personal interviews with practicing doctors as:

```{r, results = FALSE}

# confirmatory expert rules (2)
expert_interview_rules <- c("Age<=42 & BP<=80 & BMI<=29",
                         "Age>=45 & BP>=90 & Glucose>=125",
                         "Age<=31 & BP>=90 & BMI>=38",
                         "Age>=55 & BP<=80 & BMI<=29",
                         "Age>=60 & Glucose>=130 & BMI>=35", 
                         "Age>=60 & BP>=90 & BMI>=37",
                         "Age>=45 & BP>=90 & BMI>=35 & Glucose>=130",
                         "Age>=55 & BP<=90 & BMI<=30 & Glucose>=130",
                         "Age<=60 & BP<=90 & BMI<=30 & Glucose<=100")

# confirmatory linear terms
expert_interview_terms <- c("BMI", "Age", "DPF")

```

Thus we specify the ERF model as follows:

```{r}

erf_diabetes <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                              optional_expert_rules = guideline_rules2, 
                              confirmatory_expert_rules = c(guideline_rules1, expert_interview_rules),  
                              optional_linear_terms= guideline_terms
                              confirmatory_linear_terms = expert_interview_terms,
                              optional_penalty = 0.8, print_output = T)

```

The first few lines of the printed results provide the penalty parameter value ($\lambda$) employed for selecting the final ensemble. By default, the '1-SE' rule is used for selecting $\lambda$, the number of base classifiers (rules + linear terms) used in the final model as well as the average number of conditions per rule.

Next, the printed results provide the rules and linear terms selected in the final ensemble, with their estimated coefficients. For rules, the `description` column provides the conditions. The `coefficient` column presents the estimated coefficient. These are regression coefficients, reflecting the expected increase in the response for a unit increase in the predictor, keeping all other predictors constant. For rules, the coefficient thus reflects the difference in the expected value of the response when the conditions of the rule are met, compared to when they are not. Accordingly, the most important 10 rules and linear terms are listed in the model output.

The remaining part of the output relates to expert knowledge and thereby distinguishes between confirmatory and optional EK. While the confirmatory part appears safe in the final model, optional EK is either penalised in the same way as the rules from the data set, or is somewhat favoured by the parameter `optional_penalty`. This can be of judgement. Finally, unlike expert rules, the data rules can also learn noise to increase accuracy.

With the dollar access, various additional information can be retrieved from the object `diabetes.erf` of the 'ExpertRuleFit' class.



