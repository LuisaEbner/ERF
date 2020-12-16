# ExpertRuleFit -
## Complementing Rule Ensembles with Expert Knowledge


This repository includes the implementation of the ***ExpertRuleFit (ERF)*** model proposed in the Master Thesis **Expert RuleFit - Complementing Rule Ensembles with Expert Knowledge**. ERF is a novel machine learning model for binary classification that allows for a specified integration of expert knowledge in form of rules and linear terms.

ERF is based on the RuleFit method proposed by Friedman and Popescu in 2008. Within RuleFit, a large initial ensemble of candidate rules is generated from a boosted tree ensemble. Subsequently, the same are applied together with linear terms as base classifiers in a L1-regularized regression to specify model coefficients for the final ensemble and remove unimportant base classifiers. Competitive state-of-the-art performance and a comparatively high level of interpretability promote the use of RuleFit in the context of medical classification tasks, such as diagnosis. Limitations arise from overfitting and high model complexity.

For the purpose of accurate, yet interpretable decision support in medical classification
tasks, ERF modifies and extends RuleFit with the following adjustments:

1) The implementation is completely **R** based, allowing better access to results and more control over parameters used to generate the prediction model.
2) The initial tree ensembles may be generated mixing random forest and gradient boosting instead of gradient boosting only.
2) Regularization is formulated according to the elastic net instead of Lasso only, such that flexible combinations of Ridge an Lasso are built-in alternatives. Glmnet is used for implementation.
3) ERF applies adaptive regularization, where adaptive weigths allow for a customized penalization of optional expert knowledge (EK) and an expemtion from penalization for confirmatory EK.
4) ERF may be specified as to contain only expert rules and -linear terms to assess the predictive accuracy of EK alone together with respective ciefficients as measures of the degree and direction of predictive influence.

The exploitation of expert knowledge/reasoning and training examples as complementary information sources may lead to

1) more accurate / generalizable,
2) more robust to small training sets
3) interpretable (decreased model complexity, increased human involvement) prediction models.

The basic usage and default settings of the ExpertRuleFit function are as follows:

```{r, results = FALSE, eval = FALSE, warning=FALSE, message=FALSE}

set.seed(42)
diabetes.erf <- ExpertRuleFit(X, y, Xtest = NULL, ytest = NULL, intercept=T,
                              confirmatory_expert_rules = NULL, 
                              optional_expert_rules = NULL,
                              confirmatory_linear_terms = NULL, 
                              optional_linear_terms = NULL,
                              optional_penalty = 1, expert_only = F, 
                              ntree=250, ensemble= "GBM",mix=0.5, L=3, S=6,
                              minsup=.025, corelim = 1, alpha = 1,
                              s = "lambda.1se", n_imp = 10, 
                              print_output = T)
```

The following **arguments** are **mandatory**:

  - `X`: specifies a matrix containing the predictor attributes.
  - `y`: specifies a vector containing the binary target attribute.
  
The remaining **arguments** are **optional**:

  - `Xtest`: specifies a matrix containing the predictor attributes of the test set.
  - `ytest`: specifies a vector containing the binary target attribute.
  - `intercept`: specifies whether to include an intercept (highly recommended).
  - `confirmatory_expert_rules`: specifies a character vector of expert-derived rules to certainly be included as base classifiers in the final model.
  - `optional_expert_rules`: specifies a character vector of expert-derived rules tobe included as candidate base classifiers for the final model.
  - `confirmatory_linear_terms`: specifies a character vector of expert-derived predictor attributes to certainly be included as base learners in the final model.
  - `optional_linear_terms`: specifies a character vector of expert-derived predictor attributes to be included as candidate base learners in the final model.
  - `optional_penalty`: specifies the penalty factor applied to all `optional_expert_rules` and `optional_linear_terms` as a real value between 0 and 1.  May be used to prevent preference for data rules whose predictive relevance may partly result from modeling noise in the data set.
  - `expert_only`: specifies whether only expert rules and -linear terms should beincluded as candidates in the final model.
  - `ntree`: specifies the number of trees from which data rules are extracted.
  - `ensemble`: specifies whether gradient boosting (*GBM*), random forest (*RF*) or a mixture of both (*both*) shall be employed to generate tree ensembles.
  - `mix`: specifies the fraction of trees to ge generated via gradient boosting whenever `ensemble` is set to"both".
  - `L`: controls the complexity of data-generated rules whereby higher values lead to more complex rules.
  - `S`: controls the minimum number of observations per node in the tree growing process.
  - `minsup`: specifies the minimum value of support such that rules with support less than `minsup` are removed.  Higher values can be used to prevent overfitting.
  - `corelim`: specifies  the  minimum  value  of  correlation  from  which  correlated rules are cleaned.
  - `alpha`: specifies the elastic-net mixing parameter with values between 0 and 1, where 1 represents the lasso penalty and 0 the ridge penalty.
  - `s`: specifies  the  choice  of  the *λ*-value,  where *lambda.min* indicates  the  value that gives the minimum mean cross-validated error and *lambda.1se *gives the most  regularized  model  such  that  error  is  within  one  standard  error  of  the minimum.
  - `n_imp`: specifies the number of base learners (rules + linear terms) to be printed in the model output as the most important ones.
  - `print_output`: controls whether the elements of the function output are additionally printed to the console.
  - `...` specifies additional arguments to be passed to the function `glmnet`.
  
  
To get a first impression of how the function `ExpertRuleFit()` works, a short introductory example is provided regarding the prediction of Diabetes. An extensive description of the fitting procedure can be found in the folders `ERF` and `results`, as well as the respective MSc. thesis.

## Example: Predicting Diabetes

**Data.** The Pima Indian Diabetes (PID) data set is available from the UCI Machine Learning Repository. Recorded information regards a 768 adult women sampled from the Pima Indian population in Arizona. The binary target *diabetes* was diagnosed according to the WHO criteria regarding glucose
concentrations in different medical test settings. Eight numeric attributes were included as significant risk factors for an onset of
diabetes. These are: *Pregnancies, Glucose, BP, SkinThickness, Insulin, BMI, DPF* and *Age*.

```{r}

source("./results/diabetes/erf_diabetes_dataprep.R")
source("./ERF/erf_main.R")

# Diabetes UCI Data 
data <- read.csv(file = './data sets/diabetes.csv', header = T)
data <- prepare_diabetes_data(data)


head(data)

```

**Expert Knowledge** Factual medical expert knowledge may be extracted from textbooks, 
clinical practice guidelines and expert interviews/assessments. Below, we see an exemplary set of rules we acquired from medical guidelines and expert interviews, respectively.

```{r}


source("./expert knowledge/diabetes/EK_diabetes.R")

rules <- c(fdk_rules1, fdk_rules2, hek_rules)

# confirmatory rules
conf_rules <- c("Age<=39 & BP<=80 & BMI<25",
                "Age>=40 & Age<=49 & BP<=80 & BMI<25",
                "Age<=39 & BP>=81 & BMI<25",
                "Age<=39 & BP<=80 & BMI>=25 & BMI<=30",
                "Age>=60 & BP>=81 & BMI>=31 & BMI<=40",
                "Age>=60 & BP<=80 & BMI>40",
                "Age<=39 & BP>=81 & BMI>40",
                "Age>=40 & Age<=49 & BP>=81 & BMI>40",
                "Age>=50 & Age<=59 & BP>=81 & BMI>40",
                "Age>=60 & BP>=81 & BMI>40",
                "Glucose>110 & BP>90",
                "Glucose>110 & BMI>26",
                "BP>90 & BMI>26", 
                "Glucose>110 & BP>90 & BMI>26",
                "Age<=42 & BP<=80 & BMI<=29",
                "Age>=55 & BP<=80 & BMI<=29",
                "Age>=60 & Glucose>=130 & BMI>=35",
                "Age>=60 & BP>=90 & BMI>=37",
                "Age>=45 & BP>=90 & BMI>=35 & Glucose>=130",
                "Age<=60 & BP<=90 & BMI<=30 & Glucose<=100")


# optional rules
opt_rules <- setdiff(rules, conf_rules)

# confirmatory linear terms
conf_linear <- c("Age", "BMI")

# optional linear terms
opt_linear <- c("DPF", "BP", "Glucose")

```

Thus, we specify the ERF model as:

```{r}

sets <- createERFsets(data, 0.7)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

erf_diabetes <- ExpertRuleFit(X =X, y=y, Xtest = Xtest, ytest = ytest, 
                             confirmatory_expert_rules = conf_rules, 
                             optional_expert_rules = opt_rules, 
                             confirmatory_linear_terms = conf_linear,
                             optional_linear_terms = opt_linear,
                             s = "lambda.1se")
```

The first few lines of the printed results provide the penalty parameter value lambda employed for selecting the final ensemble. By default, the *1.se* rule is used for selecting lambda, the number of base classifiers (rules + linear terms) used in the final model as well as the average number of conditions per rule.

Next, the printed results provide the rules and linear terms selected in the final ensemble, with their estimated coefficients. For rules, the `description` column provides the conditions. The `coefficient` column presents the estimated coefficient. These are regression coefficients, reflecting the expected increase in the response for a unit increase in the predictor, keeping all other predictors constant. For rules, the coefficient thus reflects the difference in the expected value of the response when the conditions of the rule are met, compared to when they are not. Accordingly, the most important 10 rules and linear terms are listed in the model output.

The remaining part of the output relates to expert knowledge and thereby distinguishes between confirmatory and optional EK. While the confirmatory part appears safe in the final model, optional EK is either penalised in the same way as the rules from the data set, or is somewhat favoured by the parameter `optional_penalty`. This can be of judgement. Finally, unlike expert rules, the data rules can also learn noise to increase accuracy.

With the dollar access, various additional information can be retrieved from the object `erf_diabetes`.

Finally, using the parameter selection `expert_only = T` it is possible to learn an ERF ensemble solely on the basis of the specified expert knowledge in order to see how well and in which interaction expert knowledge classifies diabetes, even without respective training examples.

```{r, results = TRUE}

erf_diabetes_ek_only <- ExpertRuleFit(X =X, y=y, Xtest = Xtest, ytest = ytest,
                                      confirmatory_expert_rules = conf_rules, 
                                      optional_expert_rules = opt_rules, 
                                      confirmatory_linear_terms = conf_linear,
                                      optional_linear_terms = opt_linear,
                                      expert_only = T, s = "lambda.1se")
```
