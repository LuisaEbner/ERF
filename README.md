# The ExpertRuleFit model (ERF) 

This repository includes the implementation of the ExpertRuleFit Model as proposed in the Master Thesis **Complementing Prediction Rule Ensembles with Expert Knowledge**. The model is based on the RuleFit model by Friedman and Popescu 
(http://statweb.stanford.edu/~jhf/ftp/RuleFit.pdf)

**File Description:**

The following 6 files are auxiliary files used to implement the ExpertRuleFit model:
 * take1.R
 * createX.R
 * create_test.R
 * genrulesrf.R
 * genrulesgbm.R
 * erf_auxiliaries.R

The main file for model implementation is *erf_main.R*

In the Experiments section of the thesis, we are using 3 different data sources:

1. Simulated Data + Simulated Expert Knowledge
2. The UCI Dataset *Diabetes Pima Indians* + Medical Guideline Knowledge
3. The UCI Dataset *Cervical Cancer (Risk Factors)* + Medical Guideline Knowledge


The data simulation is implemented in the file *simulation.R* using auxiliary functions implemented in
*simulation_auxiliaries.R*.

Experiments with the **Simulated data** may be found in:

* erf_simulation.R
* modelcomp_simulation.R
* modelcomp_main.R.

The **Diabetes data** is stored as *diabetes.csv* and was loaded from the UCI Machine Learning Repository.
The dataset was preprocessed to be applicable to the ExpertRuleFit model in the file *erf_diabetes_dataprep.R*.
Experiments with the diabetes data may be found in:

* erf_diabetes.R
* modelcomp_diabetes.R
* modelcomp_main.R


The **Cervical Cancer data** is stored as *risk_factors_cervical_cancer.csv* in this repository and was directly loaded from the UCI Machine Learning Repository.
The dataset was preprocessed to be applicable to the ExpertRuleFit model in the file *erf_cancer_dataprep.R*.
Experiments with the cancer data may be found in:


* erf_cancer.R
* modelcomp_cancer.R
* modelcomp_main.R


The file *modelcomp_main.R* contains a function with which the ERF model without expert knowledge, with optional expert knowledge and with confirmatory expert knowledge are compared according to the number or terms, the average rule length, the AUC and Classification Error and the usage of Expert knowledge in the final model and among its most important features.
Additionally, all ERF model results are compared to the corresponding PRE model, a RuleFit implementation by Maroleijn Fokkema ( https://github.com/marjoleinF/pre).

Finally, the file *anova_expertknowledge.R* analyses the amount of variance explained by each rule or linear term within the expert knowledge inserted to the ExpertRuleFit model.

################################################################################################################################

---
output:
  md_document:
    variant: markdown_github
bibliography: README.bib
csl: inst/bib_style.csl
---

# Expert RuleFit (ERF): Complementing Rule Ensembles with Expert Knowledge

```{r, echo = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "inst/README-figures/README-",
  dpi = 124
)
```

**ExpertRuleFit** is an **R**-based implementation for deriving rule ensembles for binary responses. Input variables may be numeric, ordinal and categorical. An extensive description of the implementation and functionality is provided in the respective MSc. Thesis *Expert RuleFit - Complementing Rule Ensembles with Expert Knowledge*. The function largely implements the RuleFit algorithm proposed by Friedman and Popescu [@Friedman08], with the following adjustments: 

1) The implementation is completely **R** based, allowing users better access to the results and more control over the parameters used for generating the prediction rule ensemble.
2) The unbiased tree induction algorithms of [@Hothorn06] is used for deriving prediction rules, by default. Alternatively, the (g)lmtree algorithm of [@Zeileis08] can be employed, or the classification and regression tree (CART) algorithm of [@Breiman84].
3) The package supports a wider range of response variable types.
5) The initial ensembles may be generated as in bagging, boosting and/or random forests.
6) Hinge functions of predictor variables may be included as baselearners, as in the multivariate adaptive regression splines method of [@Friedman91], using function `gpe()`.

Below, a short introductory example is provided. [@Fokkema20] provides an extensive description of the fitting procedures implemented in function `pre()` and example analyses with more extensive explanations. 


## Example: Predicting Diabetes 

To get a first impression of how the function `ExpertRuleFit()` works, we will fit an ERF model to predict diabetes using the `Pima Indian Diabetes` dataset from the UCI Machine Learning Repository:

```{r, results = FALSE}
library("pre")
airq <- airquality[complete.cases(airquality), ]
set.seed(42)
airq.ens <- pre(Ozone ~ ., data = airq)
```

Note that it is necessary to set the random seed, to allow for later replication of the results, because the fitting procedure depends on random sampling of training observations. 

We can print the resulting ensemble (alternatively, we could use the `print` method): 

```{r}
airq.ens
```

The firest few lines of the printed results provide the penalty parameter value ($\lambda$) employed for selecting the final ensemble. By default, the '1-SE' rule is used for selecting $\lambda$; this default can be overridden by employing the `penalty.par.val` argument of the `print` method and other functions in the package. Note that the printed cross-validated error is calculated using the same data as was used for generating the rules and likely provides an overly optimistic estimate of future prediction error. To obtain a more realistic prediction error estimate, we will use function ```cvpre()``` later on.

Next, the printed results provide the rules and linear terms selected in the final ensemble, with their estimated coefficients. For rules, the `description` column provides the conditions. For linear terms (which were not selected in the current ensemble), the winsorizing points used to reduce the influence of outliers on the estimated coefficient would be printed in the `description` column. The `coefficient` column presents the estimated coefficient. These are regression coefficients, reflecting the expected increase in the response for a unit increase in the predictor, keeping all other predictors constant. For rules, the coefficient thus reflects the difference in the expected value of the response when the conditions of the rule are met, compared to when they are not.

Using the `plot` method, we can plot the rules in the ensemble as simple decision trees. Here, we will request the nine most important baselearners through specification of the `nterms` argument. Through the `cex` argument, we specify the size of the node and path labels:



# References
