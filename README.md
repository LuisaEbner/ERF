# ERF


ExpertRuleFit

This repository includes the implementation of the ExpertRuleFit Model as proposed in the Master Thesis "Complementing Prediction Rule Ensembles with Expert Knowledge".

**File Description:**

Markup : * take1.R
         * createX.R
         * create_test.R
         * genrulesrf.R
         * genrulesgbm.R
         * erf_auxiliaries.R


are auxiliary files, used to implement the ExpertRuleFit model. The main file for model implementation is:\\


Markup : * erf_main.R

In the Experiments section of the thesis, we are using 3 different data sources:

Markup : 1. Simulated Data + Simulated Expert Knowledge
         2. UCI Dataset *Diabetes Pima Indians* + Medical Guideline Knowledge
         3. UCI Dataset *Cervical Cancer (Risk Factors)* + Medical Guideline Knowledge


The data simulation is found in the file 


Markup : *  simulation.R
         * simulation_auxiliaries.R


Experiments with the simulated data may be found in:

Markup : * erf_simulation.R
         * modelcomp_simulation.R
         * modelcomp_main.R

The Diabetes data is stored as 'diabetes.csv' and was loaded from the UCI Machine Learning Repository.
The dataset was preprocessed to be applicable to the ExpertRuleFit model in the file 'erf_diabetes_dataprep'.
Experiments with the diabetes data may be found in:


Markup : * erf_diabetes.R
         * modelcomp_diabetes.R
         * modelcomp_main.R


The Cervical Cancer data is stored as 'risk_factors_cervical_cancer.csv' and was loaded from the UCI Machine Learning Repository.
The dataset was preprocessed to be applicable to the ExpertRuleFit model in the file 'erf_cancer_dataprep'.
Experiments with the cancer data may be found in:


Markup : * erf_cancer.R
         * modelcomp_cancer.R
         * modelcomp_main.R


The file 'modelcomp_main' contains a function with which the ERF model without expert knowledge, with optional expert knowledge and with confirmatory expert knowledge are compared according to the number or terms, the average rule length, the AUC and Classification Error and the usage of Expert knowledge in the final model and among its most important features.
Additionally, all ERF model results are compared to the corresponding PRE model, a RuleFit implementation of Maroleijn Fokkema (see: https://github.com/marjoleinF/pre).

