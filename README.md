# ERF


ExpertRuleFit

This repository includes the implementation of the ExpertRuleFit Model as proposed in the Master Thesis "Complementing Prediction Rule Ensembles with Expert Knowledge".

**File Description:**

\begin{itemize}
\item take1.R
\item createX.R
\item create_test.R
\item genrulesrf.R
\item genrulesgbm.R
\item erf_auxiliaries.R
\end{itemize}

are auxiliary files, used to implement the ExpertRuleFit model. The main file for model implementation is:\\

\begin{itemize}
\item erf_main.R
\end{itemize}

In the Experiments section of the thesis, we are using 3 different data sources:

\begin{enumerate}
\item Simulated Data + Simulated Expert Knowledge
\item UCI Dataset *Diabetes Pima Indians* + Medical Guideline Knowledge
\item UCI Dataset *Cervical Cancer (Risk Factors)* + Medical Guideline Knowledge
\end{enumerate}

The data simulation is found in the file 

\begin{itemize}
\item simulation.R
\item simulation_auxiliaries.R
\end{itemize}

Experiments with the simulated data may be found in:

\begin{itemize}
\item erf_simulation.R
\item modelcomp_simulation.R
\item modelcomp_main.R
\end{itemize}

The Diabetes data is stored as 'diabetes.csv' and was loaded from the UCI Machine Learning Repository.
The dataset was preprocessed to be applicable to the ExpertRuleFit model in the file 'erf_diabetes_dataprep'.
Experiments with the diabetes data may be found in:

\begin{itemize}
\item erf_diabetes.R
\item modelcomp_diabetes.R
\item modelcomp_main.R
\end{itemize}

The Cervical Cancer data is stored as 'risk_factors_cervical_cancer.csv' and was loaded from the UCI Machine Learning Repository.
The dataset was preprocessed to be applicable to the ExpertRuleFit model in the file 'erf_cancer_dataprep'.
Experiments with the cancer data may be found in:

\begin{itemize}
\item erf_cancer.R
\item modelcomp_cancer.R
\item modelcomp_main.R
\end{itemize}

The file 'modelcomp_main' contains a function with which the ERF model without expert knowledge, with optional expert knowledge and with confirmatory expert knowledge are compared according to the number or terms, the average rule length, the AUC and Classification Error and the usage of Expert knowledge in the final model and among its most important features.
Additionally, all ERF model results are compared to the corresponding PRE model, a RuleFit implementation of Maroleijn Fokkema (see: https://github.com/marjoleinF/pre).

