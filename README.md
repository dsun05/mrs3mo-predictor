# MRS-3Mo Predictor

## Table of Contents

* [I. Predictive Modeling Pipeline: A Two-Stage Hybrid Approach](#predictive-modeling-pipeline)
  * [A. Stage 1: Non-parametric Feature Screening via Random Forest](#stage-1)
  * [B. Stage 2: Regularized Regression for Parsimonious Model Selection via LASSO](#stage-2)
* [II. Model and Feature Evaluation](#model-and-feature-evaluation)
  * [A. Comprehensive Model Performance Assessment](#model-performance)
  * [B. Univariable Hypothesis Testing for Exploratory Analysis](#univariable-testing)
  * [C. Final Model Coefficient Interpretation](#coefficient-interpretation)
* [III. Results Interpretation](#results-interpretation)
  * [Final Predictive Model](#final-model)
  * [Key Variable Associations](#key-variable-associations)
  * [Overall Conclusion](#overall-conclusion)

# <a id="predictive-modeling-pipeline"></a>I. Predictive Modeling Pipeline: A Two-Stage Hybrid Approach

The analytical objective is to develop a robust and parsimonious predictive model for 3-month patient outcomes (`mrs3mo`), dichotomized into "Good" and "Poor". Given the high dimensionality and potential for complex, non-linear interactions typical of clinical datasets, a two-stage hybrid approach was implemented. This methodology leverages the strengths of a non-parametric ensemble method for initial variable reduction, followed by a penalized regression technique for final model selection and interpretation.

## <a id="stage-1"></a>A. Stage 1: Non-parametric Feature Screening via Random Forest

The first stage employs a **Random Forest** algorithm to screen for the most promising predictors from the full set of available variables. A Random Forest is an ensemble learning method that constructs a collection of decision trees during training and outputs the mode of the classes (classification) of the individual trees.

* **Methodological Rationale**: This method was chosen for several reasons that make it particularly suitable for clinical data.
    1.  **Non-Parametric Nature**: It makes no assumptions about the underlying distribution of the data, unlike traditional regression models that assume linearity or normality. This is critical in medicine, where variables are often skewed or follow complex distributions.
    2.  **Implicit Handling of Interactions**: The tree-based structure naturally captures complex, high-order interactions between variables without needing them to be specified *a priori*. For instance, the effect of blood pressure on outcome might depend on the patient's age, an interaction the model can detect automatically.
    3.  **Robustness to Missing Data**: The implementation uses a function that imputes missing values using the median for continuous variables and the mode for categorical variables, allowing for the inclusion of all patients in the initial screening phase.

* **Feature Importance Metric**: The primary output from this stage is a ranked list of variables based on their importance. Importance is quantified by the **Mean Decrease in Gini Impurity**. The Gini impurity is a measure of how often a randomly chosen element from a set would be incorrectly labeled if it was randomly labeled according to the distribution of labels in the subset. For a given node with $C$ classes and $p(i)$ being the fraction of items labeled with class $i$, the Gini impurity is calculated as $I_G(p) = \sum_{i=1}^{C} p(i)(1-p(i))$. A variable's importance is measured by calculating the total reduction in Gini impurity it provides, averaged across all trees in the forest. The top 50 variables with the highest importance scores were advanced to the next stage of modeling, providing a data-driven, robust method for dimensionality reduction.

## <a id="stage-2"></a>B. Stage 2: Regularized Regression for Parsimonious Model Selection via LASSO

The 50 variables identified by the Random Forest were subsequently used to train a **logistic regression model with LASSO (Least Absolute Shrinkage and Selection Operator) regularization**. This technique is designed to address the challenges of multicollinearity and overfitting while simultaneously performing variable selection.

* **Methodological Rationale**:
    1.  **Parsimony and Interpretability**: The primary advantage of LASSO is its ability to create a parsimonious model. It achieves this by applying an $L_1$ penalty to the logistic regression objective function, which is the sum of the absolute values of the coefficients ($\lambda \sum |\beta|$). This penalty forces the coefficients of less contributory variables to become exactly zero, effectively removing them from the model. The result is a simpler, more interpretable model that is easier to deploy in a clinical setting.
    2.  **Regularization to Prevent Overfitting**: The penalty term constrains the model complexity. The optimal penalty strength, $\lambda$, is determined via 10-fold cross-validation. The specific value chosen, `lambda.1se`, selects the most parsimonious model whose error is within one standard error of the minimum cross-validated error. This is a standard, conservative approach that prioritizes model generalizability to new patient data.

The final model, therefore, contains only the most impactful predictors, reducing noise and improving the stability of the coefficient estimates.

---

# <a id="model-and-feature-evaluation"></a>II. Model and Feature Evaluation

A comprehensive evaluation was conducted to assess the performance of the final model and to explore the unadjusted relationships between individual variables and the clinical outcome.

## <a id="model-performance"></a>A. Comprehensive Model Performance Assessment

Evaluating a clinical prediction model requires assessing both its ability to distinguish between outcomes (discrimination) and the reliability of its probability estimates (calibration).

* **Discrimination**: The model's discriminative power was quantified using the **Area Under the Receiver Operating Characteristic Curve (AUC)**. The ROC curve plots the model's sensitivity (True Positive Rate) against 1-specificity (False Positive Rate) across all possible probability thresholds. The AUC represents the probability that the model will assign a higher risk score to a randomly selected patient with a "Poor" outcome than to a randomly selected patient with a "Good" outcome. An AUC approaching 1.0 indicates excellent discrimination.

* **Calibration**: Calibration assesses the agreement between the predicted probabilities and the observed outcome frequencies. The script generates a calibration plot by grouping subjects into deciles of predicted risk and plotting the mean predicted probability against the observed proportion of "Poor" outcomes in each decile. A well-calibrated model will have points lying along the 45-degree diagonal. This is a crucial diagnostic step, as a model with good discrimination could still be poorly calibrated, leading to systematic over- or under-estimation of risk for patient subgroups.

## <a id="univariable-testing"></a>B. Univariable Hypothesis Testing for Exploratory Analysis

Prior to multivariable modeling, a series of univariable statistical tests were conducted to investigate the unadjusted association between specific variables and the dichotomized outcome. This is a standard procedure in medical research to gain initial insights into the data.

* **Welch's Two-Sample t-test**: This test was used to compare the means of key continuous variables between the "Good" and "Poor" outcome groups. This specific test was chosen over a standard Student's t-test because it does not assume homogeneity of variances between the two groups, a condition often violated in clinical data. The test determines if a statistically significant difference exists in the average value of a variable between the two outcome cohorts.

* **Pearson's Chi-squared Test**: This test was used to assess the association between categorical variables and the binary outcome. It compares the observed cell counts in a contingency table to the counts that would be expected if the two variables were independent. This allows for an initial screen of which categorical factors may be related to the outcome of interest.

## <a id="coefficient-interpretation"></a>C. Final Model Coefficient Interpretation

To translate the final LASSO model's output into clinically meaningful metrics, the non-zero coefficients were used to derive **Odds Ratios (ORs) and their corresponding 95% Confidence Intervals (CIs)**. A standard logistic regression model was fit using only the predictors selected by LASSO, and the resulting coefficients ($\beta$) were exponentiated ($e^\beta$) to produce the ORs.

* **Clinical Utility**: An Odds Ratio quantifies the multiplicative change in the odds of having a "Poor" outcome for a one-unit increase in a predictor variable, holding all other variables constant. The 95% CI provides a measure of estimate precision; if the interval does not contain 1.0, the association is considered statistically significant within the final multivariable model. This rigorous process ensures that the model's findings are not only statistically sound but also directly interpretable for clinicians.

---

# <a id="results-interpretation"></a>III. Results Interpretation 

The analysis successfully built a high-performing model to predict patient outcomes and identified several key clinical factors associated with those outcomes.

## <a id="final-model"></a>### Final Predictive Model

The final LASSO regression model demonstrated **excellent predictive accuracy**, with an Area Under the Curve (AUC) of **0.9159**. An AUC this high indicates the model is very effective at distinguishing between patients who will have a "Good" versus a "Poor" outcome. The model identified four key predictors:

* **Discharge Functional Status (`DischargeMRS`)**: This was the most powerful predictor. For each one-point increase on the modified Rankin Scale at discharge, the odds of a poor 3-month outcome increased by **169%** (Odds Ratio = 2.69).
* **Baseline Functional Status (`MRSbase_manual`)**: A patient's pre-stroke condition was also significant. Each one-point increase in their baseline mRS increased the odds of a poor outcome by **64%** (Odds Ratio = 1.64).
* **Post-Procedure Stroke Severity (`PostThrombectomyNIHSS`)**: Higher NIHSS scores after the procedure were associated with worse outcomes, increasing the odds by **10%** per point (Odds Ratio = 1.10).
* **Red Blood Cell Count (`RBC`)**: This was a protective factor. Higher RBC counts were associated with a **55% reduction** in the odds of a poor outcome (Odds Ratio = 0.45).

---

## <a id="key-variable-associations"></a>### Key Variable Associations

The univariable tests, which look at each variable individually, support the model's findings and provide additional context:

* **Clinical Scores**: All tested clinical scores—including admission GCS, baseline mRS, and discharge mRS—showed a highly significant difference between the "Good" and "Poor" outcome groups (all p-values < 0.001).
* **Categorical Factors**:
    * Patient **`sex`** was significantly associated with the outcome (p < 0.001), while a history of hypertension (`htn`) was not (p = 1.0).
    * Other factors found to be strongly associated with the outcome included **`motor`** function, **`walkstatus`**, having a **`DNRDNI`** order, and the **`TimetoDNRDNI`**.

---

## <a id="overall-conclusion"></a>### Overall Conclusion

The patient's functional status, both before the event and immediately after treatment, is the most critical determinant of their 3-month outcome. Neurological severity post-procedure and a key hematological factor (RBC count) also play significant roles. The model is robust and provides a clear, interpretable set of variables for predicting patient recovery.