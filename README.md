# R-code-for-COVID-19-Impact-on-Mental-Health-over-Time

Descriptions software and packages

All the computation in this study is conducted in R (version 3.6.1). 
The Multivariate Imputation by Chained Equation (MICE) package is used in R to do the multiple imputation.

The proceures are listed as follows:

1. Pre-processing the original data, including mitigating the measurement error effects by combining questions to create new variables, and collapsing levels of variables to form binary variables. Please refer to the code in "Step1-Data pre-processing".


2. Data imputation. 
Assuming that missing data follows the missing at random (MAR) mechanism and employing the multiple imputation by chained equations (MICE) method to impute data. 
Calling mice() function to impute m=5 complete data sets for each of 12 weeks.Then, call complete() function to extract each of 5 complete datasets for each week.
Please refer to the code in "Step2-Multiple Imputation".

3. Using Lasso method to do variable selection. 
The Lasso model is applied on all the complete datasets among 12 weeks to select the variables to obtain the reduced model and the full model. 
This is done by using R package: Lasso and Elastic-Net Regularized Generalized Linear Models (glmnet), to call function glmnet() with argument alpha=1 (Lasso). 
Please refer to the code in "Step3-Lasso-Variable Selection".

4. Implementing logistic regression on each imputed data set.
Once the explanatory variables are decided for the reduced model and the full model, the logistic regression is conducted to fit the selected reduced model and the full model. 
This is done by calling function glm() with arguments family='binomial'.

5. Select the factors in the final model based on the results in the reduced model and the full model. Then, uing logistic method fit on the final model.
