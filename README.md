# R-code-for-COVID-19-Impact-on-Mental-Health-over-Time

Descriptions of software and important packages used in the study.

All the computation in this study is conducted in R (version 3.6.1). 
The R package: Multivariate Imputation by Chained Equation (MICE) is used in R to do the multiple imputation.
The R package: Lasso and Elastic-Net Regularized Generalized Linear Models (glmnet) is used to fit Lasso model.
The function glm() is used to fit the logistic regression model on the data.

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

4. Apply logistic regression on each imputed data set for each week to get the combined estimates and s.e. of the reduced model and the full model for each week. 
This is done by calling function glm() with arguments family='binomial'. Please refer to the code in "Step4-Logistic Regression for the Reduced and the Full Models".

5. Select the factors in the final model based on the results in the reduced model and the full model. 
Then, fit the logictic model with variables selected in the final model. 
Next, calculate the estimates and s.e. for each week based on the results from 5 imputed data sets. 
Lastly, the fitted combined results for 12 weeks are exported in the form of .csv files.
Please refer to the code in "Step5-Logistic Regression for the Final Model".
