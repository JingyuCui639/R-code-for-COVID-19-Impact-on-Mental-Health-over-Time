# R-code-for-COVID-19-Impact-on-Mental-Health-over-Time

Software and packages
All the computation in this study is conducted in R (version 3.6.1). 
The Multivariate Imputation by Chained Equation (MICE) package is used in R to do the multiple imputation. 
The procedure firstly imputed m=5 sets of complete dataset for each of 12 weeks by calling mice() function. 
Then, function complete() is called to extract each of 5 complete datasets for each week. 
Next, the Lasso model is implemented on all the complete datasets to select the variables for the reduced model and the full model. 
This is done by using Lasso and Elastic-Net Regularized Generalized Linear Models (glmnet) package to call function glmnet() with argument alpha=1. 
Once the explanatory variables are decided, the logistic regression is conducted to fit the selected reduced model and the full model. 
This is done by using function glm() with arguments family='binomial'.
