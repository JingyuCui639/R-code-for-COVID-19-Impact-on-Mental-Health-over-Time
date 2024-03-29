library(glmnet)

# The function below converts the categorical variables into factors.
# The input data should be the pre-processed data in the form of .csv file.

factorization<-function(data){
  #factorization
  data$MS<-factor(data$MS)
  data$INCOME<-factor(data$INCOME)
  data$WRKLOSS.c<-factor(data$WRKLOSS.c)
  # ANYWORK->anywork
  data$anywork<-factor(data$anywork)
  data$KINDWORK<-factor(data$KINDWORK)
  data$EMPPAY<-factor(data$EMPPAY)
  data$foodcon.change<-factor(data$foodcon.change )
  data$freefood<-factor(data$freefood)
  data$FOODCONF<-factor(data$FOODCONF)
  data$HLTHSTATUS<-factor(data$HLTHSTATUS)
  data$HEALINS<-factor(data$HEALINS)
  data$MED.DELAY.NOTGET<-factor(data$MED.DELAY.NOTGET)
  data$MORT.PROB<-factor(data$MORT.PROB)
  data$SchoolEnroll<-factor(data$SchoolEnroll)
  data$Y<-factor(data$Y)
  data$EEDUC<-factor(data$EEDUC)
  data$EST_ST<-factor(data$EST_ST)
  # Additional factors
  data$MALE<-factor(data$MALE)
  data$RHISPANIC<-factor(data$RHISPANIC)
  data$RRACE<-factor(data$RRACE)
  return(as.data.frame(data))
}


#Read in each complete imputed data (.csv file).
week1.input = read.csv('completedata_week1_1.csv')

#Converting categorical variables into factors
week1.factor = factorization(week1.input)

#Extract design matrix in the regression model;
#-1 remove intercept
x=model.matrix(Y~.,week1.factor)[,-1] 

#Extract response vector Y
y=week1.factor$Y

#########
# Lasso #
#########

# Grid search for best tuning parameter lambda
grid=10^seq(10,-2,length=100) 

lasso.mod=glmnet(x,y,family = "binomial",alpha=1 ,lambda=grid) #alpha=1 indicates Lasso method

plot(lasso.mod)

set.seed(1)

cv.out=cv.glmnet(x,y,alpha=1,family = "binomial")

plot(cv.out) 

# IN the study, we choose the lambda such that the model is the most parsimonious 
# and within one standard error of the minimum cross-validation misclassification rate. 
(bestlam=cv.out$lambda.1se) 

out=glmnet(x,y,alpha=1,lambda=grid, family = "binomial")

lasso.coef=predict(out,type="coefficients",s=bestlam,family = "binomial")

lasso.coef # the full coefficient list wrt to best lambda

# Next, the full model and the reduced model are selected based on the Lasso results for each complete imputed data for each week.


####################
# Ridge Regression #
####################
#The ridge method is not used in this project, but also write here for reference.
#It basically has the same code with Lasso, but only setting "alpha=0", which indicates ridge method.

ridge.mod=glmnet(x,y,alpha=0,lambda=grid,family = 'binomial')

dim(coef(ridge.mod))

set.seed(1)

cv.out=cv.glmnet(x,y,alpha=0,family = 'binomial')

plot(cv.out)

# Here is an example that setting best lambda corresponde to smallest mse, which is different with "lambda.1se" shown in the Lasso method. 
# The difference between "lambda.min" and "lambda.1se" is:
# *"lambda.min" will select the model (variables) such that the prediction error is the smallest. Thus, the number of variables is larger and it could be overfitting.
# *"lambda.1se" will select less variables and the model MSE is within 1 standar error of the smallest MSE among all the possible models. This way can avoid overfiting.
# In practice, you need to choose the proper one based on what you need.
(bestlam=cv.out$lambda.min) 

out=glmnet(x,y,alpha=0,family = 'binomial')

ridge.coef = predict(out,type="coefficients",s=bestlam,family = 'binomial')

ridge.coef # coefficients for best lambda in ridge regression

