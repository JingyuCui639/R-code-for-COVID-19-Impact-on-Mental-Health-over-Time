# This code will read in all the pre-processed data for 12 weeks at one time, 
# Then fit logistic reduced model and the full model on each complete data for each week.
# Then, the estimates and corresponding standard error for each week are calculated based on the fitted results from 5 imputed data sets in each week.
# Finally, the estimates and corresponding s.e. for each week are exported as .csv files.

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


# read in name (the same)
name_in = matrix(rep(0,12*5),ncol = 5)
for (i in 1:12) {
  for (j in 1:5) {
    name_in[i,j] = paste0('stateComb_completedata_week', i ,'_',j,'.csv')
  }
}
name_in
name_in[1,]

# read out name
name_out_1 = rep(0,12)
name_out_2 = rep(0,12)
for (i in 1:12) {
  name_out_1[i] = paste0('smodel_week', i ,'.csv')
  name_out_2[i] = paste0('lmodel_week', i ,'.csv')
}
name_out_1[1]
name_out_2[3]

# read data
read.in = function(week){
  data1 = read.csv(name_in[week,1])
  data2 = read.csv(name_in[week,2])
  data3 = read.csv(name_in[week,3])
  data4 = read.csv(name_in[week,4])
  data5 = read.csv(name_in[week,5])
  list(data1,data2,data3,data4,data5)
}

# single logistic
logistic.single = function(data,ind1){
  data.factor = factorization(data)
  data.factor = data.factor[,-1]# remove index
  fit_1 = glm(Y~.,data.factor[,ind1],family = 'binomial')
  fit_2 = glm(Y~.,data.factor,family = 'binomial')
  out_1 = summary(fit_1)$coefficients[,c(1,2,4)]
  out_2 = summary(fit_2)$coefficients[,c(1,2,4)]
  list(out_1,out_2)
}

# combined logistic
# s_ind: the index for the variables selected in the model
logistic.out.2 = function(week,s_ind){
  data = read.in(week) # 5 data for week 
  o1 = logistic.single(data[[1]],s_ind) # coef and sd for data1(2 models)
  o2 = logistic.single(data[[2]],s_ind) # coef and sd for data2
  o3 = logistic.single(data[[3]],s_ind)
  o4 = logistic.single(data[[4]],s_ind)
  o5 = logistic.single(data[[5]],s_ind)
  o = o5 # store the final output
  est_1 = cbind(o1[[1]][,1],o2[[1]][,1],o3[[1]][,1],o4[[1]][,1],o5[[1]][,1])
  est_2 = cbind(o1[[2]][,1],o2[[2]][,1],o3[[2]][,1],o4[[2]][,1],o5[[2]][,1])
  est_sd_1 = cbind(est_1,o1[[1]][,2],o2[[1]][,2],o3[[1]][,2],o4[[1]][,2],o5[[1]][,2])
  est_sd_2 = cbind(est_2,o1[[2]][,2],o2[[2]][,2],o3[[2]][,2],o4[[2]][,2],o5[[2]][,2])
  o[[1]][,1] = apply(est_1, 1, mean)
  o[[1]][,2] = apply(est_sd_1, 1, function(x){
    s = x[6:10]
    b = x[1:5]
    b_bar = mean(b)
    M = 5
    sd = sqrt(sum(s^2)/M+(1+1/M)/(M-1)*sum((b-b_bar)^2))
    return(sd)
  })
  o[[1]][,3] = apply(o[[1]],1,function(x){
    est = x[1]
    sd = x[2]
    z = est/sd
    p_value = 2*(1-pnorm(abs(z)))
    return(p_value)
  })
  o[[2]][,1] = apply(est_2, 1, mean)
  o[[2]][,2] = apply(est_sd_2, 1, function(x){
    s = x[6:10]
    b = x[1:5]
    b_bar = mean(b)
    M = 5
    sd = sqrt(sum(s^2)/M+(1+1/M)/(M-1)*sum((b-b_bar)^2))
    return(sd)
  })
  o[[2]][,3] = apply(o[[2]],1,function(x){
    est = x[1]
    sd = x[2]
    z = est/sd
    p_value = 2*(1-pnorm(abs(z)))
    return(p_value)
  })
  return(o)
}


# Since the full model includes all the variables, we don't need to input variable index for the full model.
# The variable index for the reduced model
s_ind = c(2,3,7,9,11,12,19,20,21,22,23,26)

# test for week 1 
week1.input = read.csv('stateComb_completedata_week1_1.csv')
week1.factor = factorization(week1.input)
week1.factor = week1.factor[,-1]# remove index
fit = glm(Y~.,week1.factor[,s_ind],family = 'binomial')
summary(fit)

out = logistic.out.2(1,s_ind)
out[[1]]
out[[2]]

# loop (write in different files)
out = rep(0,2)
for (i in 1:12) {
  out = logistic.out.2(i,s_ind)
  write.csv(out[[1]],name_out_1[i]) # reduced model
  write.csv(out[[2]],name_out_2[i]) # full model
}

# loop (write in the same files)
out = logistic.out.2(1,s_ind)
out1 = out[[1]]
out2 = out[[2]]
for (j in 1:11) {
  out = logistic.out.2(j+1,s_ind)
  out1 = cbind(out1,out[[1]]) # reduced model
  out2 = cbind(out2,out[[2]]) # full model
}
write.csv(out1,'smodel.csv') # reduced model
write.csv(out2,'lmodel.csv') # full model
