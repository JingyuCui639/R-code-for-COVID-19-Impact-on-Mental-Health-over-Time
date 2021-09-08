# The final model is selected based on the results from the reduced model and the full model. 
# Then, read in all the pre-processed data sets for 12 weeks at one time, 
# Then fit the final logistic model on each complete data for each week. 
# The estimates and corresponding standard error for each week are calculated based on the fitted results from 5 imputed data sets in each week.
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

for (i in 1:12) {
  name_out_1[i] = paste0('truemodel_week', i ,'.csv')
}
name_out_1[1]


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
  #fit_2 = glm(Y~.,data.factor,family = 'binomial')
  out_1 = summary(fit_1)$coefficients[,c(1,2,4)]
  #out_2 = summary(fit_2)$coefficients[,c(1,2,4)]
  #list(out_1,out_2)
  return(out_1)
}

# combined logistic
logistic.out.2 = function(week,s_ind){
  data = read.in(week) # 5 data for week 
  o1 = logistic.single(data[[1]],s_ind) # coef and sd for data1(2 models)
  o2 = logistic.single(data[[2]],s_ind) # coef and sd for data2
  o3 = logistic.single(data[[3]],s_ind)
  o4 = logistic.single(data[[4]],s_ind)
  o5 = logistic.single(data[[5]],s_ind)
  o = o5 # store the final output
  est_1 = cbind(o1[,1],o2[,1],o3[,1],o4[,1],o5[,1])
  #est_2 = cbind(o1[[2]][,1],o2[[2]][,1],o3[[2]][,1],o4[[2]][,1],o5[[2]][,1])
  est_sd_1 = cbind(est_1,o1[,2],o2[,2],o3[,2],o4[,2],o5[,2])
  #est_sd_2 = cbind(est_2,o1[[2]][,2],o2[[2]][,2],o3[[2]][,2],o4[[2]][,2],o5[[2]][,2])
  o[,1] = apply(est_1, 1, mean)
  o[,2] = apply(est_sd_1, 1, function(x){
    s = x[6:10]
    b = x[1:5]
    b_bar = mean(b)
    M = 5
    sd = sqrt(sum(s^2)/M+(1+1/M)/(M-1)*sum((b-b_bar)^2))
    return(sd)
  })
  o[,3] = apply(o,1,function(x){
    est = x[1]
    sd = x[2]
    z = est/sd
    p_value = 2*(1-pnorm(abs(z)))
    return(p_value)
  })
  # o[[2]][,1] = apply(est_2, 1, mean)
  # o[[2]][,2] = apply(est_sd_2, 1, function(x){
  #   s = x[6:10]
  #   b = x[1:5]
  #   b_bar = mean(b)
  #   M = 5
  #   sd = sqrt(sum(s^2)/M+(1+1/M)/(M-1)*sum((b-b_bar)^2))
  #   return(sd)
  # })
  # o[[2]][,3] = apply(o[[2]],1,function(x){
  #   est = x[1]
  #   sd = x[2]
  #   z = est/sd
  #   p_value = 2*(1-pnorm(abs(z)))
  #   return(p_value)
  # })
  return(o)
}


#####Final logistic model fiting#######################
ind.true<-c(1,2,3,4,5,7,8,9,11,12,19,20,21,22,23,24,26)

for (i in 1:12) {
  out = logistic.out.2(i,ind.true)
  write.csv(out,name_out_1[i])
}

out1 = logistic.out.2(1,ind.true)
write.csv(out1,name_out_1[1])

out2 = logistic.out.2(2,ind.true)
write.csv(out2,name_out_1[2])

out3 = logistic.out.2(3,ind.true)
write.csv(out3,name_out_1[3])

out4 = logistic.out.2(4,ind.true)
write.csv(out4,name_out_1[4])

out5 = logistic.out.2(5,ind.true)
write.csv(out5,name_out_1[5])

out6 = logistic.out.2(6,ind.true)
write.csv(out6,name_out_1[6])

out7 = logistic.out.2(7,ind.true)
write.csv(out7,name_out_1[7])

out8 = logistic.out.2(8,ind.true)
write.csv(out8,name_out_1[8])

out9 = logistic.out.2(9,ind.true)
write.csv(out9,name_out_1[9])

out10 = logistic.out.2(10,ind.true)
write.csv(out10,name_out_1[10])

out11 = logistic.out.2(11,ind.true)
write.csv(out11,name_out_1[11])

out12 = logistic.out.2(12,ind.true)
write.csv(out12,name_out_1[12])
