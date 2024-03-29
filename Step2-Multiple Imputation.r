#To implement multiple imputation by chained equations (MICE) method, we use R package "mice".
library(mice)

# Default methods for different types of variable:
#  To do the imputation, different methods are used for different variable types:
#  1) numeric data, 
#  2) factor data with 2 levels, 
#  3) factor data with > 2 unordered levels, and 
#  4) factor data with > 2 ordered levels. 

#By default, 
# for 1) numeric data, predictive mean matching (pmm) method should be used
# for 2) factor data with 2 levels (binary data), logistic regression imputation (logreg) should be used
# for 3) factor data with > 2 unordered levels, polytomous regression (polyreg) imputation method should be used
# for 4) factor data with > 2 ordered levels, proportional odds (polr) model should be used

col.methods<-c("polyreg",	"polr",	"logreg",	"logreg",	"polyreg",	
               "logreg",	"logreg", "logreg",	"pmm",	
               "pmm",	"polr",	"polr",	"logreg",	
               "logreg",	"logreg","logreg",	"pmm",
               "logreg")

missing.var.names<-c("MS", "INCOME", "WRKLOSS.c", "anywork", "KINDWORK",
                     "EMPPAY", "foodcon.change", "freefood", "TSPNDFOOD",
                     "TSPNDPRPD", "FOODCONF", "HLTHSTATUS", "HEALINS",
                     "MED.DELAY.NOTGET", "MORT.PROB", "SchoolEnroll",  "TTCH_HRS",
                     "Y")    


#Imputing 12 weeks datasets by "mice()" function. Here, each input data are the pre-processed data in step 1. 
#i.e., "week1.data.new" is the pre-processed data for week 1.

imputed.week1<-mice(week1.data.new,method = col.methods,blocks = missing.var.names,m=5,maxit = 10, seed = 51162)

imputed.week2<-mice(week2.data.new,method = col.methods,blocks = missing.var.names,m=5,maxit = 10, seed = 51162)

imputed.week3<-mice(week3.data.new,method = col.methods,blocks = missing.var.names,m=5, maxit = 10, seed = 51162)

imputed.week4<-mice(week4.data.new,method = col.methods,blocks = missing.var.names,m=5, maxit = 10, seed = 51162)

imputed.week5<-mice(week5.data.new,method = col.methods,blocks = missing.var.names,m=5, maxit = 10, seed = 51162)

imputed.week6<-mice(week6.data.new,method = col.methods,blocks = missing.var.names,m=5,maxit = 10, seed = 51162)

imputed.week7<-mice(week7.data.new,method = col.methods,blocks = missing.var.names,m=5, maxit = 10, seed = 51162)

imputed.week8<-mice(week8.data.new,method = col.methods,blocks = missing.var.names,m=5, maxit = 10, seed = 51162)

imputed.week9<-mice(week9.data.new,method = col.methods,blocks = missing.var.names,m=5, maxit = 10, seed = 51162)

imputed.week10<-mice(week10.data.new,method = col.methods,blocks = missing.var.names,m=5, maxit = 10, seed = 51162)

imputed.week11<-mice(week11.data.new,method = col.methods,blocks = missing.var.names,m=5, maxit = 10, seed = 51162)

imputed.week12<-mice(week12.data.new,method = col.methods,blocks = missing.var.names,m=5, maxit = 10, seed = 51162)

summary(imputed.week1)


 

#Extract the five imputed datasets and save each imputed datasets as a .csv file

##########
# Week 1 #
##########

completedData.week1.1 <- complete(imputed.week1,1) 
write.csv(completedData.week1.1, "stateComb_completedata_week1_1.csv")

completedData.week1.2 <- complete(imputed.week1,2)
write.csv(completedData.week1.2, "stateComb_completedata_week1_2.csv")

completedData.week1.3 <- complete(imputed.week1,3)
write.csv(completedData.week1.3, "stateComb_completedata_week1_3.csv")

completedData.week1.4 <- complete(imputed.week1,4)
write.csv(completedData.week1.4, "stateComb_completedata_week1_4.csv")

completedData.week1.5 <- complete(imputed.week1,5)
write.csv(completedData.week1.5, "stateComb_completedata_week1_5.csv")

##########
# Week 2 #
##########

completedData.week2.1 <- complete(imputed.week2,1) 
write.csv(completedData.week2.1, "stateComb_completedata_week2_1.csv")

completedData.week2.2 <- complete(imputed.week2,2)
write.csv(completedData.week2.2, "stateComb_completedata_week2_2.csv")


completedData.week2.3 <- complete(imputed.week2,3)
write.csv(completedData.week2.3, "stateComb_completedata_week2_3.csv")

completedData.week2.4 <- complete(imputed.week2,4)
write.csv(completedData.week2.4, "stateComb_completedata_week2_4.csv")

completedData.week2.5 <- complete(imputed.week2,5)
write.csv(completedData.week2.5, "stateComb_completedata_week2_5.csv")

##########
# Week 3 #
##########

completedData.week3.1 <- complete(imputed.week3,1) 
write.csv(completedData.week3.1, "stateComb_completedata_week3_1.csv")

completedData.week3.2 <- complete(imputed.week3,2)
write.csv(completedData.week3.2, "stateComb_completedata_week3_2.csv")

completedData.week3.3 <- complete(imputed.week3,3)
write.csv(completedData.week3.3, "stateComb_completedata_week3_3.csv")

completedData.week3.4 <- complete(imputed.week3,4)
write.csv(completedData.week3.4, "stateComb_completedata_week3_4.csv")

completedData.week3.5 <- complete(imputed.week3,5)
write.csv(completedData.week3.5, "stateComb_completedata_week3_5.csv")

##########
# Week 4 #
##########

completedData.week4.1 <- complete(imputed.week4,1)
write.csv(completedData.week4.1, "stateComb_completedata_week4_1.csv")

completedData.week4.2 <- complete(imputed.week4,2)
write.csv(completedData.week4.2, "stateComb_completedata_week4_2.csv")

completedData.week4.3 <- complete(imputed.week4,3)
write.csv(completedData.week4.3, "stateComb_completedata_week4_3.csv")

completedData.week4.4 <- complete(imputed.week4,4)
write.csv(completedData.week4.4, "stateComb_completedata_week4_4.csv")

completedData.week4.5 <- complete(imputed.week4,5)
write.csv(completedData.week4.5, "stateComb_completedata_week4_5.csv")

##########
# Week 5 #
##########

completedData.week5.1 <- complete(imputed.week5,1)
write.csv(completedData.week5.1, "stateComb_completedata_week5_1.csv")

completedData.week5.2 <- complete(imputed.week5,2)
write.csv(completedData.week5.2, "stateComb_completedata_week5_2.csv")

completedData.week5.3 <- complete(imputed.week5,3)
write.csv(completedData.week5.3, "stateComb_completedata_week5_3.csv")

completedData.week5.4 <- complete(imputed.week5,4)
write.csv(completedData.week5.4, "stateComb_completedata_week5_4.csv")

completedData.week5.5 <- complete(imputed.week5,5)
write.csv(completedData.week5.5, "stateComb_completedata_week5_5.csv")

##########
# Week 6 #
##########

completedData.week6.1 <- complete(imputed.week6,1) 
write.csv(completedData.week6.1, "stateComb_completedata_week6_1.csv")

completedData.week6.2 <- complete(imputed.week6,2)
write.csv(completedData.week6.2, "stateComb_completedata_week6_2.csv")

completedData.week6.3 <- complete(imputed.week6,3)
write.csv(completedData.week6.3, "stateComb_completedata_week6_3.csv")

completedData.week6.4 <- complete(imputed.week6,4)
write.csv(completedData.week6.4, "stateComb_completedata_week6_4.csv")

completedData.week6.5 <- complete(imputed.week6,5)
write.csv(completedData.week6.5, "stateComb_completedata_week6_5.csv")

##########
# Week 7 #
##########

completedData.week7.1 <- complete(imputed.week7,1)
write.csv(completedData.week7.1, "stateComb_completedata_week7_1.csv")

completedData.week7.2 <- complete(imputed.week7,2)
write.csv(completedData.week7.2, "stateComb_completedata_week7_2.csv")

completedData.week7.3 <- complete(imputed.week7,3)
write.csv(completedData.week7.3, "stateComb_completedata_week7_3.csv")

completedData.week7.4 <- complete(imputed.week7,4)
write.csv(completedData.week7.4, "stateComb_completedata_week7_4.csv")

completedData.week7.5 <- complete(imputed.week7,5)
write.csv(completedData.week7.5, "stateComb_completedata_week7_5.csv")

##########
# Week 8 #
##########

completedData.week8.1 <- complete(imputed.week8,1) 
write.csv(completedData.week8.1, "stateComb_completedata_week8_1.csv")

completedData.week8.2 <- complete(imputed.week8,2)
write.csv(completedData.week8.2, "stateComb_completedata_week8_2.csv")

completedData.week8.3 <- complete(imputed.week8,3)
write.csv(completedData.week8.3, "stateComb_completedata_week8_3.csv")

completedData.week8.4 <- complete(imputed.week8,4)
write.csv(completedData.week8.4, "stateComb_completedata_week8_4.csv")

completedData.week8.5 <- complete(imputed.week8,5)
write.csv(completedData.week8.5, "stateComb_completedata_week8_5.csv")

##########
# Week 9 #
##########

completedData.week9.1 <- complete(imputed.week9,1) 
write.csv(completedData.week9.1, "stateComb_completedata_week9_1.csv")

completedData.week9.2 <- complete(imputed.week9,2)
write.csv(completedData.week9.2, "stateComb_completedata_week9_2.csv")

completedData.week9.3 <- complete(imputed.week9,3)
write.csv(completedData.week9.3, "stateComb_completedata_week9_3.csv")

completedData.week9.4 <- complete(imputed.week9,4)
write.csv(completedData.week9.4, "stateComb_completedata_week9_4.csv")

completedData.week9.5 <- complete(imputed.week9,5)
write.csv(completedData.week9.5, "stateComb_completedata_week9_5.csv")

##########
# Week 10 #
##########

completedData.week10.1 <- complete(imputed.week10,1) 
write.csv(completedData.week10.1, "stateComb_completedata_week10_1.csv")

completedData.week10.2 <- complete(imputed.week10,2)
write.csv(completedData.week10.2, "stateComb_completedata_week10_2.csv")

completedData.week10.3 <- complete(imputed.week10,3)
write.csv(completedData.week10.3, "stateComb_completedata_week10_3.csv")

completedData.week10.4 <- complete(imputed.week10,4)
write.csv(completedData.week10.4, "stateComb_completedata_week10_4.csv")

completedData.week10.5 <- complete(imputed.week10,5)
write.csv(completedData.week10.5, "stateComb_completedata_week10_5.csv")

##########
# Week 11 #
##########

completedData.week11.1 <- complete(imputed.week11,1)
write.csv(completedData.week11.1, "stateComb_completedata_week11_1.csv")

completedData.week11.2 <- complete(imputed.week11,2)
write.csv(completedData.week11.2, "stateComb_completedata_week11_2.csv")

completedData.week11.3 <- complete(imputed.week11,3)
write.csv(completedData.week11.3, "stateComb_completedata_week11_3.csv")

completedData.week11.4 <- complete(imputed.week11,4)
write.csv(completedData.week11.4, "stateComb_completedata_week11_4.csv")

completedData.week11.5 <- complete(imputed.week11,5)
write.csv(completedData.week11.5, "stateComb_completedata_week11_5.csv")

##########
# Week 12 #
##########

completedData.week12.1 <- complete(imputed.week12,1) 
write.csv(completedData.week12.1, "stateComb_completedata_week12_1.csv")

completedData.week12.2 <- complete(imputed.week12,2)
write.csv(completedData.week12.2, "stateComb_completedata_week12_2.csv")

completedData.week12.3 <- complete(imputed.week12,3)
write.csv(completedData.week12.3, "stateComb_completedata_week12_3.csv")

completedData.week12.4 <- complete(imputed.week12,4)
write.csv(completedData.week12.4, "stateComb_completedata_week12_4.csv")

completedData.week12.5 <- complete(imputed.week12,5)
write.csv(completedData.week12.5, "stateComb_completedata_week12_5.csv")
