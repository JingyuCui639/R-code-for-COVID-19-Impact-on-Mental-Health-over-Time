#### Step 1 ####
library(dplyr)
#read the data for 12 weeks
week1.data<-read.csv("pulse2020_puf_01.csv")
week2.data<-read.csv("pulse2020_puf_02.csv")
week3.data<-read.csv("pulse2020_puf_03.csv")
week4.data<-read.csv("pulse2020_puf_04.csv")
week5.data<-read.csv("pulse2020_puf_05.csv")
week6.data<-read.csv("pulse2020_puf_06.csv")
week7.data<-read.csv("pulse2020_puf_07.csv")
week8.data<-read.csv("pulse2020_puf_08.csv")
week9.data<-read.csv("pulse2020_puf_09.csv")
week10.data<-read.csv("pulse2020_puf_10.csv")
week11.data<-read.csv("pulse2020_puf_11.csv")
week12.data<-read.csv("pulse2020_puf_12.csv")

####def.1 data pre-processing: 
####mitigating the measurement error effects by combining questions to create new variables, 
####or collapsing levels of variables to form binary variables.

variable_selection<-function(data){
  
  #First, delete the imputed observation
  data<- data[data$ABIRTH_YEAR==2 & data$AGENDER==2 & 
                data$AHISPANIC==2 & data$ARACE==2 & data$AEDUC==2,]
  #Secondly, dealing with variables
  attach(data)
  Age<-2020-TBIRTH_YEAR #change the birth year to age
  
  MALE<-ifelse(EGENDER==1,1,0) #
  
  RHISPANIC<-ifelse(RHISPANIC==2,1,0)
   
  #combine WRKLOSS &EXPCTLOSS, 1: WRKLOSS happens; 
  #0: WRKLOSS doen not heppen; NA: missing
  WRKLOSS.c<-rep(0,dim(data)[1]) 
  WRKLOSS.c[WRKLOSS==1 | EXPCTLOSS==1]<-1
  WRKLOSS.c[WRKLOSS==-99 & EXPCTLOSS==-99]<-NA
  
  anywork<-rep(0,dim(data)[1])
  anywork[ANYWORK==1]<-1
  anywork[ANYWORK==-88|ANYWORK==-99]<-NA
  
  # Receiving payment or not for the time of not working?
  #1: yes, receive payment; 0: no, no payment received
  EMPPAY<-rep(0,dim(data)[1])
  EMPPAY[UNEMPPAY==1|UNEMPPAY==2|UNEMPPAY==3]<-1
  EMPPAY[UNEMPPAY==-88|UNEMPPAY==-99]<-NA
  
  #Food condition change: "foodcon.change"
  #1: food condition becomes better or the same
  #0: food condition becomes worse
  foodcon.change<-CURFOODSUF-PRIFOODSUF
  foodcon.change<-ifelse(foodcon.change>=0, 1, 0)
  foodcon.change[CURFOODSUF==-88 | PRIFOODSUF==-88|CURFOODSUF==-99 | PRIFOODSUF==-99]<-NA
  
  freefood<-ifelse(FREEFOOD==1,1,0)
  freefood[FREEFOOD==-88| FREEFOOD==-99]<-NA
  
  #deal with health insurance. 
  #1: health insurance covered; 
  #0: no health insurence; -88/-99: missing values
  HEALINS<-rep(0,dim(data)[1])
  HEALINS[HLTHINS1==1 | HLTHINS2==1 |HLTHINS3==1| HLTHINS4==1| HLTHINS5==1 
          |HLTHINS6==1 | HLTHINS7==1 | HLTHINS8==1]<-1
  HEALINS[HLTHINS1==-88 & HLTHINS2==-88 & HLTHINS3==-88 & HLTHINS4==-88& HLTHINS5==-88 
          & HLTHINS6==-88 & HLTHINS7==-88 & HLTHINS8==-88]<--88
  HEALINS[HLTHINS1==-99 & HLTHINS2==-99 & HLTHINS3==-99 & HLTHINS4==-99 & HLTHINS5==-99
          & HLTHINS6==-99 & HLTHINS7==-99 & HLTHINS8==-99]<--99
  
  #combine DELAY and NOTGET. 
  #1: dalay and notget happens; 
  #0: dalay or notget does not happens;
  #-88/-99: missing
  MED.DELAY.NOTGET<-rep(0,dim(data)[1])
  MED.DELAY.NOTGET[DELAY==1 | NOTGET==1]<-1
  MED.DELAY.NOTGET[DELAY==-88 & NOTGET==-88]<--88
  MED.DELAY.NOTGET[DELAY==-99 & NOTGET==-99]<--99
  
  #combine MORTLMTH and MORTCONF. 
  #1: mortgage has problem; 
  #0: mortgage has no problem;
  # -88/-99: missing
  MORT.PROB<-rep(0,dim(data)[1])
  MORT.PROB[MORTLMTH==2 | MORTCONF==1 | MORTCONF==2 | MORTCONF==5]<-1
  MORT.PROB[MORTLMTH==-88 & MORTCONF==-88]<--88
  MORT.PROB[MORTLMTH==-99 & MORTCONF==-99]<--99
  
  #Combine the variable of school enrollment of grade 12
  SchoolEnroll<-rep(NA,dim(data)[1])
  SchoolEnroll[ENROLL1==1 | ENROLL2==1]<-1
  SchoolEnroll[ENROLL3==1]<-0
  SchoolEnroll[ENROLL1==-88 & ENROLL2==-88 & ENROLL3==-88]<--88
  SchoolEnroll[ENROLL1==-99 & ENROLL2==-99 & ENROLL3==-99]<--99
  
  detach(data)
  data[data==-88|data==-99]<-NA
  
  Y.four<-cbind(data$ANXIOUS,data$WORRY,data$INTEREST,data$DOWN)
  Y<-apply(Y.four, 1, mean)
  Y<-ifelse(Y>2.5,1,0)
  # If response Y>=2.5, let Y=1
  #if Y<2.5, Y=0
  #Otherwise, NA
  
  attach(data)
  #Constructng pre-processed data
  X.Y<-cbind(EST_ST, Age, MALE, RHISPANIC, 
             RRACE,
             EEDUC,MS,THHLD_NUMPER,THHLD_NUMKID,INCOME,
             WRKLOSS.c, anywork, 
             KINDWORK,
             EMPPAY,
             foodcon.change, freefood,
             TSPNDFOOD,TSPNDPRPD, FOODCONF, HLTHSTATUS,
             HEALINS,MED.DELAY.NOTGET, MORT.PROB
             ,SchoolEnroll
             ,TTCH_HRS
             ,Y)
  detach(data)
  X.Y[X.Y==-88|X.Y==-99]<-NA
  return(X.Y)
}

#####################################################################################
#Factorization:
#making the categorical variables in the pre-processed data as factors

factorization<-function(data){
  
  data$EST_ST<-factor(data$EST_ST)
  #classify the States into four levels: mild; moderate daily increase; large daily increase; serious
  levels(data$EST_ST)<-
    list("mild"=c("1","2","5","8","9","10","11","15","16","18","19","20","21","22","23","24",
                 "25","26","27","28","29","30","31","32","33","35","37","38","39","40",
                 "41","42","44","45","46","47","49","50","51","53","54","55","56"),
         "moderate_daily_increase"=c("4","13","17","34"),
         "large_daily_increase"=c("6","12","48"),
         "serious"="36")
  data$MALE<-factor(data$MALE)
  data$RHISPANIC<-factor(data$RHISPANIC)
  data$RRACE<-factor(data$RRACE)
  # create a new factor variable: EEDUC2, with "1" = less than high school,
  # "2" = high school, "3" = bachelor's degree, "4" = graduate degree
  data$EEDUC<-factor(data$EEDUC)
  levels(data$EEDUC)<-list("1"="1","2"=c("2","3","4"),"3"=c("5","6"),"4"="7")
  data$MS<-factor(data$MS)
  data$INCOME<-factor(data$INCOME)
  data$WRKLOSS.c<-factor(data$WRKLOSS.c)
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
  
  return(as.data.frame(data))
}


#Pre-processing the 12 weeks' data

week1.data.new<-factorization(as.data.frame(variable_selection(week1.data)))
week2.data.new<-factorization(as.data.frame(week2.data%>%variable_selection()))
week3.data.new<-factorization(as.data.frame(week3.data%>%variable_selection()))
week4.data.new<-factorization(as.data.frame(week4.data%>%variable_selection()))
week5.data.new<-factorization(as.data.frame(week5.data%>%variable_selection()))
week6.data.new<-factorization(as.data.frame(week6.data%>%variable_selection()))
week7.data.new<-factorization(as.data.frame(week7.data%>%variable_selection()))
week8.data.new<-factorization(as.data.frame(week8.data%>%variable_selection()))
week9.data.new<-factorization(as.data.frame(week9.data%>%variable_selection()))
week10.data.new<-factorization(as.data.frame(week10.data%>%variable_selection()))
week11.data.new<-factorization(as.data.frame(week11.data%>%variable_selection()))
week12.data.new<-factorization(as.data.frame(week12.data%>%variable_selection()))

#After pre-processing, the data are clean with proper variable types.
