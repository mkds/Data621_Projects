#Tead training dataset
training=read.csv("insurance_training_data.csv")


#Data cleanup - Convert dollar with thousand separator to numeric
training$INCOME=as.numeric(gsub("\\$|,","",training$INCOME))
training$OLDCLAIM=as.numeric(gsub("\\$|,","",training$OLDCLAIM))
training$BLUEBOOK=as.numeric(gsub("\\$|,","",training$BLUEBOOK))
training$HOME_VAL=as.numeric(gsub("\\$|,","",training$HOME_VAL))
training$TARGET_FLAG=as.factor(training$TARGET_FLAG)
training$KIDSDRIV=as.factor(training$KIDSDRIV)

table(training$TARGET_FLAG)


#Drop index as we don't nned that for model
train_index=training$INDEX
training$INDEX=NULL

#Convert the Target flag to factor as it is a catageorical variable 


numeric_features=sapply(training,is.numeric)

col_summary=function(col){
  std_summary=summary(col,na.rm=T)
  col_summary=data.frame(mean=std_summary[4], median=std_summary[3], sd=sd(col,na.rm=T),
                         min=std_summary[1],max=std_summary[6],FirstQ=std_summary[2],
                         ThridQ=std_summary[5],Missing=sum(is.na(col)),row.names = NULL)
  
}

#Explore data.. Check summary statistics & missing values
summary_numeric_cols=t(sapply(training[,numeric_features],col_summary))
print(summary_numeric_cols)

#Check if any values missing for categorical variables
sapply(training[,!numeric_features],function(x) sum(is.na(x)))

#Check how many cars have negative age
sum(training$CAR_AGE<0,na.rm=T) 

training$CAR_AGE[training$CAR_AGE==-3]=0

#Histograms to check distribution



par(mfrow=c(2,4))

for (i in 1:8) hist(training[,numeric_features][,i],
                     main=colnames(training[,numeric_features])[i],
                     xlab="")


par(mfrow=c(2,3))

for (i in 9:13) hist(training[,numeric_features][,i],
                    main=colnames(training[,numeric_features])[i],
                    xlab="")


#Check Crash Ratio for numeric features

par(mfrow=c(2,3))

for (i in 2:7) {
  #Get bins and frequencey 
  h=hist(training[,numeric_features][,i], plot=F)

  #Get Crash Frequency
  h_crash=hist(training[crash,numeric_features][,i],breaks = h$breaks,
         plot=F)
  #COmpute Crash Ratio
  h_crash$counts=h_crash$counts/h$counts
  h_crash$counts[is.na(h_crash$counts)]=0
  #Plot Crash Ratio by bins
  plot(h_crash,main="Crash Ratio",
       xlab=colnames(training[crash,numeric_features])[i])
 
   
}


par(mfrow=c(2,3))

for (i in 8:13) {
  #Get bins and frequencey 
  h=hist(training[,numeric_features][,i], plot=F)
  
  #Get Crash Frequency
  h_crash=hist(training[crash,numeric_features][,i],breaks = h$breaks,
               plot=F)
  #COmpute Crash Ratio
  h_crash$counts=h_crash$counts/h$counts
  h_crash$counts[is.na(h_crash$counts)]=0
  #Plot Crash Ratio by bins
  plot(h_crash,main="Crash Ratio",
       xlab=colnames(training[crash,numeric_features])[i])
  
  
}

par(mfrow=c(3,4))
for (i in 2:12) plot(training[,!numeric_features][,i],
                     training$TARGET_FLAG,ylab="Crash",
                     xlab=colnames(training[,!numeric_features])[i])


#Transfromation 
training$TARGET_AMT=log1p(training$TARGET_AMT)
training$BLUEBOOK=log1p(training$BLUEBOOK)
training$HOME_VAL=log1p(training$HOME_VAL)
training$INCOME=log1p(training$INCOME)

par(mfrow=c(3,4))
for (i in 2:12) plot(training[crash,!numeric_features][,i],
                     training$TARGET_AMT[crash],ylab="Amount",
                     xlab=colnames(training[,!numeric_features])[i])




par(mfrow=c(3,4))

for (i in 2:13) {

  #Plot Crash Amount
  plot(training[crash,numeric_features][,i],
       training$TARGET_AMT[crash],main="Crash Amount",
       xlab=colnames(training[crash,numeric_features])[i], ylab="Crash Amount")
  
  
}



#Fix NA

training$YOJ[is.na(training$YOJ)]=median(training$YOJ,na.rm=T)
training$INCOME[is.na(training$INCOME)]=median(training$INCOME,na.rm=T)
training$CAR_AGE[is.na(training$CAR_AGE)]=median(training$CAR_AGE,na.rm=T)
training$HOME_VAL[is.na(training$HOME_VAL)]=median(training$HOME_VAL,na.rm=T)
training$AGE[is.na(training$AGE)]=median(training$AGE,na.rm=T)

crash_all_model=glm(TARGET_FLAG~.,data=training[,-2],family="binomial")
crash_model1=step(crash_all_model)

training$AGE_ABOVE50=training$AGE>50
training$TRV_ABOVE50=training$TRAVTIME>50

crash_all_model2=glm(TARGET_FLAG~.+I(AGE*AGE_ABOVE50)
                     +I(TRAVTIME*TRV_ABOVE50),data=training[,-2],family="binomial")
crash_model2=step(crash_all_model2)

training_crashed=training[training$TARGET_FLAG==1,]
amt_all_model1=lm(TARGET_AMT~.,training_crashed[,2:25])
step_model1=step(amt_all_model1)

amt_model1=lm(formula = TARGET_AMT ~ MSTATUS + SEX + BLUEBOOK + OLDCLAIM + 
                CLM_FREQ + REVOKED + MVR_PTS, data = training_crashed[, 2:25])

amt_all_model2=lm(TARGET_AMT~.+I(AGE*AGE_ABOVE50)
                  +I(TRAVTIME*TRV_ABOVE50),training_crashed[,2:27])

step_model2=step(amt_all_model2)
amt_model2=lm(formula = TARGET_AMT ~ MSTATUS + SEX + BLUEBOOK + OLDCLAIM + 
                CLM_FREQ + REVOKED + MVR_PTS + AGE_ABOVE50 + I(AGE * AGE_ABOVE50), 
              data = training_crashed[, 2:27])