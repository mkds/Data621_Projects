#Read training data
train=read.csv("moneyball-training-data.csv")
#test=read.csv("moneyball-evaluation-data.csv")

col_summary=function(col){
  std_summary=summary(col,na.rm=T)
  col_summary=data.frame(mean=std_summary[4], median=std_summary[3], sd=sd(col,na.rm=T),
                          min=std_summary[1],max=std_summary[6],FirstQ=std_summary[2],
                          ThridQ=std_summary[5],Missing=sum(is.na(col)),row.names = NULL)
  
}

#Explore data.. Check summary statistics & missing values
summary_cols=t(sapply(train[,-1],col_summary))
print(summary_cols)

cat("\nComplete Cases:",sum(complete.cases(train)))

#Visualize 
par(mfrow=c(1,2))
#Histogram and scatter plot of Outcome variable
hist(train[,2],main="Number of Wins",xlab="Wins")
plot(train[,2],main="Number of Wins",xlab="Wins")
par(mfrow=c(2,3))

#Histograms of Predictor variables
hist(train[,3],main="Base Hits by batters",xlab="Base Hits")
hist(train[,4],main="Doubles by batters",xlab="Doubles")
hist(train[,5],main="Triples by batters",xlab="Triples")
hist(train[,6],main="Homeruns by batters",xlab="Homeruns")
hist(train[,7],main="Walks by batters",xlab="Walks (batter)")
hist(train[,8],main="Batters hit by pitch",xlab="Hit by pitch")

hist(train[,9],main="Strikeouts by batters",xlab="Strikeout")
hist(train[,10],main="Stolen bases",xlab="Stolen bases")
hist(train[,11],main="Caught stealing",xlab="Caught Stealing")


par(mfrow=c(2,3))
hist(train[,12],main="Field Errors",xlab="Errors")
hist(train[,13],main="Double Plays",xlab="Double Plays")
hist(train[,14],main="Walks allowed",xlab="Walks allowed")
hist(train[,15],main="Hits allowed ",xlab="Hits")
hist(train[,16],main="Homeruns allowed",xlab="Homeruns allowed")
hist(train[,17],main="Strikeouts by pitchers",xlab="Strikeouts")

cat("Complete cases after dropping 'Walks by batters'",
    "and 'Batters hit by pitch':",sum(complete.cases(train[,-c(10,11)])))





