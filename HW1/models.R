train=read.csv("moneyball-training-data.csv")
#Drop index, TEAM_BASERUN_CS and TEAM_BATTING_HBP
train.trans=train[,-c(1,10,11)]
train.trans=train.trans[complete.cases(train.trans),]
nrow(train.trans)


#Plot Correlation Matrix
correlation=(as.data.frame(cor(train.trans)))
correlation$Variable1=row.names(correlation)

#Convert Correlation Matrix to long form 
Chethna Mohan 
library(reshape2)
corr_long=melt(correlation,id.vars="Variable1",value.name="Corr")
library(ggplot2)
#Plot Correlation matrix
ggplot(corr_long,aes(Variable1,variable)) + 
  geom_tile(aes(fill=Corr)) + 
  scale_fill_gradient(low = "red", high = "green",limits=c(-1,1)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title="Correlation Matrix",x="Variables",y="Variables") 

#Create a Model with All Varaibles 

all_var_model=lm(formula = TARGET_WINS ~ ., data = train.trans)
summary(all_var_modl)

#Step down
step_down_model=step(all_var)

summary(step_down_model)

#Residual Plot - Residual against fitted values
plot(step_down_model$fitted.values,step_down_model$residuals,
     main="Residual Vs Fitted Value",
     xlab="Fitted Value",
     ylab="Residual")

#Standard Residual Plot
standard_residuals=(step_down_model$residuals-mean(step_down_model$residuals))/sd(step_down_model$residuals)
plot(step_down_model$fitted.values,standard_residuals,
     main="Residual Vs Fitted Value",
     xlab="Fitted Value",
     ylab="Standard Residual")