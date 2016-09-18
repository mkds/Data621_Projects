evaldata <- read.csv('moneyball-evaluation-data.csv')
trainingdata <- read.csv('moneyball-training-data.csv')

head(evaldata)
head(trainingdata)

# drop the TEAM_BATTING_HBP variable
xformevaldata <- subset(evaldata, select = -TEAM_BATTING_HBP)
xformtrainingdata <- subset(trainingdata, select = -TEAM_BATTING_HBP)

# lm test
model.omit <- lm(TARGET_WINS ~ TEAM_BASERUN_SB, data=xformtrainingdata, na.action = na.omit)
model.exclude <- lm(TARGET_WINS ~ TEAM_BASERUN_SB , data=xformtrainingdata, na.action = na.exclude)
summary(model.omit)
plot(xformtrainingdata$TEAM_BASERUN_SB, xformtrainingdata$TARGET_WINS)
abline(model.omit)
summary(model.exclude)
plot(xformtrainingdata$TEAM_BASERUN_SB, xformtrainingdata$TARGET_WINS)
abline(model.exclude)