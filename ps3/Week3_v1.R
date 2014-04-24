## nhl hockey analysis

## the data is in gamlr.  
## You need to first install this, 
## via install.packages("gamlr")

library(gamlr) # loads Matrix as well
help(hockey) # describes the hockey data and shows an example regression

data(hockey) # load the data

##### Processing Detail: 
## build matrices of team, situation, and player effects
#
# Create home and away team indicator matrices 
# first, make sure the factorization levels match up
all(levels(goal$team.home)==levels(goal$team.away))
teams <- levels(goal$team.home) #our list of teams

# An aside... something I get asked often: 
#   how do I get indicators for all levels of a factor?
#   i.e., can we avoid dropping the reference level?
# Easiest thing to do is create an extra factor level as reference
goal$team.home <- factor(goal$team.home, levels=c(NA,teams), exclude=NULL)
goal$team.away <- factor(goal$team.away, levels=c(NA,teams), exclude=NULL)
# do a similar thing for goal$season
goal$season <- as.factor(goal$season) # first convert from numeric, then:
goal$season <- factor(goal$season, levels=c(NA,levels(goal$season)), exclude=NULL)
## The exclude=NULL argument is necessary so that R doesn't skip the NA.
## Now the factors have reference level NA: R's symbol for missing data.
levels(goal$team.home)
levels(goal$season)

## get a separate effect for each team in each season
# home team indicators
homemat <- sparse.model.matrix(~ team.home*season, data=goal)[,-1]
# away team version
awaymat <- sparse.model.matrix(~ team.away*season, data=goal)[,-1]
# column names
colnames(homemat)
colnames(awaymat)
# combine them: +1 for home, -1 for away
xteam <- suppressWarnings(homemat-awaymat) # warns about colnames not matching (is OK)
# because I'm obsessive about sensical names
colnames(xteam) <- sub("team.home","",colnames(xteam)) # drop `team.home' from varnames
xteam[1,] # goal 1 is in a game of DAL @ EDM

# also, config contains play configuration info
# e.g., S5v4 is 5 on 4 hockey, +1 if it is for home-team and -1 for away team
config[1,]


##### Analysis

# Combine the covariates all together
x <- cBind(config,xteam,onice) # cBind binds together two sparse matrices
# build 'y': home vs away, binary response
y <- goal$whoscored=="HOME"

nhlreg <- gamlr(x, y, 
	free=1:(ncol(config)+ncol(xteam)), 
	family="binomial", standardize=FALSE)
## free denotes unpenalized columns

## coefficients (grab only the players)
# AICc selection 
Baicc <- coef(nhlreg)[colnames(onice),]

## Problem 1
## Look at the coefficients of the model to see what it tells you
coef(nhlreg)
## There is a positive--albeit small--intercept inferred that there is a tendency for goals to be scored than not.
## All uneven play has strong positive influence on the probability to score a goal with 2 person advantages tending to be stronger than 1 person advantages
## There are coefficients associated with each team (both positive and negative) although these tend to be small (between -.1 and +.1).  These show how consistently good (bad) teams are over time and capture team specific factors such as coaching, ability to attract free agents, etc.
## All season coefficients are zero implying that there are not seasons where more goals are scored on average (that can not be catpured by other factors in teh model)
## There are team-season interactions showing the relative strength of teams over time
## There are player specific coefficients ranging from ~-.9 to ~.8, with more than half being 0 and an average of ~0.008.  Implying most players have little influence on whether a goal is score, but some players have a strong influence

## Problem 2
## You don't want to standardarize because all the variables are already on the same scale.  In this case the sd
## is only a measure of the spread of the data (i.e., is there a disproportiate number of 0s or 1s) which would
## cause the sd to shrink.  This is unnecessarily punishing the variables with a more even distributino of 0s and 1s
## in the data set.  Also, the underlying data is binomial so standard deviation does not have the same interpretation.
## There is no need to standardize again because all data is already on the same scale.

## run with standardize
nhlreg_stand <- gamlr(x, y, 
	free=1:(ncol(config)+ncol(xteam)), 
	family="binomial", standardize=TRUE)
Baicc_stand <- coef(nhlreg_stand)[colnames(onice),]

## get the distribution of coeff for non stand
cv.nhlreg <- cv.gamlr(x, y, 
	free=1:(ncol(config)+ncol(xteam)), 
	family="binomial", standardize=FALSE)

## get the distribution of coeff for stand
cv.nhlreg_stand <- cv.gamlr(x, y, 
	free=1:(ncol(config)+ncol(xteam)), 
	family="binomial", standardize=TRUE)

par(mfrow=c(1,2))
plot(cv.nhlreg, ylim = c(1.15,1.2), main = "nonstand")
plot(cv.nhlreg_stand, ylim = c(1.15,1.2), main = "stand")

## Problem 3

coef(cv.nhlreg,arg="lse") 
matplot(x=seq(1,100, by =1),cbind(AIC(nhlreg),AICc(nhlreg),BIC(nhlreg)))
matplot(x=seq(1,100, by =1),cbind(AIC(nhlreg),AICc(nhlreg)))
LL <- log(nhlreg$lambda)
plot(cv.nhlreg, ylim = c(1.15,1.2))
abline(v=LL[which.min(AIC(nhlreg))], col="orange", lty=3)
abline(v=LL[which.min(AICc(nhlreg))], col="green", lty=3)
abline(v=LL[which.min(BIC(nhlreg))], col="red", lty=3)

## Problem 4
nhlreg_nofree <- gamlr(x, y,
	free=1, 
	family="binomial", standardize=FALSE, lambda.min.ratio = 0.0001)
cv.nhlreg_nofree <- cv.gamlr(x, y, 
	free=1,
	family="binomial", standardize=FALSE, lambda.min.ratio = 0.0001)

x2 <- cBind(onice)
nhlreg_onlyps <- gamlr(x2, y,
	family="binomial", standardize=FALSE, lambda.min.ratio = 0.00000001)
cv.nhlreg_onlyps <- cv.gamlr(x2, y, 
	family="binomial", standardize=FALSE, lambda.min.ratio = 0.00000001)
Baicc_onlyps <- coef(nhlreg_onlyps)[colnames(onice),]

par(mfrow=c(1,3))
plot(cv.nhlreg, ylim = c(1.15,1.4), xlim = c(-11,-2), main = "nonstand")
abline(v=LL[which.min(AICc(nhlreg))], col="green", lty=3)

plot(cv.nhlreg_nofree, ylim = c(1.15,1.4), xlim = c(-11,-2), main = "nofree")
LL_nofree = log(nhlreg_nofree$lambda)
abline(v=LL[which.min(AICc(nhlreg_nofree))], col="green", lty=3)

plot(cv.nhlreg_onlyps, ylim = c(1.15,1.4), xlim = c(-11,-2), main = "onlyps")
LL_onlyps = log(nhlreg_nofree$lambda)
abline(v=LL[which.min(AICc(nhlreg_onlyps))], col="green", lty=3)

par(mfrow=c(1,2))
matplot(x=seq(1,100, by =1),cbind(AIC(nhlreg),AICc(nhlreg),BIC(nhlreg)))
matplot(x=seq(1,100, by =1),cbind(AIC(nhlreg_nofree),AICc(nhlreg_nofree),BIC(nhlreg_nofree)))

Baicc_nofree <- coef(nhlreg_nofree)[colnames(onice),]

## It does make a difference
## Freeing the non-player variables is better because then we get coefficients for teams and team:season interactions, which we would expect
## not freeing these variables we obtain no team:season interactions
## From an CV perspective the best performing non-freed model performs worse than the best freed model
## Still not sure how to plot the ICs very well
