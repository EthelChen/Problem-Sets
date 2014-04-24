####### donohue and levitt 2001/2008: abortion and crime

## example reading non csv data: this is a dump from STATA
## skip says skip the first line of the file, sep="/t" says 'tab separated'
data <- read.table("abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
	"a_murd","a_viol","a_prop",'prison','police',
	'ur','inc','pov','afdc','gun','beer')

## prison: log of lagged prisoners per capita
## police: the log of lagged police per capita
## ur: the unemployment rate
## inc: per-capita income
## pov: the poerty rate
## AFDC: generosity at year t-15
## gun: dummy for concealed weapons law
## beer: beer consumption per capita 

data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$state <- factor(data$state) # alphabetical order
data$pop <- log(data$pop)
t <- data$year-85
t2 <- t^2

controls <- data.frame(data[,c(1,3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
y <- data$y_murd
d <- data$a_murd

## The abortion 'a_' variables are weighted average of abortion rates where
## weights are determined by the fraction of the type of crime committed by
## various age groups. For example, if 60% of violent crime were committed by 18
## year olds and 40% were committed by 19 year olds in state i, the abortion rate
## for violent crime at time t in state i would be constructed as .6 times the
## abortion rate in state i at time t − 18 plus .4 times the abortion rate in
## state i at time t − 19. See Donohue and Levitt (2001) for further detail.

## we'll just look at murder
## note for convenience here I've made y,d,t, global: they are not in controls.
summary(orig <- glm(y ~ d+t+., data=controls) )$coef['d',]
## this is the levitt analysis: higher abortion leads to lower crime

## Now the same analysis, but for cellphones rather than abortion
cell <- read.csv("us_cellphone.csv")
cellrate <- (cell[,2]-cell[1,2])/(cell[13,2]-cell[1,2])
## what if we're just fitting a quadratic trend?
## there are many things that increased with similar shapes over time
## (cellphone usage, yoga revenues, home prices, ...)
plot(1985:1997, tapply(d, t, mean), xlab="year", ylab="adjusted rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")
phone <- cellrate[t+1]
## clearly, cellphones fight crime.
summary(tech <- glm(y ~ phone+t+., data=controls))$coef['phone',]

## what is happening here is that murder has been increasing quadratically,
## and we have no other controls that do so.  To be correct, you need
## to allow all of your variables to interact with each other and with 
## quadratic trends that could be caused by other confounding variables (e.g. technology)

## But this doesn't work!!
summary(interact <- glm(y ~ d + t + t2*.^2, data=controls))$coef['d',] 
## Quadratic trend interaction leads to more variables than observations
## if we were stopped by this, we'd never be able to discover anything!
## so we need a way to include quadratic trend, but only with important variables.

## try using a lasso 
library(gamlr)
## refactor state to have NA reference level
controls$state <- factor(controls$state, 
	levels=c(NA,levels(controls$state)), exclude=FALSE)
x = model.matrix(~ t + t2*.^2, data=controls)[,-1]
dim(x)

## naive lasso regression
naive <- gamlr(cBind(d,x),y)
coef(naive)["d",] # effect is AICc selected <0

## now, what if we explicitly include dhat confounding:
treat <- gamlr(x,d,lambda.min.ratio=0.001)
# we needed to drop lambda.min.ratio because AICc wants a complex model
# that indicates that abortion rates are highly correlated with controls.
plot(treat)

# Now, grab the predicted treatment
# type="response" is redundant here (gaussian), 
# but you'd want it if d was binary
dhat <- predict(treat, x, type="response") 
## not much signal in d not predicted by dhat
plot(dhat,d,bty="n",pch=21,bg=8) 
## that means we have little to resemble an experiment here...

## IS R^2?
cor(drop(dhat),d)^2
## Note: IS R2 is what governs how much independent signal
## you have for estimating 

# re-run lasso, with this (2nd column) included unpenalized
causal <- gamlr(cBind(d,dhat,x),y,free=2)
coef(causal)["d",] # AICc says abortion has no causal effect.

## BOOTSTRAP 
n <- nrow(x)

## Bootstrapping our lasso causal estimator is easy
gamb <- c() # empty gamma
for(b in 1:20){
	## create a matrix of resampled indices
	ib <- sample(1:n, n, replace=TRUE)
	## create the resampled data
	xb <- x[ib,]
	db <- d[ib]
	yb <- y[ib]
	## run the treatment regression
	treatb <- gamlr(xb,db,lambda.min.ratio=0.001)
	dhatb <- predict(treatb, xb, type="response")

	fitb <- gamlr(cBind(db,dhatb,xb),yb,free=2)
	gamb <- c(gamb,coef(fitb)["db",])
	print(b)
}
## not very exciting though: all zeros
summary(gamb) 
## it's saying there's near zero chance AICc selects gamma!=0


#######################
## EXTRA
##
## FREQUENTIST OPTION: the Belloni, Chernozukov, Hansen version of this
yonx <- gamlr(x,y,lambda.min.ratio=0.001)
## get the union of controls that predict y or d
inthemodel <- unique(c(which(coef(yonx)[-1]!=0), # -1 drops intercept
						which(coef(treat)[-1]!=0))) # unique grabs union
selectdata <- cBind(d,x[,inthemodel]) 
selectdata <- as.data.frame(as.matrix(selectdata)) # make it a data.frame
dim(selectdata) ## p about half n

## run a glm
causal_glm <- glm(y~., data=selectdata)
## The BCH theory says that the standard SE calc for gamma is correct
summary(causal_glm)$coef["d",] # BCH agrees with AICc: not significant.

## Bootsrapping: BCH algorithm makes things more complex
## note that due to instability of MLEs you might 
## get some really big or really small gammas in your sample.
bootgamma <- c() # empty vector
for(b in 1:100){ 
	ib <- sample(1:n, n, replace=TRUE)
	xb <- x[ib,]
	db <- d[ib]
	yb <- y[ib]
	## run the BCH procedure
	treatb <- gamlr(xb,db,lambda.min.ratio=0.001)
	yonxb <- gamlr(xb,yb,lambda.min.ratio=0.001)
	itmb <- unique(c(which(coef(yonxb)[-1]!=0),which(coef(treatb)[-1]!=0))) 
	sdb <- cBind(db,xb[,itmb])
	sdb <- as.data.frame(as.matrix(sdb))

	causeb <- glm(yb~., data=sdb)
	bootgamma <- c(bootgamma,coef(causeb)["db"])
 	cat(b,": ", bootgamma[b], "\n")
}

## proportion of sampling distrib for a negative effect
mean(bootgamma<0)

# plot it (lots going on here; use help and feel free to ask me about it)
hist(bootgamma, col="grey25", border="grey90", main="",xlab="gamma")
# mark the mean
abline(v=mean(bootgamma))

## you could also do it in parallel!
## this is double black diamond stuff; 
## It interacts with your OS, and so can be very finicky
## (and machine specific).  The below should work, but 
## if it doesn't let me know and we can try to work out why.  
library(parallel) # a bunch of tools for parallel computing
# create a `cluster' of processors
cl = makeCluster(detectCores())
## create a list (data.frame) of resampled indices
B <- 100
resamp <- as.data.frame(
	matrix(sample(1:n, B*n, replace=TRUE),ncol=B))
## write a function to replicate BCH procedure on resampled data
## takes a column of resamp as input
bootfit <- function(ib){ 
	## since the processors need to load libraries, say what you use
	require(gamlr)

	## create the resampled data
	xb <- x[ib,]
	db <- d[ib]
	yb <- y[ib]

	## run the BCH procedure
	treatb <- gamlr(xb,db)
	yonxb <- gamlr(xb,yb)
	itmb <- unique(c(which(coef(yonxb)[-1]!=0),which(coef(treatb)[-1]!=0))) 
	sdb <- cBind(db,xb[,itmb])
	sdb <- as.data.frame(as.matrix(sdb))

	causeb <- glm(yb~., data=sdb)
	if(causeb$converged) ## check for glm convergence
		effect <- coef(causeb)["db"]
	else effect <- NA
	return(effect)
}
## 'export' the data for all our processors to see
clusterExport(cl, c("x","d","y"))
pbootg <- parLapply(cl,resamp,bootfit)
## parLapply returns a list.  to make it a vector:
pbootg <- unlist(pbootg)
hist(pbootg)

stopCluster(cl) ## good habit to stop when you're done




