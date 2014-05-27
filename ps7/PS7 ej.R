sp500 <- read.csv("sp500.csv")
fx <- read.csv("FXmonthly.csv")
fx <- (fx[2:120,]-fx[1:119,])/(fx[1:119,])


#1. Correlatoins
corfx <- cor(fx)
hist(corfx)
#Shows that there isn't a bel curve around 0. There is a spike there, but there is a much bigger spike between .4 - .6

#2. Fit, plot, and interpret principal components.
pcfx <- prcomp(fx, scale=TRUE)
zfx <- predict(pcfx,x = fx)
plot(pcfx)
#Shows that the the first one is very greedy, and includes almost all the varianc.
summary(pcfx)
#confirms the plot. PC1 summarizes 44% of the variance. PC2 to PC4 are the only ones that explain more than 5%
t( round(pcfx$rotation[,1:2],2) )
#Still not able to find why this is.  I looked at what countries were 
#really strongly positvely and negatively predictive, and couldn't
#find much of a pattern there. It is true that the large values for
#PC1 are positive, and for PC2 they are negative. Maybe PC1 just takes
#into account years currencies depreciated, and PC2 years they apprecieated?

#3. Regress SP500 returns onto currency movement factors,
#using both ‘glm on first K’ and lasso techniques.
#Use the results to add to your factor interpretation.

#first convert to a data frame
zfxdf <- as.data.frame(zfx)
#get the return dimension isolated to get your outcome variable
return <- sp500$sp500

source("kIC.R")
source("deviance.R")
library(textir)

kfx <- lapply(1:20, function(K) glm(return ~., data=zfxdf[,1:K,drop=FALSE]))
aiccfx <- sapply(kfx, AICc)
which.min(aiccfx)
#returns K = 3
kbic <- sapply(kfx, BIC)
which.min(kbic)
#min.bic also returns K = 3
#if want to do see what looks like with the first k, use the below code
kfx[1]


#lasso technique
lassofx <- cv.gamlr(x=zfx, y=return)
plot(lassofx)
#cv.1se returns what looks like 4 or 5.  Regular returns about 20

#All this tells me is that you don't need that many buckets to explain
#what is going on with the currency exchange.

#4. Fit lasso to the original covariates and describe how it differs from PCR here.
lasso.simple <- cv.gamlr(x=as.matrix(fx), y=return)
plot(lasso.simple)
#1SE is 4 covariates, and the min is about 17. Not exactly sure how to interpret