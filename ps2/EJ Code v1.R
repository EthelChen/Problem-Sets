##0 Getting Data set up
homes <- read.csv("homes2004.csv")

# create a var for downpayment being greater than 20%
homes$gt20dwn <- 
	factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)

##Question 1

##I first plotted a lot of things to see if there is any correlation between price and value with indicators for a bad neighborhood.  The biggest change I found between the yes/no was the fact looking at abandoned homes.  Very sad to see that odor didn't really make that big of a difference. Because not a lot of these things are that interesting, I put it down below.

#I then started to look at what the difference was by demographic indicators, and started with interest rate.  I found a ton of variation there, so would like to spend more time digging into it.  

## The first thing that I did was plot price/value against the interest rate.  I think that this is interesting because of the bell shaped curve that you get when you plot in
old.par <- par(mfrow=c(1, 2))
boxplot(VALUE ~ INTW, data=homes)
plot(LPRICE ~ INTW, data=homes)
par(old.par)

#again, this same strange bell curve appears when you plot the first mortgage amount against the interest rate
boxplot(AMMORT ~ INTW, data=homes)

#Plotting interest rate against state I thought was interesting since states either had a lot of variation (like Texas and Indiana) or barely any
plot(INTW ~ STATE, data=homes)

#The spread on interest rate and whether the downpayment came from the previous home was really interesting too. If it came from another source, the median wasn't that different, but the spread was much greater.
boxplot(INTW ~ DWNPAY, data=homes)

#again, the spread was really big in the areas where you couldn't put the downpayment in the same year that you got the mortgage
plot(INTW ~ MATBUY, data=homes)

#again, there is a huge disparity fo interest rate when plotted against if you had abandoned apartments buildings next to you
plot(INTW ~ EABAN, data=homes)


#bigger difference here - abandandoned houses aren't good
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ EABAN, data=homes)
plot(LPRICE ~ EABAN, data=homes)
par(old.par)


##not that much differeince
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ EAPTBL, data=homes)
plot(LPRICE ~ EAPTBL, data=homes)
par(old.par)

##not that much differeince
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ECOM1, data=homes)
plot(LPRICE ~ ECOM1, data=homes)
par(old.par)

##not that much differeince
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ECOM2, data=homes)
plot(LPRICE ~ ECOM2, data=homes)
par(old.par)

##not that much difference
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ EGREEN, data=homes)
plot(LPRICE ~ EGREEN, data=homes)
par(old.par)

##not that much difference
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ EJUNK, data=homes)
plot(LPRICE ~ EJUNK, data=homes)
par(old.par)

##not that much difference
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ECOM2, data=homes)
plot(LPRICE ~ ECOM2, data=homes)
par(old.par)

##not that much difference
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ESFD, data=homes)
plot(LPRICE ~ ESFD, data=homes)
par(old.par)

#value decreases with close highway
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ETRANS, data=homes)
plot(LPRICE ~ ETRANS, data=homes)
par(old.par)


old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ HOWH, data=homes)
plot(LPRICE ~ HOWH, data=homes)
par(old.par)

#disappointingly, there wasn't much of a difference with odors
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ODORA, data=homes)
plot(LPRICE ~ ODORA, data=homes)
par(old.par)

#not that much difference
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ STRNA, data=homes)
plot(LPRICE ~ STRNA, data=homes)
par(old.par)

##basically exactly the same
old.par <- par(mfrow=c(1, 2))
plot(VALUE ~ ELOW1, data=homes)
plot(LPRICE ~ ELOW1, data=homes)
par(old.par)

##Question 2
classreg <- glm(log(VALUE) ~ .-AMMORT -LPRICE, data=homes)
pvals <- summary(classreg)$coef[-1,4]
source("fdr.R")
cutoff <- fdr_cut(pvals, 0.1)
names(pvals)[pvals<.05]
#think that there are 33 covariates that are significant.  But don't understand how that can be true with the number of covariates regressing against.
names(pvals)[pvals>.05]
#dropped all of the names that came up
classreg2b <- glm(log(VALUE) ~ .-AMMORT -LPRICE -EAPTBL -ECOM1 -ECOM2 -EGREEN -ELOW1 -ETRANS -ODORA -PER -ZADULT -NUNITS, data=homes)
#r squared comparisions
summary(classreg)
1 - 10373/14920 
# Rsquared for the first model is 0.3047587
summary(classreg2b)
1 - 10359/14920
# Rsquared for the second model is 0.3056971. Lower because though a better model, doesn't have a lot of unnecessary things that are creating an overfitting of the data.

##Question 3
summary(homes$gt20dwn)
classreg3a <- glm(gt20dwn ~ .-LPRICE - AMMORT, data=homes, family=binomial)
## looks like if this was someone's first home, they were less likely to have to put greater than 20% down by ~40%.  For every extra bathroom, 24% more likely to have to put down greater than 20%
classreg3b = glm(gt20dwn ~ . +BATHS*FRSTHO -AMMORT -LPRICE, data=homes, family=binomial)
## first home buyer, for every new bathroom only 10% more likely to have to pay mortgage greater than 20%. for return buyers, 30% more likely.

##Question 4

gt100 <- which(homes$VALUE>1e5)
#create the two data sets.  homes >100000 are in homes99
homes100 <- homes[gt100,]
homes99 <- homes[-gt100,]
classreg4a = glm(gt20dwn ~ . +BATHS*FRSTHO -AMMORT -LPRICE, data=homes100, family=binomial)
#Rsquared for this is (1 - 13617/15210) = 0.1047337
classreg4b = glm(gt20dwn ~ . +BATHS*FRSTHO -AMMORT -LPRICE, data=homes99, family=binomial)
#Rsquared for this is (1- 3214.5/3494.1) = 0.1047337