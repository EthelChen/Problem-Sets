# PS6 script for analysis of congress109 data

library(textir) # to get the data
library(maptpx) # for the topics function
source("kIC.R")
source("deviance.R")



data(congress109)
totalcongress <- cbind(as.matrix(congress109Counts),congress109Ideology)


# get a sense for these data sets by looking at labels and rows
dim(congress109Counts)
congress109Counts[1:10,] #sparse matrix of phrase counts by rep
labels(congress109Counts) #look at the phrases

# get a sense for these data sets by looking at labels and rows
dim(congress109Ideology)
congress109Ideology[1:10,]
labels(congress109Ideology)

# [1]

#  [from wine and we8there]
#  upon what scale to measure `words' for k-means is unclear (the answer is
#  actually: don't use kmeans; use a multinomial mixture instead).
#  You could do log(1+f) instead, or something like tf-idf (google that)
#  but in absence of better guidance, I just look at scaled f
#  takes time, because you're making it dense from sparse

#scale
f <- as.matrix(congress109Counts/colSums(congress109Counts))
fs <- scale(f)

# run kmeans with 1:25 and pick find BIC and AIC
kfit <- lapply(1:25, function(k) kmeans(fs,k))
kaicc <- sapply(kfit,kIC)
kbic <- sapply(kfit,kIC,"B")

# plot them
plot(kaicc, xlab="K", ylab="IC", 
     ylim=range(c(kaicc,kbic)), # get them on same page
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc))
lines(kbic, col=4, lwd=2)
abline(v=which.min(kbic),col=4)

plot(kbic, xlab="K", ylab="IC", lines(kbic, col=4, lwd=2), abline(v=which.min(kbic)))

# what is lowest BIC?
which.min(kbic) #lowest BIC is 2 ... There are only two parties?  Hard to interpret when you look at the analysis and cuts below

kmfs <- kmeans(fs,which.min(kbic)) # run a clustering and look at it
print(apply(kmfs$centers,1,function(c) colnames(fs)[order(-c)[1:10]]))


tapply(congress109Ideology$party,kmfs$cluster,table)
tapply(congress109Ideology$chamber,kmfs$cluster,table)
congress109Ideology_s <- congress109Ideology
congress109Ideology_s[,5:7] <- scale(congress109Ideology[,5:7])
congress109Ideology_s$rsb <- congress109Ideology_s$repshare <= 0
tapply(congress109Ideology_s$cs1<=0,kmfs$cluster,table)
tapply(congress109Ideology_s$cs2<=0,kmfs$cluster,table)
tapply(congress109Ideology_s$rsb<=0,kmfs$cluster,table)


# [2]

c <- as.simple_triplet_matrix(congress109Counts)
tpcs <- topics(c,K=5*(1:10),tol=10) # selected 10 so do a another search around 10
tpcs2 <- topics(c,K=(5:20),tol=10) # chooses 11

summary(tpcs2, n=10) 
# there are several topics that float to the top including credit cards
# gun control, bringing the troops home, etc.  These represent topics 
# up for debate in the public sphere this year.

rownames(tpcs2$theta)[order(tpcs2$theta[,1], decreasing=TRUE)[1:10]]
rownames(tpcs2$theta)[order(tpcs2$theta[,2], decreasing=TRUE)[1:10]]
rownames(tpcs2$theta)[order(tpcs2$theta[,3], decreasing=TRUE)[1:10]]
rownames(tpcs2$theta)[order(tpcs2$theta[,4], decreasing=TRUE)[1:10]]
rownames(tpcs2$theta)[order(tpcs2$theta[,5], decreasing=TRUE)[1:10]]
rownames(tpcs2$theta)[order(tpcs2$theta[,6], decreasing=TRUE)[1:10]]
rownames(tpcs2$theta)[order(tpcs2$theta[,7], decreasing=TRUE)[1:10]]
rownames(tpcs2$theta)[order(tpcs2$theta[,8], decreasing=TRUE)[1:10]]
rownames(tpcs2$theta)[order(tpcs2$theta[,9], decreasing=TRUE)[1:10]]
rownames(tpcs2$theta)[order(tpcs2$theta[,10], decreasing=TRUE)[1:10]]
rownames(tpcs2$theta)[order(tpcs2$theta[,11], decreasing=TRUE)[1:10]]


# [3]

#tab party and kmeans clusters
tapply(congress109Ideology$party,kmfs$cluster,table)


#regress topics onto party and reshape
party <- congress109Ideology$party
repshare <- congress109Ideology$repshare
x <- 100*congress109Counts/colSums(congress109Counts)




#AICc plots for repshare
repshare_regP <- gamlr(x = x, y = repshare)
repshare_tpcs2 <- gamlr(x = tpcs2$omega, y = repshare)
plot(repshare_regP, main="repshare_regP")
plot(repshare_tpcs2, main="repshare_tpcs2")

#predict and look at deviance and r2 for both
p1 <- predict(repshare_regP, x, family = "gaussian")
deviance(repshare, p1, family = "gaussian")
R2(repshare, p1, family = "gaussian")

p2 <- predict(repshare_tpcs2, tpcs2$omega, family = "gaussian")
deviance(repshare, p2, family = "gaussian")
R2(repshare, p2, family = "gaussian")


#CV plots
cv.repshare_regP <- cv.gamlr(x = x, y = repshare)
cv.repshare_tpcs2 <- cv.gamlr(x = tpcs2$omega, y = repshare, lambda.min.ratio = 0.001)
plot(cv.repshare_regP, main="cv.repshare_regP")
plot(cv.repshare_tpcs2, main="cv.repshare_tpcs2")


coef(cv.repshare_reg, select = "1se")
coef(cv.repshare_regP, select = "1se")

# multinomial regression
cl <- makeCluster(2,type=ifelse(.Platform$OS.type=="unix","FORK","PSOCK")) 
party_reg <- dmr(cl = cl, covars = tpcs2$omega, counts = party, verb = 1)
party_regP <- dmr(cl = cl, covars = x, counts = party, verb = 1)
stopCluster(cl)

#predict and look at deviance and r2 for both
p1 <- predict(party_regP, x, type = "response")
deviance(party, p1, family = "binomial")
R2(party, p1, family = "binomial")

p2 <- predict(party_reg, tpcs2$omega, type = "response")
deviance(party, p2, family = "binomial")
R2(party, p2, family = "binomial")

coef(party_reg)
coef(party_regP)
