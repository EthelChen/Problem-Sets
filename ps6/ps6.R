# PS6 script for analysis of congress109 data

library(textir) # to get the data
library(maptpx) # for the topics function
source("kIC.R")


data(congress109)
congress109[1:10,]
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
f <- as.matrix(congress109Counts/rowSums(congress109Counts))
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

# what is lowest BIC?
which.min(kbic) #lowest BIC is 2 ... There are only two parties?  Hard to interpret when you look at the analysis and cuts below

kmfs <- kmeans(fs,2) # run a clustering and look at it
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
tpcs2 <- topics(c,K=(4:17),tol=10) # chooses 11

summary(tpcs2, n=10) 

tapply(congress109Ideology_s$rsb<=0,kmfs$cluster,table)

# there are several topics that float to the top including credit cards
# gun control, bringing the troops home, etc.  These represent topics 
# up for debate in the public sphere this year.

# [3]

#tab party and kmeans clusters
tapply(congress109Ideology$party,kmfs$cluster,table)

#regress topics onto party and reshape
party <- congress109Ideology$party
repshare <- congress109Ideology$repshare
x <- 100*congress109Counts/rowSums(congress109Counts)

# multinomial regression
cl <- makeCluster(2,type=ifelse(.Platform$OS.type=="unix","FORK","PSOCK")) 
party_reg <- dmr(cl = cl, covars = tpcs2$omega, counts = party, verb = 1)
party_regP <- dmr(cl = cl, covars = x, counts = party, verb = 1)
stopCluster(cl)

coef(party_reg)
coef(party_regP)

# regression
repshare_reg <- gamlr(x = tpcs2$omega, y = repshare)
cv.repshare_reg <- cv.gamlr(x = tpcs2$omega, y = repshare, lambda.min.ratio = 0.001)
plot(cv.repshare_reg)

repshare_regP <- gamlr(x = x, y = repshare)
cv.repshare_regP <- cv.gamlr(x = x, y = repshare, lambda.min.ratio = 0.001)
plot(cv.repshare_regP)

coef(cv.repshare_reg, select = "1se")
coef(cv.repshare_regP, select = "1se")

