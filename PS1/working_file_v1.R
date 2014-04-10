##Section 0

#getting the data set up
#added new variable for total amount paid and took the log of it
benjer = read.csv("BenAndJerry.csv")
priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity
lpriceper1 <- log(1+priceper1)

##Section 1
##ran some demographics on price, variable

plot(coupon_value ~ household_income, data=benjer)  ##shows that there's are greate values of coupons for richer household, which is somewhat surprising

par(mfrow=c(1,3))
  boxplot(priceper1 ~ race, data=benjer, main = "Price v Race", xlab = "Race", ylab = "Price")  ##shows greatest spread among whites
  boxplot(priceper1 ~ hispanic_origin, data=benjer, main = "Price v Hisp Orig", xlab = "Hisp Orig", ylab = "Price") ##shows greatest non-hispanics
  boxplot(priceper1 ~ region, data=benjer, main = "Price v Region", xlab = "Region", ylab = "Price")

par(mfrow=c(1,2))
  boxplot(priceper1 ~ household_size, data=benjer, main = "Price v Household Size", xlab = "Household Size", ylab = "Price")
  boxplot(priceper1 ~ age_and_presence_of_children, data=benjer, main = "Price v Age + Presence of Children", xlab = "Age + Presence of Children", ylab = "Price")

par(mfrow=c(2,2))
  boxplot(priceper1 ~ male_head_employment, data=benjer, main = "Price v Male Head Empl.", xlab = "Male Head Empl.", ylab = "Price")
  boxplot(priceper1 ~ female_head_employment, data=benjer, main = "Price v Female Head Empl.", xlab = "Female Head Empl.", ylab = "Price")
  boxplot(priceper1 ~ male_head_occupation, data=benjer, main = "Price v Male Head Occ.", xlab = "Male Head Occ.", ylab = "Price")
  boxplot(priceper1 ~ female_head_occupation, data=benjer, main = "Price v Female Head Empl.", xlab = "Female Head Empl.", ylab = "Price")

par(mfrow=c(1,1))
  boxplot(priceper1 ~ flavor_descr, data=benjer, main = "Price v Flavor", xlab = "Flavor", ylab = "Price")

par(mfrow=c(1,2))
  boxplot(priceper1 ~ household_size, data=benjer, main = "Price v Household Size", xlab = "Household Size", ylab = "Price")
  boxplot(priceper1 ~ age_and_presence_of_children, data=benjer, main = "Price v Age + Presence of Children", xlab = "Age + Presence of Children", ylab = "Price")

par(mfrow=c(2,2))
  boxplot(priceper1 ~ type_of_residence, data=benjer, main = "Price v Type of Residence", xlab = "Residence", ylab = "Price")
  boxplot(priceper1 ~ kitchen_appliances, data=benjer, main = "Price v Kitchen Appliances", xlab = "Kitchen Appliances", ylab = "Price")
  boxplot(priceper1 ~ tv_items, data=benjer, main = "Price v TV Items", xlab = "TV Items", ylab = "Price")
  boxplot(priceper1 ~ household_internet_connection, data=benjer, main = "Price v Internet Conn.", xlab = "Internet Conn.", ylab = "Price")


##Section 2
##ran the same process that he did to create a new data set
x <- benjer[,c("flavor_descr","size1_descr",
	"household_income","household_size")]
x$flavor_descr <- relevel(x$flavor_descr,"VAN")
x$usecoup = factor(benjer$coupon_value>0)
x$couponper1 <- benjer$coupon_value/benjer$quantity
x$region <- factor(benjer$region, 
	levels=1:4, labels=c("East","Central","South","West"))
	x$married <- factor(benjer$marital_status==1)
x$race <- factor(benjer$race,
	levels=1:4,labels=c("white","black","asian","other"))
x$hispanic_origin <- benjer$hispanic_origin==1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- benjer$type_of_residence==1
x$internet <- benjer$household_internet_connection==1
x$tvcable <- benjer$tv_items>1


fit <- glm(lpriceper1~.,data=x)
##problem with how it exists now is that the flavor description should be predicting all of the price. that is why household income coefficient is really small, but still significant - because all of the variation is explained in the price description. if were to improve, would drop several of the variables from x since what really interested in isn't what price peoplel pay based on the flavor (we set that), but rather what are predictors for how much different groups will pay. or at the very least take away things that we would assume to just be proxies for income, like microwave, cable, dishwasher, and internet


##dropping other variables that are proxies for wealth. makes no impact
x$tvcable <- NULL
x$internet <- NULL
x$microwave <- NULL
x$dishwasher <- NULL
fit <- glm(lpriceper1~.,data=x)
summary(fit)

##add the dropped variables back in
x$tvcable <- benjer$tv_items>1
x$internet <- benjer$household_internet_connection==1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)


full <- fit
step(full, direction = "backward", trace=FALSE, steps = 5)

##Section 3
pvals <- summary(fit)$coef[-1,4] 
pvals
#this prints off all of the pvals

#Section +
#fdr_cut
source("fdr.R")
cutoff <- fdr_cut(pvals, 0.01) #shows several different values that are still statistically significant even if we keep the r level pretty low (happened even when changed pvals to .001)
