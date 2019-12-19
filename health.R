library(faraway)
library(car)
library(leaps)
### 1. GET DATA
# load dataset
WHO <- read.csv("~/Documents/Grad School/Applied Regression Analysis/Life Expectancy Data.csv")
# create a column with the names of the countries
row.names(WHO) = WHO$Country
# delete the old column with names
WHO$Country = NULL

WHO[44,1] = "Developed" # greece
WHO[45,1] = "Developed" # france
WHO[50,1] = "Developed" # finland
WHO[25,1] = "Developed" # canada
WHO = subset(WHO, select = -c(BMI)) # remove BMI
WHO<-na.omit(WHO)

summary(WHO$Life.expectancy)


### 2. DATA EXPLORATION
d = density(WHO$Life.expectancy)
plot(d, main = "Density plot of Average Life Expectancy", xlab = "Years")

library(ggplot2)
ggplot(WHO, aes(x=WHO$Life.expectancy))+
  geom_density(color="darkblue", fill="lightblue") + 
  labs(title="Density Plot of Life Expectancy",x="Years")

library(ggplot2)
library(Rmisc)
p1 = qplot(WHO$Polio,WHO$Life.expectancy, xlab = "Polio", ylab = "Life Expectancy")
qplot(WHO$Schooling,WHO$Life.expectancy, xlab = "Schooling", ylab = "Life Expectancy")
p2 = qplot(WHO$Status, WHO$thinness..1.19.years, xlab = "Status", ylab = "Thinness 10 to 19 years")
p3 = qplot(WHO$Status, WHO$thinness.5.9.years, xlab = "Status", ylab = "Thinness 5 to 9 years")
p4 = qplot(WHO$thinness..1.19.years, WHO$Life.expectancy, xlab = "Thinness 10 to 19 years", ylab = "Life Expectancy")
p4 = qplot(WHO$thinness.5.9.years, WHO$Life.expectancy, xlab = "Thinness 5 to 9 years", ylab = "Life Expectancy")
p5 = qplot(WHO$thinness..1.19.years, WHO$thinness.5.9.years, xlab = "Thinness 10 to 19 years", ylab = "Thinness 5 to 9 years")
p6 = qplot(WHO$Measles, WHO$Life.expectancy, xlab = "Measles", ylab = "Life Expectancy")
multiplot(p1, p2, p3, p4, p5, p6, cols = 2)

p13 = qplot(WHO$Schooling, WHO$under.five.deaths, xlab = "Schooling", ylab = "Under 5 deaths")
p14 = qplot(WHO$Status, WHO$Schooling, xlab = "Status", ylab = "Schooling")
p15 = qplot(WHO$HIV.AIDS, WHO$Life.expectancy, xlab = "HIV/AIDS", ylab = "Life Expectancy")
p16 = qplot(WHO$HIV.AIDS, WHO$Measles, xlab = "HIV/AIDS", ylab = "Measles")
p17 = qplot(WHO$HIV.AIDS, WHO$Polio, xlab = "HIV/AIDS", ylab = "Polio")
p18 = qplot(WHO$Income.composition.of.resources, WHO$Schooling, xlab = "Income Composition of Resources", ylab = "Schooling")
multiplot(p13, p14, p15, p16, p17, p18, cols = 2)

p7 = qplot(WHO$Diphtheria, WHO$Life.expectancy, xlab = "Diphtheria", ylab = "Life Expectancy")
p10 = qplot(WHO$Polio, WHO$Life.expectancy, xlab = "Polio", ylab = "Life Expectancy")
p11 = qplot(WHO$Schooling, WHO$Life.expectancy, xlab = "Schooling", ylab = "Life Expectancy")
p12 = qplot(WHO$Status, WHO$Life.expectancy, ylab = "Life Expectancy")
multiplot(p7, p10, p11, p12, cols = 2)



### ASSESS COLLINEARITY
# you have to use the fitted model
lmod0 = lm(WHO$Life.expectancy ~ . - Income.composition.of.resources -HIV.AIDS -Adult.Mortality, data = WHO)
vif(lmod0)
# we can interpret sqrt(VIF) as meaning that the SE for infant.deaths is ~sqrt(VIF) 
# times larger than it would have been without collinearity. note that these observations 
# are imperfect because this is observational data and we cannot make orthogonal predictors

# I will amputate 2 regressors
lmod1 = lm(Life.expectancy ~. - infant.deaths  -Income.composition.of.resources -HIV.AIDS -Adult.Mortality, data = WHO)
vif(lmod1)
lmod2 = lm(Life.expectancy ~. - infant.deaths  -Income.composition.of.resources -HIV.AIDS -thinness..1.19.years -Adult.Mortality, data = WHO)
vif(lmod2)

### 4. VARIABLE SELECTION
b <- regsubsets(Life.expectancy ~ . - infant.deaths  -Income.composition.of.resources -HIV.AIDS -thinness..1.19.years -Adult.Mortality, data = WHO, nvmax = 14)
p = 2:11
rs <- summary(b)
# best subset w/ AIC
aic = rs$bic + p * (2 - log(137)) # log(n)
plot(aic ~ p, ylab = "AIC", xlab = "Number of Predictors", main = "Best Subset with AIC Criterion")
# AIC shows the model with 5 predictors is the best
# Status + Polio + Diphtheria + thinness..1.19.years + Schooling

# best subset with BIC
subsets(b, statistic = "bic", legend = TRUE, main = "Best Subset with BIC Criterion")
# BIC says 2 predictors: 
# Polio and Schooling
lmoda = lm(Life.expectancy ~ Schooling + Polio, data = WHO)

# best subset with adjusted R^2
subsets(b, statistic = "adjr2", legend = TRUE, main = "Best Subset with adj R^2 Criterion")
lmodb = lm(Life.expectancy ~ Status + Polio + Diphtheria + Schooling + thinness.5.9.years, data = WHO)
# adjusted R^2 says 6 predictors: 
# Status + Polio + thinness + Diphtheria + Schooling

#stepwise selection with AIC criterion
step(lmod2, direction = "both")
lmodc = lm(Life.expectancy ~ Polio + Diphtheria + Schooling + thinness.5.9.years, data = WHO)
# 4 predictors:
# Adult.Mortality + Hepatitis.B   + HIV.AIDS + Income.composition.of.resources

# TRANSFORM THE DATA
residualPlots(lmoda) # linear
residualPlots(lmodb) # thinness.5.9.years slightly nonlinear p-val supports
residualPlots(lmodc) # thinness.5.9.years slightly nonlinear p-val supports


marginalModelPlots(lmoda) # some issues near the right end point for schooling
marginalModelPlots(lmodb)
marginalModelPlots(lmodc)

# the marginal model plots do not provide clear evidence of a model problem for the WHO data

# examine added variable plots
avPlots(lmoda, id = TRUE) # appears linear
avPlots(lmodb, id = TRUE)
avPlots(lmodc, id = TRUE)

#CR plot
crPlots(lmoda)

lmoda2 = lm(Life.expectancy ~ sqrt(Schooling) + Polio, data = WHO)
lmoda3 = lm(Life.expectancy ~ log(Schooling) + Polio, data = WHO)
crPlots(lmodb)
crPlots(lmodc) # bulging in thinness

# residuals plot 
plot(residuals(lmoda))
abline(1,0)
# good structure. Possibly a trend of negative residuals


# IDENTIFY UNUSUAL OBSERVATIONS
# find leverage points
h = hatvalues(lmoda)
countries <- row.names(WHO) 
# construct half-normal plot 
halfnorm(h, nlab = 3, labs = countries, ylab = "leverage")
# Maybe Tonga?
# construct index plot of the leverages
infIndexPlot(lmoda, vars = "hat", id = list(n = 3))
# Tonga

# remove leverage points
library(Hmisc)
K = c("Sierra Leone", "Djibouti", "Australia", "Afghanistan", "Lesotho", "Angola", "Guatemala", "Tonga", "Nepal", "Nigeria", "Pakistan")
countries <- row.names(WHO) 
lmodAll <- lm(Life.expectancy ~ Schooling + Polio, data = WHO, subset = countries %nin% K)
h = hatvalues(lmodAll)# Cook's half-normal plot
halfnorm(h, n =5, labs = countries, ylab = "Cook's distances")
# construct index plot of the leverages
infIndexPlot(lmodAll, vars = "hat", id = list(n = 10))

K = c("Sierra Leone", "Djibouti", "Australia", "Afghanistan", "Lesotho", "Angola", "Guatemala", "Tonga", "Nepal", "Nigeria", "Pakistan", "Indonesia", "Zambia", "Kiribati", "Mozambique", "Equatorial Guinea")
lmodAll <- lm(Life.expectancy ~ Schooling + Polio, data = WHO, subset = countries %nin% K)
h = hatvalues(lmodAll)# Cook's half-normal plot
halfnorm(h, n =5, labs = countries, ylab = "Leverage")

# IDENTIFY INFLUENTIAL OBSERVATIONS
# obtain studentized residuals
stud <- rstudent(lmoda)
# largest magnitude studentized residual
max(abs(stud))
# since we are doing a two-sided test, we need the 
# 1 - alpha/2n quantile not 1-alpha/n.  
# df = 129 - 2 - 1 found in summary lmoda
qt(1 - .05/(129*2), df = 126) 
# because max < qt -> the studentized residuals are all within the expected range based
# on the quantile from the t distribution

# perform outler check using Bonferroni correction
outlierTest(lmoda)
# normal Bonferonni

# index plots of studentized residuals and Bonferroni p-values
infIndexPlot(lmoda, vars = c("Studentized", "Bonf"))
# Zimbabwe and Angola are 


# identify leverage points
cook <- cooks.distance(lmoda)
# Cook's half-normal plot
halfnorm(cook, n =11, labs = countries, ylab = "Cook's distances")
# 5 influential observations
# Djiboutit, Seirra Leonoe, Lesotho, Guatemala, Angola
# Cook's index plot
infIndexPlot(lmoda, var = "Cook", id = list(n = 11))

# index plot of dfbetas
dfbetasPlots(lmoda, id.n = 5, lab = countries)
# influence plot of model
influencePlot(lmoda)

### we're going to refit the mdel after removing top 4 seen influential observations (one-by-one)
# refit model after removing El Salvador
lmodA <- lm(Life.expectancy ~ Schooling + Polio, data = WHO, subset = (countries != "Angola"))
compareCoefs(lmoda, lmodA) # no change or increases
library(Hmisc)
K = c("Sierra Leone", "Djibouti", "Australia", "Afghanistan", "Lesotho", "Angola", "Guatemala", "Tonga", "Nepal", "Nigeria", "Pakistan")
countries <- row.names(WHO) 
lmodAll <- lm(Life.expectancy ~ Schooling + Polio, data = WHO, subset = countries %nin% K)
compareCoefs(lmoda, lmodAll) 

cook <- cooks.distance(lmodAll)
# Cook's half-normal plot
halfnorm(cook, n =5, labs = countries, ylab = "Cook's distances")



### 5. ASSESS OTHER ASSUMPTIONS
# constant variance and error stuff
residualPlot(lmoda, main = "Residual Plot")
plot(lmoda, which = 1) 
# zero error assumption reasonable
# constant variance assumption reasonable because spread of residuals has a constant thickness 
plot(lmoda, which = 8, main = "sqrt(ehat) vs yhat") # plot of sqrt(ehat) vs yhat
# this effectively "doubles" the resolution for detecting nonconstant variance

# normality
qqPlot(lmoda) # 2 points outside CB (1 on border)\
# Eq Guinea and Zimbabwe are outside the confidence bands
plot(lmoda, which = 2)
shapiro.test(residuals(lmoda)) 
# there is insufficient evidence to conclude the residuals come from a nonnormal distribution
# normality assumption is met

# independence (correlation of errors)
# plot successive pairs of residuals
n = nobs(lmoda)
plot(tail(residuals(lmoda), n-1) ~
       head(residuals(lmoda), n-1),
     xlab = expression(hat(epsilon)[i]),
     ylab = expression(hat(epsilon)[i+1]), main = "ehat_[i+1] vs ehat_i")
abline(h=0, v=0, col = grey(0.75))
# roughly an even spread about ehat_i+1 = 0 


library(lmtest)
# Under the null hypothesis of uncorrelated errors, the test statistic follows a linear combination of X^2 distribution
dwtest(Life.expectancy ~ Polio + Schooling, data = WHO)
# p-value is greater than alpha threshold -> "accept" the null hyp: true autocorrelation is less than 0


### 6. PERFORM STATISTICAL INFERENCE IF NECESSARY
# CI and/or HT
set.seed(8) # for reproducible results

# CI for the mean of life expectancy
mean(WHO$Life.expectancy) - qnorm(0.975) * 1/sqrt(10)
mean(WHO$Life.expectancy) + qnorm(0.975) * 1/sqrt(10)

library(ISwR)
confint(lmoda)
#The 95% confidence interval for the slope is the estimated coefficient (7.0595) ± two standard errors (0.9776).


# calculate average lifeExpF for each level of Status
# Developed countries have a significantly higher life expectancy than developing countries


