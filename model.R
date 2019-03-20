library(nortest)
library(Hmisc)
library(psych)
library(car)
library(MASS)
library(zoo)
library(lmtest)
library(car)
library(leaps)
help("psych")

##Got raw data from https://www.kaggle.com/devisangeetha/calories-in-food/data 
#Topic: Estimating calories by nutritional contents
#Data manipulation includes: a) set the weight of each type of food equal to 100 grams; b) Values of calories and nutritional contents multiplied by 100/(serving size) to get the corresponding values
# c) Deleting all beverages, coffees, and smoothies.
#added a column of Unsaturated fat by substracting sum of saturated.fat and trans.fat from Total.fat

Mydata<-read.csv("Menu_modified.csv") 
summary(Mydata)
attach(Mydata)
Mydata_sub<- subset(Mydata,select=c(Calories,Total.Fat,Unsaturated..Fat,Trans.Fat,Saturated.Fat,Sugars,Carb ohydrates,Vitamin.A....Daily.Value.,Vitamin.C....Daily.Value.,Cholesterol,Sodium,Protein,Dietary .Fiber))
pairs.panels(Mydata_sub,col="red")

#Brief inspection on predictors. Following code produces scatter plots and correlations between variables.
pairs.panels(Mydata,col="red")
plot(Mydata)

#initial regression model with all variables
fit<- lm(Calories~Total.Fat+Unsaturated..Fat+Carbohydrates+Protein+Saturated.Fat+Trans.Fat+Vita min.A....Daily.Value.+Vitamin.C....Daily.Value.+Sodium+Cholesterol+Dietary.Fiber+Sugars+Iron.. ..Daily.Value.+Calcium....Daily.Value.,data=Mydata)
summary(fit)

vif(fit)

avPlots(fit,main = 'Added-Variable Plots for fit') 
summary(model)
hist(Calories)
e1<-residuals(fit)
plot(fitted(fit),e1) qqnorm(e1,ylab="Standardized Residuals",
xlab="Z Scores",main="Normality Test") 
qqline(e1,col="red") 
ad.test(e1) 
bptest(fit,student=F) 

#####################
durbinWatsonTest(fit) 
#p-value=0.702

#1st round of variable selection by using stepwise backward elimination 
#also tested multicollinearity
fit1<-step(fit,dirction="backward",trace=1) 
summary(fit1)
avPlots(fit1,main = 'Added-Variable Plots for fit1') 

#Variable selection with AIC's
fit1AIC <- stepAIC(fit,direction = 'both')
summary(fit1AIC)
e2<-residuals(fit1AIC) 
plot(fitted(fit1AIC),e2,) 
qqnorm(e2,ylab="Standardized Residuals",xlab="Z Scores",main="Normality Test") 
qqline(e2,col="red")
ad.test(e2) 
bptest(fit1AIC,student=F)
durbinWatsonTest(fit1)
#p-value=0.702 
vif(fit1AIC)

#seems like total.fat and Unsaturated.fat are hightly correlated. Removing usaturated.fat since it should be a component of total.fat
#fit model again without unsaturated.fat, also test for multicollinearity 
fit2<-lm(Calories~Total.Fat+Protein+Carbohydrates,data=Mydata)
summary(fit2)

#R2 remains the same at 99.76%
###Regression Diagnostic
vif(fit2)

#all variance inflation factors are now below 4.
e<-residuals(fit2)

#identify outliers
boxplot(e)
boxplot.stats(e)$out

#continue plotting for residual vs. fitted value plot(fitted(fit),e)
bptest(fit2,student=F)

# p-value=.7345 which is greater than 0.05 --->Failed to reject homoscedasticity ---->constant variance
ad.test(e)

#p-value is 0.3414. Therefore, normality is proved
durbinWatsonTest(fit2)
#test for auto-correlation, p-value = 0.488, Ho:rho = 0

#QQ residual 
plot qqnorm(e,ylab="Standardized Residuals",xlab="Z Scores",main="Normality Test") qqline(e,col="red")
#Ha:rho!=0
                 
#Linearity diagnostic
boxcox(fit2)

#lamda is approximately 1. therefore no transfromation is suggested.
avPlots(fit2,main = 'Added-Variable Plots for fit2') 

#also called individual coefficient plots, this displays the relationship between Calories and one of the predictors, but partialling out the other predictors.
crPlots(fit2)

#Component residual plots, an extension of partial residual plots, are a good way to see if the predictors have a linear relationship to the dependent variable
#A partial residual plot essentially attempts to model the residuals of one predictor against the dependent variable. A component residual plot adds a line indicating where the line of best fit lies. A significant difference between the residual line and the component line indicates that the predictor does not have a linear relationship with the dependent variable.
newData<-data.frame(Protein=12,Total.Fat=15,Carbohydrates=20) predict(fit2,newData,interval="confidence") predict(fit2,newData,interval="predict")

## final regression model.
# Calories = -2.60904 + 8.97773*Total.Fat + 3.88198*Protein + 4.17105*Carbohydrates
