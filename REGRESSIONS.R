student.New<-read.csv(file.choose(),fileEncoding="UTF-8-BOM")
head(student.New)
tinytex::reinstall_tinytex()
# Part 1
# 1: Our data set is about the student performances and we are choosing G3 as Y and G2 as X because since G1 represent the first period grade, G2 the second period grade and G3 the final grade therefore G3 will be obtain based on G2 and G1. That is why G3 and G2 will respectively be good response variable and good predictor variable

# X = G2    , y = G3

# 2: Construction a scatterplot of G3 against G2
plot(G3~G2,data=student.New,xlab="G2",ylab="G3")


## We have a strong positive association between the final grade and the second period grade

# 3: Regression and it function 

m<-lm(G3~G2,data=student.New)
abline(m,col="red")
summary(m)
## The slope of the regression line (0.98518), this confirm that  we have a positive association between G3 and G2.
## The estimated regression function is : G3 = 0.55461 + 0.98518G2

# 4 MSE and R^2
MSE<-summary(m)$sigma^2
MSE
anova(m)
## MSE = 1.47 is very low which is an indication of good model. The error values are low

summary(m)$r.squared
legend("topleft",legend=paste("R-squared= ",
                              format(summary(m)$adj.r.squared,digits=2)))

## Our R^2 = 0.84 is pretty high which confirm our comment on the association between the two variables (G3 and G2)

# 5: Testing whether ????1 is different from zero or not at 0.05 level of significance.
summary(m)
qt(1-0.05/2,200-2)
# t = qt(1-0.05/2,200-2) = 1.97
# t*= b1\s{b1} =  0.98518/0.02996 = 32.88
## we conclude Ha because |t*|> t.
## if we conclude Ha, this means that ????1 is different than 0 and therefore 
#  there is a significant relation between G2 and G3

#The alternate decision rule and conclusion state that:
# we conclude Ho if |t*|< t

#if P-value < alpha then we conclude Ha, otherwise, we conclude Ho

#The P-value = 2.2e-16 < 0.05, which confirm our conclusion of Ha 

# 6: Superimposing the (pointwise) 90% confidence band on the scatterplot.
newx=seq(0,20,by=5)
ci90=predict(m,newdata=data.frame(G2=newx),level=0.90,interval="confidence")
lines(newx,ci90[,2],col="blue"); lines(newx,ci90[,3],col="blue")

# 7: Constructing a scatterplot of semi-studentized residuals against the predicted values and a normal probability plot of semi-studentized residuals
m$residuals/summary(m)$sigma 
plot(m$residuals/summary(m)$sigma~m$fitted.values,xlab="Fitted value",ylab="Semistudentized residual")
abline(h=0,col="red")
## There is no significant evidence supporting the violation of constant variance

# Normal probability plot 
qqnorm(m$residuals,main="Normal Q-Q plot of residuals")
qqline(m$residuals, col="red")

## It is not clear for us, we need to do the shapiro-wilk test
shapiro.test(m$residuals)

## p-value < 2.2e-16 < 0.05, therefore we conclude Ha: nonnormality

## Part 2

# (i): Producing an ANOVA table. Reporting SST,SSR and SSE, and their corresponding degrees of freedom.

m1<-update(m,~.+G1)
anova(m,m1)
m2<-update(m1,~.+absences) 
anova(m2)
1589.22 + 5.94 + 0.72
# SSR =  1589.22 + 5.94 +0.72 = 1595.88 , with df = 3
# SSE = 284.34, with df = 196
# SSTO = SSR + SSE = 1595.88 + 284.34 = 1879.88, with df = 199

## (ii): Performing  the F test of overall linear relationship
qf(0.95,3,196) # F = 2.65
summary(m2)
# F* = 366.7
# From the summary table F* = 366.7 > F 
# Therefore,we conclude Ha. 
# We would have concluded Ho if  F* tend to be 1.

#  If p-value < alpha, we conclude Ha
# Other wise, we conclude Ho.
# Here p-value = 2.2e-16  < alpha, therefore, which confirm our conclusion of Ha

# (iii): Computing the extra sum of squares SSR(X3|X1,X2)
m1<-lm(G3~G2,data=student.New)
m2<-update(m1,~.+G1)
m3<-update(m2,~.+absences)
anova(m2,m3)
285.06 - 284.33
# From the output of anova(m1,m2), we have extra sum of squares SSR(X3|X1,X2) = 0.73
library(rsq)
rsq.partial(m2,m3) # which is -0.025

# (iv): Testing whether X3 is helpful, given that X1 and X2 are in a model
qf(0.95,1,196) # which is F = 3.88
anova(m2,m3)
# F* = 0.4981 <  F , therefore we conclude Ho, which means that  ????3 = 0 and we 
# drop absences(X3) giving that G1 and G2 are in the model. absences is not helpful
# p-value = 0.4812 > 0.05, which confirm our conclusion of Ho.

## Model #2: a regression model including a numerical predictor variable, a categorical predictor variable, and their interaction term.

## (v): Incorporating the categorical variable into the model by defining indicator variable(s).

mm<-lm(G3~G2*sex, data = student.New)
summary(mm)
# (vi) The categorical variable and the interaction term are insignificant which mean that we can drop one giving the other one in the model 

# (vii) Yes we should drop the interaction term because it is highly insignificant 

# (viii)
AAA<-lm(G3~., data = student.New)
summary(AIC)
m.backward<-step(AAA,direction="backward")
summary(m.backward)

## The subset of predictor variables to be included in the model is: 
## school + sex + Mjob + Fjob + traveltime + schoolsup + paid + activities + famrel + goout + Dalc + Walc + G2
## The corresponding  AIC=55.68