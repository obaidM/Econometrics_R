###########################################
## QUESTION 8.12
rm(list=ls(all=TRUE))


library(multcomp)
library(data.table)
library(dplyr)
library(plotly)
library(lmtest)
library(sandwich)
library(car)

context = fread('pubexp.csv')

context = mutate(context, ycol = ee/p)
context = mutate(context, xcol = gdp/p)
## WE expect heteroskadasticity because we know that as Gdp per capita will increase then 
## variance in Exp per capita will widen. As some countries like austria or denmark will have 
## high gdp per capita but very low Exp per capita
## but larger countries like US, CHINA , rUSSIA will also have high GPD per capita but very
## large EE per capita 

model = lm(ycol ~ xcol , data =context)
summary(model)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.124573   0.048523  -2.567   0.0151 *  
#  xcol         0.073173   0.005179  14.128 2.65e-15 ***
#context =  mutate(context,residual= (context$ycol-predict(model))^2)

plot_ly(context,x= ~xcol, y= ~ycol)

# part b 
## test for hetero with white test

ncvTest(model)
# Chisquare = 9.92055    Df = 1     p = 0.001634435
# p VALUE IS LOWER THAN 0.05 meaning that HO ( homoskaticity) is rejecetd
# This is not the White test.

qchisq(0.95,2)  # 5.9914 becomes the critical value

#t test of coefficients:
  
#  Estimate Std. Error t value  Pr(>|t|)    
#(Intercept) -0.1245728  0.0424400 -2.9353  0.006123 ** 
#  xcol         0.0731732  0.0066033 11.0813 1.735e-12 ***

## part c 
coeftest(model, vcov. = vcovHC)
tc <- 2.037

# confidence interval without heter correction
high = 0.073173  + (tc*0.005179)   # 0.083722
low =   0.073173  - (tc*0.005179)   # 0.062623

# confidence interval without heter correction
high = 0.073173  + (tc*0.0066033)  # 0.0866
low =   0.073173  - (tc*0.0066033)  # 0.0597





## interval with correction is wider

# part d
###### Linear modelling with weighted 
w= 1/context$xcol
modelw = lm(ycol ~ xcol , data =context,weights = w)
summary(modelw)


#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.092921   0.028904  -3.215  0.00298 ** 
 # xcol         0.069321   0.004412  15.713  < 2e-16 ***
