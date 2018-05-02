
###########################################
## QUESTION 6.15
rm(list=ls(all=TRUE))


library(foreign)
library(multcomp)
library(data.table)
library(dplyr)


context = fread('stckton.csv')
head(context)
View(context)
## part A
model = lm( sprice~livarea+beds+baths+age,data=context)
print(summary(model))
SSE.r =   sum((context$sprice-predict(model))^2)
print(SSE.r)
## PartB

## price with age 2
Intercept = 11154.29
B_age = -11.33
price2 = Intercept + B_age*2

## Pric with age 10
price10 = Intercept + B_age*10

## she can expect a difference of 
difference = price2 - price10

## 95% interval for price2
tc=1.96
se_age = 80.50

intervalL = 8*(B_age -(tc*se_age))
print(intervalL)

intervalH =8* (B_age +(tc*se_age))
print(intervalH)
##[-1352.88,1171.6]

##PARTC
## house increased by 200 sqft 
Intercept = 11154
B_livarea = 10680
SE_livarea = 273.15

IncreaseInprice = (B_livarea*2)
print(IncreaseInprice)
##21360
#  20,000/2 is that 10,000
# HO is that B2(livarea) < 10,000
#H1 is that B2(livarea) >10,000
## critical value of t = 1.645 (positive)
tval= (B_livarea-10000)/SE_livarea
print(tval)
## It is in negative which means that we reject 

## Part D 
B_livarea= 10680
B_bath = -15552.44
IncreaseInprice = (B_livarea*2) + (B_bath)*1
print(IncreaseInprice)
## Interval for the price difference goes to 
summary(glht(model, linfct = c("2*livarea + beds=0")))  #### This is gold mine


se_eq1 = 1870

Lo = IncreaseInprice-(1.96*se_eq1)
Hi= IncreaseInprice + (1.96*se_eq1)
print(Lo)
print(Hi)

## The model may not be aboslutely correct as it assumes that relations between important parameteres 
## are simple linear

###################################################################
####################6.16
## Part A
rm(list=ls(all=TRUE))
library(data.table)
library(dplyr)
library(foreign)
library(multcomp)
context = fread('stckton.csv')
head(context)
context1  <- context %>%
             mutate(livareasq=livarea^2) %>%
            mutate(agesq=age^2)

head(context1)
model1 = lm( sprice~livarea+beds+baths+age+livareasq+agesq ,data=context1)
print(summary(model1))


## Part B 
## SSE unrestricted is the equation with all the parameters 
SSE.u = sum((context1$sprice-predict(model1))^2)
print(SSE.u)
## SSE restricted is the equation with less  parameters 
SSE.r = 2.111122e+12
J=2
N=1500
K=7
fvalue = (  (SSE.r-SSE.u)/J  ) /  (  SSE.u/(N-K) )
print(fvalue)
## HO --> b8 =0 and B9 =0
## H1 --> B8 is not 0, B9 is not 0, Or both are not zero
#f_critical value is f(2,infinity) =3
## 18.53 is well above 3 so HO can be rejected It is better to use Un restricted than restricted

## PART C 
## Redoing the parts in question 6.15 but with the new model
## get difference in price with age =2 and age =10 
##marginal effect of sprice with age 
age=8
difference = -830.37 +(14.233*2)*(age)
print(difference)
## -602
## find the SE for the equation    DP/DA = B5+2B7.age
var.b5 = covmat[5,5]
var.b7 = covmat[7,7]
var.b7.b5 = covmat[7,5]

temp= var.b5 +(16*16*var.b7)+(2*2*8*var.b7.b5)
se1 = sqrt(temp)
print(se1)

hi = difference +(1.96*se1)
print(hi)
li = difference -(1.96*se1)
print(li)
 ##
## Find the difference that 200 sqft will make
## marginal effect of sqft 
LVG = 2
diff = 2994.652 + (2*169.092*LVG)
print(diff)
## 3671.02
## H0 -->  DP.DSQ <10,000
## H1 --> DP.DSQ >= 10,000
## calculate DP.DSQ for one sqft (HUNDREDS)
var.b2 = covmat[2,2]
var.b6 = covmat[6,6]
var.b2.b6 = covmat[2,6]

temp1= var.b2 +(4*var.b6)+(2*2*var.b2.b6)
se2 = sqrt(temp1)
print(se2)


## now that SE is calculated now we can calculate the TVAL

LVG = 1
BETA = 2994.652 + (2*169.092*LVG)
Tval = (BETA - 10000)/se2
print(Tval)
## Tc =1.645 and tval is -8.984 meaning that HO cannot be rejected.

############
## Now 200 sqft increase in house LVG and 1 bed added
## DP/DB+DSQ = B2 +2.B6.LVG + B3
LVG = 2
diff1 = 2994.652 + (2*169.092*LVG)+ (-11921.923)
print(diff1)
## -8250

## find intervals with 95%  so t =1.96 
var.b2 = covmat[2,2]
var.b6 = covmat[6,6]
var.b3 =covmat[3,3]
var.b2.b6 = covmat[2,6]
var.b3.b2 =covmat[3,2]
var.b6.b3 =covmat[6,3]

temp2= var.b2 + (16*var.b6) + var.b3 + (8*var.b2.b6) + (2*var.b3.b2) + (8*var.b6.b3)
se3 = sqrt(temp2)
print(se3)

hi = diff1 + (1.96*se3)
print(hi)
li = diff1 -(1.96*se3)
print(li)

###########################################################################################################################################
############### 6.22 ###################################################
rm(list=ls(all=TRUE))
library(data.table)
library(dplyr)
context = fread('pizza.csv')
head(context)
summary(context)
context <-  context %>%
            mutate(AGE_SQ_INCOME= (age^2)*income)
View(context)
model = lm(pizza~age+income+agetimeincome, data=context)
print(summary(model))
SSE.u =   sum( (context$pizza - predict(model))^2 )
finmat = vcov(model)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)   161.46543  120.66341   1.338   0.1892  
#age            -2.97742    3.35210  -0.888   0.3803  
#income          6.97991    2.82277   2.473   0.0183 *
# agetimeincome  -0.12324    0.06672  -1.847   0.0730 .

### Restricted Model  pizza~income
##Part a 
## ho b2=0 , B4=0 

## Restricted model is as follows
model.r = lm(pizza~income, data=context)
print(summary(model.r))
SSE.r =   sum( (context$pizza - predict(model.r))^2 )
print(SSE.r)

## Fcritical value (2,38) = 3.23
J=2
N=40
K=4
Fval = ((SSE.r-SSE.u)/J ) / (SSE.u/(N-K))
print(Fval)
## HO can be rejeceted , meaning that AGE is very important to be used in predicting Pizza

### PART B 
## Find point estimates
## marginal effect of income is 
##DP/DI = Beta(income) + B3.age
age = 20
MPE = 6.9799 +(-0.123*age)
print(MPE)
tc = 2.028
### Find standard error 
var.b3 = finmat[3,3]
var.b4 = finmat[4,4]
var.b2.b3 = finmat[4,3]
temp = var.b3 + (age^2)*var.b4 + (2*age*var.b2.b3)
se = sqrt(temp)
print(se)

hi = MPE + (se*tc)
print(hi)
li = MPE - (se*tc)
print(li)


######### PART C 
head(context)
new_model <- lm(pizza~age+income+agetimeincome+ AGE_SQ_INCOME, data =context)
summary(new_model)
matddd = vcov(new_model)
 
######PART D
### Marginal effect of Price withe income 
# DP/DI = B2 + B3*AGE+ B4.AGE^2
age =20
MPE = 14.0961 + (age*-0.47037) + (age^2)*0.004205
print(MPE)
tc=2.028

var.b3 = matddd[3,3]
var.b4 =matddd[4,4]
var.b5 = matddd[5,5]
var.b3.b4 = matddd[3,4]
var.b4.b5 = matddd[4,5]
var.b5.b3 = matddd[3,5]
temp = var.b3 + (age^2)*var.b4 + (age^4)*var.b5  + (2*age)*var.b3.b4 + 2*(age^3)*var.b4.b5 +2*(age^2)*var.b5.b3
se= sqrt(temp)
print(se)

hi = MPE + (se*tc)
print(hi)
li = MPE - (se*tc)
print(li)

####
tc =  abs(qt(0.05,1000))    ### qt() always gives left sided one tail . So its negative
tc = abs(qt(0.05/2,998))    ## simply adding /2 makes it two sided tail

qf(0.95,3,120)  ## critical value of F. 3 is the 
