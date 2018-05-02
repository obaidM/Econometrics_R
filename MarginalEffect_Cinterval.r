########################################################################################################
## Author:   Obaid Masih
## Econometrics Q 5.13
## Dataset:  housing data from Baton rouge 
##Model: We will use multiple Linear model
## Purpose: Understand how sales of housing is influenced
########################################################################################################

## Drop everything
rm(list=ls(all=TRUE))

## Use the data.table library for fast importing and manipulation
library(data.table)

## Read the data
context <- fread("BR2.csv")
## get the summary of data and get familiar with it 
summary(context)
head(context)


## part a 
model <- lm(price~sqft+age, data=context)
summary(model)

# 95% INTERVAL FOR b1 
Inter1 <- 90.97 + (2.403*1.96)
print(Inter1)
Inter2 <- 90.97 -(2.403*1.96)
print(Inter2)

## HO is B3 >-1000  and H1 B3 < 1000
## critical t value is -1.645

tvalhO <-(-755.041 - (-1000) )/140.894
print(tvalhO)
## val is 1.7386
## val is more than -1.645 so HO cannot be rejected

#########PART B 
sqsqft= context$sqft^2
sqage = context$age^2
model2 <- lm(price~sqft+age +sqsqft + sqage, data=context)
summary(model2)

## find marginal effect Price / SQ 
##  B2 + 2*B3*SQFT
SQFT = min(context$sqft)
mareff <- -55.78 +(2*0.02315*SQFT)
print(mareff)
## -25.129
SQFT = max(context$sqft)
mareff <- -55.78 +(2*0.02315*SQFT)
print(mareff)
##309.8511
SQFT = 2300
mareff <- -55.78 +(2*0.02315*SQFT)
print(mareff)
## 50.71

## find marginal effect Price / age 
##  B3 + 2*B5*AGE
AGE = min(context$age)
mareff <- -2798 +(2*30.16*AGE)
print(mareff)
## -2737.68
AGE = max(context$age)
mareff <- -2798 +(2*30.16*AGE)
print(mareff)
## 2027
AGE = 20
mareff <- -2798 +(2*30.16*AGE)
print(mareff)
## -1591.6

# iii
##  B2 + 2*B3*SQFT
SQFT = 2300
mareff <- -55.78 +(2*0.02315*SQFT)
print(mareff)
## 50.71
## find standard error for (B2 + 2b3*SQFT)
sum_model2 = vcov(model2)

var1 = sum_model2[2,2]
print(var1)
var2 = sum_model2[4,4]
print(var2)
var3 = sum_model2[4,2]
print(var3)
temp = var1 + 4600*4600*var2 +2*4600*var3
SE <- sqrt(temp)
print(SE)

Inter1 = mareff + SE*1.96
print(Inter1)
Inter2 = mareff - SE*1.96
print(Inter2)

###############################################
### D Price /D Age  = B3 + a* B5 * AGE
AGE = 20
mareff <- -2798 +(2*30.16*AGE)
print(mareff)
## -1591.6
## HO   D price / D age >= -1000 
### H1  D Price /D age < -1000

## critical t value is -1.645

## find SE(B3 +2*20*B5)
## again use sum_model2[]
var1 = sum_model2[3,3]
print(var1)
var2 = sum_model2[5,5]
print(var2)
var3 = sum_model2[5,3]
print(var3)
temp = var1 + 1600*var2 +2*40*var3
SE <- sqrt(temp)
print(SE)
tvalho <- (-1591.6 + 1000)/SE
print(tvalho)
## -2.13394
## HO can be rejecetd since t 

####################################################
############ Part c 

sq_age = context$sqft * context$age
model3 <- lm(price~sqft+age +sqsqft + sqage + sq_age, data=context)
summary(model3)
sum_model3 = vcov(model3)

### part (i)
## Marginal effect of DW/SSQT 
## B2 + 2B4+ AGE*B6
## PART (i)
MIN_SQFT = min(context$sqft)
AGE=20
me1 <- -30.7 + 2*0.02218*MIN_SQFT +  AGE*(-0.93)
print(me1)
## -19.93

MAX_SQFT = max(context$sqft)
AGE=20
me1 <- -30.7 + 2*0.02218*MAX_SQFT +  AGE*(-0.93)
print(me1)
## 301.01

Val_SQFT = 2300 
AGE=20
me1 <- -30.7 + 2*0.02218*Val_SQFT +  AGE*(-0.93)
print(me1)
##52.728

##### pART (ii)
### part (i)
## Marginal effect of D W/D AGE 
## B3 + 2B5 + B6*SQFT
SQFT = 2300
min_age = min(context$age)
me2 = -442 + (2*26.52*min_age) - (0.93*SQFT)
print(me2)
### -2527

SQFT = 2300
max_age = max(context$age)
me2 = -442 + (2*26.52*max_age) -0.93*SQFT
print(me2)
### 1662

SQFT = 2300
val_age = 20
me2 = -442 + (2*26.52*val_age) -0.93*SQFT
print(me2)
##-1520

### Part (iii)
## Marginal effect of DW/SSQT 
## B2 + 2B4 SQFT+ AGE*B6

Val_SQFT = 2300 
AGE=20
me1 <- -30.7 + 2*0.02218*Val_SQFT +  AGE*(-0.93)
print(me1)
## 52.728

## find standard error to find the interval
## get the equation in the format of Betas only 
## ( B2 +4600B4+20B6)^2
##  B2.b2 + 4600B4.B2 + 20.b6.b2 + 4600b4.B2 + 4600*4600*B4.B4 + 4600.B4.20.B6
##  20B6.B2 + 20.4600.B4.B6 + 20.20.b6.b6
##--- concise equation becomes
##  Var(B2) + 9200.cov(B4,B2) + 40.COV(B6,B2) + (4600)^2.VAR(B4) + 2.20.4600. COV(B4,B6)
### + 400.var(b6)
varB2 = sum_model3[2,2]
print(varB2)
varb4 = sum_model3[4,4]
print(varb4)
varB6 = sum_model3[6,6]
print(varB6)
covB4.B2 = sum_model3[4,2]
print(covB4.B2)
covb6.b2 = sum_model3[6,2]
print(covb6.b2)
covb4.b6 = sum_model3[6,4]
print(covb4.b6)
temp = varB2 + 400*varB6 + 4600*4600*varb4 + 2*4600*covB4.B2 + 40*covb6.b2+ 20*4600*covb4.b6
SE1 <- sqrt(temp)
print(SE1)
## 95% INTERVAL 
i1 = me1 + SE1*1.96
print(i1)
i2 = me1 - SE1*1.96
print(i2)

#######Part (iv)
### D pRICE / D  age 
## Marginal effect of D W/D AGE 
## B3 + 2B5 + B6*SQFT
SQFT = 2300
age = 20
me2 = -442 + (2*26.52*age) -(0.93*SQFT)
print(me2)
### H0 is D price /D age >= -1000
### H1 is D price /D age < - 1000
#### Find the standard error for ( B3 + 2*20*B5+2300*B6)
### we will use sum_model3 to find variances and covariances 
varB3 =  sum_model3[3,3]
print(varB3)
varB5 = sum_model3[5,5]
print(varB5)
varB6 = sum_model3[6,6]
print(varB6)
covB3.B5 =   sum_model3[3,5]
print(covB3.B5)
covB3.B6 = sum_model3[3,6]
covB5.b6 =  sum_model3[5,6]
#### finding stanndard error of (B3 +40B5 +2300B6)^2
te <- varB3 + 40*40*varB5 + 2300*2300*varB6 + 80*covB3.B5 + 4600*covB3.B6 + 80*2300*covB5.b6
SE3 = sqrt(te)
print(SE3)

### finding t val \
k = -1000
tval3 = (me2 - k)/SE3
print(tval3)

