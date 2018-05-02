###########################################
## QUESTION 15.6
rm(list=ls(all=TRUE))


#library(multcomp)
library(data.table)
library(dplyr)
library(plotly)
#library(lmtest)
#library(sandwich)
#library(car)
library(AER)
library(plm)

context <- fread("mexican.csv")

## Part a
fixed.model <- plm(lnprice ~bar +street + nocondom +rich +regular +alcohol,
                   data = context,index =c("id", "trans") , model = "within")
# within means that varuation within one individual or entity
summary(fixed.model)
## part(i)

## part b 

random.model <- plm(lnprice ~age+school+attractive+bar +street + nocondom +rich +regular +alcohol,
                    data = context,index =c("id", "trans"), model = "random")
summary(random.model)

##########################
rm(list=ls(all=TRUE))
#library(multcomp)
library(data.table)
library(dplyr)
library(plotly)
#library(lmtest)
#library(sandwich)
#library(car)
library(AER)
library(plm)

context <- fread("crime.csv")
context <- mutate(context,Lcrmrte = log(crmrte))

#(i) deterrence increases, will decrease crime rate 
#(ii) wage increase in the private sector will decrease the crime rate
#(iii) population density increase, will increase the crime rate
#(iv) percentage of young males may increase the crime rate


## part b 

OLS.model <- lm( Lcrmrte ~ log(prbarr) + log(prbconv)+ log(prbpris) + log(avgsen)+ log(wmfg), data=context)
summary(OLS.model)

## part c 
FE.model <- plm( Lcrmrte ~ log(prbarr) + log(prbconv)+ log(prbpris) + log(avgsen)+ log(wmfg), data=context, 
                index=c("county","year"), model="within")
summary(FE.model)


## part d 
# Unrestricted model will be FE model 
# restricted model will be Pooled model 
SSE.U =   16.149

pooled.model <- plm( Lcrmrte ~ log(prbarr) + log(prbconv)+ log(prbpris) + log(avgsen)+ log(wmfg), data=context, 
                  model="pooling")
summary(pooled.model)

SSE.R =   106.81



 n = 630 
 k = 95
## J = 89 because there are 50 more variables in FE the Unrestricted model
fVAL = ((SSE.R - SSE.U)/89)/ (SSE.U/(n-k))
fc <-  qf(0.95,89,535)


## part e 
## lets compare OLS model with FE model with dummy variables 

OLS.model <- lm( Lcrmrte ~ log(prbarr) + log(prbconv)+ log(prbpris) + log(avgsen)+ log(wmfg)
                 + ldensity + lpctymle+ d82 +d83 + d84 + d85+ d86 + d87, data=context)
summary(OLS.model)

FE.model <- plm( Lcrmrte ~ log(prbarr) + log(prbconv)+ log(prbpris) + log(avgsen)+ log(wmfg)
                 + ldensity + lpctymle+ d82 +d83 + d84 + d85+ d86 + d87, data=context, index=c("county","year"),
                model="within" )
summary(FE.model)

#(ii)
## compare OLS models with and without dummy variables 

U.OLS.model <- lm( Lcrmrte ~ log(prbarr) + log(prbconv)+ log(prbpris) + log(avgsen)+ log(wmfg)
                 + ldensity + lpctymle+ d82 +d83 + d84 + d85+ d86 + d87, data=context)

SSE.U = sum( (U.OLS.model$residuals)^2)


R.OLS.model <- lm( Lcrmrte ~ log(prbarr) + log(prbconv)+ log(prbpris) + log(avgsen)+ log(wmfg)
                 + ldensity + lpctymle, data=context)

SSE.R = sum( (R.OLS.model$residuals)^2)

# restricted model is OLS with dummy variables 

n = 630 
k = 14
## J = 6
fVAL = ((SSE.R - SSE.U)/6)/ (SSE.U/(n-k))
fc <-  qf(0.95,6,616)

## compare FE models with and without dummy variables 

U.FE.model <- plm( Lcrmrte ~ log(prbarr) + log(prbconv)+ log(prbpris) + log(avgsen)+ log(wmfg)
                   + ldensity + lpctymle+ d82 +d83 + d84 + d85+ d86 + d87,
                   data=context, index=c("county","year"), model = "within")
summary(U.FE.model)

SSE.U = 14.383

R.FE.model <- plm( Lcrmrte ~ log(prbarr) + log(prbconv)+ log(prbpris) + log(avgsen)+ log(wmfg)
                   + ldensity + lpctymle,
                   data=context, index=c("county","year"), model = "within")
summary(R.FE.model)

SSE.R = 15.876


n = 630 
k = 14
## J = 6 
fVAL = ((SSE.R - SSE.U)/6)/ (SSE.U/(n-k))
fc <-  qf(0.95,6,616)

