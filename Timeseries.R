
### QUESTION 9.2

rm(list=ls(all=TRUE))


library(multcomp)
library(data.table)
library(dplyr)
library(lmtest)
library(tseries)
library(dynlm)

context = fread('ex9.csv')


#table <- read.dta13("okun.dta")
#check.ts <- is.ts(table) # "is structured as time series?"
#okun.ts <- ts(table, start=c(1985,2), end=c(2009,3),frequency=4)
#okunL3.dyn <- dynlm(d(u)~L(g, 0:3), data=okun.ts)  # g is explanotary variable and u is teh column we tryin to preict
#summary(okunL3.dyn)

table <- context
check.ts <- is.ts(table) # "is structured as time series?"
table.ts <- ts(table, start=c(2008,1),frequency=7)
check.ts <- is.ts(table.ts)
model.dyn <- dynlm(sales~L(adv, 0:2), data=table.ts)  # g is explanotary variable and u is teh column we tryin to preict
summary(model.dyn)
apply(table.ts, MARGIN = 2, class)

## 95% interval for impact multiplier 
# tc = 1.96
# sqrt of  var(adv t) + var( adv t-1) + 2cov(advt , advt-1)

se = sqrt(1.3946)
beta = 1.842 
Lo = beta-(1.96*se) # -0.472
Hi= beta+ (1.96*se) # 4.15


## 95% interval for one period interim multiplier
# tc = 1.96


se = sqrt(1.3946 + 2.1606 + 2 *(-1.0406)  )
beta = 1.842 + 3.802
Lo = beta-(1.96*se)  #  3.263
Hi= beta+ (1.96*se)   # 8.02

## 95% interval for two period interim multiplier
# tc = 1.96
#     = a2 + b2 + c2 + 2ab + 2bc + 2ca
#     var. a + var b + var c + 2cov(a,b) + 2cov(b,c)+ 2 cov(c,a)
se = sqrt(1.3946 + 2.1606 + 1.414 +  2 *(-1.0406) + 2*(-1.0367) + 2*(0.098) )
beta = 1.842 + 3.802 + 2.265
Lo = beta-(1.96*se)  #  5.9386
Hi= beta+ (1.96*se)   # 9.8793


### QUESTION 9.22 

rm(list=ls(all=TRUE))


library(multcomp)
library(data.table)
library(dplyr)
library(lmtest)
library(tseries)
library(dynlm)
library(forecast)

context = fread('consumption.csv')
# income growth 
# consumption growth 

context = context[ 4:200, c("incgwth","congwth")]

table <- context
check.ts <- is.ts(table) # "is structured as time series?"
table.ts <- ts(table, start=c(1960,4),frequency=4)
check.ts <- is.ts(table.ts)
model.dyn <- dynlm(congwth~L(incgwth, 0), data=table.ts)  
summary(model.dyn)
plot <- ggAcf(model.dyn$residuals,lag.max = 20)
plot
AIC(model.dyn)
BIC(model.dyn)
# part c  

table <- context
check.ts <- is.ts(table) # "is structured as time series?"
table.ts <- ts(table, start=c(1960,4),frequency=4)
check.ts <- is.ts(table.ts)
model.dyn <- dynlm(congwth~L(congwth, 1) + L(incgwth, 0), data=table.ts)  
summary(model.dyn)
plot <- ggAcf(model.dyn$residuals,lag.max = 20)
plot
AIC(model.dyn)
BIC(model.dyn)

# part d 

table <- context
check.ts <- is.ts(table) # "is structured as time series?"
table.ts <- ts(table, start=c(1960,4),frequency=4)
check.ts <- is.ts(table.ts)
model.dyn <- dynlm(congwth~L(congwth, 1)+L(congwth, 2) + L(incgwth, 0), data=table.ts)  
summary(model.dyn)
plot <- ggAcf(model.dyn$residuals,lag.max = 20)
plot
AIC(model.dyn)
BIC(model.dyn)

# part e 

table <- context
check.ts <- is.ts(table) # "is structured as time series?"
table.ts <- ts(table, start=c(1960,4),frequency=4)
check.ts <- is.ts(table.ts)
model.dyn <- dynlm(congwth~L(congwth, 1)+L(congwth, 2) + L(incgwth, 0) + L(incgwth, 1)  , data=table.ts)  
summary(model.dyn)
plot <- ggAcf(model.dyn$residuals,lag.max = 20)
plot
AIC(model.dyn)
BIC(model.dyn)

# part f  add congwth at lag 3 

table <- context
check.ts <- is.ts(table) # "is structured as time series?"
table.ts <- ts(table, start=c(1960,4),frequency=4)
check.ts <- is.ts(table.ts)
model.dyn <- dynlm(congwth~L(congwth, 1)+L(congwth, 2) + L(congwth, 3) +  L(incgwth, 0) + L(incgwth, 1)  , data=table.ts)  
summary(model.dyn)
plot <- ggAcf(model.dyn$residuals,lag.max = 20)
plot
AIC(model.dyn)
BIC(model.dyn)


# part g drop congwth t-1 from part e 

table <- context
check.ts <- is.ts(table) # "is structured as time series?"
table.ts <- ts(table, start=c(1960,4),frequency=4)
check.ts <- is.ts(table.ts)
model.dyn <- dynlm(congwth~ L(congwth, 2) + L(incgwth, 0) + L(incgwth, 1)  , data=table.ts)  
summary(model.dyn)
plot <- ggAcf(model.dyn$residuals,lag.max = 20)
plot
AIC(model.dyn)
BIC(model.dyn)
