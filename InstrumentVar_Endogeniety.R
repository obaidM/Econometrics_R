###########################################
## QUESTION 8.12
rm(list=ls(all=TRUE))


#library(multcomp)
library(data.table)
library(dplyr)
library(plotly)
#library(lmtest)
#library(sandwich)
#library(car)
library(AER)

context <- fread("ivreg2.csv")

# part b   use b1 = 3 and b2 = 1  find e 

context<- mutate(context, y_pred= 3+x )
context <- mutate(context, e= y - y_pred)

cor(context$x,context$e)

## part c  plot the graphs 

plot_ly(context)%>%
add_trace(x = ~x, y = ~y_pred, type = 'scatter', mode = 'lines', line = list(color = 'red')) %>%
add_trace(x=~x, y = ~y)  

## part d 

# N=10 
context10 <- context[0:10,]
model10 <- lm(y~x,context10)
summary(model10)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.7775     0.3608   7.698 5.76e-05 ***
#  x             1.3722     0.1727   7.945 4.59e-05 ***

# N=20 

context20 <- context[0:20,]
model20 <- lm(y~x,context20)
summary(model20)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.0169     0.2036   14.81 1.59e-11 ***
#  x             1.3876     0.1211   11.46 1.05e-09 ***

# N=100 

context100 <- context[0:100,]
model100 <- lm(y~x,context100)
summary(model100)

#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.00783    0.07872   38.21   <2e-16 ***
#  x            1.40164    0.05330   26.30   <2e-16 ***

# N=500 

context500 <- context[0:500,]
model500 <- lm(y~x,context500)
summary(model500)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.01825    0.03410    88.5   <2e-16 ***
#  x            1.45352    0.02367    61.4   <2e-16 ***

# part e 

cor(context$x,context$z1)
# 0.6208
cor(context$z1,context$e)
# -0.003447192

cor(context$x,context$z2)
# 0.289
cor(context$z2,context$e)
#0.02777

## Z1 is  a better choice

## Part f lets use z1 as the instrumental variable 
#################################################
#################################################
# USE Z1 as instrumental variable 


# n=10 
modelz1 <- ivreg(y~x|z1,data=context[0:10,])
summary(modelz1)

# n=20
modelz1 <- ivreg(y~x|z1,data=context[0:20,])
summary(modelz1)

# n=100

modelz1 <- ivreg(y~x|z1,data=context[0:100,])
summary(modelz1)

#n=500
modelz1 <- ivreg(y~x|z1,data=context[0:500,])
summary(modelz1)

##################
#part f 
###  use z2 as the instrument variable 

# n=10 
modelz2 <- ivreg(y~x|z2, data = context[0:10,])
summary(modelz2)

# n=20
modelz2 <- ivreg(y~x|z2, data = context[0:20,])
summary(modelz2)

# n=100
modelz2 <- ivreg(y~x|z2, data = context[0:100,])
summary(modelz2)

# n=500 
modelz2 <- ivreg(y~x|z2, data = context[0:500,])
summary(modelz2)

##############################
# part h 
### using z1 and z2 as the instrument variable 

# n=10
modelz1.z2 <- ivreg(y~x|z1+z2, data = context[0:10,])
summary(modelz1.z2)

# n=20
modelz1.z2 <- ivreg(y~x|z1+z2, data = context[0:20,])
summary(modelz1.z2)

# n=100
modelz1.z2 <- ivreg(y~x|z1+z2, data = context[0:100,])
summary(modelz1.z2)

# n=500 
modelz1.z2 <- ivreg(y~x|z1+z2, data = context[0:500,])
summary(modelz1.z2)



