###########################################
## QUESTION 7.15 
rm(list=ls(all=TRUE))


library(foreign)
library(multcomp)
library(data.table)
library(dplyr)
library(plotly)
library(car)

context = fread('BR2.csv')
View(context)


###PART A
summary(context)
plot_ly(context,x= ~price,type ="histogram")

###PART B

context <- mutate(context,price=log(price/1000))
context <- mutate(context,sqft=sqft/100)

model1 = lm( price~sqft+bedrooms+baths+age+owner+pool+traditional+fireplace+waterfront,data=context)
print(summary(model1))
SSE =   sum((context$price-predict(model1))^2)
###PART C 

context <- mutate(context,trad_waterfront = traditional*waterfront)
model2 = lm( price~sqft+bedrooms+baths+age+owner+pool+traditional+fireplace+waterfront +trad_waterfront,data=context)
print(summary(model2))

## PART D 
## model 3 becomes the restricted model 
model3 <- lm( price~sqft+bedrooms+baths+age+owner+pool+fireplace+waterfront ,data=context)
print(summary(model3))
SSE.R=   sum((context$price-predict(model3))^2)

### model 4 becomes the unrestricted model
context <- mutate(context,trad_sqft = sqft*traditional)
context <-  mutate(context,trad_beds = bedrooms*traditional)
context <-  mutate(context,trad_baths = baths*traditional)
context <-  mutate(context,trad_age = age*traditional)
context <-  mutate(context,trad_owner = owner*traditional)
context <-  mutate(context,trad_pool = pool*traditional)
context <-  mutate(context,trad_fireplace = fireplace*traditional)



model4 <- lm( price~sqft+bedrooms+baths+age+owner+pool+fireplace+waterfront + traditional + trad_waterfront +
              trad_sqft + trad_beds + trad_baths + trad_age + trad_owner + trad_pool + trad_fireplace,
              data=context)
print(summary(model4))
SSE.U =   sum((context$price-predict(model4))^2)

## HO will bE THAT all variables multiplied with traditional are 0
## H1 will be that atleast one of the estimaors ( multilplied with tradition is not 0)


## n =1080 observations
## k = 19
## J = 9
fVAL = ((SSE.R - SSE.U)/9)/ (SSE.U/1061)
fc <-  qf(0.95,9,1061)
## HO can be rejected , so now we know THAT traditional should be included in the model

### PART E 

## use model 2 from part c
sqft= 0.0300308
age =-0.0061470 
owner = 0.0683701
fireplace = 0.0873139
trad = -0.0449127
beds = -0.0313330
baths = 0.1882577
Price_partE = 3.971111 + sqft*25 + age*20 + owner + fireplace +trad +beds*3 + baths*2
## once convreted it is 147,000

###########################################
## QUESTION 7.16
##########################################
rm(list=ls(all=TRUE))


library(foreign)
library(multcomp)
library(data.table)
library(dplyr)
library(plotly)

context = fread('stckton.csv')
View(context)

## part A 

plot_ly(context,x= ~sprice,type ="histogram")

plot_ly(context,x= ~log(sprice),type ="histogram")
## PART B 

context <- mutate(context,sprice=log(sprice/1000))

model1 = lm( sprice~livarea + beds+baths+lgelot+age+pool + age,data=context)
print(summary(model1))
SSE =   sum((context$sprice-predict(model1))^2)

## part c 
## if they are then increases the price by 4.83%

## part d 

context <- mutate(context,lge_livarea=lgelot*livarea)

model2 = lm( sprice~livarea + beds+baths+lgelot + age +pool +age+ lge_livarea ,data=context)
print(summary(model2))
SSE =   sum((context$sprice-predict(model2))^2)

## -0.428% decrease because at that point livaera is giving diminshing returns

## PART E Carry out the Chow test
## Restricted model
model3 <- lm( sprice~livarea + beds+baths + age +pool +age ,data=context)
print(summary(model3))
SSE.R=   sum((context$sprice-predict(model3))^2)

### model 4 becomes the unrestricted model


model4 <- lm( sprice~livarea + beds+baths+lgelot + age +pool +age +(lgelot*livarea)
              + (lgelot*beds)+(lgelot*baths)+(lgelot*age)+(lgelot*pool),data=context)
print(summary(model4))
SSE.U=   sum((context$sprice-predict(model4))^2)


## HO will be that Beta(lgelot and lge_livarea ) are 0
## H1 not 0 , both are not 0


## n = 1500 observations
## k = 13
## J =6
fVAL = ((SSE.R - SSE.U)/6 )/ (SSE.U/1487)
fc <-  qf(0.95,6,1487)
## HO can be rejected , so now we know THAT traditional should be included in the model
