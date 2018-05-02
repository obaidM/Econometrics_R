########################################################################################################
## Author:   Obaid Masih
## Econometrics Q 5.12 
## Dataset:  Cocaine data 
##Model: We will use Linear model
## Purpose: Understand how sales of cocaine are influenced
########################################################################################################

## Drop everything
rm(list=ls(all=TRUE))

## Use the data.table library for fast importing and manipulation
library(data.table)

## Read the WAGE1.CSV
context <- fread("coc.csv")
## get the summary of data and get familiar with it 
summary(context)
head(context)

model <- lm(price~quant +qual +trend, data=context)
summary(model)
