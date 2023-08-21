#Sample ABCD Behavior Regression - goal 3

####STEP 0: INSTALL LIBRARIES 
install.packages("tidyverse")
library(tidyverse)
install.packages("psych")
library(psych)
install.packages("GPArotation")
library(GPArotation)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot) #plotting correlation matrices
install.packages("lavaan")
library(lavaan)  #for fitting structural equation models
install.packages("semPlot")
library(semPlot)  #for automatically making diagrams
install.packages("sqldf")
library(sqldf)
install.packages("writexl")
library(writexl)
install.packages("fastDummies")
library(fastDummies)
install.packages("recipes")
library(recipes)

####STEP 1: CREATE DATA FRAME WITH SUBJECT KEY, DIFFERENCES AND ALL BEHAVIOR PREDICTORS


#####STEP 2: USE IQ TO PREDICT 

IQFactor1Lm <- lm(changeinfactor1 ~ IQ, data = )
IQFactor2Lm <- lm(changeinfactor2 ~ IQ, data = )
IQFactor3Lm <- lm(changeinfactor3 ~ IQ, data = )
IQFactor4Lm <- lm(changeinfactor4 ~ IQ, data = )
IQFactor5Lm <- lm(changeinfactor5 ~ IQ, data = )

summary(IQFactor1Lm)
#coefficents: P-value: #R-squared:
summary(IQFactor2Lm)
#coefficents: P-value: #R-squared:
summary(IQFactor3Lm)
#coefficents: P-value: #R-squared:
summary(IQFactor4Lm)
#coefficents: P-value: #R-squared:
summary(IQFactor5Lm)
#coefficents: P-value: #R-squared:


#####STEP 3: USE SOCIOECONOMIC STATUS (SE) TO PREDICT
SEFactor1Lm <- lm(changeinfactor1 ~ SE, data = )
SEFactor2Lm <- lm(changeinfactor2 ~ SE, data = )
SEFactor3Lm <- lm(changeinfactor3 ~ SE, data = )
SEFactor4Lm <- lm(changeinfactor4 ~ SE, data = )
SEFactor5Lm <- lm(changeinfactor5 ~ SE, data = )

summary(SEFactor1Lm)
#coefficents: P-value: #R-squared:
summary(SEFactor2Lm)
#coefficents: P-value: #R-squared:
summary(SEFactor3Lm)
#coefficents: P-value: #R-squared:
summary(SEFactor4Lm)
#coefficents: P-value: #R-squared:
summary(SEFactor5Lm)
#coefficents: P-value: #R-squared:


#####STEP 4: USE GENDER
##Create Dummy variable if needed so remove comment 
#df$Male <- ifelse(df$sex == 'male', 1, 0) 
#df$Female <- ifelse(df$sex == 'female', 1, 0)

##Otherwise, R should do so automatically 
GenderFactor1Lm <- lm(changeinfactor1 ~ sex, data = )
GenderFactor2Lm <- lm(changeinfactor2 ~ sex, data = )
GenderFactor3Lm <- lm(changeinfactor3 ~ sex, data = )
GenderFactor4Lm <- lm(changeinfactor4 ~ sex, data = )
GenderFactor5Lm <- lm(changeinfactor5 ~ sex, data = )

summary(GenderFactor1Lm)
#coefficents: P-value: #R-squared:
summary(GenderFactor2Lm)
#coefficents: P-value: #R-squared:
summary(GenderFactor3Lm)
#coefficents: P-value: #R-squared:
summary(GenderFactor4Lm)
#coefficents: P-value: #R-squared:
summary(GenderFactor5Lm)
#coefficents: P-value: #R-squared:


####STEP 5: USE PUBERTY 
##Check if the puberty is a factor and make it a factor 
is.factor(data$puberty) #change accordingly to the data
#If not 
data$pubertyF <- factor(data$puberty)
#Check with...
data$pubertyF[1:15]

##Predict with linear regression 
PubertyFactor1Lm <- lm(changeinfactor1 ~ pubertyF, data = )
PubertyFactor2Lm <- lm(changeinfactor2 ~ pubertyF, data = )
PubertyFactor3Lm <- lm(changeinfactor3 ~ pubertyF, data = )
PubertyFactor4Lm <- lm(changeinfactor4 ~ pubertyF, data = )
PubertyFactor5Lm <- lm(changeinfactor5 ~ pubertyF, data = )

summary(PubertyFactor1Lm)
#coefficents: P-value: #R-squared:
summary(PubertyFactor2Lm)
#coefficents: P-value: #R-squared:
summary(PubertyFactor3Lm)
#coefficents: P-value: #R-squared:
summary(PubertyFactor4Lm)
#coefficents: P-value: #R-squared:
summary(PubertyFactor5Lm)
#coefficents: P-value: #R-squared:










