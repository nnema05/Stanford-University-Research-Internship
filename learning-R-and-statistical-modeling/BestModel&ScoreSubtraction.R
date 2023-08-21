#WHICH MODEL IS BEST AND SCORE SUBTRACTION 

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


#####STEP 1: ANOVA
#Are the models signifcantly different?

#H0: Configural does not fit worse than weak
anova(configural_fit1, weak_fit1)
#H0: Weak does not fit worse than strong 
anova(weak_fit1, strong_fit1)
#H0: Strong does not fit worse than strict 
anova(strong_fit1, strict_fit1)

#H0: Configural does not fit worse than weak
anova(configural_fit2, weak_fit2)
#H0: Weak does not fit worse than strong
anova(weak_fit2, strong_fit2)
#H0: Strong does not fit worse than strict
anova(strong_fit2, strict_fit2)

#####STEP 2: FIT MEASURES
round(cbind(configural1=inspect(configural_fit1, 'fit.measures'), 
            weak1=inspect(weak_fit1, 'fit.measures'),
            strong1=inspect(strong_fit1, 'fit.measures'),
            strict1=inspect(strict_fit1, 'fit.measures'),
            configural2=inspect(configural_fit2, 'fit.measures'), 
            weak2=inspect(weak_fit2, 'fit.measures'),
            strong2=inspect(strong_fit2, 'fit.measures'),
            strict2=inspect(strict_fit2, 'fit.measures')
            ),3)

##Which one fits the best based on CFI, RMSEA?

#####STEP 3: LOADINGS AND SCORES, BASED ON WHAT FITS 
#Factor Loadings
inspect(strict_fit1 ,what="std")$lambda
inspect(strict_fit2 ,what="std")$lambda
inspect(configural_fit1 ,what="std")$lambda
inspect(configural_fit2 ,what="std")$lambda
inspect(strong_fit1 ,what="std")$lambda
inspect(strong_fit2 ,what="std")$lambda
inspect(weak_fit1 ,what="std")$lambda
inspect(weak_fit2 ,what="std")$lambda

#Factor Scores
strict1_scores <- lavPredict(strict_fit1)
strict2_scores <- lavPredict(strict_fit2)
configural1_scores <- lavPredict(configural_fit1)
configural2_scores <- lavPredict(configural_fit2)
strong1_scores <- lavPredict(strong_fit1)
strong2_scores <- lavPredict(strong_fit2)
weak1_scores <- lavPredict(weak_fit1)
weak2_scores <- lavPredict(weak_fit2)

#####STEP 3: SUBTRACTION (SCORE COMPARISON)
#Will be written once we receive scores and the format and the best model 

#create a df where we added subject keys and scores filtering on starts with base
#then starts with one
#then start with two




