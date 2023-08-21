#Sample ABCD - Goal #1

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
install.packages("psychTools")
library(psychTools)


#####STEP 1. PARALLEL ANALYSIS, PCA AND EFA IN BASELINE

####STEP 1a. PARALLEL ANALYSIS IN BASELINE
#To run, data must be numeric. Out of 107 columns, 5:97, 99:107 are the numeric columns 
#Remove the columns that are not numeric 
baselineDataNumeric <- select(baselineData, c(5:97, 99:107)) 

#Remove Comment below code if there is a need to troubleshoot
#str(baselineDataNumeric) 
#head(baselineDataNumeric)
#view(baselineDataNumeric) 

##Run parallel analysis  
#Use fa = pc because the article recommends finding principal components (pc) instead of principal factors (fa) 
baselineParallel <- fa.parallel(baselineDataNumeric, fa = "pc") 



####STEP 1b PCA (delineating paper stated to empirically extract with PCA) IN BASELINE
baselinePca <- princomp(baselineDataNumeric) 

#Assumption: parallel analysis gave me 16 factors, PCA gave me 5 based on delineating paper 


####STEP 1c: EFA FOR EACH FACTOR, EXTRACT AND ROTATE  IN BASELINE

#FIRST FACTOR
#Use fm = ml or maximum likelihood because this is the correct factor analysis for normally distributed data
baselineFa1 <- fa(r = baselineDataNumeric, 
                    nfactors = 1, 
                    fm = "ml",
                    residuals = TRUE) 

#evaluate loadings (need to be presence of >3 clear primary loading, greater then .1 for each factor)
#save scores for each successive number of components 
baselineFa1Scores <- baselineFa1$scores

#TWO FACTORS (now rotate with geominT)
#Rotate with geomin as stated in Delineating paper
#Use geominT instead of geominQ because geominT is orthogonal and the Goldberg method uses orthoganol rotations
baselineFa2 <- fa(r = baselineDataNumeric,
                    nfactors = 2,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE) 
baselineFa2Scores <- baselineFa2$scores

#THREE FACTORS 
baselineFa3 <- fa(r = baselineDataNumeric,
                    nfactors = 3,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa3Scores <- baselineFa3$scores

#FOUR FACTORS 
baselineFa4 <- fa(r = baselineDataNumeric,
                    nfactors = 4,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa4Scores <- baselineFa4$scores

#FIVE FACTORS
baselineFa5 <- fa(r = baselineDataNumeric,
                    nfactors = 5,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa5scores <- baselineFa5$scores

#SIX FACTORS  
baselineFa6 <- fa(r = baselineDataNumeric,
                    nfactors = 6,
                    fm = "ml",
                    rotate = "geominT",
                    residuals = TRUE)
baselinefa6Scores <- baselineFa6$scores
#Assumption that at 6 factors and beyond it is no longer interpretable

#Names of 5 factors based on loadings: 



#####STEP 2: CORRELATION ANALYSIS IN BASELINE
#correlation between factor scores starting from the first factor at the top all the way to the last factor

#1 and 2 scores 
baselineCor1_2 <- cor.test(baselinefa1Scores, baselinefa2Scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#2 and 3 scores
baselineCor2_3 <- cor.test(baselinefa2Scores, baselinefa3Scores,
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#3 and 4 scores
baselineCor3_4 <- cor.test(baselinefa3Scores, baselinefa4Scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#4 and 5 scores 
baselineCor4_5 <- cor.test(baselinefa4Scores, baselinefa5Scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

##ARE THESE FACTORS SIMILAR TO THE FACTORS FROM THE DELIENTATING STUDY?


######STEP 3: PARALLEL ANALYSIS, PCA AND EFA IN TIMELINE 1
#To run, data must be numeric. Out of 107 columns, 5:97, 99:107 are the numeric columns 
#Remove the columns that are not numeric 
oneyearDataNumeric <- select(oneyearData, c(5:97, 99:107)) 

#Remove Comment below code if there is a need to troubleshoot
#str(oneyearDataNumeric) 
#head(oneyearDataNumeric)
#view(oneyearDataNumeric) 

##Run parallel analysis  
#Use fa = pc because the article recommends finding principal components (pc) instead of principal factors (fa) 
oneyearParallel <- fa.parallel(oneyearDataNumeric, fa = "pc")

#Run PCA
oneyearPca <- princomp(oneyearDataNumeric) 
#Assumption: parallel analysis gave me 16 factors, PCA gave me 5 based on delineating paper

#EFA
#FIRST FACTOR
oneyearFa1 <- fa(r = oneyearDataNumeric, 
                   nfactors = 1, 
                   fm = "ml",
                   residuals = TRUE) 

#evaluate loadings (need to be presence of >3 clear primary loading, greater then .1 for each factor)
#save scores for each successive number of components 
oneyearfa1Scores <- oneyearFa1$scores

#TWO FACTORS (now rotate with geomin)
oneyearFa2 <- fa(r = oneyearDataNumeric,
                   nfactors = 2,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE) 
oneyearfa2Scores <- oneyearFa2$scores

#THREE FACTORS 
oneyearFa3 <- fa(r = oneyearDataNumeric,
                   nfactors = 3,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa3Scores <- oneyearFa3$scores

#FOUR FACTORS 
oneyearFa4 <- fa(r = oneyearDataNumeric,
                   nfactors = 4,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa4Scores <- oneyearFa4$scores

#FIVE FACTORS
oneyearFa5 <- fa(r = oneyearDataNumeric,
                   nfactors = 5,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa5scores <- oneyearFa5$scores

#SIX FACTORS  
oneyearFa6 <- fa(r = oneyearDataNumeric,
                   nfactors = 6,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa6Scores <- oneyearFa6$scores
#Assumption that at 6 factors and beyond it is no longer interpretable

#Names of 5 factors based on loadings: 


######STEP 4 CORRELATION ANALYSIS IN TIMELINE 1
#correlation between factor scores starting from the first factor at the top all the way to the last factor

#1 and 2 scores 
oneyearCor1_2 <- cor.test(oneyearfa2Scores, oneyearfa2Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#2 and 3 scores
oneyearCor2_3 <- cor.test(oneyearfa2Scores, oneyearfa3Scores,
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#3 and 4 scores
oneyearCor3_4 <- cor.test(oneyearfa3Scores, oneyearfa4Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#4 and 5 scores 
oneyearCor4_5 <- cor.test(oneyearfa4Scores, oneyearfa5Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:


######STEP 5: PARALLEL ANALYSIS, PCA AND EFA IN TIMELINE 2
#To run, data must be numeric. Out of 107 columns, 5:97, 99:107 are the numeric columns 
#Remove the columns that are not numeric 
twoyearDataNumeric <- select(twoyearData, c(5:97, 99:107)) 

#Remove Comment below code if there is a need to troubleshoot
#str(twoyearDataNumeric) 
#head(twoyearDataNumeric)
#view(twoyearDataNumeric) 

##Run parallel analysis  
#Use fa = pc because the article recommends finding principal components (pc) instead of principal factors (fa) 
twoyearParallel <- fa.parallel(twoyearDataNumeric, fa = "pc", use = "pairwise")
#use = "pairwise" is a way to deal with missing data

#Run PCA
twoyearPca <- princomp(twoyearDataNumeric, na.rm = T) 
#Assumption: parallel analysis gave me 16 factors, PCA gave me 5 based on delineating paper

#EFA
#FIRST FACTOR
twoyearFa1 <- fa(r = twoyearDataNumeric, 
                   nfactors = 1, 
                   fm = "ml",
                   residuals = TRUE)
                 #missing = TRUE,
                 #impute = "mean") 

#How to deal with missing values
twoyearFa1 <- fa(r = twoyearDataNumeric, 
                 nfactors = 1, 
                 fm = "ml",
                 residuals = TRUE,
                 missing = TRUE,
                 impute = "mean") 

#evaluate loadings (need to be presence of >3 clear primary loading, greater then .1 for each factor)
#save scores for each successive number of components 
twoyearfa1Scores <- twoyearFa1$scores

#TWO FACTORS (now rotate with geominT)
twoyearFa2 <- fa(r = twoyearDataNumeric,
                   nfactors = 2,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE,
                 missing = TRUE,
                 impute = "mean")
twoyearfa2Scores <- twoyearFa2$scores

#THREE FACTORS 
twoyearFa3 <- fa(r = twoyearDataNumeric,
                   nfactors = 3,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE,
                 missing = TRUE,
                 impute = "mean")
twoyearfa3Scores <- twoyearFa3$scores

#FOUR FACTORS 
twoyearFa4 <- fa(r = twoyearDataNumeric,
                   nfactors = 4,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE,
                 missing = TRUE,
                 impute = "mean")
twoyearfa4Scores <- twoyearFa4$scores

#FIVE FACTORS
twoyearFa5 <- fa(r = twoyearDataNumeric,
                   nfactors = 5,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE,
                 missing = TRUE,
                 impute = "mean")
twoyearfa5scores <- twoyearFa5$scores

#SIX FACTORS  
twoyearFa6 <- fa(r = twoyearDataNumeric,
                   nfactors = 6,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE,
                 missing = TRUE,
                 impute = "mean")
twoyearfa6Scores <- twoyearFa6$scores
#Assumption that at 6 factors and beyond it is no longer interpretable

#Names of 5 factors based on loadings: 


######STEP 6 CORRELATION ANALYSIS IN TIMELINE 2
#correlation between factor scores starting from the first factor at the top all the way to the last factor

#1 and 2 scores 
twoyearCOR1_2 <- cor.test(twoyearfa1Scores, twoyearfa2Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#2 and 3 scores
twoyearCOR2_3 <- cor.test(twoyearfa2Scores, twoyearfa3Scores,
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#3 and 4 scores
twoyearCOR3_4 <- cor.test(twoyearfa3Scores, twoyearfa4Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#4 and 5 scores 
twoyearCOR4_5 <- cor.test(twoyearfa4Scores, twoyearfa5Scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#####STEP 10: Check if factors are congruent 
factorBaseLoadings <- loadings(baselineFa5)  
factorOneLoadings <- loadings(oneyearFa5)
factorTwoLoadings <- loadings(twoyearFa5)

#Factor congruence on baseline and one year
congruenceBaseOne <- factor.congruence(factorBaseLoadings,factorOneLoadings)
congruenceBaseTwo <- factor.congruence(factorBaseLoadings,factorTwoLoadings)