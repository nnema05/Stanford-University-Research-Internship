#Goldberg bass ackwards 
#####STEP 0: LIBRARIES
library(stats)
library(psych)
library(tidyverse)
library(GPArotation)
library(psychTools)

#####STEP 1: DATA NUMERIC 
baselineDataNumeric <- select(baselineData, c(10:102, 106:114))
oneyearDataNumeric <- select(oneyearData, c(10:102, 106:114)) 
twoyearDataNumeric <- select(twoyearData, c(10:102, 106:114))
print("DATA IS NUMERIC")

#####STEP 2: GOLDBERG BASSACKWARD 
##BASELINE
#change dataframe to matrix 
print("MAKING BASE DATA INTO MATRIX ")
baselineMatrix <- data.matrix(baselineDataNumeric, rownames.force = TRUE)
#view(baselineMatrix)
print("BASE DATA MATRIX DONE")

##Run Goldberg
#1 factor
print("BASS ACKWARDS F1")
baselineGoldberg1 <- bassAckward(r = baselineMatrix, nfactors = 1,
                                 fm = "pca", rotate = "geominT", 
                                 rownames.force = TRUE)
baselineGoldberg1$fa
baselineGoldberg1$labels(

                                 use = "pairwise",
                                 items = TRUE,
                                 sort = TRUE, 
                                 rownames.force = TRUE)
baselineGoldberg1 <- matrix(unlist(baselineGoldberg1))
baselineGoldberg1
baselineLoadings1 <- fa.lookup(baselineGoldberg1, dictionary = psych::describe(bfi))
baselineLoadings1

bestItems(baselineGoldberg1)
baselineGoldberg1(bfi.dictionary)

baselineLoadings1 <- fa.lookup(baselineGoldberg1, dictionary=psychTools::bfi.dictionary)
baselineLoadings1 <- baselineGoldberg1$fa
baselineLoadings1

x <- baselineGoldberg1$fa
print(x)

#colnames(baselineGoldberg1$fa) <-
   # paste0('q', 1:ncol(baselineGoldberg1$fa))


print(baselineLoadings1)

print("BASS ACKWARDS F1 DONE")

#See correlations: 
summary(baselineGoldberg1)
print("BASE CORRELATIONS F1")
#See loadings 
print("FINDING BASE LOADINGS F1")
print(baselineGoldberg1$fa)
print(baselineLoadings1)

#baselineLoadings1 <- fa.lookup(baselineGoldberg1, dictionary=psychTools::bfi.dictionary)
#baselineLoadings1 <- fa.sort(baselineGoldberg1$loadings)
#print(baselineLoadings1)

#5 factor
print("BASS ACKWARDS F5")
baselineGoldberg5 <- bassAckward(r = baselineMatrix, nfactors = 5,
                                 fm = "pca", rotate = "geominT", 
                                 use = "pairwise",
                                 items = TRUE,
                                 sort = TRUE, 
                                 rownames.force = TRUE)

print("BASS ACKWARDS F5")

#See correlations: 
summary(baselineGoldberg5)
print("BASE CORRELATIONS F5")
#See loadings 
print("FINDING BASE LOADINGS F5")
baselineLoadings5 <- fa.lookup(baselineGoldberg5, dictionary=psychTools::bfi.dictionary)
print(baselineLoadings5)

