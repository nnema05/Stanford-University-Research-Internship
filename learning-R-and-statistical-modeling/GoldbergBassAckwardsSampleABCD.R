#Goldberg Bass Ackward

#Goldberg Bass Ackward
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

#####STEP 1: MAKE THE DATA FROM ALL 3 TIMELINES NUMERIC
baselineDataNumeric <- select(baselineData, c(10:102, 106:114))
oneyearDataNumeric <- select(oneyearData, c(10:102, 106:114)) 
twoyearDataNumeric <- select(twoyearData, c(10:102, 106:114))

#####STEP 2: RUN PARALLEL ANALYSIS AND PCA TO DETERMINE NUMBER OF FACTORS

##Baseline
baselineParallel <- fa.parallel(baselineDataNumeric, fa = "pc")
baselinePca <- princomp(baselineDataNumeric) 

##One year
oneyearParallel <- fa.parallel(oneyearDataNumeric, fa = "pc")
oneyearPca <- princomp(oneyearDataNumeric)

##Two year
twoyearParallel <- fa.parallel(twoyearDataNumeric, fa = "pc", use = "pairwise")
twoyearPca <- princomp(twoyearDataNumeric, na.rm = T) 

#ASSUMPTION: Parallel analyses gives 16 factors and PCA gives 5 factors 


#####STEP 3: GOLDBERG BASSACKWARD 

##BASELINE
#change dataframe to matrix 
baselineMatrix <- data.matrix(baselineDataNumeric, rownames.force = TRUE)
#view(baselineMatrix)

##Run Goldberg
#1 factor
baselineGoldberg1 <- bassAckward(r = baselineMatrix, nfactors = 1,
                            fm = "pca", rotate = "geominT", 
                            use = "pairwise",
                            items = TRUE,
                            sort = TRUE, 
                            rownames.force = TRUE)
#See correlations: 
summary(baselineGoldberg1)
#See loadings 
baselineLoadings1 <- fa.lookup(baselineGoldberg1, dictionary=psychTools::bfi.dictionary)
baselineLoadings1

#2 factor
baselineGoldberg2 <- bassAckward(r = baselineMatrix, nfactors = 2,
                                 fm = "pca", rotate = "geominT", 
                                 use = "pairwise",
                                 items = TRUE,
                                 sort = TRUE)
#See correlations: 
summary(baselineGoldberg2)
#See loadings 
baselineLoadings2 <- fa.lookup(baselineGoldberg2, dictionary=psychTools::bfi.dictionary)
baselineLoadings2

#3 factor 
baselineGoldberg3 <- bassAckward(r = baselineMatrix, nfactors = 3,
                                 fm = "pca", rotate = "geominT", 
                                 use = "pairwise",
                                 items = TRUE,
                                 sort = TRUE)
#See correlations: 
summary(baselineGoldberg3)
#See loadings 
baselineLoadings3 <- fa.lookup(baselineGoldberg3, dictionary=psychTools::bfi.dictionary)
baselineLoadings3

#4 factor
baselineGoldberg4 <- bassAckward(r = baselineMatrix, nfactors = 4,
                                 fm = "pca", rotate = "geominT", 
                                 use = "pairwise",
                                 items = TRUE,
                                 sort = TRUE)
#See correlations: 
summary(baselineGoldberg4)
#See loadings 
baselineLoadings4 <- fa.lookup(baselineGoldberg4, dictionary=psychTools::bfi.dictionary)
baselineLoadings4

#5 factor 
baselineGoldberg5 <- bassAckward(r = baselineMatrix, nfactors = 5,
                                 fm = "pca", rotate = "geominT", 
                                 use = "pairwise",
                                 items = TRUE,
                                 sort = TRUE,
                                 rownames.force = TRUE)
#See correlations: 
summary(baselineGoldberg5)
#See loadings 
baselineLoadings5 <- fa.lookup(baselineGoldberg5, dictionary=psychTools::bfi.dictionary)
baselineLoadings5

#SEE SCORES
baselineGoldbergScores5 <- baselineGoldberg5$scores
baselineGoldbergScores5



##ONE YEAR
#Change dataframe to matrix 
oneyearMatrix <- data.matrix(oneyearDataNumeric, rownames.force = TRUE)
#view(oneyearMatrix)

##Run Goldberg
#1 factor
oneyearGoldberg1 <- bassAckward(r = oneyearMatrix, nfactors = 1,
                                fm = "pca", rotate = "geominT", 
                                use = "pairwise",
                                items = TRUE,
                                sort = TRUE)
#See correlations: 
summary(oneyearGoldberg1)
#See loadings 
oneyearLoadings1 <- fa.lookup(oneyearGoldberg1, dictionary=psychTools::bfi.dictionary)
oneyearLoadings1

#2 factor
oneyearGoldberg2 <- bassAckward(r = oneyearMatrix, nfactors = 2,
                                fm = "pca", rotate = "geominT", 
                                use = "pairwise",
                                items = TRUE,
                                sort = TRUE)
#See correlations: 
summary(oneyearGoldberg2)
#See loadings 
oneyearLoadings2 <- fa.lookup(oneyearGoldberg2, dictionary=psychTools::bfi.dictionary)
oneyearLoadings2

#3 factor 
oneyearGoldberg3 <- bassAckward(r = oneyearMatrix, nfactors = 3,
                                fm = "pca", rotate = "geominT", 
                                use = "pairwise",
                                items = TRUE,
                                sort = TRUE)
#See correlations: 
summary(oneyearGoldberg3)
#See loadings 
oneyearLoadings3 <- fa.lookup(oneyearGoldberg3, dictionary=psychTools::bfi.dictionary)
oneyearLoadings3

#4 factor
oneyearGoldberg4 <- bassAckward(r = oneyearMatrix, nfactors = 4,
                                fm = "pca", rotate = "geominT", 
                                use = "pairwise",
                                items = TRUE,
                                sort = TRUE)
#See correlations: 
summary(oneyearGoldberg4)
#See loadings 
oneyearLoadings4 <- fa.lookup(oneyearGoldberg4, dictionary=psychTools::bfi.dictionary)
oneyearLoadings4

#5 factor 
oneyearGoldberg5 <- bassAckward(r = oneyearMatrix, nfactors = 5,
                                fm = "pca", rotate = "geominT", 
                                use = "pairwise",
                                items = TRUE,
                                sort = TRUE)
#See correlations: 
summary(oneyearGoldberg5)
#See loadings 
oneyearLoadings5 <- fa.lookup(oneyearGoldberg5, dictionary=psychTools::bfi.dictionary)
oneyearLoadings5

#SEE SCORES
oneyearGoldbergScores5 <- oneyearGoldberg5$scores


##TWO YEAR
#Change dataframe to matrix 
twoyearMatrix <- data.matrix(twoyearDataNumeric, rownames.force = TRUE)
#view(twoyearMatrix)

##Run Goldberg
#1 factor
twoyearGoldberg1 <- bassAckward(r = twoyearMatrix, nfactors = 1,
                                fm = "pca", rotate = "geominT", 
                                use = "pairwise",
                                items = TRUE,
                                sort = TRUE)
#See correlations: 
summary(twoyearGoldberg1)
#See loadings 
twoyearLoadings1 <- fa.lookup(twoyearGoldberg1, dictionary=psychTools::bfi.dictionary)
twoyearLoadings1

#2 factor
twoyearGoldberg2 <- bassAckward(r = twoyearMatrix, nfactors = 2,
                                fm = "pca", rotate = "geominT", 
                                use = "pairwise",
                                items = TRUE,
                                sort = TRUE)
#See correlations: 
summary(twoyearGoldberg2)
#See loadings 
twoyearLoadings2 <- fa.lookup(twoyearGoldberg2, dictionary=psychTools::bfi.dictionary)
twoyearLoadings2

#3 factor 
twoyearGoldberg3 <- bassAckward(r = twoyearMatrix, nfactors = 3,
                                fm = "pca", rotate = "geominT", 
                                use = "pairwise",
                                items = TRUE,
                                sort = TRUE)
#See correlations: 
summary(twoyearGoldberg3)
#See loadings 
twoyearLoadings3 <- fa.lookup(twoyearGoldberg3, dictionary=psychTools::bfi.dictionary)
twoyearLoadings3

#4 factor
twoyearGoldberg4 <- bassAckward(r = twoyearMatrix, nfactors = 4,
                                fm = "pca", rotate = "geominT", 
                                use = "pairwise",
                                items = TRUE,
                                sort = TRUE)
#See correlations: 
summary(twoyearGoldberg4)
#See loadings 
twoyearLoadings4 <- fa.lookup(twoyearGoldberg4, dictionary=psychTools::bfi.dictionary)
twoyearLoadings4

#5 factor 
twoyearGoldberg5 <- bassAckward(r = twoyearMatrix, nfactors = 5,
                                fm = "pca", rotate = "geominT", 
                                use = "pairwise",
                                items = TRUE,
                                sort = TRUE)
#See correlations: 
summary(twoyearGoldberg5)
#See loadings 
twoyearLoadings5 <- fa.lookup(twoyearGoldberg5, dictionary=psychTools::bfi.dictionary)
twoyearLoadings5

#SEE SCORES
twoyearGoldbergScores5 <- twoyearGoldberg5$scores 
twoyearGoldbergScores5


