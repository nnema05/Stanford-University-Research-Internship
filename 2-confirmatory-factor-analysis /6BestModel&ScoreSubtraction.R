
## This program figures out which model was the best fit best based on conventional fit thresholds like CFI, TLI, RMSEA, 
  # Configural (both models) ended up being the best fit with (configural 1 CFI .707, TLI: .703, RMSEA: .031, configural 2 CFI:.659 TLI: .655 RMSEA: .033).
  # Then we get the difference in the configural factor analysis scores 
      # difference between baseline scores and one year scores for each factor and p-factor
      # difference between baseline scores and two year scores for each factor and p-factor
      ## THIS SUBTRACTION IS IMPORTANT
        ## It allows us to find out what the change in mental health symptoms is between the baseline and the one year mark and two year mark

####STEP 0: INSTALL LIBRARIES 
library(sqldf)
library(psych)
library(lavaan)
library(tidyselect)
library(writexl)

#####STEP 1: FIT MEASURES

#5-factor
fitmeasures <- round(cbind(configural1=inspect(configural_fit1, 'fit.measures'),
            metric1=inspect(metric_fit1, 'fit.measures'),
            scalar1=inspect(scalar_fit1, 'fit.measures'),
            residual1=inspect(residual_fit1, 'fit.measures'),
            configural2=inspect(configural_fit2, 'fit.measures'),
            metric2=inspect(metric_fit2, 'fit.measures'),
            scalar2=inspect(scalar_fit2, 'fit.measures'),
            residual2=inspect(residual_fit2, 'fit.measures')),3)
print(fitmeasures)

#P-factor 
round(cbind(pFactorConfigural1=inspect(pFactorConfigural_fit1, 'fit.measures'),
            pFactorConfigural2=inspect(pFactorConfigural_fit2, 'fit.measures')), 3)



#####STEP 2: ANOVA
#Are the models significantly different?
anova(configural_fit1, metric_fit1)
anova(metric_fit1, scalar_fit1)
anova(scalar_fit1, residual_fit1)
anova(configural_fit2, metric_fit2)
anova(metric_fit2, scalar_fit2)
anova(scalar_fit2, residual_fit2)

##Which one fits the best based on CLI, RMSEA?


#####STEP 3: LOADINGS AND SCORES, BASED ON WHAT FITS 
#5 Factor Loadings
inspect(configural_fit1,what="std")$lambda
inspect(configural_fit2 ,what="std")$lambda
inspect(metric_fit1 ,what="std")$lambda
inspect(metric_fit2 ,what="std")$lambda
inspect(scalar_fit1 ,what="std")$lambda
inspect(scalar_fit2 ,what="std")$lambda
inspect(residual_fit1 ,what="std")$lambda
inspect(residual_fit2 ,what="std")$lambda

#p-factor loadings 
inspect(pFactorConfigural_fit1 ,what="std")$lambda
inspect(pFactorConfigural_fit2 ,what="std")$lambda

# 5 Factor Scores
configural1_scores <- lavPredict(configural_fit1)
configural2_scores <- lavPredict(configural_fit2)
#metric1_scores <- lavPredict(metric_fit1)
#metric2_scores <- lavPredict(metric_fit2)
#scalar1_scores <- lavPredict(scalar_fit1)
#scalar2_scores <- lavPredict(scalar_fit2)
#residual1_scores <- lavPredict(residual_fit1)
#residual2_scores <- lavPredict(residual_fit2)

#p-factor Scores
pFactorConfigural1_scores <- lavPredict(pFactorConfigural_fit1)
pFactorConfigural2_scores <- lavPredict(pFactorConfigural_fit2)


#####STEP 3: SUBTRACTION (SCORE COMPARISON)

# CONFIGURAL is the best fit

#Convert the matrix of scores as dataframe
DiffOneScoresDF <- as.data.frame(configural1_scores)
DiffTwoScoresDF <- as.data.frame(configural2_scores)

DiffOnePfactorScores <- as.data.frame(pFactorConfigural1_scores)
DiffTwoPfactorScores <- as.data.frame(pFactorConfigural2_scores)

#Subtraction for configural
#P-factor
DiffOnePfactorScores$DiffOnePFactor <- DiffOnePfactorScores$onePFactor - DiffOnePfactorScores$basePFactor
DiffTwoPfactorScores$DiffTwoPFactor <- DiffTwoPfactorScores$TwoPFactor - DiffTwoPfactorScores$basePFactor

#5-factor
DiffOneScoresDF$DiffOneFactor1 <- DiffOneScoresDF$oneFactor1 - DiffOneScoresDF$baseFactor1
DiffOneScoresDF$DiffOneFactor2 <- DiffOneScoresDF$oneFactor2 - DiffOneScoresDF$baseFactor2
DiffOneScoresDF$DiffOneFactor3 <- DiffOneScoresDF$oneFactor3 - DiffOneScoresDF$baseFactor3
DiffOneScoresDF$DiffOneFactor4 <- DiffOneScoresDF$oneFactor4 - DiffOneScoresDF$baseFactor4
DiffOneScoresDF$DiffOneFactor5 <- DiffOneScoresDF$oneFactor5 - DiffOneScoresDF$baseFactor5

DiffTwoScoresDF$DiffTwoFactor1 <-DiffTwoScoresDF$twoFactor1 - DiffTwoScoresDF$baseFactor1
DiffTwoScoresDF$DiffTwoFactor2 <-DiffTwoScoresDF$twoFactor2 - DiffTwoScoresDF$baseFactor2
DiffTwoScoresDF$DiffTwoFactor3 <-DiffTwoScoresDF$twoFactor3 - DiffTwoScoresDF$baseFactor3
DiffTwoScoresDF$DiffTwoFactor4 <-DiffTwoScoresDF$twoFactor4 - DiffTwoScoresDF$baseFactor4
DiffTwoScoresDF$DiffTwoFactor5 <-DiffTwoScoresDF$twoFactor5 - DiffTwoScoresDF$baseFactor5

#Make subject key into a dataframe 
baselineSubjectKeyDFone <- select(cbclDF1, c(basesubjectkey))
baselineSubjectKeyDFtwo <- select(cbclDF2, c(basesubjectkey))

#Combine Difference in Scores with Subject Keys

DiffOneScores <- cbind(baselineSubjectKeyDFone, DiffOnePfactorScores, DiffOneScoresDF)
DiffTwoScores <- cbind(baselineSubjectKeyDFtwo, DiffTwoPfactorScores, DiffTwoScoresDF)
print(DiffOneScores)
print(DiffTwoScores)

#Combine DiffOne and DiffTwo 

#DiffScores <- full_join(DiffOneScores, DiffTwoScores, 
                      #by = "basesubjectkey") 

#print(sum(is.na(DiffScores$DiffTwoPFactor)))
#4774
#print(sum(is.na(DiffScores$DiffTwoFactor1)))
#4774
#print(sum(is.na(DiffScores$DiffTwoFactor2)))
#4774
#print(sum(is.na(DiffScores$DiffTwoFactor3)))
#4774
#print(sum(is.na(DiffScores$DiffTwoFactor4)))
#4774
#print(sum(is.na(DiffScores$DiffTwoFactor5)))
#4774

#Write in Excel Sheet 
write.csv(DiffOneScores, "DiffOneScores.csv")
write.csv(DiffTwoScores, "DiffTwoScores.csv")





###Method 2
#Make subject key into a dataframe 
#oneSubjectKeyDFone <- select(cbclDF1, c(onesubjectkey))
#twoSubjectKeyDFtwo <- select(cbclDF2, c(twosubjectkey))

#Combine Difference in Scores with Subject Keys

#DiffOneScores <- cbind(oneSubjectKeyDFone, DiffOnePfactorScores,DiffOneScoresDF)
#DiffTwoScores <- cbind(twoSubjectKeyDFtwo, DiffTwoPfactorScores, DiffTwoScoresDF)

#Combine Diff One and Diff Two 

#testdf <- sqldf('SELECT onesubjectkey AS subjectkey 
                          #,CASE
                          #WHEN twosubjectkey IS NOT NULL THEN
                                #diff2.DiffTwoPFactor
                          #ELSE
                                #diff1.DiffOnePFactor
                         # END DiffPFactor
                         # ,CASE
                          #WHEN twosubjectkey IS NOT NULL THEN
                                #diff2.DiffTwoFactor1
                          #ELSE
                               # diff1.DiffOneFactor1
                         # END DiffFactor1 
                          #,CASE
                          #WHEN twosubjectkey IS NOT NULL THEN
                               # diff2.DiffTwoFactor2
                          #ELSE
                                #diff1.DiffOneFactor2
                         # END DiffFactor2 
                         # ,CASE
                          #WHEN twosubjectkey IS NOT NULL THEN
                                #diff2.DiffTwoFactor3
                         # ELSE
                                #diff1.DiffOneFactor3
                         # END DiffFactor3
                          #,CASE
                          #WHEN twosubjectkey IS NOT NULL THEN
                                #diff2.DiffTwoFactor4
                         # ELSE
                                #diff1.DiffOneFactor4
                         # END DiffFactor4
                          #,CASE
                         # WHEN twosubjectkey IS NOT NULL THEN
                               # diff2.DiffTwoFactor5
                          #ELSE
                               # diff1.DiffOneFactor5
                         # END DiffFactor5 
     # FROM DiffOneScores AS diff1
      #LEFT JOIN DiffTwoScores AS diff2 ON onesubjectkey=twosubjectkey')
      













