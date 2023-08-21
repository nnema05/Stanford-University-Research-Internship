#Sample ABCD - Goal # 2

install.packages(tidyverse)
library(tidyverse)
install.packages(tidyverse)
library(psych)
install.packages("writexl")
library("writexl")
install.packages(sqldf)
library(sqldf)
install.packages("GPArotation")
library("GPArotation")

######STEP 1: PARALLEL ANALYSIS, PCA AND EFA IN TIMELINE 1
#To run, data must be numeric. Out of 107 columns, 5:97, 99:107 are the numeric columns 
#Remove the columns that are not numeric 
oneyear.data.numeric <- select(oneyear.data, c(5:97, 99:107)) 

#Remove Comment below code if there is a need to troubleshoot
#str(oneyear.data.numeric) 
#head(oneyear.data.numeric)
#view(oneyear.data.numeric) 

##Run parallel analysis  
#Use fa = pc because the article recommends finding principal components (pc) instead of principal factors (fa) 
oneyear.parallel <- fa.parallel(oneyear.data.numeric, fa = "pc")

#Run PCA
oneyear.pca <- princomp(oneyear.data.numeric) 
#Assumption: parallel analysis gave me 16 factors, PCA gave me 5 based on delineating paper

#EFA
#FIRST FACTOR
oneyear.fa.1 <- fa(r = oneyear.data.numeric, 
                   nfactors = 1, 
                   fm = "ml",
                   residuals = TRUE) 

#evaluate loadings (need to be presence of >3 clear primary loading, greater then .1 for each factor)
#save scores for each successive number of components 
oneyearfa1Scores <- oneyear.fa.1$scores

#TWO FACTORS (now rotate with geomin)
oneyear.fa.2 <- fa(r = oneyear.data.numeric,
                   nfactors = 2,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE) 
oneyearfa2Scores <- oneyear.fa.2$scores

#THREE FACTORS 
oneyear.fa.3 <- fa(r = oneyear.data.numeric,
                   nfactors = 3,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa3Scores <- oneyear.fa.3$scores

#FOUR FACTORS 
oneyear.fa.4 <- fa(r = oneyear.data.numeric,
                   nfactors = 4,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa4Scores <- oneyear.fa.4$scores

#FIVE FACTORS
oneyear.fa.5 <- fa(r = oneyear.data.numeric,
                   nfactors = 5,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa5scores <- oneyear.fa.5$scores

#SIX FACTORS  
oneyear.fa.6 <- fa(r = oneyear.data.numeric,
                   nfactors = 6,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
oneyearfa6Scores <- oneyear.fa.6$scores
#Assumption that at 6 factors and beyond it is no longer interpretable

#Names of 5 factors based on loadings: 


######STEP 2 CORRELATION ANALYSIS IN TIMELINE 1
#correlation between factor scores starting from the first factor at the top all the way to the last factor

#1 and 2 scores 
oneyear.cor.1_2 <- cor.test(oneyear.fa.1.scores, oneyear.fa.2.scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#2 and 3 scores
oneyear.cor.2_3 <- cor.test(oneyear.fa.2.scores, oneyear.fa.3.scores,
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#3 and 4 scores
oneyear.cor.3_4 <- cor.test(oneyear.fa.3.scores, oneyear.fa.4.scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:

#4 and 5 scores 
oneyear.cor.4_5 <- cor.test(oneyear.fa.4.scores, oneyear.fa.5.scores, 
                             method = "pearson")
#correlation coefficient or r: 
#p-value:



######STEP 3: PARALLEL ANALYSIS, PCA AND EFA IN TIMELINE 2
#To run, data must be numeric. Out of 107 columns, 5:97, 99:107 are the numeric columns 
#Remove the columns that are not numeric 
twoyear.data.numeric <- select(twoyear.data, c(5:97, 99:107)) 

#Remove Comment below code if there is a need to troubleshoot
#str(twoyear.data.numeric) 
#head(twoyear.data.numeric)
#view(twoyear.data.numeric) 

##Run parallel analysis  
#Use fa = pc because the article recommends finding principal components (pc) instead of principal factors (fa) 
twoyear.parallel <- fa.parallel(twoyear.data.numeric, fa = "pc")

#Run PCA
twoyear.pca <- princomp(twoyear.data.numeric) 
#Assumption: parallel analysis gave me 16 factors, PCA gave me 5 based on delineating paper

#EFA
#FIRST FACTOR
twoyear.fa.1 <- fa(r = twoyear.data.numeric, 
                   nfactors = 1, 
                   fm = "ml",
                   residuals = TRUE) 

#evaluate loadings (need to be presence of >3 clear primary loading, greater then .1 for each factor)
#save scores for each successive number of components 
twoyearfa1Scores <- twoyear.fa.1$scores

#TWO FACTORS (now rotate with geominT)
twoyear.fa.2 <- fa(r = twoyear.data.numeric,
                   nfactors = 2,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE) 
twoyearfa2Scores <- twoyear.fa.2$scores

#THREE FACTORS 
twoyear.fa.3 <- fa(r = twoyear.data.numeric,
                   nfactors = 3,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
twoyearfa3Scores <- twoyear.fa.3$scores

#FOUR FACTORS 
twoyear.fa.4 <- fa(r = twoyear.data.numeric,
                   nfactors = 4,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
twoyearfa4Scores <- twoyear.fa.4$scores

#FIVE FACTORS
twoyear.fa.5 <- fa(r = twoyear.data.numeric,
                   nfactors = 5,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
twoyearfa5scores <- twoyear.fa.5$scores

#SIX FACTORS  
twoyear.fa.6 <- fa(r = twoyear.data.numeric,
                   nfactors = 6,
                   fm = "ml",
                   rotate = "geominT",
                   residuals = TRUE)
twoyearfa6Scores <- twoyear.fa.6$scores
#Assumption that at 6 factors and beyond it is no longer interpretable

#Names of 5 factors based on loadings: 


######STEP 4 CORRELATION ANALYSIS IN TIMELINE 2
#correlation between factor scores starting from the first factor at the top all the way to the last factor

#1 and 2 scores 
twoyear.cor.1_2 <- cor.test(twoyear.fa.1.scores, twoyear.fa.2.scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#2 and 3 scores
twoyear.cor.2_3 <- cor.test(twoyear.fa.2.scores, twoyear.fa.3.scores,
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#3 and 4 scores
twoyear.cor.3_4 <- cor.test(twoyear.fa.3.scores, twoyear.fa.4.scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:

#4 and 5 scores 
twoyear.cor.4_5 <- cor.test(twoyear.fa.4.scores, twoyear.fa.5.scores, 
                            method = "pearson")
#correlation coefficient or r: 
#p-value:


#####STEP 5: SUBTRACTION OF FACTOR SCORES ONEYEAR- BASELINE

#convert matrix baselinefa5scores to dataframe
baselinefa5ScoresDF <- as.data.frame(baselinefa5scores)
#make subjectkey from baseline into a dataframe
baselineSubjectKeyDF <-select(baseline.data, c(subjectkey))
#combine both dataframes into one data frame with subjectkeys and factor scores
baselineScoresDF <- cbind(baselineSubjectKeyDF, baselinefa5ScoresDF)

##Repeat the above steps for oneyear
#convert matrix oneyearfa5Scores to dataframe
oneyearfa5ScoresDF <- as.data.frame(oneyearfa5Scores)
#make subjectkey from baseline into a dataframe
oneyearSubjectKeyDF <- select(oneyear.data, c(subjectkey))
#combine both dataframes into one data frame with subjectkeys and factor scores
oneyearScoresDF <- cbind(oneyearSubjectKeyDF, oneyearfa5ScoresDF)

#Join and Subtract 

BaseTimeline1Diff <- sqldf("SELECT subjectkey, baselineScoresDF.Factor1 as baselineScoresFactor1
                                      , oneyearScoresDF.Factor1 as oneyearScoresFactor1
                                      , oneyearScoresDF.Factor1 - baselineScoresDF.Factor1 as diff1
                                      , baselineScoresDF.Factor2 as baselineScoresFactor2
                                      , oneyearScoresDF.Factor2 as oneyearScoresFactor2
                                      , oneyearScoresDF.Factor2 - baselineScoresDF.Factor2 as diff2
                                      , baselineScoresDF.Factor3 as baselineScoresFactor3
                                      , oneyearScoresDF.Factor3 as oneyearScoresFactor3
                                      , oneyearScoresDF.Factor3 - baselineScoresDF.Factor3 as diff3
                                      , baselineScoresDF.Factor4 as baselineScoresFactor4
                                      , oneyearScoresDF.Factor4 as oneyearScoresFactor4
                                      , oneyearScoresDF.Factor4 - baselineScoresDF.Factor4 as diff4
                                      , baselineScoresDF.Factor5 as baselineScoresFactor5
                                      , oneyearScoresDF.Factor5 as oneyearScoresFactor5
                                      , oneyearScoresDF.Factor5 - baselineScoresDF.Factor5 as diff5
                         FROM baselineScoresDF
                         OUTER JOIN oneyearScoresDF USING(subjectkey)")

##put in excel 

write_xlsx(BaseTimeline1Diff, "BaseTimeline1Diff.xlsx")

#####STEP 5: SUBTRACTION OF FACTOR SCORES TWOYEAR-  BASELINE

#convert matrix twoyearfa5Scores to dataframe
twoyearfa5ScoresDF <- as.data.frame(twoyearfa5Scores)
#make subjectkey from baseline into a dataframe
twoyearSubjectKeyDF <- select(twoyear.data, c(subjectkey))
#combine both dataframes into one data frame with subjectkeys and factor scores
twoyearScoresDF <- cbind(twoyearSubjectKeyDF, twoyearfa5ScoresDF)

#Join and Subtract 
BaseTimeline2Diff <- sqldf("SELECT subjectkey, baselineScoresDF.Factor1 as baselineScoresFactor1
                                      , twoyearScoresDF.Factor1 as twoyearScoresFactor1
                                      , twoyearScoresDF.Factor1 - baselineScoresDF.Factor1 as diff1
                                      , baselineScoresDF.Factor2 as baselineScoresFactor2
                                      , twoyearScoresDF.Factor2 as twoyearScoresFactor2
                                      , twoyearScoresDF.Factor2 - baselineScoresDF.Factor2 as diff2
                                      , baselineScoresDF.Factor3 as baselineScoresFactor3
                                      , twoyearScoresDF.Factor3 as twoyearScoresFactor3
                                      , twoyearScoresDF.Factor3 - baselineScoresDF.Factor3 as diff3
                                      , baselineScoresDF.Factor4 as baselineScoresFactor4
                                      , twoyearScoresDF.Factor4 as twoyearScoresFactor4
                                      , twoyearScoresDF.Factor4 - baselineScoresDF.Factor4 as diff4
                                      , baselineScoresDF.Factor5 as baselineScoresFactor5
                                      , twoyearScoresDF.Factor5 as twoyearScoresFactor5
                                      , twoyearScoresDF.Factor5 - baselineScoresDF.Factor5 as diff5
                         FROM baselineScoresDF
                         OUTER JOIN twoyearScoresDF USING(subjectkey)")

##put in excel 
write_xlsx(BaseTimeline2Diff, "BaseTimeline2Diff.xlsx")







